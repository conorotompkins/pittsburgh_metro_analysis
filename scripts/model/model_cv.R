#cross-validated model

#set up environment
library(tidyverse)
library(tidymodels)
library(janitor)
library(tidycensus)
library(sf)
library(hrbrthemes)
library(GGally)

theme_set(theme_ipsum(base_size = 20))

options(scipen = 999, digits = 4, tigris_use_cache = TRUE)

set.seed(1234)

#goal is to create a classification model that can distinguish between tracts 
#that are in the city vs. not in the city

#load data about census tracts
tracts <- get_decennial(year = 2010, state = "PA", county = "Allegheny County", 
                        variables = "P001001",
                        geography = "tract", geometry = TRUE)

city_tracts <- read_csv("data/selected_tracts.csv", col_types = cols("c", "l")) %>% 
  filter(selected == TRUE)

glimpse(city_tracts)

pgh_official_boundary <- st_read("data/Pittsburgh_City_Boundary-shp") %>% 
  mutate(geography = "City boundary") %>% 
  st_transform(crs = "NAD83") %>% 
  st_cast("POLYGON") %>% 
  filter(FID != 7)

tracts %>% 
  left_join(city_tracts) %>% 
  mutate(type = case_when(selected == TRUE ~ "city",
                          is.na(selected) ~ "non_city")) %>% 
  ggplot() +
  geom_sf(aes(fill = type), size = .1) +
  geom_sf(data = pgh_official_boundary, color = "white", linetype = 2, alpha = 0) +
  theme_void()

all_data <- read_csv("data/combined_census_data_tract.csv", col_types = cols(.default = "c")) %>%
  left_join(city_tracts) %>% 
  mutate(type = case_when(selected == TRUE ~ "city",
                          is.na(selected) ~ "non_city")) %>% 
  mutate(across(pct_units_owned_loan:housed_population_density_pop_per_square_km, as.numeric)) %>% 
  select(GEOID, type, everything()) %>%
  select(-c(selected, total_population_housed))

glimpse(all_data)

#%>% 
#  select(-c(total_population, total_population_housed, pct_asian, pct_hispanic))

#eda
pairwise_plot <- all_data %>% 
  select(-c(GEOID)) %>% 
  ggpairs(aes(color = type)) +
  theme(axis.text = element_text(size = 8))

# pairwise_plot %>% 
#    ggsave(filename = "output/ggpairs_plot.png", height = 24, width = 24)

census_combined <- all_data %>% 
  select(GEOID, type, 
         pct_units_owned_loan, pct_units_owned_entire, pct_units_rented,
         housed_population_density_pop_per_square_km,
         pct_white, pct_black,
         workers, jobs)

glimpse(census_combined)

census_combined %>% 
  tabyl(type)

#bootstrap

tract_boot <- bootstraps(census_combined, strata = type, times = 100)

#split
# split <- initial_split(census_combined, prop = 3/4, strata = type)
# training <- training(split)
# validation <- mc_cv(training, prop = 3/4, times = 100, strata = type)
# testing <- testing(split)

#recipe
model_recipe <- recipe(type ~ ., data = census_combined) %>% 
  update_role(GEOID, new_role = "id") %>% 
  #step_dummy(flag_void) %>% 
  step_normalize(all_predictors())

model_recipe_prep <- model_recipe %>% 
  prep(strings_as_factors = FALSE)

model_recipe_prep %>% 
  summary()



#create model specifications
ranger_model <- rand_forest(trees = 1000, mode = "classification") %>%
  set_engine("ranger", importance = "impurity")

lm_model <- logistic_reg(mode = "classification") %>% 
  set_engine("glm")


#workflow
#lm
lm_workflow <- workflow() %>% 
  add_recipe(model_recipe_prep) %>% 
  add_model(lm_model)

lm_res <- lm_workflow %>% 
  fit_resamples(resamples = tract_boot) %>% 
  mutate(model = "lm")

lm_res %>% 
  collect_metrics()

#rf
rf_workflow <- workflow() %>% 
  add_recipe(model_recipe_prep) %>% 
  add_model(ranger_model)

rf_res <- rf_workflow %>% 
  fit_resamples(resamples = tract_boot,
                control = control_resamples(save_pred = TRUE)) %>% 
  mutate(model = "rf")

rf_res %>% 
  collect_metrics()

combined_res <- bind_rows(rf_res, lm_res)

combined_res %>% 
  unnest(.metrics) %>% 
  ggplot(aes(.estimate, color = model, fill = model)) +
  geom_density(alpha = .5) +
  facet_wrap(~.metric)

#rf does much better in general. i will choose that one

### predict on training
# predictions_training <- fit(rf_workflow, juice(model_recipe_prep)) %>% 
#   predict(juice(model_recipe_prep)) %>% 
#   bind_cols(juice(model_recipe_prep)) %>% 
#   mutate(type = as.factor(type))

#rf does much better on full training set
# predictions_training %>% 
#   metrics(truth = type, estimate = .pred_class)

#failures are false negatives
# predictions_training %>% 
#   conf_mat(truth = type, estimate = .pred_class)

#roc
# df_roc_curve <- fit(rf_workflow, juice(model_recipe_prep)) %>% 
#   predict(juice(model_recipe_prep), type = "prob") %>% 
#   bind_cols(juice(model_recipe_prep)) %>% 
#   mutate(type = as.factor(type)) %>% 
#   roc_curve(truth = type, .pred_city)
# 
# df_roc_curve %>% 
#   autoplot()

#variable importance
var_imp <- rf_workflow %>% 
  fit(juice(model_recipe_prep)) %>%
  pull_workflow_fit() %>% 
  vip::vi()

var_imp %>%
  mutate(Variable = fct_reorder(Variable, Importance)) %>% 
  ggplot(aes(Importance, Variable)) +
  geom_point() +
  theme_bw()

#test vi variance
test_vi_data <- juice(model_recipe_prep) %>% 
  mutate(set = 1) %>% 
  bind_rows(juice(model_recipe_prep) %>% mutate(set = 2)) %>% 
  bind_rows(juice(model_recipe_prep) %>% mutate(set = 3)) %>% 
  bind_rows(juice(model_recipe_prep) %>% mutate(set = 4)) %>% 
  bind_rows(juice(model_recipe_prep) %>% mutate(set = 5)) %>% 
  group_by(set) %>% 
  nest()

test_vi_data %>% 
  mutate(predictions = map(data, ~fit(rf_workflow, data = .x)),
         predictions = map(predictions, pull_workflow_fit),
         var_imp = map(predictions, vip::vi)) %>% 
  unnest(var_imp) %>% 
  mutate(set = as.factor(set)) %>% 
  mutate(Variable = fct_reorder(Variable, Importance, .fun = median)) %>% 
  ggplot(aes(Importance, Variable, color = set)) +
  geom_point()
#

### predict on testing
# predictions_testing <- fit(rf_workflow, bake(model_recipe_prep, testing)) %>% 
#   predict(bake(model_recipe_prep, testing)) %>% 
#   bind_cols(bake(model_recipe_prep, testing)) %>% 
#   mutate(type = as.factor(type))

#rf only does very slightly worse on test set
# predictions_testing %>% 
#   metrics(truth = type, estimate = .pred_class)

#failures are false positives this time
# predictions_testing %>% 
#   conf_mat(truth = type, estimate = .pred_class)


#map results



#pull full predictions out of bootstrap resamples
full_predictions <- rf_res %>% 
  collect_predictions() %>% 
  mutate(correct = type == .pred_class) %>%
  left_join(census_combined %>%
              mutate(.row = row_number()))

full_predictions %>% 
  count(type, GEOID) %>% 
  ggplot(aes(n, fill = type, color = type)) +
  geom_density(alpha = .3)

full_predictions_pct <- full_predictions %>% 
  group_by(GEOID) %>% 
  summarize(pct_correct = mean(correct),
            mean_city = mean(.pred_city),
            mean_non_city = mean(.pred_non_city),
            n = n())

# full_predictions_pct %>%
#   write_csv("output/full_prediction_percent.csv")

full_predictions_pct %>% 
  ggplot(aes(n)) +
  geom_histogram()

prediction_pct_correct_map <- tracts %>% 
  left_join(full_predictions_pct) %>% 
  ggplot() +
  geom_sf(aes(fill = pct_correct), size = NA) +
  geom_sf(data = pgh_official_boundary, alpha = 0, color = "black", size = 2) +
  geom_sf(data = pgh_official_boundary, alpha = 0, color = "yellow", size = .3) +
  scale_fill_viridis_c(labels = scales::percent) +
  theme_void()

prediction_pct_correct_map

# prediction_pct_correct_map %>% 
#   ggsave(filename = "output/prediction_pct_correct_map.png", width = 12, height = 12, dpi = 300)

prediction_pct_map <- tracts %>% 
  left_join(full_predictions_pct) %>% 
  ggplot() +
  geom_sf(aes(fill = mean_city), size = NA) +
  geom_sf(data = pgh_official_boundary, alpha = 0, color = "black", size = 2) +
  geom_sf(data = pgh_official_boundary, alpha = 0, color = "yellow", size = .3) +
  scale_fill_viridis_c(labels = scales::percent) +
  theme_void()

prediction_pct_map

# prediction_pct_map %>% 
#   ggsave(filename = "output/prediction_pct_map.png", width = 12, height = 12, dpi = 300)


#diagnostics

census_combined %>% 
  left_join(full_predictions_pct) %>% 
  pivot_longer(pct_units_owned_loan:jobs) %>% 
  ggplot(aes(value, pct_correct)) +
    geom_point(alpha = .3) +
    facet_wrap(~name, scales = "free_x")




#references
#https://juliasilge.com/blog/multinomial-volcano-eruptions/
#http://www.rebeccabarter.com/blog/2020-03-25_machine_learning/#split-into-traintest
#https://www.brodrigues.co/blog/2018-11-25-tidy_cv/
#https://agailloty.rbind.io/en/post/tidymodels/
#https://alison.rbind.io/post/2020-02-27-better-tidymodels/
#https://hansjoerg.me/2020/02/09/tidymodels-for-machine-learning/
#https://towardsdatascience.com/modelling-with-tidymodels-and-parsnip-bae2c01c131c
#https://www.benjaminsorensen.me/post/modeling-with-parsnip-and-tidymodels/
#https://rviews.rstudio.com/2019/06/19/a-gentle-intro-to-tidymodels/
