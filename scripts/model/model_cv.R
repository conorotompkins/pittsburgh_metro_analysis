#cross-validated model

#set up environment
library(tidyverse)
library(tidymodels)
library(janitor)
library(tidycensus)
library(sf)
library(hrbrthemes)

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

census_combined <- read_csv("data/combined_census_data_tract.csv", col_types = cols(.default = "c")) %>%
  left_join(city_tracts) %>% 
  mutate(type = case_when(selected == TRUE ~ "city",
                          is.na(selected) ~ "non_city")) %>% 
  mutate(across(pct_units_owned_loan:housed_population_density_pop_per_km, as.numeric)) %>% 
  select(GEOID, type, everything()) %>%
  mutate(flag_void = (total_population == 0 &
                        housed_population_density_pop_per_km == 0 &
                        jobs == 0 &
                        workers == 0) %>% as.factor) %>% 
  select(-c(flag_void, selected, total_population, total_population_housed, pct_asian, pct_hispanic))


#split
split <- initial_split(census_combined, prop = 3/4, strata = type)
training <- training(split)
validation <- mc_cv(training, prop = 3/4, times = 100, strata = type)
testing <- testing(split)

#recipe
model_recipe <- recipe(type ~ ., data = training) %>% 
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
  fit_resamples(resamples = validation) %>% 
  mutate(model = "lm")

lm_res %>% 
  collect_metrics()

#rf
rf_workflow <- workflow() %>% 
  add_recipe(model_recipe_prep) %>% 
  add_model(ranger_model)

rf_res <- rf_workflow %>% 
  fit_resamples(resamples = validation) %>% 
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
predictions_training <- fit(rf_workflow, juice(model_recipe_prep)) %>% 
  predict(juice(model_recipe_prep)) %>% 
  bind_cols(juice(model_recipe_prep)) %>% 
  mutate(type = as.factor(type))

#rf does much better on full training set
predictions_training %>% 
  metrics(truth = type, estimate = .pred_class)

#failures are false negatives
predictions_training %>% 
  conf_mat(truth = type, estimate = .pred_class)

#roc
df_roc_curve <- fit(rf_workflow, juice(model_recipe_prep)) %>% 
  predict(juice(model_recipe_prep), type = "prob") %>% 
  bind_cols(juice(model_recipe_prep)) %>% 
  mutate(type = as.factor(type)) %>% 
  roc_curve(truth = type, .pred_city)

df_roc_curve %>% 
  autoplot()

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

### predict on testing
predictions_testing <- fit(rf_workflow, bake(model_recipe_prep, testing)) %>% 
  predict(bake(model_recipe_prep, testing)) %>% 
  bind_cols(bake(model_recipe_prep, testing)) %>% 
  mutate(type = as.factor(type))

#rf only does very slightly worse on test set
predictions_testing %>% 
  metrics(truth = type, estimate = .pred_class)

#failures are false positives this time
predictions_testing %>% 
  conf_mat(truth = type, estimate = .pred_class)


#map results

#predict on full dataset
#probabilities
full_predictions <- fit(rf_workflow, bake(model_recipe_prep, census_combined)) %>% 
  predict(bake(model_recipe_prep, census_combined), type = "prob") %>% 
  bind_cols(bake(model_recipe_prep, census_combined)) %>% 
  mutate(type = as.factor(type))

full_predictions %>%
  write_csv("output/full_prediction_percent.csv")

full_predictions_small <- full_predictions %>% 
  select(GEOID, type, contains(".pred")) %>% 
  pivot_longer(cols = contains(".pred"))

prediction_pct_map <- tracts %>% 
  select(GEOID) %>% 
  left_join(full_predictions_small, by = "GEOID") %>% 
  mutate(name = case_when(name == ".pred_city" ~ "predicted_city",
                          name == ".pred_non_city" ~ "predicted_non_city")) %>% 
  filter(!is.na(name)) %>% 
  ggplot() +
  geom_sf(aes(fill = value), color = NA) +
  geom_sf(data = pgh_official_boundary, alpha = 0, color = "black") +
  geom_sf(data = pgh_official_boundary, alpha = 0, color = "yellow", linetype = 2) +
  facet_wrap(~name) +
  labs(fill = "Percent") +
  scale_fill_viridis_c() +
  theme_void()

prediction_pct_map

#binary predictions
full_predictions_binary <- fit(rf_workflow, bake(model_recipe_prep, census_combined)) %>% 
  predict(bake(model_recipe_prep, census_combined)) %>% 
  bind_cols(bake(model_recipe_prep, census_combined)) %>% 
  mutate(type = as.factor(type),
         correct = type == .pred_class)

full_predictions_binary_small <- full_predictions_binary %>% 
  select(GEOID, type, .pred_class, correct)

prediction_binary_map <- tracts %>% 
  select(GEOID) %>% 
  left_join(full_predictions_binary_small, by = "GEOID") %>% 
  ggplot() +
  geom_sf(aes(fill = correct), color = NA, alpha = .7) +
  geom_sf(data = pgh_official_boundary, alpha = 0, color = "black") +
  geom_sf(data = pgh_official_boundary, alpha = 0, color = "yellow", linetype = 2) +
  scale_fill_viridis_d() +
  theme_void()

prediction_binary_map