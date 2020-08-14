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
  filter(selected == TRUE) %>% 
  rename(GEOID = id)

glimpse(city_tracts)

pgh_official_boundary <- st_read("data/Pittsburgh_City_Boundary-shp") %>% 
  mutate(geography = "City boundary") %>% 
  st_transform(crs = "NAD83") %>% 
  st_cast("POLYGON") %>% 
  filter(FID != 7)

tracts %>% 
  left_join(city_tracts) %>% 
  ggplot() +
  geom_sf(aes(fill = selected)) +
  geom_sf(data = pgh_official_boundary, color = "white", linetype = 2, alpha = 0) +
  theme_void()


census_combined <- read_csv("data/combined_census_data_tract.csv", col_types = cols(.default = "c")) %>%
  left_join(city_tracts) %>% 
  mutate(type = case_when(selected == TRUE ~ "city",
                          is.na(selected) ~ "non_city")) %>% 
  mutate(across(pct_units_owned_loan:housed_population_density_1k_per_km, as.numeric)) %>% 
  select(GEOID, type, everything()) %>%
  mutate(flag_void = (total_population == 0 &
                        housed_population_density_1k_per_km == 0 &
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

lm_workflow %>% 
  fit(training) %>% 
  pull_workflow_fit()


lm_res <- lm_workflow %>% 
  fit_resamples(resamples = validation) %>% 
  mutate(model = "lm")

lm_res %>% 
  collect_metrics()

lm_res %>% 
  unnest(.metrics) %>% 
  select(id, .metric, .estimate) %>% 
  View()
  #mutate(id = str_extract(id, "[:digit:]+") %>% parse_number) %>% 
  ggplot(aes(.estimate, color = .metric, fill = .metric)) +
  geom_density() +
  facet_wrap(~.metric, scales = "free_y")
  


rf_workflow <- workflow() %>% 
  add_recipe(model_recipe_prep) %>% 
  add_model(ranger_model)

rf_res <- rf_workflow %>% 
  fit_resamples(resamples = validation) %>% 
  mutate(model = "rf")

rf_res %>% 
  collect_metrics()

rf_res %>% 
  unnest(.metrics) %>% 
  #mutate(id = str_extract(id, "[:digit:]+") %>% parse_number) %>% 
  ggplot(aes(.estimate, color = .metric, fill = .metric)) +
  geom_density() +
  facet_wrap(~.metric, scales = "free_y")

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
