library(tidyverse)
library(tidymodels)
library(janitor)
library(tidycensus)
library(sf)

set.seed(1234)

block_types <- read_csv("data/city_non_city_block_geoids.csv", col_types = cols(.default = "c"))

glimpse(block_types)

census_combined <- read_csv("data/combined_census_data.csv", col_types = cols(.default = "c")) %>% 
  mutate(across(total_population_housed:pct_hispanic, as.numeric)) %>% 
  left_join(block_types) %>% 
  select(GEOID, type, everything()) %>% 
  filter(total_population > 0)

census_combined %>% 
  arrange(desc(total_population)) %>% 
  slice(1:100) %>% 
  View()

glimpse(census_combined)

census_combined %>% 
  filter(is.na(GEOID))

census_combined %>% 
  janitor::tabyl(type)

split <- initial_split(census_combined, strata = type)
training <- training(split)
testing <- testing(split)


model_recipe <- recipe(type ~ ., data = training) %>% 
  update_role(GEOID, new_role = "id") %>% 
  step_normalize(all_predictors())

model_recipe_prep <- model_recipe %>% 
  prep(strings_as_factors = FALSE)

model_recipe_prep %>% 
  summary()

model_recipe_prep %>% 
  juice() %>% 
  glimpse()

model_recipe_prep %>% 
  bake(testing) %>% 
  glimpse()

model_recipe_prep %>% 
  bake(testing) %>% 
  filter(is.na(GEOID))


ranger_model <- rand_forest(trees = 1000, mode = "classification") %>%
  set_engine("ranger", importance = "impurity")


rf_workflow <- workflow() %>% 
  add_recipe(model_recipe_prep) %>% 
  add_model(ranger_model)

### predict on training
predictions_training <- fit(rf_workflow, juice(model_recipe_prep)) %>% 
  predict(juice(model_recipe_prep)) %>% 
  bind_cols(juice(model_recipe_prep)) %>% 
  mutate(type = as.factor(type))

predictions_training %>% 
  glimpse()

predictions_training %>% 
  metrics(truth = type, estimate = .pred_class)

fit(rf_workflow, juice(model_recipe_prep)) %>% 
  predict(juice(model_recipe_prep), type = "prob") %>% 
  bind_cols(juice(model_recipe_prep)) %>% 
  mutate(type = as.factor(type)) %>% 
  roc_curve(truth = type, .pred_city) %>% 
  autoplot()

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


predictions_testing %>% 
  metrics(truth = type, estimate = .pred_class)


#### map full results
blocks <- get_decennial(year = 2010, state = "PA", county = "Allegheny County", 
                        variables = "P001001",
                        geography = "block", geometry = TRUE)

full_predictions <- fit(rf_workflow, bake(model_recipe_prep, census_combined)) %>% 
  predict(bake(model_recipe_prep, census_combined), type = "prob") %>% 
  bind_cols(bake(model_recipe_prep, census_combined)) %>% 
  mutate(type = as.factor(type))

full_predictions_binary <- fit(rf_workflow, bake(model_recipe_prep, census_combined)) %>% 
  predict(bake(model_recipe_prep, census_combined)) %>% 
  bind_cols(bake(model_recipe_prep, census_combined)) %>% 
  mutate(type = as.factor(type),
         correct = type == .pred_class)


glimpse(full_predictions)

full_predictions_small <- full_predictions %>% 
  select(GEOID, type, contains(".pred")) %>% 
  pivot_longer(cols = contains(".pred"))

pgh_official_boundary <- st_read("data/Pittsburgh_City_Boundary-shp") %>% 
  mutate(type = "City boundary") %>% 
  st_transform(crs = "NAD83")

prediction_pct_map <- blocks %>% 
  select(GEOID) %>% 
  left_join(full_predictions_small, by = "GEOID") %>% 
  filter(!is.na(name)) %>% 
  ggplot() +
  geom_sf(aes(fill = value), color = NA) +
  geom_sf(data = pgh_official_boundary, alpha = 0, color = "yellow") +
  facet_wrap(~name) +
  scale_fill_viridis_c() +
  theme_void()

prediction_pct_map %>% 
  ggsave(filename = "output/prediction_pct_map.png", 
         width = 12, height = 12, dpi = 300)


full_predictions_binary_small <- full_predictions_binary %>% 
  select(GEOID, type, .pred_class, correct)

prediction_binary_map <- blocks %>% 
  select(GEOID) %>% 
  left_join(full_predictions_binary_small, by = "GEOID") %>% 
  ggplot() +
  geom_sf(aes(fill = correct), color = NA) +
  geom_sf(data = pgh_official_boundary, alpha = 0, color = "yellow") +
  scale_fill_viridis_d(option = 1) +
  theme_void()

prediction_binary_map %>% 
  ggsave(filename = "output/prediction_binary_map.png", 
         width = 12, height = 12, dpi = 300)

