library(tidyverse)
library(tidymodels)
library(janitor)
library(tidycensus)
library(sf)

options(scipen = 999, digits = 4, tigris_use_cache = TRUE)


set.seed(1234)



city_tracts <- read_csv("data/selected_tracts.csv", col_types = cols("c", "l")) %>% 
  filter(selected == TRUE) %>% 
  rename(GEOID = id)

glimpse(city_tracts)

census_combined <- read_csv("data/combined_census_data_tract.csv", col_types = cols(.default = "c")) %>%
  left_join(city_tracts) %>% 
  mutate(type = case_when(selected == TRUE ~ "city",
                          is.na(selected) ~ "non-city")) %>% 
  mutate(across(total_population_housed:jobs, as.numeric)) %>% 
  select(GEOID, type, everything()) %>%
  mutate(flag_void = (total_population == 0 &
                        total_population_housed == 0 &
                        jobs == 0 &
                        workers == 0) %>% as.factor) %>% 
  select(-c(flag_void, selected))

# census_combined %>% 
#   count(flag_void)

census_combined %>% 
  filter(total_population == 0,
         total_population_housed == 0,
         jobs == 0,
         workers == 0)

census_combined %>% 
  arrange(desc(population_density)) %>% 
  slice(1:10)

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
  #step_dummy(flag_void) %>% 
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

#logistic_model <- glm

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

predictions_training %>% 
  conf_mat(truth = type, estimate = .pred_class)

df_roc_curve <- fit(rf_workflow, juice(model_recipe_prep)) %>% 
  predict(juice(model_recipe_prep), type = "prob") %>% 
  bind_cols(juice(model_recipe_prep)) %>% 
  mutate(type = as.factor(type)) %>% 
  roc_curve(truth = type, .pred_city)

df_roc_curve %>% 
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

predictions_testing %>% 
  conf_mat(truth = type, estimate = .pred_class)


#### map full results
tracts <- get_decennial(year = 2010, state = "PA", county = "Allegheny County", 
                        variables = "P001001",
                        geography = "tract", geometry = TRUE)

full_predictions <- fit(rf_workflow, bake(model_recipe_prep, census_combined)) %>% 
  predict(bake(model_recipe_prep, census_combined), type = "prob") %>% 
  bind_cols(bake(model_recipe_prep, census_combined)) %>% 
  mutate(type = as.factor(type))

full_predictions %>% 
  write_csv("output/full_prediction_percent.csv")

full_predictions_binary <- fit(rf_workflow, bake(model_recipe_prep, census_combined)) %>% 
  predict(bake(model_recipe_prep, census_combined)) %>% 
  bind_cols(bake(model_recipe_prep, census_combined)) %>% 
  mutate(type = as.factor(type),
         correct = type == .pred_class)

full_predictions_binary %>% 
  write_csv("output/full_prediction_binary.csv")

top_misses <- full_predictions %>% 
  select(GEOID, .pred_city, type) %>% 
  filter(type == "non_city") %>% 
  arrange(desc(.pred_city)) %>% 
  slice(1:10)

county <- tracts %>% 
  summarize()

pgh_official_boundary <- st_read("data/Pittsburgh_City_Boundary-shp") %>% 
  mutate(geography = "City boundary") %>% 
  st_transform(crs = "NAD83") %>% 
  st_cast("POLYGON") %>% 
  filter(FID != 7)


tracts %>% 
  semi_join(top_misses) %>% 
  #mutate(flag = GEOID == "420035138002019") %>% 
  #filter(flag == TRUE) %>% 
  ggplot() +
  geom_sf(fill = "black", color = NA) +
  geom_sf(data = pgh_official_boundary, color = "yellow", alpha = 0) +
  geom_sf(data = county, alpha = 0) +
  geom_sf_text(aes(label = GEOID)) +
  #scale_fill_manual(values = c("black", "red")) +
  theme_void()

glimpse(full_predictions)

full_predictions_small <- full_predictions %>% 
  select(GEOID, type, contains(".pred")) %>% 
  pivot_longer(cols = contains(".pred"))

prediction_pct_map <- tracts %>% 
  select(GEOID) %>% 
  left_join(full_predictions_small, by = "GEOID") %>% 
  filter(!is.na(name)) %>% 
  ggplot() +
  geom_sf(aes(fill = value), color = NA) +
  geom_sf(data = pgh_official_boundary, alpha = 0, color = "black") +
  geom_sf(data = pgh_official_boundary, alpha = 0, color = "yellow", linetype = 2) +
  facet_wrap(~name) +
  scale_fill_viridis_c() +
  theme_void()

prediction_pct_map %>% 
  ggsave(filename = "output/prediction_pct_map.pdf", 
         width = 12, height = 12, dpi = 300)

tracts %>% 
  select(GEOID) %>% 
  left_join(full_predictions, by = "GEOID") %>% 
  mutate(certainty = abs(.pred_city - `.pred_non-city`)) %>% 
  ggplot() +
  geom_sf(aes(fill = certainty), color = NA) +
  geom_sf(data = pgh_official_boundary, alpha = 0, color = "black") +
  geom_sf(data = pgh_official_boundary, alpha = 0, color = "yellow", linetype = 2) +
  scale_fill_viridis_c() +
  theme_void()
  


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

prediction_binary_map %>% 
  ggsave(filename = "output/prediction_binary_map.pdf", 
         width = 12, height = 12, dpi = 300)

