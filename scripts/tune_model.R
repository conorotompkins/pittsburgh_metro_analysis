library(tidyverse)
library(tidymodels)
library(janitor)
library(tidycensus)
library(sf)
library(doParallel)
library(hrbrthemes)
library(vip)

theme_set(theme_ipsum())

set.seed(1234)

block_types <- read_csv("data/city_non_city_block_geoids.csv", col_types = cols(.default = "c"))

glimpse(block_types)

census_combined <- read_csv("data/combined_census_data.csv", col_types = cols(.default = "c")) %>%
  semi_join(block_types) %>% 
  mutate(across(total_population_housed:jobs, as.numeric)) %>% 
  left_join(block_types) %>% 
  select(GEOID, type, everything()) %>%
  mutate(flag_void = (total_population == 0 &
                        total_population_housed == 0 &
                        jobs == 0 &
                        residents == 0) %>% as.factor) %>% 
  select(-flag_void)

census_combined %>% 
  count(flag_void)

census_combined %>% 
  filter(total_population == 0,
         total_population_housed == 0,
         jobs == 0,
         residents == 0)

census_combined %>% 
  arrange(desc(total_population)) %>% 
  slice(1:100)

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

tune_spec <- rand_forest(mtry = tune(),
                         trees = 1000,
                         min_n = tune()) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_wf <- workflow() %>%
  add_recipe(model_recipe_prep) %>%
  add_model(tune_spec)

trees_folds <- vfold_cv(juice(model_recipe_prep))



doParallel::registerDoParallel()

tune_res <- tune_grid(
  tune_wf,
  resamples = trees_folds,
  grid = 10
)

tune_res

tune_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")



rf_grid <- grid_regular(
  mtry(range = c(1, 3)),
  min_n(range = c(1, 40)),
  levels = 5
)

regular_res <- tune_grid(
  tune_wf,
  resamples = trees_folds,
  grid = rf_grid
)

regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

best_auc <- select_best(regular_res, "roc_auc")

final_rf <- finalize_model(
  tune_spec,
  best_auc
)

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(type ~ .,
      data = juice(model_recipe_prep) %>% 
        select(-GEOID) %>% 
        mutate(type = as.factor(type))) %>%
  vip(num_features = 11, geom = "point")

#https://juliasilge.com/blog/sf-trees-random-tuning/
#http://www.rebeccabarter.com/blog/2020-03-25_machine_learning/
