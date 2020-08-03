library(tidyverse)
library(hrbrthemes)

options(scipen = 999, digits = 4, tigris_use_cache = TRUE)

theme_set(theme_ipsum())

full_predictions <- read_csv("output/full_prediction_percent.csv", 
                             col_types = cols(.default = "c")) %>% 
  select(GEOID, matches("^.pred")) %>% 
  mutate(across(matches(".pred_"), as.numeric)) %>% 
  mutate(across(where(is.numeric), round, digits = 2))

tract_info <- read_csv("data/combined_census_data_tract.csv", 
                       col_types = cols(.default = "c")) %>% 
  mutate(across(total_population_housed:jobs, as.numeric)) %>% 
  mutate(across(where(is.numeric), round, digits = 2))

tract_combined <- full_predictions %>% 
  left_join(tract_info) %>% 
  rename(.pred_non_city = `.pred_non-city`)

tract_combined %>% 
  select(GEOID, matches("^.pred")) %>% 
  pivot_longer(cols = matches("^.pred"), names_to = "pred_type", values_to = "prediction") %>% 
  ggplot(aes(prediction, fill = pred_type, color = pred_type)) +
  geom_density(alpha = .5)

tract_combined %>% 
  ggplot(aes(population_density, .pred_city)) +
  geom_point()

city_tracts <- read_csv("data/selected_tracts.csv", col_types = cols("c", "l")) %>% 
  filter(selected == TRUE) %>% 
  rename(GEOID = id)

tract_combined %>% 
  select(GEOID, .pred_city, total_population_housed:jobs) %>% 
  left_join(city_tracts) %>% 
  mutate(type = case_when(selected == TRUE ~ "city",
                          is.na(selected) ~ "non_city")) %>% 
  select(-GEOID) %>% 
  pivot_longer(cols = total_population_housed:jobs, names_to = "predictor_var", values_to = "value") %>% 
  ggplot(aes(value, .pred_city, color = type)) +
  geom_point(alpha = .5) +
  facet_wrap(~predictor_var, scales = "free_x") +
  scale_y_percent()
