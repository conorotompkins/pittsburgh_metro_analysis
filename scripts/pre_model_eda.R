library(tidyverse)
library(GGally)
library(hrbrthemes)

options(scipen = 999, digits = 4, tigris_use_cache = TRUE)

theme_set(theme_ipsum())

city_tracts <- read_csv("data/selected_tracts.csv", col_types = cols("c", "l")) %>% 
  filter(selected == TRUE) %>% 
  rename(GEOID = id)

tract_info <- read_csv("data/combined_census_data_tract.csv", 
                       col_types = cols(.default = "c")) %>% 
  mutate(across(total_population_housed:jobs, as.numeric)) %>% 
  mutate(across(where(is.numeric), round, digits = 2)) %>% 
  left_join(city_tracts) %>% 
  mutate(type = case_when(selected == TRUE ~ "city",
                          is.na(selected) ~ "non_city")) %>% 
  select(-selected)

pairwise_plot <- tract_info %>% 
  select(-c(GEOID, total_population, pct_asian, pct_hispanic)) %>% 
  ggpairs(aes(color = type))

ggsave(pairwise_plot, filename = "output/pairwise_plot.png", width = 24, height = 24, dpi = 150)


