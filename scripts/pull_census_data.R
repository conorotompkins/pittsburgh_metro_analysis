library(tidyverse)
library(tidycensus)
library(sf)

options(scipen = 999, digits = 4, tigris_use_cache = TRUE)

#theme_set(theme_void())

### pull census data
census_vars <- load_variables(2010, "sf1", cache = TRUE)

census_vars %>% 
  #filter(str_detect(label, "Hispanic")) %>%
  filter(concept == "RACE") %>% 
  filter(str_detect(label, "alone"))

census_vars %>% 
  filter(name == "P003001")

vars_demo <- c(white = "P003002", 
               black = "P003003", 
               asian = "P003005", 
               hispanic = "P005010")

census_vars %>% 
  semi_join(enframe(vars_demo), by = c("name" = "value"))

blocks_demographics <- get_decennial(year = 2010, state = "PA", county = "Allegheny County", 
                                     variables = vars_demo, summary_var = "P003001",
                                     geography = "block", geometry = FALSE)

tigris_blocks <- tigris::blocks(year = 2010, state = "PA", county = "Allegheny") %>% 
  select(GEOID10, ALAND10) %>% 
  rename(GEOID = GEOID10,
         ALAND = ALAND10)

# tigris_blocks %>% 
#   left_join(blocks_demographics %>% select(GEOID, summary_value)) %>% 
#   mutate(population_density = (summary_value / ALAND) * 100000,
#          population_density = case_when(is.nan(population_density) ~ 0,
#                                         !is.nan(population_density) ~ population_density)) %>% 
#   ggplot() +
#   geom_sf(aes(fill = population_density), color = NA) +
#   scale_fill_viridis_c()

tigris_blocks <- tigris_blocks %>% 
  st_drop_geometry() %>% 
  as_tibble()

tigris_blocks %>% 
  filter(is.na(ALAND))


# tigris_blocks %>% 
#   ggplot(aes(ALAND)) +
#   geom_boxplot() +
#   theme_bw()

blocks_demographics %>% 
  anti_join(tigris_blocks)

tigris_blocks %>% 
  anti_join(blocks_demographics)


blocks_demographics <- blocks_demographics %>% 
  mutate(pct_demographic = value / summary_value) %>% 
  mutate(pct_demographic = case_when(is.nan(pct_demographic) ~ 0,
                                     !is.nan(pct_demographic) ~ pct_demographic)) %>% 
  rename(total_population = summary_value) %>% 
  pivot_wider(id_cols = c(GEOID, NAME, total_population), 
              names_from = variable, names_prefix = "pct_",
              values_from = pct_demographic) %>% 
  select(-NAME)

blocks_demographics <- blocks_demographics %>% 
  left_join(tigris_blocks) %>% 
  mutate(population_density = (total_population / ALAND) * 100000,
         population_density = case_when(is.nan(population_density) ~ 0,
                                        !is.nan(population_density) ~ population_density)) %>% 
  select(-ALAND)

  

### housing
vars_housing <- c(units_owned_loan = "H011002",
                  units_owned_entire = "H011003",
                  units_rented = "H011004")

census_vars %>% 
  #filter(str_detect(label, "units")) %>% 
  filter(name == "H011001")

census_vars %>% 
  semi_join(enframe(vars_housing), by = c("name" = "value"))

census_housing <- get_decennial(geography = "block", 
                                variables = vars_housing,
                                state = "PA", 
                                county = "Allegheny", 
                                geometry = FALSE,
                                summary_var = "H011001")

blocks_housing <- census_housing %>% 
  rename(total_population_housed = summary_value) %>% 
  mutate(pct_housing_var = value / total_population_housed) %>% 
  mutate(pct_housing_var = case_when(is.nan(pct_housing_var) ~ 0,
                                     !is.nan(pct_housing_var) ~ pct_housing_var)) %>% 
  pivot_wider(id_cols = c(GEOID, NAME, total_population_housed), 
              names_from = variable, values_from = pct_housing_var) %>%
  select(-NAME)

glimpse(census_housing)  


#load lodes data
lodes <- read_csv("data/pa_lodes_data.csv", col_types = cols(.default = "c")) %>% 
  select(-c(state, year)) %>% 
  mutate(across(residents:jobs, as.numeric))

glimpse(lodes)

#combine census data

census_combined <- blocks_housing %>% 
  left_join(blocks_demographics, by = c("GEOID")) %>% 
  left_join(lodes, by = c("GEOID")) %>% 
  replace_na(list(residents = 0, jobs = 0))

glimpse(census_combined)

census_combined %>% 
  count(GEOID, sort = TRUE)

census_combined %>% 
  write_csv("data/combined_census_data.csv")
