library(tidyverse)
library(tidycensus)
library(sf)

theme_set(theme_void())

### pull census data
census_vars <- load_variables(2010, "sf1", cache = TRUE)

census_vars %>% 
  #filter(str_detect(label, "Hispanic")) %>%
  filter(concept == "RACE") %>% 
  filter(str_detect(label, "alone")) %>% 
  View()

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


blocks_demographics <- blocks_demographics %>% 
  mutate(pct_demographic = value / summary_value) %>% 
  mutate(pct_demographic = case_when(is.nan(pct_demographic) ~ 0,
                                     !is.nan(pct_demographic) ~ pct_demographic)) %>% 
  rename(total_population = summary_value) %>% 
  pivot_wider(id_cols = c(GEOID, NAME, total_population), 
              names_from = variable, names_prefix = "pct_",
              values_from = pct_demographic) %>% 
  select(-NAME)




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

#combine census data

census_combined <- blocks_housing %>% 
  left_join(blocks_demographics, by = c("GEOID"))

census_combined %>% 
  count(GEOID, sort = TRUE)

census_combined %>% 
  write_csv("data/combined_census_data.csv")
