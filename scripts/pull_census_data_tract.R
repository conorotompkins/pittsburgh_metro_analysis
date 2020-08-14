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

tract_demographics <- get_decennial(year = 2010, state = "PA", county = "Allegheny County", 
                                     variables = vars_demo, summary_var = "P003001",
                                     geography = "tract", geometry = FALSE)

tigris_tracts <- tigris::tracts(year = 2010, state = "PA", county = "Allegheny") 


tigris_tracts %>% 
  st_crs()

tigris_tracts <- tigris_tracts %>% 
  select(GEOID10, ALAND10) %>% 
  rename(GEOID = GEOID10,
         ALAND_square_meters = ALAND10)


tigris_tracts <- tigris_tracts %>% 
  st_drop_geometry() %>% 
  as_tibble()

tigris_tracts %>% 
  filter(is.na(ALAND_square_meters))


tigris_tracts %>%
  ggplot(aes(ALAND_square_meters)) +
  geom_boxplot() +
  theme_bw()

tigris_tracts %>% 
  anti_join(tract_demographics)


tract_demographics <- tract_demographics %>% 
  mutate(pct_demographic = value / summary_value) %>% 
  mutate(pct_demographic = case_when(is.nan(pct_demographic) ~ 0,
                                     !is.nan(pct_demographic) ~ pct_demographic)) %>% 
  rename(total_population = summary_value) %>% 
  pivot_wider(id_cols = c(GEOID, NAME, total_population), 
              names_from = variable, names_prefix = "pct_",
              values_from = pct_demographic) %>% 
  select(-NAME)

tract_demographics <- tract_demographics %>% 
  left_join(tigris_tracts) %>% 
  select(-ALAND_square_meters)



### housing
vars_housing <- c(units_owned_loan = "H011002",
                  units_owned_entire = "H011003",
                  units_rented = "H011004")

census_vars %>% 
  #filter(str_detect(label, "units")) %>% 
  filter(name == "H011001")

census_vars %>% 
  semi_join(enframe(vars_housing), by = c("name" = "value"))

census_housing <- get_decennial(geography = "tract", 
                                variables = vars_housing,
                                state = "PA", 
                                county = "Allegheny", 
                                geometry = FALSE,
                                summary_var = "H011001")

tract_housing <- census_housing %>% 
  rename(total_population_housed = summary_value) %>% 
  mutate(pct_housing_var = value / total_population_housed) %>% 
  mutate(pct_housing_var = case_when(is.nan(pct_housing_var) ~ 0,
                                     !is.nan(pct_housing_var) ~ pct_housing_var)) %>% 
  pivot_wider(id_cols = c(GEOID, NAME, total_population_housed), 
              names_from = variable, values_from = pct_housing_var,
              names_prefix = "pct_") %>%
  select(-NAME)

glimpse(tract_housing)  


#load lodes data
tract_lodes <- read_csv("data/pa_lodes_data_tracts.csv", col_types = cols(.default = "c")) %>% 
  select(-c(state, year)) %>% 
  mutate(across(workers:jobs, as.numeric))

glimpse(tract_lodes)

#combine census data

census_combined <- tract_housing %>% 
  left_join(tract_demographics, by = c("GEOID")) %>% 
  left_join(tract_lodes, by = c("GEOID")) %>% 
  left_join(tigris_tracts) %>% 
  replace_na(list(workers = 0, jobs = 0)) %>% 
  mutate(ALAND_square_km = (ALAND_square_meters / 1000) / 1000,
         #updated density calculation to per km
         housed_population_density_pop_per_square_km = total_population_housed / ALAND_square_km,
         housed_population_density_pop_per_square_km = case_when(is.nan(housed_population_density_pop_per_square_km) ~ 0,
                                        !is.nan(housed_population_density_pop_per_square_km) ~ housed_population_density_pop_per_square_km)) %>% 
  select(-contains("ALAND"))


glimpse(census_combined)

tracts_geo <- tigris::tracts(state = "PA", county = "Allegheny", cb = TRUE) 

tracts_geo %>% 
  left_join(census_combined) %>%
  ggplot() +
  geom_sf(aes(fill = housed_population_density_pop_per_square_km), color = NA) +
  scale_fill_viridis_c()

census_combined %>% 
  count(GEOID, sort = TRUE)

census_combined %>% 
  write_csv("data/combined_census_data_tract.csv")
