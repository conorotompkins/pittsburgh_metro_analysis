library(tidyverse)
library(tidycensus)
library(sf)
library(leaflet)
library(hrbrthemes)

options(scipen = 999, digits = 4, tigris_use_cache = TRUE)

full_predictions <- read_csv("output/full_prediction_percent.csv", 
                             col_types = cols(.default = "c")) 

tract_info <- read_csv("data/combined_census_data_tract.csv", 
                       col_types = cols(.default = "c")) %>% 
  mutate(across(total_population_housed:housed_population_density_pop_per_square_km, as.numeric)) %>% 
  mutate(across(where(is.numeric), round, digits = 2))

full_predictions_small <- full_predictions %>% 
  select(GEOID, mean_city) %>% 
  mutate(GEOID = as.character(GEOID),
         mean_city = as.double(mean_city),
         mean_city = round(mean_city, 2)) %>% 
  left_join(tract_info)


tracts <- get_decennial(year = 2010, state = "PA", county = "Allegheny County", 
                        variables = "P001001",
                        geography = "tract", geometry = TRUE) %>% 
  st_transform(crs = "WGS84")

pgh_official_boundary <- st_read("data/Pittsburgh_City_Boundary-shp") %>% 
  mutate(geography = "City boundary") %>% 
  st_transform(crs = "WGS84") %>% 
  st_cast("POLYGON") %>% 
  filter(FID != 7)

tract_pred <- tracts %>% 
  left_join(full_predictions_small)

threshhold_map <- seq(.1, .9, by = .1) %>% 
  set_names() %>% 
  map_df(~mutate(tracts %>% 
                   left_join(full_predictions_small), flag_city = mean_city >= .x), .id = "threshhold") %>%
  mutate(threshhold = as.double(threshhold)) %>% 
  arrange(desc(threshhold))
