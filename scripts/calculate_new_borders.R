library(tidyverse)
library(tidycensus)
library(sf)
library(leaflet)

options(scipen = 999, digits = 4, tigris_use_cache = TRUE)

full_predictions <- read_csv("output/full_prediction_percent.csv", 
                             col_types = cols(.default = "c")) 

tract_info <- read_csv("data/combined_census_data_tract.csv", 
                       col_types = cols(.default = "c")) %>% 
  mutate(across(total_population_housed:housed_population_density_1k_per_km, as.numeric)) %>% 
  mutate(across(where(is.numeric), round, digits = 2))

full_predictions_small <- full_predictions %>% 
  select(GEOID, contains(".pred")) %>% 
  mutate(GEOID = as.character(GEOID),
         across(contains(".pred"), as.numeric),
         across(contains(".pred"), round, digits = 2)) %>% 
  left_join(tract_info)

glimpse(full_predictions_small)

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

total <- filter_threshhold_list %>% 
  set_names() %>% 
  map_df(~filter(state_graph, coeff > .x), .id = "filter_threshhold") %>% 
  filter(!is.na(coeff),
         state != other_state) %>% 
  mutate(filter_threshhold = as.numeric(filter_threshhold))

threshhold_map <- seq(.1, .5, by = .01) %>% 
  set_names() %>% 
  map_df(~mutate(tracts %>% 
        left_join(full_predictions_small), flag_city = .pred_city >= .x), .id = "threshhold") %>%
  arrange(desc(threshhold)) %>% 
  mutate(threshhold = str_c("City probability >= ", threshhold, "%", sep = ""),
         threshhold = fct_inorder(threshhold))


threshhold_map %>% 
  group_by(flag_city, threshhold) %>% 
  summarize() %>% 
  filter(flag_city == TRUE) %>% 
  st_convex_hull() %>% 
  ggplot() +
  geom_sf(data = threshhold_map #%>% 
            #group_by(flag_city, threshhold) %>% 
            #summarize(), 
          ,aes(fill = flag_city), color = NA, alpha = .8) +
  geom_sf(data = pgh_official_boundary, color = "yellow", fill = NA) +
  geom_sf(color = "black", fill = NA, size = 1) +
  geom_sf(color = "white", fill = NA, linetype = 2, size = 1) +
  facet_wrap(~threshhold) +
  scale_fill_viridis_d() +
  theme_void()

library(gganimate)
library(gifski)

map <- threshhold_map %>% 
  group_by(flag_city, threshhold) %>% 
  summarize() %>% 
  filter(flag_city == TRUE) %>% 
  st_convex_hull() %>% 
  ggplot() +
  geom_sf(data = threshhold_map %>% 
            group_by(flag_city, threshhold) %>% 
            summarize(),
          aes(fill = flag_city), color = NA, alpha = .8) +
  geom_sf(data = pgh_official_boundary, color = "black", fill = NA) +
  geom_sf(color = "black", fill = NA, size = 1) +
  geom_sf(color = "white", fill = NA, linetype = 2, size = 1) +
  scale_fill_viridis_d() +
  labs(title = "{previous_frame}") +
  theme_void()

# bad_anim <- map +
#   transition_states(threshhold)
# 
# anim_save(filename = "output/bad_animation.gif", bad_anim)

anim <- map +
  transition_manual(threshhold)

anim_save(filename = "output/animated_expanded_city_borders.gif", animation = anim)
