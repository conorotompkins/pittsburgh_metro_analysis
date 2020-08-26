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

tract_pred %>% 
  ggplot(aes(mean_city)) +
  geom_histogram()

tract_pred %>% 
  ggplot(aes(mean_city)) +
  stat_ecdf(geom = "step")

threshhold_map <- seq(.1, .5, by = .01) %>% 
  set_names() %>% 
  map_df(~mutate(tracts %>% 
        left_join(full_predictions_small), flag_city = mean_city >= .x), .id = "threshhold") %>%
  arrange(desc(threshhold)) %>% 
  mutate(threshhold_label = str_c("City probability >= ", threshhold, "%", sep = ""),
         threshhold_label = fct_inorder(threshhold))

threshhold_map %>% 
  filter(flag_city == TRUE)

threshhold_map %>% 
  filter(threshhold %in% c(.1, .3, .5)) %>% 
  group_by(flag_city, threshhold, threshhold_label) %>% 
  summarize() %>% 
  filter(flag_city == TRUE) %>% 
  st_convex_hull() %>% 
  ggplot() +
  geom_sf(data = filter(threshhold_map, threshhold %in% c(.1, .3, .5)) #%>% 
            #group_by(flag_city, threshhold) %>% 
            #summarize(), 
          ,aes(fill = flag_city), color = NA, alpha = .8) +
  geom_sf(data = pgh_official_boundary, alpha = 0, color = "black", size = 1) +
  geom_sf(data = pgh_official_boundary, alpha = 0, color = "yellow", size = .3) +
  geom_sf(color = "black", fill = NA, size = 1) +
  geom_sf(color = "white", fill = NA, linetype = 2, size = 1) +
  facet_wrap(~threshhold_label) +
  scale_fill_viridis_d() +
  theme_void()

library(gganimate)
library(gifski)

map <- threshhold_map %>% 
  group_by(flag_city, threshhold, threshhold_label) %>% 
  summarize() %>% 
  filter(flag_city == TRUE) %>% 
  st_convex_hull() %>% 
  ggplot() +
  geom_sf(data = threshhold_map %>% 
            group_by(flag_city, threshhold, threshhold_label) %>% 
            summarize(),
          aes(fill = flag_city), color = NA, alpha = .8) +
  geom_sf(data = pgh_official_boundary, alpha = 0, color = "black", size = 4) +
  geom_sf(data = pgh_official_boundary, alpha = 0, color = "yellow", size = 1) +
  geom_sf(color = "black", fill = NA, size = 1) +
  geom_sf(color = "white", fill = NA, linetype = 2, size = 1) +
  scale_fill_viridis_d() +
  labs(title = "City threshhold >= {previous_frame}%") +
  theme_void() +
  theme(title = element_text(size = 18))

# bad_anim <- map +
#   transition_states(threshhold)
# 
# anim_save(filename = "output/bad_animation.gif", bad_anim)

anim <- map +
  transition_manual(threshhold_label)

anim_save(filename = "output/animated_expanded_city_borders.gif", animation = anim,
          width = 1600, height = 1600, duration = 20, fps = 30, end_pause = 10)


index_change <- threshhold_map %>% 
  ungroup() %>% 
  st_drop_geometry() %>% 
  filter(mean_city >= threshhold) %>% 
  mutate(threshhold = as.numeric(threshhold)) %>% 
  select(threshhold, GEOID, total_population_housed:housed_population_density_pop_per_square_km) %>% 
  pivot_longer(cols = -c(threshhold, GEOID), names_to = "variable") %>% 
  mutate(variable = str_c("mean_", variable, sep = "")) %>% 
  group_by(threshhold, variable) %>% 
  summarize(value_mean = mean(value)) %>% 
  ungroup() %>% 
  arrange(desc(threshhold)) %>% 
  group_by(variable) %>% 
  mutate(var_index = first(value_mean),
         pct_change = (value_mean - var_index) / 100,
         pct_change = pct_change / 100) %>%
  ungroup()

library(ggrepel)
index_change %>% 
  group_by(variable) %>% 
  mutate(last_label = case_when(threshhold == min(threshhold) ~ variable,
                                threshhold != min(threshhold) ~ NA_character_)) %>% 
  ungroup() %>% 
  ggplot(aes(threshhold, pct_change, color = variable)) +
  geom_point(alpha = .3) +
  geom_smooth() +
  geom_label_repel(aes(x = .08, label = last_label)) +
  scale_x_percent(trans = "reverse") +
  scale_y_percent() +
  guides(color = FALSE) +
  labs(y = "Pct change indexed at first value") +
  theme_ipsum()
  
