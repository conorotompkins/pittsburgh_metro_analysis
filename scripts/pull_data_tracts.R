library(tidyverse)
library(tidycensus)
library(sf)

theme_set(theme_void())

urban <- get_acs(geography = "urban area", variables = "B19013_001", geometry = TRUE)
counties <- get_acs(geography = "county", variables = "B19013_001", geometry = TRUE)
counties <- get_acs(geography = "county", variables = "B19013_001", geometry = TRUE)
cities <- tigris::core_based_statistical_areas(cb = TRUE, resolution = "500k")
tracts <- get_acs(state = "Pennsylvania", county = "Allegheny", geography = "tract", variables = "B19013_001", geometry = TRUE)


urban_pgh <- urban %>% 
  filter(str_detect(NAME, "Pittsburgh")) %>% 
  mutate(type = "Census-defined urbanized area")

cities_pgh <- cities %>% 
  filter(NAME == "Pittsburgh, PA") %>% 
  mutate(type = "core_based_statistical_area")

allegheny_county <- counties %>% 
  filter(NAME == "Allegheny County, Pennsylvania") %>% 
  mutate(type = "County")

pgh_official_boundary <- st_read("data/Pittsburgh_City_Boundary-shp") %>% 
  mutate(type = "City boundary") %>% 
  st_transform(crs = "NAD83")

allegheny_tracts <- tracts %>% 
  filter(str_detect(NAME, "Allegheny County")) %>% 
  mutate(type = "tract")

st_crs(cities_pgh)[1]

geo_list <- list(cities_pgh, urban_pgh, allegheny_county, allegheny_tracts, pgh_official_boundary)

geo_list %>% 
  map(st_crs, .id = "type") %>% 
  map(1)

combined <- geo_list %>% 
  bind_rows()

glimpse(cities)

glimpse(urban)

cities %>% 
  filter(str_detect(NAME, ", PA$")) %>% 
  mutate(pgh_flag = str_detect(NAME, "Pittsburgh")) %>% 
  ggplot() +
  geom_sf(aes(fill = pgh_flag))



combined %>% 
  ggplot() +
  geom_sf(aes(color = type), alpha = 0) +
  facet_wrap(~type) +
  theme_void()

pgh_official_boundary_separate <- pgh_official_boundary %>% 
  st_cast("POLYGON") %>% 
  filter(FID != 7)

pgh_official_boundary_separate %>% 
  ggplot() +
  geom_sf(aes(fill = as.factor(FID)))

pgh_official_boundary_separate_1 <- pgh_official_boundary_separate %>% 
  filter(FID == 1)

pgh_official_boundary_separate_2 <- pgh_official_boundary_separate %>% 
  filter(FID == 2)

pgh_official_boundary_separate_3 <- pgh_official_boundary_separate %>% 
  filter(FID == 3)

pgh_official_boundary_separate_4 <- pgh_official_boundary_separate %>% 
  filter(FID == 4)

pgh_official_boundary_separate_5 <- pgh_official_boundary_separate %>% 
  filter(FID == 5)

pgh_official_boundary_separate_6 <- pgh_official_boundary_separate %>% 
  filter(FID == 6)

pgh_official_boundary_separate_7 <- pgh_official_boundary_separate %>% 
  filter(FID == 7)

pgh_official_boundary_separate_8 <- pgh_official_boundary_separate %>% 
  filter(FID == 8)




st_join(allegheny_tracts, pgh_official_boundary_separate, 
        join = st_covered_by) %>% 
  ggplot() +
  geom_sf(aes(fill = type.y), size = .1) +
  geom_sf(data = pgh_official_boundary, 
          alpha = 0, color = "yellow") +
  theme_void()

st_join(allegheny_tracts, pgh_official_boundary_separate, 
        join = st_intersects) %>% 
  ggplot() +
  geom_sf(aes(fill = type.y), size = .1) +
  geom_sf(data = pgh_official_boundary, 
          alpha = 0, color = "yellow") +
  theme_void()

st_join(allegheny_tracts, pgh_official_boundary_separate, 
        join = st_intersects) %>% 
  ggplot() +
  geom_sf(aes(fill = type.y), size = .1) +
  geom_sf(data = pgh_official_boundary, 
          alpha = 0, color = "yellow") +
  theme_void()

pgh_boundary_list <- list(pgh_official_boundary_separate_1, 
                          pgh_official_boundary_separate_2)

st_join(allegheny_tracts, pgh_official_boundary_separate_1,
        join = st_covered_by, left = TRUE) %>% 
  ggplot() +
  geom_sf(aes(fill = type.y), size = .1) +
  geom_sf(data = pgh_official_boundary, alpha = 0, color = "yellow")

st_filter(allegheny_county, pgh_official_boundary, .predicate = st_intersects) %>% 
  ggplot() +
  geom_sf()

allegheny_county_cave <- allegheny_county %>% 
  st_difference(pgh_official_boundary_separate_1) %>% 
  st_difference(pgh_official_boundary_separate_2) %>% 
  st_difference(pgh_official_boundary_separate_3) %>%
  st_difference(pgh_official_boundary_separate_4) %>% 
  st_difference(pgh_official_boundary_separate_5) %>% 
  st_difference(pgh_official_boundary_separate_6) %>% 
  #st_difference(pgh_official_boundary_separate_7) %>% 
  st_difference(pgh_official_boundary_separate_8) %>% 
  select(geometry) %>% 
  mutate(type = "allegheny_cave")

allegheny_county_cave %>%
  ggplot() +
  geom_sf(fill = "red")

pgh_official_boundary_separate_1 %>% 
  #st_difference(allegheny_county) %>% 
  ggplot() +
  geom_sf()

city_tracts <- st_difference(allegheny_tracts, allegheny_county_cave) %>% 
   mutate(type = "city")

# city_tracts_within <- st_filter(allegheny_tracts, pgh_official_boundary, .predicate = st_within)
# city_tracts_covered_by <- st_filter(allegheny_tracts, pgh_official_boundary, .predicate = st_covered_by)

city_tracts %>%  
  ggplot() +
  geom_sf(aes(fill = GEOID), show.legend = FALSE) +
  geom_sf(data = pgh_official_boundary, color = "yellow", size = 2, alpha = 0) +
  theme(panel.background = element_rect(fill = "black"))

df_city_tracts <- city_tracts %>% 
  st_drop_geometry() %>% 
  select(GEOID) %>% 
  mutate(type = "city") %>% 
  as_tibble()

df_allegheny_tracts <- allegheny_tracts %>% 
  st_drop_geometry() %>% 
  select(GEOID) %>% 
  mutate(type = "county") %>% 
  as_tibble()

non_city_tracts <- df_allegheny_tracts %>% 
  left_join(df_city_tracts, by = "GEOID") %>% 
  select(GEOID, contains("type")) %>% 
  filter(is.na(type.y)) %>% 
  select(GEOID)

allegheny_tracts %>% 
  semi_join(non_city_tracts) %>% 
  ggplot() +
  geom_sf()

non_city_tracts <- st_drop_geometry(city_tracts) %>% 
  select(GEOID) %>%
  left_join(st_drop_geometry(allegheny_tracts) %>% select(GEOID) %>% mutate(type = "county"))

non_city_tracts <- allegheny_tracts %>% 
  semi_join(non_city_tracts) %>% 
  mutate(type = "non_city")

non_city_tracts %>% 
  ggplot() +
  geom_sf() +
  geom_sf(data = pgh_official_boundary, color = "red", alpha = 0)

city_tracts %>% 
  bind_rows(non_city_tracts) %>% 
  ggplot() +
  geom_sf(data = allegheny_tracts, fill = "black") +
  geom_sf(aes(fill = type)) +
  geom_sf(data = pgh_official_boundary, color = "yellow", alpha = 0)

combined <- city_tracts %>% 
  bind_rows(non_city_tracts) %>% 
  st_drop_geometry() %>% 
  select(GEOID, type) %>% 
  as_tibble()

non_city_tracts_2 <- allegheny_tracts %>% 
  st_drop_geometry() %>% 
  anti_join(st_drop_geometry(city_tracts), by = "GEOID") %>% 
  as_tibble()

allegheny_tracts %>% 
  semi_join(non_city_tracts_2, by = "GEOID") %>% 
  ggplot() +
  geom_sf()

st_filter(allegheny_tracts, city_tracts, .predicate = st_equals) %>% 
  ggplot() +
  geom_sf()


