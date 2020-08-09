library(tidyverse)
library(tidycensus)
library(sf)

theme_set(theme_void())

urban <- get_acs(geography = "urban area", variables = "B19013_001", geometry = TRUE)

counties <- get_acs(geography = "county", variables = "B19013_001", geometry = TRUE)

counties <- get_acs(geography = "county", variables = "B19013_001", geometry = TRUE)

cities <- tigris::core_based_statistical_areas(cb = TRUE, resolution = "500k")

blocks <- get_decennial(year = 2010, state = "PA", county = "Allegheny County", 
                        variables = "P001001",
                        geography = "block", geometry = TRUE)

blocks_municipality <- st_read("data/blockcodes2016/blockcodes2016.shp") %>% 
  filter(geo_name_c == "Allegheny") %>% 
  mutate(block_type = case_when(geo_name_1 == "Pittsburgh city" ~ "city",
                                geo_name_1 != "Pittsburgh city" ~ "non_city")) %>% 
  select(GEOID = GEOID10, geo_name_c, geo_name_1, block_type)

glimpse(blocks_municipality)

blocks_municipality %>%
  st_drop_geometry() %>% 
  count(geo_name_1, sort = TRUE) %>% 
  as_tibble()

blocks_municipality %>% 
  ggplot() +
  geom_sf(aes(fill = block_type), color = NA)


urban_pgh <- urban %>% 
  filter(str_detect(NAME, "Pittsburgh")) %>% 
  mutate(geography = "Census-defined urbanized area")

cities_pgh <- cities %>% 
  filter(NAME == "Pittsburgh, PA") %>% 
  mutate(geography = "core_based_statistical_area")

allegheny_county <- counties %>% 
  filter(NAME == "Allegheny County, Pennsylvania") %>% 
  mutate(geography = "County")

pgh_official_boundary <- st_read("data/Pittsburgh_City_Boundary-shp") %>% 
  mutate(geography = "City boundary") %>% 
  st_transform(crs = "NAD83")

allegheny_blocks <- blocks %>% 
  filter(str_detect(NAME, "Allegheny County")) %>% 
  mutate(geography = "block")

st_crs(cities_pgh)[1]

geo_list <- list(cities_pgh, urban_pgh, allegheny_county, allegheny_blocks, pgh_official_boundary)

geo_list %>% 
  map(st_crs, .id = "type") %>% 
  map(1)

combined <- geo_list %>% 
  bind_rows()

combined %>% 
  ggplot() +
  geom_sf(aes(color = geography), size = .1, alpha = 0) +
  facet_wrap(~geography) +
  theme_void()

pgh_official_boundary_separate <- pgh_official_boundary %>% 
  st_cast("POLYGON") %>% 
  filter(FID != 7)

pgh_official_boundary_separate %>% 
  ggplot() +
  geom_sf()

allegheny_blocks %>% 
  ggplot() +
  geom_sf(size = .1) +
  geom_sf(data = pgh_official_boundary_separate, alpha = 0, color = "yellow", size = 2)
  

city_blocks <- st_filter(allegheny_blocks, pgh_official_boundary_separate,
                         .predicate = st_covered_by) %>% 
  mutate(type = "city")

city_blocks %>% 
  ggplot() +
  geom_sf(size = .1) +
  geom_sf(data = pgh_official_boundary_separate, alpha = 0, color = "yellow", size = 1)

df_city_blocks <- city_blocks %>% 
  st_drop_geometry() %>% 
  select(GEOID, geography, type)

non_city_blocks <- allegheny_blocks %>% 
  anti_join(df_city_blocks, by = "GEOID") %>% 
  mutate(type = "non_city")

non_city_blocks %>% 
  ggplot() +
  geom_sf(size = .1) +
  geom_sf(data = pgh_official_boundary_separate, alpha = 0, color = "yellow", size = .5)

blocks_combined <- bind_rows(city_blocks, non_city_blocks)

blocks_combined %>% 
  ggplot() +
  geom_sf(aes(fill = type), color = NA) +
  geom_sf(data = pgh_official_boundary_separate, 
          alpha = 0, color = "yellow", size = .5, linetype = "dashed")

blocks_combined %>% 
  st_drop_geometry() %>% 
  select(GEOID, type) %>% 
  as_tibble() %>% 
  write_csv("data/city_non_city_block_geoids.csv")





         