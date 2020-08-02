library(tidyverse)
library(tidycensus)
library(sf)
library(leaflet)
library(leafem)
library(mapedit)

#https://github.com/r-spatial/mapedit
#https://www.r-spatial.org/r/2017/01/30/mapedit_intro.html#selecting-regions

options(scipen = 999, digits = 4, tigris_use_cache=TRUE)

theme_set(theme_void())

#urban <- get_acs(geography = "urban area", variables = "B19013_001", geometry = TRUE)
#counties <- get_acs(geography = "county", variables = "B19013_001", geometry = TRUE)
#counties <- get_acs(geography = "county", variables = "B19013_001", geometry = TRUE)
#cities <- tigris::core_based_statistical_areas(cb = TRUE, resolution = "500k")
blocks <- get_decennial(year = 2010, state = "PA", county = "Allegheny County", 
                        variables = "P001001",
                        geography = "block", geometry = TRUE)

tracts <- get_decennial(year = 2010, state = "PA", county = "Allegheny County", 
                        variables = "P001001",
                        geography = "tract", geometry = TRUE) %>% 
  st_transform(crs = "WGS84")


# urban_pgh <- urban %>% 
#   filter(str_detect(NAME, "Pittsburgh")) %>% 
#   mutate(geography = "Census-defined urbanized area")
# 
# cities_pgh <- cities %>% 
#   filter(NAME == "Pittsburgh, PA") %>% 
#   mutate(geography = "core_based_statistical_area")
# 
# allegheny_county <- counties %>% 
#   filter(NAME == "Allegheny County, Pennsylvania") %>% 
#   mutate(geography = "County")

pgh_official_boundary <- st_read("data/Pittsburgh_City_Boundary-shp") %>% 
  mutate(geography = "City boundary") %>% 
  st_transform(crs = "WGS84") %>% 
  st_cast("POLYGON") %>% 
  filter(FID != 7)

pgh_official_boundary %>% 
  ggplot() +
  geom_sf(aes(fill = as.factor(FID)))

# allegheny_blocks <- blocks %>% 
#   filter(str_detect(NAME, "Allegheny County")) %>% 
#   mutate(geography = "block")

tracts %>% 
  ggplot() +
  geom_sf(color = "grey", size = .1, fill = NA) +
  geom_sf(data = pgh_official_boundary, color = "yellow", fill = NA)

tracts %>% 
  bind_rows(pgh_official_boundary) %>% 
  ggplot() +
  geom_sf()

tracts %>% 
  st_bbox()

tracts %>% 
  attributes()

tracts %>% 
  st_crs()

pgh_official_boundary %>% 
  st_crs()

bounds <- pgh_official_boundary %>% st_bbox() %>% unlist()

names(bounds) <- NULL

#bounds <- c(-80.36087, 40.19435 , -79.68885, 40.67493)

leaflet() %>% 
  #addTiles() %>% 
  addPolygons(data = tracts,
              weight = 1, 
              stroke = TRUE,
              fillOpacity= 0,
              layerId = ~GEOID) %>%
  addPolygons(data = pgh_official_boundary,
              weight = 2,
              stroke = TRUE,
              color = "yellow")

(lf <- leaflet(
   options=
     leafletOptions(
       worldCopyJump = FALSE#,
  #     crs=leafletCRS(
  #       crsClass="L.Proj.CRS",
  #       code='EPSG:2163',
  #       proj4def='+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs',
  #       resolutions = c(65536, 32768, 16384, 8192, 4096, 2048,1024, 512, 256, 128)
       )
  ) %>%
    fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
    setMaxBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
    addPolygons(data = pgh_official_boundary,
                weight = 2,
                stroke = TRUE,
                color = "yellow") %>% 
    addPolygons(data = tracts,
                weight = 1, 
                stroke = TRUE,
                fillOpacity= 0,
                layerId = ~GEOID)
    
)

#test
selectMap(
  lf,
  styleFalse = list(weight = .5,
                    fillOpacity = 0),
  styleTrue = list(weight = 4,
                   fillOpacity = .3)
)


# select tracts
selected_tracts <- selectMap(
  lf,
  styleFalse = list(weight = .5,
                    fillOpacity = 0),
  styleTrue = list(weight = 4,
                   fillOpacity = .3)
)

selected_tracts %>% 
  rename(GEOID = id) %>% 
  write_csv("data/selected_tracts.csv")

tracts %>% 
  inner_join(selected_tracts, by = c("GEOID" = "id")) %>% 
  #filter(selected == TRUE) %>% 
  ggplot() +
  geom_sf(aes(fill = selected)) +
  geom_sf(data = pgh_official_boundary, fill = NA, color = "yellow", size = 2)
