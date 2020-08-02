library(tidyverse)
library(sf)
library(leaflet)

options(scipen = 999, digits = 4, tigris_use_cache = TRUE)

full_predictions <- read_csv("output/full_prediction_binary.csv", col_types = cols(.default = "c")) 

tract_info <- read_csv("data/combined_census_data_tract.csv", col_types = cols(.default = "c")) %>% 
  mutate(across(total_population_housed:jobs, as.numeric)) %>% 
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


#leaflet
bounds <- tracts %>% st_bbox() %>% unlist()

names(bounds) <- NULL

prediction_palette <- colorNumeric(palette = "viridis", domain = tract_pred$.pred_city)

labels <- sprintf(
  "GEOID: %s<br/>Percent sure it is in the city: %g
  <br/>Total population: %g
  <br/>Population density: %g",
  tract_pred$GEOID, tract_pred$.pred_city, tract_pred$total_population, tract_pred$population_density
) %>% lapply(htmltools::HTML)

map <- leaflet(tract_pred) %>% 
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>% 
  addPolygons(#data = tract_pred,
              weight = .5,
              color = "grey",
              fillColor = ~prediction_palette(.pred_city),
              fillOpacity = .8,
              #
              group = "tracts",
              #
              highlightOptions = highlightOptions(weight = 3,
                                                  bringToFront = TRUE),
              #
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addLegend(pal = prediction_palette, values = ~.pred_city, opacity = 0.7, title = "% city",
            position = "bottomright") %>% 
  addPolylines(data = pgh_official_boundary,
               weight = 5,
               stroke = TRUE,
               color = "black",
               
               #
               
               group = "city boundary") %>% 
  addLayersControl(
    baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
    overlayGroups = c("tracts", "city boundary"),
    options = layersControlOptions(collapsed = FALSE)
  )



#%>% 
  # setView(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
  # fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>% 
  # setMaxBounds(bounds[1], bounds[2], bounds[3], bounds[4])

map  
