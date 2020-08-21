library(tidyverse)
library(tidycensus)
library(sf)
library(leaflet)

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
         mean_city = as.numeric(mean_city),
         mean_city = round(mean_city, digits = 2),
         mean_city = mean_city * 100) %>% 
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

prediction_palette <- colorNumeric(palette = "viridis", domain = tract_pred$mean_city)

labels <- sprintf(
  "GEOID: %s<br/>Average city classification: %g%%
  <br/>Total population: %g
  <br/>Housed population density per sq km: %g
  <br/>Percent white: %g%%
  <br/>Percent black: %g%%,
  <br/>Jobs: %g
  <br/>Workers: %g",
  tract_pred$GEOID, tract_pred$mean_city, 
  tract_pred$total_population, tract_pred$housed_population_density_pop_per_square_km, tract_pred$pct_white,
  tract_pred$pct_black, tract_pred$jobs, tract_pred$workers
) %>% lapply(htmltools::HTML)

map <- leaflet(tract_pred) %>% 
  setView(lat = 40.441606, lng = -80.010957, zoom = 10) %>% 
  addProviderTiles(providers$Stamen.Toner, 
                   group = "Toner (default)") %>%
  addTiles(group = "OSM") %>%
  addMapPane("Census tracts", zIndex = 400) %>% # shown below ames_circles               
  addMapPane("City boundary", zIndex = 490) %>% # shown above ames_lines 
  addPolygons(#data = tract_pred,
              weight = .5,
              color = "grey",
              fillColor = ~prediction_palette(mean_city),
              fillOpacity = .8,
              #
              group = "Census tracts",
              #
              highlightOptions = highlightOptions(weight = 3,
                                                  bringToFront = TRUE),
              #
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              
              #
              options = pathOptions(pane = "Census tracts")
              ) %>% 
  addLegend(pal = prediction_palette, 
            values = ~mean_city, 
            opacity = 0.7, 
            labFormat = labelFormat(suffix = "%"),
            title = "Average city classification %",
            position = "bottomright") %>% 
  addPolylines(data = pgh_official_boundary,
               weight = 10,
               stroke = TRUE,
               color = "black",
               
               #
               
               group = "City boundary",
               options = pathOptions(pane = "City boundary")
               ) %>% 
  addPolylines(data = pgh_official_boundary,
               weight = 2,
               stroke = TRUE,
               color = "yellow",
               
               #
               
               group = "City boundary",
               options = pathOptions(pane = "City boundary")
               ) %>% 
  addLayersControl(
    baseGroups = c("Toner (default)", "OSM"),
    overlayGroups = c("Census tracts", "City boundary"),
    options = layersControlOptions(collapsed = FALSE)
  )



map  
