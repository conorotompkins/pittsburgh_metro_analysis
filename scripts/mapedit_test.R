library(tidyverse)
library(tidycensus)
library(sf)
library(leaflet)
library(mapview)
library(mapedit)
library(shiny)
library(crosstalk)
library(albersusa)


spdf <- usa_composite() %>% st_as_sf()
pal <- colorNumeric(
  palette = "Blues",
  domain = spdf$pop_2014
)

bounds <- c(-125, 24 ,-75, 45)

(lf <- leaflet(
  options=
    leafletOptions(
      worldCopyJump = FALSE,
      crs=leafletCRS(
        crsClass="L.Proj.CRS",
        code='EPSG:2163',
        proj4def='+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs',
        resolutions = c(65536, 32768, 16384, 8192, 4096, 2048,1024, 512, 256, 128)
      ))) %>%
    fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
    setMaxBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
    mapview::addFeatures(
      data=spdf, weight = 1, color = "#000000",
      # adding group necessary for identification
      layerId = ~iso_3166_2,
      fillColor=~pal(pop_2014),
      fillOpacity=0.7,
      label=~stringr::str_c(name,' ', format(pop_2014, big.mark=",")),
      labelOptions= labelOptions(direction = 'auto')#,
      #highlightOptions = highlightOptions(
      #  color='#00ff00', bringToFront = TRUE, sendToBack = TRUE)
    )
)


# test out selectMap with albers example
selected_states <- selectMap(
  lf,
  styleFalse = list(weight = 1),
  styleTrue = list(weight = 4)
)

selectFeatures(drawing$finished)

read_delim("id selected
1 WA     TRUE
2 OR     TRUE
3 CA     TRUE
4 NV     TRUE
5 CO     TRUE
6 AZ     TRUE
7 UT     TRUE", delim = "\n")
