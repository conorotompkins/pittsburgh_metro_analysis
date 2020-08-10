library(tidyverse)
library(tidycensus)
library(janitor)
library(lehdr)
library(tigris)
library(sf)
library(ggraph)
library(tidygraph)

options(tigris_use_cache = TRUE,
        scipen = 999,
        digits = 4)

pa_lodes_od <- grab_lodes(state = "pa", year = 2010, 
                                 lodes_type = "od", job_type = "JT00", 
                                 segment = "S000", state_part = "main", 
                                 agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year)


pa_counties <- tigris::counties(state = "PA") %>% 
  select(GEOID, NAME) %>% 
  arrange(GEOID)

pa_counties %>% 
  st_distance()

node_pos <- pa_counties %>% 
  mutate(centroid = map(geometry, st_centroid),
         x = map_dbl(centroid, 1),
         y = map_dbl(centroid, 2)) %>% 
  select(GEOID, NAME, x, y) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  clean_names() %>% 
  select(-c(name, geoid))



network_graph <- pa_lodes_od %>% 
  rename(workers = S000) %>% 
  select(w_county, h_county, workers) %>% 
  as_tbl_graph(directed = TRUE) %>% 
  activate(edges) %>% 
  filter(workers > 1000,
         !edge_is_loop())

#geo_layout <- layout_to_table(graph = network_graph, node_pos)

geo_layout <- create_layout(graph = network_graph,
                            layout = node_pos)

ggraph(geo_layout) +
  geom_sf(data = pa_counties, fill = NA) +
  geom_edge_fan(aes(edge_width = workers, edge_alpha = workers)) +
  geom_node_point(color = "red") +
  scale_edge_alpha(range = c(.1, .7)) +
  scale_edge_width(range = c(1, 3)) +
  theme_void()

ggplotly(p = network_graph)

network_graph %>% 
  plotly::

ggiris <- qplot(Petal.Width, Sepal.Length, data = iris, color = Species)
ggplotly(ggiris)
