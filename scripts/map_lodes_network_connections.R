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
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

ny_lodes_od <- grab_lodes(state = "ny", year = 2010, 
                          lodes_type = "od", job_type = "JT00", 
                          segment = "S000", state_part = "main", 
                          agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

nj_lodes_od <- grab_lodes(state = "nj", year = 2010, 
                          lodes_type = "od", job_type = "JT00", 
                          segment = "S000", state_part = "main", 
                          agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

lodes_combined <- bind_rows(pa_lodes_od, ny_lodes_od, nj_lodes_od)

# pa_lodes_od %>% 
#   rename(workers = S000) %>% 
#   select(h_county, w_county, workers) %>% 
#   pivot_wider(id_cols = h_county, names_from = w_county, values_from = workers) %>% 
#   clean_names() %>% 
#   mutate(across(matches("^x"), ~replace_na(., 0)))
  

# pa_lodes_od %>% 
#   rename(workers = S000) %>% 
#   select(h_county, w_county, workers) %>% 
#   mutate(workers = scale(workers)) %>% 
#   widyr::widely_svd(item = h_county, feature = w_county, value = workers) %>% 
#   filter(dimension < 3) %>% 
#   pivot_wider(names_from = dimension, values_from = value, names_prefix = "dim_") %>% 
#   ggplot(aes(dim_1, dim_2)) +
#   geom_label(aes(label = h_county))


pa_counties <- tigris::counties(state = "PA") %>% 
  select(GEOID, NAME) %>% 
  arrange(GEOID)

ny_counties <- tigris::counties(state = "NY") %>% 
  select(GEOID, NAME) %>% 
  arrange(GEOID)

nj_counties <- tigris::counties(state = "NJ") %>% 
  select(GEOID, NAME) %>% 
  arrange(GEOID)

counties_combined <- bind_rows(pa_counties, ny_counties, nj_counties)

states <- counties_combined %>% 
  group_by()

counties_combined %>% 
  ggplot() +
  geom_sf()

node_pos <- counties_combined %>% 
  mutate(centroid = map(geometry, st_centroid),
         x = map_dbl(centroid, 1),
         y = map_dbl(centroid, 2)) %>% 
  select(GEOID, NAME, x, y) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  clean_names() %>% 
  select(-c(name, geoid))



network_graph <- lodes_combined %>% 
  select(w_county, h_county, commuters) %>% 
  as_tbl_graph(directed = TRUE) %>% 
  # activate(nodes) %>% 
  # mutate(community1 = as.factor(group_infomap()),
  #        community2 = as.factor(group_edge_betweenness(directed = TRUE)),
  #        community2 = fct_lump_min(community2, 2)) %>% 
  activate(edges) %>% 
  filter(commuters > 1000,
         !edge_is_loop())

network_graph %>% 
  activate(edges) %>% 
  arrange(desc(commuters)) %>% 
  as_tibble()

network_graph %>%
  activate(nodes) %>% 
  as_tibble() %>% 
  mutate(community2 = fct_lump_min(community2, 2)) %>% 
  count(community2, sort = TRUE)


#geo_layout <- layout_to_table(graph = network_graph, node_pos)

geo_layout <- create_layout(graph = network_graph,
                            layout = node_pos)

plot <- ggraph(geo_layout) +
  geom_sf(data = counties_combined, fill = NA) +
  geom_edge_fan(aes(edge_width = commuters, edge_alpha = commuters),
                arrow = arrow(length = unit(4, 'mm')), 
                start_cap = circle(3, 'mm'),
                end_cap = circle(6, 'mm'),
                color = "black") +
  geom_node_point(color = "red", size = 3) +
  #geom_node_point(aes(color = community2)) +
  scale_edge_alpha(range = c(.1, .9)) +
  scale_edge_width(range = c(1, 7)) +
  theme_void()

plot %>% 
  ggsave(filename = "output/lodes_network_graph_combined.png", width = 24, height = 24, dpi = 300)

