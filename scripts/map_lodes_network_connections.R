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

lodes_od_pa_main <- grab_lodes(state = "pa", year = 2010, 
                                 lodes_type = "od", job_type = "JT00", 
                                 segment = "S000", state_part = "main", 
                                 agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

lodes_od_pa_aux <- grab_lodes(state = "pa", year = 2010, 
                               lodes_type = "od", job_type = "JT00", 
                               segment = "S000", state_part = "aux", 
                               agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

lodes_od_ny_main <- grab_lodes(state = "ny", year = 2010, 
                          lodes_type = "od", job_type = "JT00", 
                          segment = "S000", state_part = "main", 
                          agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

lodes_od_ny_aux <- grab_lodes(state = "ny", year = 2010, 
                               lodes_type = "od", job_type = "JT00", 
                               segment = "S000", state_part = "aux", 
                               agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

lodes_od_nj_main <- grab_lodes(state = "nj", year = 2010, 
                          lodes_type = "od", job_type = "JT00", 
                          segment = "S000", state_part = "main", 
                          agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

lodes_od_nj_aux <- grab_lodes(state = "nj", year = 2010, 
                               lodes_type = "od", job_type = "JT00", 
                               segment = "S000", state_part = "aux", 
                               agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

lodes_combined <- bind_rows(lodes_od_pa_main, lodes_od_pa_aux,
                            lodes_od_ny_main, lodes_od_ny_aux,
                            lodes_od_nj_main, lodes_od_nj_aux)

lodes_combined %>% 
  count(h_county, w_county, sort = TRUE)

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
  mutate(state = "PA") %>%
  select(state, NAME, GEOID) %>% 
  arrange(GEOID)

ny_counties <- tigris::counties(state = "NY") %>% 
  mutate(state = "NY") %>%
  select(state, NAME, GEOID) %>%  
  arrange(GEOID)

nj_counties <- tigris::counties(state = "NJ") %>% 
  mutate(state = "NJ") %>%
  select(state, NAME, GEOID) %>% 
  arrange(GEOID)

counties_combined <- bind_rows(pa_counties, ny_counties, nj_counties)

states <- counties_combined %>% 
  group_by(state) %>% 
  summarize()

counties_combined %>% 
  ggplot() +
  geom_sf() +
  geom_sf(data = states, fill = NA, color = "red")

node_pos <- counties_combined %>% 
  mutate(centroid = map(geometry, st_centroid),
         x = map_dbl(centroid, 1),
         y = map_dbl(centroid, 2)) %>% 
  select(GEOID, NAME, x, y) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(-NAME) %>% 
  arrange(GEOID)

node_pos %>% 
  filter(GEOID == "34001")

network_graph <- lodes_combined %>%
  semi_join(counties_combined, by = c("w_county" = "GEOID")) %>%
  semi_join(counties_combined, by = c("h_county" = "GEOID")) %>%
  select(h_county, w_county, commuters) %>% 
  as_tbl_graph(directed = TRUE) %>% 
  # activate(nodes) %>% 
  # mutate(community1 = as.factor(group_infomap()),
  #        community2 = as.factor(group_edge_betweenness(directed = TRUE)),
  #        community2 = fct_lump_min(community2, 2)) %>% 
  activate(edges) %>% 
  filter(commuters >= 5000,
         !edge_is_loop()) %>%
  activate(nodes) %>%
  arrange(name)

nodes <- network_graph %>%
  activate(nodes) %>% 
  as_tibble()

all(node_pos$GEOID == nodes$name)

identical(node_pos$GEOID, nodes$name)

length(node_pos$GEOID) == length(node_pos$GEOID)

network_graph %>% 
  activate(edges) %>% 
  arrange(desc(commuters)) %>% 
  as_tibble()

geo_layout <- create_layout(graph = network_graph,
                            layout = node_pos)

plot <- ggraph(geo_layout) +
  geom_sf(data = counties_combined, fill = NA, 
          #color = NA, 
          size = .1) +
  geom_sf(data = states, aes(color = state), fill = NA, size = 2) +
  geom_edge_fan(aes(edge_width = commuters, edge_alpha = commuters),
                arrow = arrow(length = unit(4, 'mm')), 
                start_cap = circle(3, 'mm'),
                end_cap = circle(6, 'mm'),
                color = "white") +
  geom_node_point(color = "red", size = 3) +
  #geom_node_point(aes(color = community2)) +
  scale_edge_alpha(range = c(.2, .8)) +
  scale_edge_width(range = c(1, 7)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "black"))

plot %>% 
  ggsave(filename = "output/lodes_network_graph_combined.png", width = 24, height = 24, dpi = 300)

