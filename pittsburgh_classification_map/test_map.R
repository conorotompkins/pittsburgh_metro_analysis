convex_hull <- threshhold_map %>% 
  filter(threshhold == .1) %>% 
  group_by(flag_city, threshhold) %>% 
  summarize() %>% 
  filter(flag_city == TRUE) %>% 
  st_convex_hull() %>% 
  st_cast("LINESTRING")
  
  
threshhold_map %>% 
  filter(threshhold == .1) %>% 
  ggplot() +
  geom_sf(aes(fill = flag_city), color = NA, alpha = .8) +
  geom_sf(data = pgh_official_boundary, alpha = 0, color = "black", size = 1) +
  geom_sf(data = pgh_official_boundary, alpha = 0, color = "yellow", size = .3) +
  geom_sf(data = convex_hull, color = "red") +
  #geom_sf(color = "black", fill = NA, size = 1) +
  #geom_sf(color = "white", fill = NA, linetype = 2, size = 1) +
  #facet_wrap(~threshhold) +
  scale_fill_viridis_d() +
  theme_void()

glimpse(threshhold_map)
