library(tidyverse)
library(tidycensus)
library(lehdr)

#https://github.com/jamgreen/lehdr

tracts <- get_decennial(year = 2010, state = "PA", county = "Allegheny County", 
                        variables = "P001001",
                        geography = "tract", geometry = FALSE)



pa_lodes_residents <- grab_lodes(state = "pa", year = 2010, 
                                 lodes_type = "rac", job_type = "JT00", 
                                 segment = "S000", state_part = "main", 
                                 agg_geo = "tract") %>% 
  select(state, h_tract, C000, year) %>% 
  rename(GEOID = h_tract,
         workers = C000) %>% 
  semi_join(tracts, by = "GEOID")

pa_lodes_jobs <- grab_lodes(state = "pa", year = 2010, 
                            lodes_type = "wac", job_type = "JT00", 
                            segment = "S000", state_part = "main", 
                            agg_geo = "tract") %>% 
  select(state, w_tract, C000, year) %>% 
  rename(GEOID = w_tract,
         jobs = C000) %>% 
  semi_join(tracts, by = "GEOID")


glimpse(pa_lodes_jobs)


pa_lodes_residents %>% 
  left_join(pa_lodes_jobs) %>% 
  replace_na(list(jobs = 0, workers = 0)) %>% 
  select(state, GEOID, year, workers, jobs) %>% 
  write_csv("data/pa_lodes_data_tracts.csv")

pa_lodes_residents %>% 
  left_join(pa_lodes_jobs) %>% 
  replace_na(list(jobs = 0, workers = 0)) %>% 
  ggplot(aes(jobs, workers)) +
  geom_point()
