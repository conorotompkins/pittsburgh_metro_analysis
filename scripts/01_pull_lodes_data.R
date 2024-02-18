library(tidyverse)
library(tidycensus)
library(lehdr)

#https://github.com/jamgreen/lehdr

blocks <- get_decennial(year = 2010, state = "PA", county = "Allegheny County", 
                        variables = "P001001",
                        geography = "block", geometry = FALSE)



pa_lodes_residents <- grab_lodes(state = "pa", year = 2010, 
                       lodes_type = "rac", job_type = "JT00", 
                       segment = "S000", state_part = "main", 
                       agg_geo = "block") %>% 
  select(state, h_geocode, C000, year) %>% 
  rename(GEOID = h_geocode,
         residents = C000) %>% 
  semi_join(blocks, by = "GEOID")

pa_lodes_jobs <- grab_lodes(state = "pa", year = 2010, 
                                 lodes_type = "wac", job_type = "JT00", 
                                 segment = "S000", state_part = "main", 
                                 agg_geo = "block") %>% 
  select(state, contains("geocode"), C000, year) %>% 
  rename(GEOID = contains("geocode"),
         jobs = C000) %>% 
  semi_join(blocks, by = "GEOID")


glimpse(pa_lodes_jobs)


pa_lodes_residents %>% 
  left_join(pa_lodes_jobs) %>% 
  replace_na(list(jobs = 0, residents = 0)) %>% 
  select(state, GEOID, year, residents, jobs) %>% 
  write_csv("data/pa_lodes_data.csv")

pa_lodes_residents %>% 
  left_join(pa_lodes_jobs) %>% 
  replace_na(list(jobs = 0, residents = 0)) %>% 
  ggplot(aes(jobs, residents)) +
  geom_point()
