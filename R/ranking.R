library(osmdata)
library(dplyr)
library(sf)
library(here)


streets <- readRDS(here("data", "bw.Rds"))
landkreise <- read_sf(here("data", "bw_landkreise.geojson"))
gemeinden <- read_sf(here("data", "bw_gemeinden.geojson"))

# cycleway attributes
# surface
# smoothness
# width
cycleways <- streets %>% filter(cycleway_combined %in% c("lane", "track", "opposite_lane")) %>% 
  select(highway, name, cycleway_combined, cycleway_width_combined, surface_combined, smoothness_combined)
cycleways$missing_values <- rowSums(is.na(cycleways[ ,c("cycleway_width_combined", "surface_combined", "smoothness_combined")]))
round(100  - (colSums(is.na(cycleways)) / nrow(cycleways) * 100), 1)

cycleways_intersected <- cycleways %>% 
  st_intersection(landkreise)

cycleway_ranking <- cycleways_intersected %>% 
  st_drop_geometry() %>% 
  group_by(kreis_name) %>% 
  summarize(count_cycleways = n(),
            with_width = sum(!is.na(cycleway_width_combined)),
            with_surface = sum(!is.na(surface_combined)),
            with_smoothness = sum(!is.na(smoothness_combined)),
            count_missing = sum(missing_values, na.rm = T)) %>% 
  mutate(perc_width =round( with_width / count_cycleways * 100),
         perc_surface = round(with_surface / count_cycleways * 100),
         perc_smoothness = round(with_smoothness / count_cycleways * 100))

# barrier attributes
# maxwidth:physical
barriers <- read_sf(here("data", "baden-wuerttemberg-latest.osm.pbf"),
  layer = "points",
  query = "SELECT * FROM points WHERE BARRIER <> '' "
) %>% 
  filter(barrier %in% c("bollard", "block", "cylce_barrier"))

barriers$maxwidth <- str_match(barriers$other_tags, '\"maxwidth\"=>\"\\s*(.*?)\\s*\"')[, 2]
barriers$maxwidth_cleaned <- gsub("[^0-9.-]", "", barriers$maxwidth) %>% as.numeric()
barriers$maxwidth_physical <- str_match(barriers$other_tags, '\"maxwidth:physical\"=>\"\\s*(.*?)\\s*\"')[, 2]
barriers$maxwidth_physical_cleaned <- gsub("[^0-9.-]", "", barriers$maxwidth_physical) %>% as.numeric()
barriers <- barriers %>%
  mutate(maxwidth_barriers_combined = pmin(maxwidth_cleaned, maxwidth_physical_cleaned, na.rm = T))

barriers_intersected <- barriers %>% 
  select(maxwidth_barriers_combined) %>% 
  st_intersection(landkreise)

# st_write(barriers_intersected, here("data", "barriers_bw_landkreise.gpkg"))

barrier_ranking <- barriers_intersected %>% 
  st_drop_geometry() %>% 
  group_by(kreis_name) %>% 
  summarize(count_barriers = n(),
            with_barrier_maxwidth = sum(!is.na(maxwidth_barriers_combined))) %>% 
  mutate(perc_barrier = round(with_barrier_maxwidth / count_barriers * 100))


combined <- cycleway_ranking %>% left_join(barrier_ranking, by = "kreis_name") %>% 
  mutate(perc_total = round(((3*count_cycleways - count_missing) + with_barrier_maxwidth) / 
                                                                     (count_barriers + 3*count_cycleways) * 100))

combined %>% select(kreis_name, count_cycleways, count_barriers, perc_width, 
                    perc_surface, perc_smoothness, perc_barrier, perc_total) %>% arrange(desc(perc_total)) %>% 
  readr::write_csv(here("data", "ranking.csv"))
mapview(cycleways, zcol = "missing_values")

# street surface
s <- streets %>% select(surface, smoothness) 
round(100  - (colSums(is.na(s)) / nrow(s) * 100), 1)
