# Map a map to plot all Berlin bollards and cycle_barriers

library(mapdeck)
library(dplyr)
library(sf)

source(here("R", "env.R"))
set_token(mapbox_key)

city <- "berlin"

# Download Berlin pbf file: https://download.geofabrik.de/europe/germany/berlin.html
barriers <- read_sf("data/berlin-latest.osm.pbf", 
                    layer = "points",
                    query = "SELECT * FROM points WHERE BARRIER <> '' ")


##### Analysis ######
# check how many NAs are in each columns (tag)
barriers$barrier %>% table ()
100  - (colSums(is.na(barriers)) / nrow(barriers) * 100)

cycle_barriers <- barriers %>%
  filter(barrier == "cycle_barrier")
100  - (colSums(is.na(cycle_barriers)) / nrow(cycle_barriers) * 100)

bollards <- barriers %>%
  filter(barrier == "bollard")
100  - (colSums(is.na(bollards)) / nrow(bollards) * 100)

blocks <- barriers %>%
  filter(barrier == "block")
100  - (colSums(is.na(blocks)) / nrow(blocks) * 100)
########

barriers$barrier_simple <- barriers$barrier
barriers[!barriers$barrier_simple %in% c("gate", "bollard", "cycle_barrier", "lift_gate", "block") &
           !is.na(barriers$barrier_simple), "barrier_simple"] <- "other"

# radius column for display
barriers$radius <- ifelse(barriers$barrier == "cycle_barrier", 10, 3)

m <- mapdeck(style = mapdeck_style("light")) %>% 
  add_scatterplot(data = barriers, radius = "radius", 
                  fill_colour = "barrier_simple",
                  palette =  "spectral",
                  legend = T,
                  tooltip = "barrier",
                  update_view = T)

mapview::mapshot(m, here("html", paste0(city, "_bollards.html")))
