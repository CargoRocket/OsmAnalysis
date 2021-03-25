library(osmdata)
library(mapview)
library(dplyr)
library(mapdeck)
library(sf)
library(here)
library(stringr)

source(here("R", "env.R"))
set_token(mapbox_key)
mapviewOptions(fgb = FALSE)

# ----------------------------------------------------- #
# prepare necessary data ----------------------------  

# Download Berlin pbf file: https://download.geofabrik.de/europe/germany/berlin.html
### Streets ###
streets <- read_sf(here("data", "berlin-latest.osm.pbf"), 
                   layer = "lines",
                   query = "SELECT * FROM lines WHERE highway <> ''")

streets$bicycle <- str_match(streets$other_tags, '\"bicycle\"=>\"\\s*(.*?)\\s*\"')[,2]
streets$cycleway <- str_match(streets$other_tags, '\"cycleway\"=>\"\\s*(.*?)\\s*\"')[,2]
streets$cycleway_right <- str_match(streets$other_tags, '\"cycleway:right\"=>\"\\s*(.*?)\\s*\"')[,2]
streets$cycleway_left <- str_match(streets$other_tags, '\"cycleway:left\"=>\"\\s*(.*?)\\s*\"')[,2]
streets$cycleway_both <- str_match(streets$other_tags, '\"cycleway:both\"=>\"\\s*(.*?)\\s*\"')[,2]
# streets$cycleway_foot <- str_match(streets$other_tags, '\"cycleway:foot\"=>\"\\s*(.*?)\\s*\"')[,2]
streets$motorroad <- str_match(streets$other_tags, '\"motorroad\"=>\"\\s*(.*?)\\s*\"')[,2]
streets$surface <- str_match(streets$other_tags, '\"surface\"=>\"\\s*(.*?)\\s*\"')[,2]
streets$cycleway_surface <- str_match(streets$other_tags, '\"cycleway:surface\"=>\"\\s*(.*?)\\s*\"')[,2]
streets$smoothness <- str_match(streets$other_tags, '\"smoothness\"=>\"\\s*(.*?)\\s*\"')[,2]
streets$access <- str_match(streets$other_tags, '\"access\"=>\"\\s*(.*?)\\s*\"')[,2]
streets$oneway <- str_match(streets$other_tags, '\"oneway\"=>\"\\s*(.*?)\\s*\"')[,2]
streets$oneway_bicycle <- str_match(streets$other_tags, '\"oneway:bicycle\"=>\"\\s*(.*?)\\s*\"')[,2]
streets$is_sidepath <- str_match(streets$other_tags, '\"is_sidepath\"=>\"\\s*(.*?)\\s*\"')[,2]
streets$bicycle_road <- str_match(streets$other_tags, '\"bicycle_road\"=>\"\\s*(.*?)\\s*\"')[,2]
streets$embedded_rails <- str_match(streets$other_tags, '\"embedded_rails\"=>\"\\s*(.*?)\\s*\"')[,2]
streets$maxspeed <- str_match(streets$other_tags, '\"maxspeed\"=>\"\\s*(.*?)\\s*\"')[,2]
streets$incline_across <- str_match(streets$other_tags, '\"incline:across\"=>\"\\s*(.*?)\\s*\"')[,2] # proposed tag
streets$width <- str_match(streets$other_tags, '\"width\"=>\"\\s*(.*?)\\s*\"')[,2]
streets$width_cleaned <- gsub("[^0-9.-]", "",  streets$width) %>% as.numeric()

# how often do all these tags occur
# round(100  - (colSums(is.na(streets)) / nrow(streets) * 100), 1)
colSums(!is.na(streets))

streets <- streets %>% 
  filter((bicycle != "no") | is.na(bicycle),
         ! access %in% c("agricultural", "customers", "delivery", "private", 
                         "permit", "bus", "public_transport", "emergency", "forestry"),
         highway != "motorway", #Autobahn
         highway != "motorway_link", #Autobahnauffahrt
         highway != "trunk", #Schnellstraße
         highway != "trunk_link", #Schnellstraße Auffahrt
         highway != "bus_guideway", # Suprbus-Strecke
         highway != "escape", # Notbresmweg
         (highway != "pedestrian") | (bicycle == "yes"), # Fußgängerzone
         (highway != "footway") | (bicycle == "yes"), # Fußweg
         (highway != "bridleway") | (bicycle == "yes"), # Reitweg
         highway != "steps",
         highway != "corridor", # Gang in inneren eines Gebäudes
         (motorroad != "yes") | is.na(motorroad) # keine Kraftfahrstraße
)

# xhain <- read_sf(here("data", "bezirke.geojson")) %>% 
#   filter(name == "Friedrichshain-Kreuzberg")
# 
# xhain_streets <- streets[st_intersects(streets, xhain, sparse = F),]
# xhain_streets_selection <- streets_selection[st_intersects(streets_selection, xhain, sparse = F),]
# 
# mapview(xhain_streets, color = "red") + xhain_streets_selection


### Parks ###
# parks <- opq(bbox = city) %>%
#   add_osm_feature(key = 'leisure', value = 'park') %>%
#   osmdata_sf ()

### Barriers ###
barriers <- read_sf("data/berlin-latest.osm.pbf", 
                    layer = "points",
                    query = "SELECT * FROM points WHERE BARRIER <> '' ")

barriers$width <- str_match(barriers$other_tags, '\"width\"=>\"\\s*(.*?)\\s*\"')[,2]
barriers$width_cleaned <- gsub("[^0-9.-]", "",  barriers$width) %>% as.numeric()
barriers$maxwidth <- str_match(barriers$other_tags, '\"maxwidth\"=>\"\\s*(.*?)\\s*\"')[,2]
barriers$maxwidth_cleaned <- gsub("[^0-9.-]", "",  barriers$maxwidth) %>% as.numeric()
barriers$maxwidth_physical <- str_match(barriers$other_tags, '\"maxwidth:physical\"=>\"\\s*(.*?)\\s*\"')[,2]
barriers$maxwidth_physical_cleaned <- gsub("[^0-9.-]", "",  barriers$maxwidth_physical) %>% as.numeric()
barriers <- barriers %>% 
  mutate(width_barriers_combined = pmin(width_cleaned, maxwidth_cleaned, maxwidth_physical_cleaned, na.rm =T))

cycle_barriers <- barriers %>% 
  filter(barrier == "cycle_barrier") %>% 
  st_transform(3035) 

bollards <- barriers %>% 
  filter(barrier == "bollard") %>% 
  st_transform(3035)

blocks <- barriers %>% 
  filter(barrier == "block") %>% 
  st_transform(3035) 

lift_gates <- barriers %>% 
  filter(barrier == "lift_gate") %>% 
  st_transform(3035) 

kerbs <- barriers %>% 
  filter(barrier == "kerb") %>% 
  st_transform(3035) 

streets_3035 <- st_transform(streets, 3035)
# get streets with barriers
streets$has_cycle_barrier <- st_intersects(streets_3035, st_union(cycle_barriers), sparse = F)[,1]
streets$has_bollard <- st_intersects(streets_3035, st_union(bollards), sparse = F)[,1]
streets$has_block <- st_intersects(streets_3035, st_union(blocks), sparse = F)[,1]
streets$has_lift_gate <- st_intersects(streets_3035, st_union(lift_gates), sparse = F)[,1]
streets$has_kerb <- st_intersects(streets_3035, st_union(kerbs), sparse = F)[,1]

streets_3035 <- st_transform(streets, 3035)

# get width of barrier for streets (to intersect everything in one step is too large of an operation)
cycle_barrier_widths <- streets_3035 %>% 
  filter(has_cycle_barrier) %>% 
  st_intersection(select(cycle_barriers, c(barrier, width_barriers_combined))) %>% 
  st_drop_geometry() %>% 
  select(osm_id, width_barriers_combined) %>% 
  group_by(osm_id) %>% 
  summarise(width_cycle_barrier = min(width_barriers_combined)) %>% 
  ungroup()

bollard_barrier_widths <- streets_3035 %>% 
  filter(has_bollard) %>% 
  st_intersection(select(bollards, c(barrier, width_barriers_combined))) %>% 
  st_drop_geometry() %>% 
  select(osm_id, width_barriers_combined) %>% 
  group_by(osm_id) %>% 
  summarise(width_bollard = min(width_barriers_combined)) %>% 
  ungroup()

block_barrier_widths <- streets_3035 %>% 
  filter(has_block) %>% 
  st_intersection(select(blocks, c(barrier, width_barriers_combined))) %>% 
  st_drop_geometry() %>% 
  select(osm_id, width_barriers_combined) %>% 
  group_by(osm_id) %>% 
  summarise(width_block = min(width_barriers_combined)) %>% 
  ungroup()

streets <- streets %>% 
  left_join(cycle_barrier_widths, by = "osm_id") %>% 
  left_join(bollard_barrier_widths, by = "osm_id") %>% 
  left_join(block_barrier_widths, by = "osm_id")
  

## Querneigung
# TODO

#streets %>% 
#  filter(has_cycle_barrier | has_bollards | has_block) %>% 
#  mapview() + filter(barriers, barrier %in% c("bollard", "block", "cycle_barrier"))

## Car traffic
# TODO: get car traffic

## pedestrian traffic
# TODO: rate Fussgaengerzonen
# TODO: get traffic on shared streets


# --------------------------------------------------------- #
# set "cargo bikability" values ----------------------------  

streets <- streets %>%
  mutate(
    cargoindex_surface = case_when(
      smoothness == "excellent" ~ 10,
      smoothness == "good" ~ 9,
      smoothness == "intermediate" ~ 5,
      smoothness == "bad" ~ 3,
      smoothness == "very bad" ~ 1,
      smoothness == "horrible" ~ 1,
      smoothness == "very horrible" ~ 1,
      smoothness == "impassable" ~ 0,
      surface == "asphalt" ~ 10,
      surface == "paving_stones" ~ 9,
      surface == "concrete" ~ 9,
      surface == "sett" ~ 3,
      surface == "cobblestone" ~ 3,
      surface == "unhewn_cobblestone" ~ 1,
      surface %in% c("unpaved", "compacted", "fine_gravel", "rock", "grass", "ground", "gravel", "dirt",
        "pebblestone", "earth", "grass_paver", "mud", "sand", "woodchips") ~ 1,
      TRUE ~ 8 # if no value given
    ) ,
    cargoindex_width = case_when( # width cycleway
      width_cleaned >= 2 & highway == "cycleway" ~ 10,
      width_cleaned >= 1.6 & width_cleaned <=2 & highway == "cycleway" ~ 8,
      width_cleaned >= 1.0 & width_cleaned <=1.6 & highway == "cycleway" ~ 6,
      width_cleaned < 1.0 ~ 0,
    ) ,
    cargoindex_barrier = case_when (
      has_cycle_barrier & is.na(width_cycle_barrier) ~ 0,
      has_lift_gate ~ 0,
      has_bollard & width_bollard < 0.9 ~ 0, # default: bollard is passable if no width given
      has_bollard & width_bollard < 1.0 ~ 1,
      has_bollard & width_bollard < 1.2 ~ 5,
      has_bollard & width_bollard < 1.5 ~ 7, 
      has_block & width_block < 0.9 ~ 0, # default: block is passable if no width given
      has_block & width_block < 1.0 ~ 1,
      has_block & width_block < 1.2 ~ 5,
      has_block & width_block < 1.5 ~ 7,
      has_cycle_barrier & width_cycle_barrier > 1.5 ~ 8
    )
  )

### prefer bike lanes ??
# streets[identical(streets$highway, "cycleway"), "cargoindex_cycleway"] <- 1
# streets[identical(streets$bicycle, "designated"), "cargoindex_cycleway"] <- 1
# streets[!is.na(streets$cycleway), "cargoindex_cycleway"] <- 0.8
# streets[is.na(streets$cargoindex_cycleway), "cargoindex_cycleway"] <- 0.5

# prefer roads in parks
# streets["in_park"] <- st_intersects(streets, st_union(parks$osm_multipolygons), sparse = F)
# streets[streets$streets, "cargoindex_parkroad"] <- 1.2

### One way street ###
# is this a oneway street?
# streets[streets$oneway == "yes" & 
#              streets$oneway.oneway.bicycle == "no" , "cargoindex_oneway"] <- 0.2

# ----------------------------------------------------- #
# combine to a single index ----------------------------  

streets <- streets %>% 
  rowwise() %>% 
  mutate(cargoindex = prod(c(cargoindex_width, cargoindex_surface, cargoindex_oneway, 
                             cargoindex_cycleway, cargoindex_parkroad, 100), na.rm = T)) %>% 
  st_as_sf() %>% 
  select(highway, width, surface, cycleway, oneway, oneway.bicycle, cargoindex_width, 
         cargoindex_surface, cargoindex_oneway, cargoindex_cycleway, cargoindex_parkroad, cargoindex)


# only use Xhain for testing
xhain <- read_sf(here("data", "bezirke.geojson")) %>% 
   filter(name == "Friedrichshain-Kreuzberg")
streets_xhain <- streets[st_intersects(streets, xhain, sparse = F)[,1],] %>% 
  select(-waterway, -aerialway, -barrier, -man_made, -z_order, -other_tags, - incline_across, - width)

m <- mapview(streets_xhain, zcol = "cargoindex_surface", layer.name = "surface", 
        color = colorRampPalette(c("red", "yellow", "darkgreen"))) +
  mapview(filter(streets_xhain, !is.na(cargoindex_barrier)), zcol = "cargoindex_barrier", layer.name = "barrier", 
          color = colorRampPalette(c("red", "orange", "yellow"))) +
  mapview(filter(streets_xhain, !is.na(cargoindex_width)), zcol = "cargoindex_width", layer.name = "width", 
          color = colorRampPalette(c("red", "yellow", "darkgreen")))

mapshot(m, "docs/xhain_index.html", selfcontained = F)
