# library(osmdata)
library(osmextract)
library(dplyr)
library(sf)
library(here)
library(stringr)
library(hereR)

# source(here("R", "env.R"))
# hereR::set_key(here_key)


# load data ---------------------------------------------------------------

streets_data <- function(pbf_file_name = "Baden-Württemberg", geo_clip_file_path) {
  pedestrian_street_types <- c("path", "footway", "pedestrian")
  bicycle_allowed <- c("yes", "permissive", "designated")
  
  extra_tags <- c(
    "bicycle", "access", "oneway:bicycle", "use_sidepath", "embedded_rails", "maxspeed", "incline:across",
    "traffic", "segregated", "tracktype",
    "cycleway", "cycleway:right", "cycleway:left", "cycleway:both",
    "surface", "cycleway:surface", "cycleway:right:surface", "cycleway:left:surface", "cycleway:both:surface",
    "smoothness", "cycleway:smoothness", "cycleway:right:smoothness", "cycleway:left:smoothness", "cycleway:both:smoothness",
    "width", "cycleway:width", "cycleway:right:width", "cycleway:left:width", "cycleway:both:width",
    "width:lanes", "width:lanes:forward", "width:lanes:backward",
    "bicycle:lanes", "bicycle:lanes:forward", "bicycle:lanes:backward",
    "oneway", "cycleway:oneway", "cycleway:right:oneway", "cycleway:left:oneway", "cycleway:both:oneway",
    "bicycle_road", "motorroad"
  )
  streets <- oe_get(
  pbf_file_name,
    # force_download = T,
    # force_vectortranslate = TRUE,
    # quiet = T,
    stringsAsFactors = F,
    max_file_size = 1000000000, # ~ 1GB - default is too small
    extra_tags = extra_tags,
    query = "SELECT * FROM 'lines' WHERE highway <> ''"
  )

  # streets <- oe_read(here("data", "playground", "stuttgart-regbez-latest.osm.pbf"),
  #         stringsAsFactors = F,
  #         extra_tags = extra_tags,
  #         query = "SELECT * FROM 'lines' WHERE highway <> ''")

  barriers <- oe_get(
    pbf_file_name,
    # quiet = F,
    stringsAsFactors = F,
    extra_tags = c("maxwidth", "maxwidth:physical", "kerb", "height", "traffic_calming"),
    layer = "points",
    query = "SELECT * FROM 'points' WHERE barrier IN ('bollard', 'block', 'cycle_barrier', 'kerb', 'lift_gate') OR traffic_calming IN ('bump', 'mini_bumps', 'rumble_strip') "
  )
  #
  #   barriers <- oe_read(here("data", "playground", "stuttgart-regbez-latest.osm.pbf"),
  #                       extra_tags = c("maxwidth", "maxwidth:physical", "kerb", "height", "traffic_calming"),
  #                       layer = 'points',
  #   query = "SELECT * FROM 'points' WHERE barrier IN ('bollard', 'block', 'cycle_barrier', 'kerb', 'lift_gate') OR traffic_calming IN ('bump', 'mini_bumps', 'rumble_strip') "
  #   )

  barriers[!is.na(barriers$traffic_calming), "barrier"] <- "traffic_calming"
  barriers$kerb <- factor(barriers$kerb, levels = c("flush", "lowered", "raised"), ordered = T)


  if (!is.na(geo_clip_file_path) & geo_clip_file_path != "") {
    geo_selection <- read_sf(geo_clip_file_path)
    streets <- streets[st_intersects(streets, geo_selection, sparse = F)[, 1], ]
    barriers <- barriers[st_intersects(barriers, geo_selection, sparse = F)[, 1], ]
  }

  streets$width_cleaned <- gsub("[^0-9.-]", "", streets$width) %>% as.numeric()
  streets$cycleway_width_cleaned <- gsub("[^0-9.-]", "", streets$cycleway_width) %>% as.numeric()
  streets$cycleway_right_width_cleaned <- gsub("[^0-9.-]", "", streets$cycleway_right_width) %>% as.numeric()
  streets$cycleway_left_width_cleaned <- gsub("[^0-9.-]", "", streets$cycleway_left_width) %>% as.numeric()
  streets$cycleway_both_width_cleaned <- gsub("[^0-9.-]", "", streets$cycleway_both_width) %>% as.numeric()
  streets$maxspeed_cleaned <- gsub("[^0-9.-]", "", streets$maxspeed) %>% as.numeric()


  # filter streets -------------------------------------------------

  # all road types not allowed for cargo bikes
  streets <- streets %>%
    filter(
      highway != "proposed", # street doesnt exist yet
      (motorroad != "yes") | is.na(motorroad), # no motorroad
      !bicycle %in% c("no", "private") | is.na(bicycle), # bikes not allowed
      (!access %in% c( # special purpose access not allowed for bikes - except they are explicitly allowed
        "agricultural", "customers", "delivery", "private",
        "permit", "bus", "public_transport", "emergency", "forestry" # destination (Anlieger frei?)
      )) | (bicycle %in% c("yes", "designated", "permissive", "dismount")),
      (!highway %in% c( # exclude all street types not usable for bikes - except they are explicitly allowed
        "motorway", "motorway_link", "trunk", "trunk_link",
        "bus_guideway", "escape", "bridleway", "corridor"
      )) | # pedestrian and footway -> there you can always dismount?)
        (bicycle %in% c("yes", "designated", "permissive", "dismount")),
    )


  # split and duplicate right and left cycleway -------------------------------------------

  # split all streets that include information for two directions
  split_streets <- ((!is.na(streets$cycleway_right) & !is.na(streets$cycleway_left)) | # cycleway:right and cycleway:left
    (!is.na(streets$bicycle_lanes_forward) & !is.na(streets$bicycle_lanes_backward)) | # bicycle:lanes:forward and bicycle:lanes:backward
    !is.na(streets$cycleway) & (!streets$oneway %in% "yes") | # only cylceway (without right / left specification) and not oneway
    !is.na(streets$cycleway_both)) # cyclway:both

  ### all cycleways "left" or "backward"
  only_left <- streets %>%
    filter(split_streets | !is.na(cycleway_left) | !is.na(bicycle_lanes_backward)) %>%
    mutate( # remove information all information for "right" or "forward"
      cycleway_right = NA,
      cycleway_right_surface = NA,
      cycleway_right_smoothness = NA,
      cycleway_right_width = NA,
      cycleway_right_width_cleaned = NA,
      cycleway_right_oneway = NA,
      bicycle_lanes_forward = NA,
      width_lanes_forward = NA
    ) %>%
    st_transform(3035)
  start_points_left <- st_line_sample(st_geometry(only_left), sample = 0)
  end_points_left <- st_line_sample(only_left, sample = 1)
  diff_l <- (end_points_left - start_points_left) %>%
    st_coordinates() %>%
    as_tibble() %>%
    mutate(
      north = ifelse(X > 5, 3, 0), # shift north if line goes east
      east = ifelse(Y < -5, 3, 0)
    ) # shift east if line goes south
  new_geo_l <- c()
  for (i in 1:nrow(only_left)) {
    new_geo_l <- c(new_geo_l, st_geometry(only_left[i, ]) + c(diff_l[i, ]$east, diff_l[i, ]$north))
  }
  st_geometry(only_left) <- st_sfc(new_geo_l, crs = 3035)

  ### all cycleways "right" or "forward"
  only_right <- streets %>%
    filter(split_streets | !is.na(cycleway_right) | !is.na(bicycle_lanes_forward)) %>%
    mutate(
      cycleway_left = NA,
      cycleway_left_surface = NA,
      cycleway_left_smoothness = NA,
      cycleway_left_width = NA,
      cycleway_left_width_cleaned = NA,
      cycleway_left_oneway = NA,
      bicycle_lanes_backward = NA,
      width_lanes_backward = NA
    ) %>%
    st_transform(3035)
  start_points_right <- st_line_sample(st_geometry(only_right), sample = 0)
  end_points_right <- st_line_sample(only_right, sample = 1)
  diff_r <- (end_points_right - start_points_right) %>%
    st_coordinates() %>%
    as_tibble() %>%
    mutate(
      north = ifelse(X < -5, 3, 0), # shift north if line goes west
      east = ifelse(Y > 5, 3, 0)
    ) # shift east if line goes north
  new_geo_r <- c()
  for (i in 1:nrow(only_right)) {
    new_geo_r <- c(new_geo_r, st_geometry(only_right[i, ]) + c(diff_r[i, ]$east, diff_r[i, ]$north))
  }
  st_geometry(only_right) <- st_sfc(new_geo_r, crs = 3035)

  # join split right and left cycleways back with other streets
  streets <- streets %>%
    # remove
    filter(!osm_id %in% (c(only_right$osm_id, only_left$osm_id))) %>%
    # bind new left cycleways
    rbind(st_transform(only_left, 4326)) %>%
    # bind new right cycleways
    rbind(st_transform(only_right, 4326)) %>%
    mutate(
      bicycle_lanes_combined = coalesce(bicycle_lanes_forward, bicycle_lanes_backward, bicycle_lanes),
      width_lanes_combined = coalesce(width_lanes_forward, width_lanes_backward, width_lanes)
    )


  # combine different width tagging schemes into single column -------------------------------------------

  extract_bicycle_lane_id <- function(bicycle_lanes) {
    return(which(str_split(bicycle_lanes, pattern = "\\|")[[1]] == "designated")[1])
  }
  extract_bicycle_lane_width <- function(width_lanes, id) {
    return(as.numeric(str_split(width_lanes, pattern = "\\|")[[1]][id]))
  }
  streets$bicycle_lane_id <- sapply(streets$bicycle_lanes_combined, extract_bicycle_lane_id)
  streets$cycleway_width_extracted <- mapply(extract_bicycle_lane_width, streets$width_lanes_combined, streets$bicycle_lane_id)

  # combine all possible cycleway and width tagging into single column
  streets <- streets %>%
    mutate(
      cycleway_combined = coalesce(cycleway_right, cycleway_left, cycleway, cycleway_both),
      cycleway_width_combined = coalesce(
        cycleway_width_extracted,
        cycleway_right_width_cleaned, cycleway_left_width_cleaned,
        cycleway_width_cleaned, cycleway_both_width_cleaned
      ),
      cycleway_oneway_combined = coalesce(
        cycleway_right_oneway, cycleway_left_oneway, cycleway_oneway, cycleway_both_oneway
      )
    )

  streets[streets$highway == "cycleway", ]$cycleway_combined <- "track"
  streets[streets$highway == "cycleway", ]$cycleway_width_combined <- streets[streets$highway == "cycleway", ]$width_cleaned
  streets[streets$highway == "cycleway", ]$cycleway_oneway_combined <- streets[streets$highway == "cycleway", ]$oneway

  # combine all surface tagging schemes into single column ------------------
  
  # combine all cycleways surface to single variable
  streets <- streets %>%
    mutate(
      cycleway_surface_combined = coalesce(cycleway_right_surface, cycleway_left_surface, cycleway_surface),
      cycleway_smoothness_combined = coalesce(cycleway_right_smoothness, cycleway_left_smoothness, cycleway_smoothness)
    ) %>%
    mutate(
      # combine all streets surface to single variable
      surface_combined = case_when(
        highway == "cycleway" ~ surface,
        !is.na(cycleway_surface_combined) ~ cycleway_surface_combined,
        ! cycleway_combined %in% c("track") ~ surface, # if cycleway is not a separate track assume same surface as road surface
      ),
      smoothness_combined = case_when(
        highway == "cycleway" ~ smoothness,
        ! is.na(cycleway_smoothness_combined) ~ cycleway_smoothness_combined,
        ! cycleway_combined %in% c("track") ~ smoothness, # if cycleway is not a separate track assume same smoothness as road smoothness
      )
    )

  # seperate or use_sidepath indicates own line for cycleway - therefore remove those and only keep the actual cycleway
  streets <- streets %>%
    filter(!cycleway_combined %in% c("separate", "use_sidepath") | is.na(cycleway_combined))


  # parks -------------------------------------------------------------------
  # TODO: upgrade if road in park?
  # parks <- opq(bbox = city) %>%
  #   add_osm_feature(key = 'leisure', value = 'park') %>%
  #   osmdata_sf ()


  # incline across (Querneigung) --------------------------------------------
  # TODO


  # barriers ----------------------------------------------------------------
  barriers$maxwidth_cleaned <- gsub("[^0-9.-]", "", barriers$maxwidth) %>% as.numeric()
  barriers$maxwidth_physical_cleaned <- gsub("[^0-9.-]", "", barriers$maxwidth_physical) %>% as.numeric()
  barriers$height_cleaned <- gsub("[^0-9.-]", "", barriers$height) %>% as.numeric()
  barriers <- barriers %>%
    mutate(maxwidth_barriers_combined = pmin(maxwidth_cleaned, maxwidth_physical_cleaned, na.rm = T))

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

  traffic_calming <- barriers %>%
    filter(barrier == "traffic_calming") %>%
    st_transform(3035)

  streets_3035 <- st_transform(streets, 3035)
  st_precision(streets_3035) <- 1
  st_precision(cycle_barriers) <- 1
  st_precision(bollards) <- 1
  st_precision(blocks) <- 1
  st_precision(lift_gates) <- 1
  st_precision(kerbs) <- 1
  st_precision(traffic_calming) <- 1
  # get streets with barriers
  streets$has_cycle_barrier <- st_intersects(streets_3035, st_union(cycle_barriers), sparse = F)[, 1]
  streets$has_bollard <- st_intersects(streets_3035, st_union(bollards), sparse = F)[, 1]
  streets$has_block <- st_intersects(streets_3035, st_union(blocks), sparse = F)[, 1]
  streets$has_lift_gate <- st_intersects(streets_3035, st_union(lift_gates), sparse = F)[, 1]
  streets$has_kerb <- st_intersects(streets_3035, st_union(kerbs), sparse = F)[, 1]
  streets$has_traffic_calming <- st_intersects(streets_3035, st_union(traffic_calming), sparse = F)[, 1]
  
  # transform again to get newly created variables into streets-3035
  streets_3035 <- st_transform(streets, 3035)
  st_precision(streets_3035) <- 1

  # get width of barrier for streets (to intersect everything in one step is too large of an operation)
  cycle_barrier_widths <- streets_3035 %>%
    filter(has_cycle_barrier) %>%
    st_intersection(select(cycle_barriers, c(barrier, maxwidth_barriers_combined))) %>%
    st_drop_geometry() %>%
    select(osm_id, maxwidth_barriers_combined) %>%
    group_by(osm_id) %>%
    summarise(width_cycle_barrier = min(maxwidth_barriers_combined)) %>%
    ungroup()

  bollard_barrier_widths <- streets_3035 %>%
    filter(has_bollard) %>%
    st_intersection(select(bollards, c(barrier, maxwidth_barriers_combined))) %>%
    st_drop_geometry() %>%
    select(osm_id, maxwidth_barriers_combined) %>%
    group_by(osm_id) %>%
    summarise(width_bollard = min(maxwidth_barriers_combined)) %>%
    ungroup()

  block_barrier_widths <- streets_3035 %>%
    filter(has_block) %>%
    st_intersection(select(blocks, c(barrier, maxwidth_barriers_combined))) %>%
    st_drop_geometry() %>%
    select(osm_id, maxwidth_barriers_combined) %>%
    group_by(osm_id) %>%
    summarise(width_block = min(maxwidth_barriers_combined)) %>%
    ungroup()
  
  liftgate_barrier_widths <- streets_3035 %>%
    filter(has_lift_gate) %>%
    st_intersection(select(lift_gates, c(barrier, maxwidth_barriers_combined))) %>%
    st_drop_geometry() %>%
    select(osm_id, maxwidth_barriers_combined) %>%
    group_by(osm_id) %>%
    summarise(width_liftgate = min(maxwidth_barriers_combined)) %>%
    ungroup()

  kerb_height <- streets_3035 %>%
    filter(has_kerb) %>%
    st_intersection(select(kerbs, c(barrier, kerb, height_cleaned))) %>%
    st_drop_geometry() %>%
    select(osm_id, kerb, height_cleaned) %>%
    group_by(osm_id) %>%
    summarise(kerb = min(kerb),
              kerb_height = min(height_cleaned)) %>%
    ungroup() %>% 
    mutate(kerb_height_both = paste(kerb, kerb_height, "m"))

  streets <- streets %>%
    left_join(cycle_barrier_widths, by = "osm_id") %>%
    left_join(bollard_barrier_widths, by = "osm_id") %>%
    left_join(block_barrier_widths, by = "osm_id") %>%
    left_join(liftgate_barrier_widths, by = "osm_id") %>%
    left_join(kerb_height, by = "osm_id")

  # create additional variables ´-------------------------------

  streets <- streets %>%
    # label for barriers
    mutate(
      dismount_necessary = ifelse((highway %in% pedestrian_street_types) & 
                                    (!bicycle %in% bicycle_allowed) & 
                                    (!segregated %in% c("yes")), T, F),
      which_barrier = case_when(
        has_cycle_barrier ~ "Umlaufsperre",
        has_bollard ~ "Poller",
        has_block ~ "Block",
        has_kerb ~ "Bordstein",
        has_lift_gate ~ "Schranke",
        has_traffic_calming ~ "Bodenwelle"
      ),
      # get single variable for maxwidth corresponding to barriers label
      maxwidth_combined = case_when(
        has_cycle_barrier ~ paste(width_cycle_barrier, "m"),
        has_bollard ~ paste(width_bollard, "m"),
        has_block ~ paste(width_block, "m"),
        has_liftgate ~ paste(width_liftgate, "m"),
        has_kerb ~ kerb_height_both
      )
    )


  # set cargo bikability values ---------------------------------------------
  streets <- streets %>%
    mutate(
      cbindex_cycleways = case_when(
        bicycle_road == "yes" ~ 5,
        cycleway_width_combined >= 4 & cycleway_oneway_combined == "no" ~ 5,
        (cycleway_width_combined >= 3.2 & cycleway_width_combined <= 4.0) & cycleway_oneway_combined == "no" ~ 4,
        (cycleway_width_combined >= 2.4 & cycleway_width_combined <= 3.2) & cycleway_oneway_combined == "no" ~ 3,
        cycleway_width_combined < 2.4 & cycleway_oneway_combined == "no" ~ 1,
        cycleway_width_combined >= 2 & cycleway_combined %in% c("track", "lane") ~ 5,
        cycleway_width_combined >= 1.6 & cycleway_width_combined <= 2 & cycleway_combined %in% c("track", "lane") ~ 4,
        cycleway_width_combined >= 1.2 & width_cleaned <= 1.6 & cycleway_combined == "lane" ~ 4,
        cycleway_width_combined >= 1.2 & width_cleaned <= 1.6 & cycleway_combined == "track" ~ 3, # schmalerer track schlechter als schmale lane
        width_cleaned < 1.2 & cycleway_combined == "lane" ~ 2,
        width_cleaned < 1.2 & cycleway_combined == "track" ~ 1,
        cycleway_combined == "lane" ~ 4,
        cycleway_combined == "track" ~ 3,
        cycleway_combined == "opposite_lane" ~ 5, # eigene Spur für Gegenrichtung in Einbahnstraße
        cycleway_combined == "opposite" ~ 3, # keine Spur für Gegenrichtung in der Einbahnstraße
        cycleway_combined == "share_busway" ~ 3,
        # oneway_bicycle == "yes" ~ 8, #  how should this be rated?
        highway == "steps" ~ 0, # stairs not passable
        highway == "track" & tracktype == "grade1" ~ 4,
        highway == "track" & tracktype == "grade2" ~ 3,
        highway == "track" & tracktype == "grade5" ~ 0, # not passable for regular cargobikes
        highway == "track" ~ 1, # track without tracktype or tracktype < grade 2
        (highway %in% pedestrian_street_types) & (segregated == "yes") ~ 4, # is there a separate cycleway?
        (highway %in% pedestrian_street_types) & (bicycle %in% bicycle_allowed) ~ 2, # bicycle share street with pedestrians
        (highway %in% pedestrian_street_types) ~ 1, # cyclists have to dismount
        highway == "residential" | highway == "living_street" ~ 4, # residential streets
        highway == "corridor" ~ 1,
        highway == "service" ~ 2,
        bicycle %in% c("no") ~ 0, # motorways or bridlway that do not allow bicycles - not even pushing the bike
        highway == "bridleway" ~ 1,
        highway == "trunk" ~ 2,
        highway == "trunk_link" ~ 2,
        highway == "primary" ~ 1, # Hauptstraße ohne Radwege
        highway == "primary_link" ~ 1,
        highway == "secondary" ~ 2,
        highway == "secondary_link" ~ 2,
        highway == "tertiary" ~ 3,
        highway == "tertiary_link" ~ 3,
        highway == "road" ~ 2,
        highway == "unclassified" ~ 2,
        maxspeed >= 50 ~ 1,
        TRUE ~ 3
      ),
      # TODO: properly distinguish between cycleway, right, left and road
      cbindex_surface = case_when( # first: test if cycleway attributes are present. Then check general smoothness
        smoothness_combined == "excellent" ~ 5,
        smoothness_combined == "good" ~ 4,
        smoothness_combined == "intermediate" ~ 3,
        smoothness_combined == "bad" ~ 2,
        smoothness_combined == "very bad" ~ 1,
        smoothness_combined == "horrible" ~ 0,
        smoothness_combined == "very horrible" ~ 0,
        smoothness_combined == "impassable" ~ 0,
        surface_combined == "paved" ~ 4,
        surface_combined == "asphalt" ~ 5,
        surface_combined == "paving_stones" ~ 4,
        surface_combined == "concrete" ~ 4,
        surface_combined == "sett" ~ 2,
        surface_combined == "cobblestone" | surface_combined == "cobblestone:flattened" ~ 2,
        surface_combined == "unhewn_cobblestone" ~ 1,
        surface_combined == "compacted" ~ 3,
        surface_combined == "fine_gravel" ~ 2,
        surface_combined == "metal" ~ 3,
        surface_combined == "rock" ~ 0,
        surface_combined == "sand" ~ 0,
        surface_combined == "mud" ~ 0,
        surface_combined %in% c(
          "unpaved", "grass", "ground", "gravel", "dirt",
          "pebblestone", "earth", "grass_paver", "woodchips"
        ) ~ 1
      ) %>% as.numeric(),
      cbindex_barrier = case_when(
        has_cycle_barrier ~ 0,
        has_bollard & width_bollard < 0.9 ~ 0,
        has_bollard & width_bollard < 1.0 ~ 1,
        has_bollard & width_bollard < 1.2 ~ 2,
        has_bollard & width_bollard < 1.5 ~ 4,
        has_bollard & width_bollard >= 1.5 ~ 5,
        has_bollard ~ 4, # default: bollard is passable if no width given
        has_block & width_block < 0.9 ~ 0, # default: block is passable if no width given
        has_block & width_block < 1.0 ~ 1,
        has_block & width_block < 1.2 ~ 2,
        has_block & width_block < 1.5 ~ 4,
        has_block & width_block >= 1.5 ~ 5,
        has_block ~ 4,
        has_lift_gate & width_bollard < 0.9 ~ 0,
        has_lift_gate & width_bollard < 1.0 ~ 1,
        has_lift_gate & width_bollard < 1.2 ~ 2,
        has_lift_gate & width_bollard < 1.5 ~ 4,
        has_lift_gate & width_bollard >= 1.5 ~ 5,
        has_lift_gate ~ 2,
        has_kerb & kerb_height == "flush" ~ 5,
        has_kerb & kerb_height == "lowered" ~ 3,
        has_kerb & kerb_height == "raised" ~ 1,
        has_kerb ~ 2,
        has_traffic_calming ~ 3
        # has_cycle_barrier & width_cycle_barrier > 1.5 ~ 8
      ),  
      pedestrian_traffic = case_when(
        highway %in% c("footway", "path", "pedestrian", "bridlway", "track") & 
          (segregated != "yes") & (bicycle %in% c("yes", "permissive", "designated")) ~ 2, # path shared bike pedestrians, cycling allowed
        highway %in% c("footway", "path", "pedestrian", "bridlway", "track") & 
          (segregated != "yes") & (!bicycle %in% c("yes", "permissive", "designated")) ~ 1 # dismount bike
      )
    )


  # combine to one index ----------------------------------------------------

  streets <- streets %>%
    mutate(
      cbindex_street_quality = case_when(
        !is.na(cbindex_surface) & !is.na(cbindex_cycleways) ~ round(sqrt(cbindex_surface * cbindex_cycleways), 1),
        is.na(cbindex_surface) ~ cbindex_cycleways,
        is.na(cbindex_cycleways) ~ cbindex_surface
      ),
      cbindex = case_when(
        !is.na(cbindex_street_quality) & !is.na(cbindex_barrier) ~ round(sqrt(cbindex_surface * cbindex_cycleways), 1),
        is.na(cbindex_street_quality) ~ cbindex_barrier,
        is.na(cbindex_barrier) ~ cbindex_street_quality
      )
    )

  return(streets %>%
           select(osm_id, name, highway,
                  cbindex, cbindex_street_quality,
                  cbindex_cycleways, cycleway_combined, cycleway_width_combined, 
                  cbindex_surface, surface_combined, smoothness_combined, 
                  cbindex_barrier, which_barrier, maxwidth_combined,
                  pedestrian_traffic, maxspeed, bicycle_road, segregated, dismount_necessary))
}

pedestrian_traffic_data <- function(pbf_file_name, geo_clip_file_path) {
  ## pedestrian traffic
  markets <- oe_get(
    pbf_file_name,
    # force_download = T,
    # force_vectortranslate = TRUE,
    # quiet = T,
    stringsAsFactors = F,
    max_file_size = 1000000000, # ~ 1GB - default is too small
    extra_tags = c("opening_hours", "amenity"),
    layer = "points",
    query = "SELECT * FROM 'points' WHERE  amenity = 'marketplace'"
  )
  
  if (!is.na(geo_clip_file_path) & geo_clip_file_path != "") {
    geo_selection <- read_sf(geo_clip_file_path)
    markets <- markets[st_intersects(markets, geo_selection, sparse = F)[, 1], ]
  }
  
  # poly <- oe_get(
  #   pbf_file_name,
  #   # force_download = T,
  #   # force_vectortranslate = TRUE,
  #   # quiet = T,
  #   stringsAsFactors = F,
  #   max_file_size = 1000000000, # ~ 1GB - default is too small
  #   extra_tags = c("opening_hours", "amenity"),
  #   layer = "multipolygons",
  #   query = "SELECT * FROM 'multipolygons'"
  # )
  
  return (markets)
}

car_traffic_data <- function() {
  ## Car traffic
  # for testing only get traffic for xhain
  # xhain <- read_sf(here("data", "bezirke.geojson")) %>%
  #   filter(name == "Friedrichshain-Kreuzberg")
  # traffic_xhain <- hereR::flow(xhain)
  # traffic_xhain <- traffic_xhain %>%
  #   mutate(traffic_index = 10 - JF) %>%
  #   st_transform(3035) %>%
  #   st_buffer(5) # set 5 m buffer
  
  # TODO: map car traffic onto osm streets
  # streets_with_traffic <- streets %>%
  #   st_transform(3035) %>%
  #   st_within(traffic_xhain, sparse = F)
  #
  # streets_with_traffic <- streets[streets_with_traffic[,1], ]%>%
  #   select(osm_id, traffic_index) %>%
  #   st_drop_geometry()
  #
  # streets <- streets %>%
  #   left_join(streets_with_traffic, by = "osm_id")
  
}
