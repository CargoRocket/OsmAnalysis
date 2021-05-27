# library(osmdata)
library(osmextract)
library(dplyr)
library(sf)
library(here)
library(stringr)
#library(hereR)
library(data.table)
library(tidyr)
library(tictoc)
library(RPostgreSQL)
library(postGIStools)

source(here("R", "env.R"))
# hereR::set_key(here_key)


get_streets_pbf_data <- function(pbf_file_name, geo_clip_file_path) {
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
  options(timeout = 240) # prevent timeout when downloading pbf file
  # streets <- oe_get(
  #   pbf_file_name,
  #   #force_download = T,
  #   # force_vectortranslate = TRUE,
  #   # quiet = T,
  #   stringsAsFactors = F,
  #   max_file_size = 1000000000, # ~ 1GB - default is too small
  #   extra_tags = extra_tags,
  #   query = "SELECT * FROM 'lines' WHERE highway <> ''"
  # )
  
  streets <- oe_read(here("data", "berlin-latest.osm.pbf"),
                     stringsAsFactors = F,
                     extra_tags = extra_tags,
                     query = "SELECT * FROM 'lines' WHERE highway <> ''")
  
  # clip to given boundaries
  if (!is.na(geo_clip_file_path) & geo_clip_file_path != "") {
    geo_selection <- read_sf(geo_clip_file_path) %>% st_transform(3035)
    streets <- streets[st_intersects(st_transform(streets, 3035), geo_selection, sparse = F)[, 1], ]
  }
  
  st_precision(streets) <- 1000000
  
  return(streets)
}

get_barrier_pbf_data <- function(pbf_file_name, geo_clip_file_path) {
  
  # barriers <- oe_get(
  #   pbf_file_name,
  #   # quiet = F,
  #   stringsAsFactors = F,
  #   extra_tags = c("maxwidth", "maxwidth:physical", "kerb", "height", "traffic_calming"),
  #   layer = "points",
  #   query = "SELECT * FROM 'points' WHERE barrier IN ('bollard', 'block', 'cycle_barrier', 'kerb', 'lift_gate') OR traffic_calming IN ('bump', 'mini_bumps', 'rumble_strip') "
  # )
  
  barriers <- oe_read(here("data", "berlin-latest.osm.pbf"),
                      extra_tags = c("maxwidth", "maxwidth:physical", "kerb", "height", "traffic_calming"),
                      layer = 'points',
                      query = "SELECT * FROM 'points' WHERE barrier IN ('bollard', 'block', 'cycle_barrier', 'kerb', 'lift_gate') OR traffic_calming IN ('bump', 'mini_bumps', 'rumble_strip') "
  )
  
  if (!is.na(geo_clip_file_path) & geo_clip_file_path != "") {
    geo_selection <- read_sf(geo_clip_file_path) %>% st_transform(3035)
    barriers <- barriers[st_intersects(st_transform(barriers, 3035), geo_selection, sparse = F)[, 1], ]
  }
  st_precision(barriers) <- 1000000
  
  return(barriers)
}

get_streets_pg_data <- function(con) {
  streets <- get_postgis_query(con, paste0("SELECT id AS osm_id, tags, 
                                    tags -> 'highway' AS highway,
                                    tags -> 'bicycle' AS bicycle,
                                    tags -> 'bicycle_road' AS bicycle_road,
                                    tags -> 'motorroad' AS motorroad,
                                    tags -> 'oneway' AS oneway,
                                    tags -> 'oneway:bicycle' AS oneway_bicycle,
                                    tags -> 'access' AS access,
                                    tags -> 'use_sidepath' AS use_sidepath,
                                    tags -> 'embedded_rails' AS embedded_rails,
                                    tags -> 'maxspeed' AS maxspeed,
                                    tags -> 'segregated' AS segregated,
                                    tags -> 'tracktype' AS tracktype,
                                    tags -> 'cycleway' AS cycleway,
                                    tags -> 'cycleway:right' AS cycleway_right,
                                    tags -> 'cycleway:left' AS cycleway_left,
                                    tags -> 'cycleway:both' AS cycleway_both,
                                    tags -> 'surface' AS surface,
                                    tags -> 'cycleway:surface' AS cycleway_surface,
                                    tags -> 'cycleway:right:surface' AS cycleway_right_surface,
                                    tags -> 'cycleway:left:surface' AS cycleway_left_surface,
                                    tags -> 'cycleway:both:surface' AS cycleway_both_surface,
                                    tags -> 'smoothness' AS smoothness,
                                    tags -> 'cycleway:smoothness' AS cycleway_smoothness,
                                    tags -> 'cycleway:right:smoothness' AS cycleway_right_smoothness,
                                    tags -> 'cycleway:left:smoothness' AS cycleway_left_smoothness,
                                    tags -> 'cycleway:both:smoothness' AS cycleway_both_smoothness,
                                    tags -> 'width' AS width,
                                    tags -> 'cycleway:width' AS cycleway_width,
                                    tags -> 'cycleway:right:width' AS cycleway_right_width,
                                    tags -> 'cycleway:left:width' AS cycleway_left_width,
                                    tags -> 'cycleway:both:width' AS cycleway_both_width,
                                    tags -> 'width:lanes' AS width_lanes,
                                    tags -> 'width:lanes:forward' AS width_lanes_forward,
                                    tags -> 'width:lanes:backward' AS width_lanes_backward,
                                    tags -> 'bicycle:lanes' AS bicycle_lanes,
                                    tags -> 'bicycle:lanes:forward' AS bicycle_lanes_forward,
                                    tags -> 'bicycle:lanes:backward' AS bicycle_lanes_backward,
                                    tags -> 'cycleway:oneway' AS cycleway_oneway,
                                    tags -> 'cycleway:right:oneway' AS cycleway_right_oneway,
                                    tags -> 'cycleway:left:oneway' AS cycleway_left_oneway,
                                    tags -> 'cycleway:both:oneway' AS cycleway_both_oneway
                                    FROM ways  WHERE tags -> 'highway' <> ''"),
                               hstore_name = "tags") %>% 
    mutate(osm_id = as.character(osm_id))
  
  return(streets)
}

get_barriers_pg_data <- function(con) {
  barriers <- get_postgis_query(con, "SELECT id as osm_id, tags->'barrier' as barrier,
     tags->'maxwidth' as maxwidth,
     tags->'maxwidth:physical' as maxwidth_physical,
     tags->'kerb' as kerb,
     tags->'height' as height,
     tags->'traffic_calming' as traffic_calming
     FROM nodes where tags->'barrier' IN ('bollard', 'block', 'cycle_barrier', 'kerb', 'lift_gate')") %>% 
    mutate(osm_id = as.character(osm_id))
  
  return(barriers)
}

# load data ---------------------------------------------------------------

streets_data <- function(source = "pbf", pbf_file_name = NA, geo_clip_file_path = NA,
                         sub_regions_path = NA) {
  pedestrian_street_types <- c("path", "footway", "pedestrian")
  bicycle_allowed <- c("yes", "permissive", "designated")
  cycleway_options <-  c("track", "lane", "opposite_lane", "share_busway")
  
  tic()
  if(source == "pbf") {
    streets <- get_streets_pbf_data(pbf_file_name, geo_clip_file_path)
    barriers <- get_barrier_pbf_data(pbf_file_name, geo_clip_file_path)
  } else if (source == "postgis") {
    m <- dbDriver("PostgreSQL")
    con <- dbConnect(m, dbname= pg_db, host = pg_host, 
                     user = pg_user, password = pg_password)
    streets <- get_streets_pg_data(con)
    barriers <- get_barriers_pg_data(con)
  }
  toc()
  
  streets$width <- gsub("[^0-9.-]", "", gsub(",", ".", streets$width)) %>% as.numeric()
  streets$cycleway_width <- gsub("[^0-9.-]", "", gsub(",", ".", streets$cycleway_width)) %>% as.numeric()
  streets$cycleway_right_width <- gsub("[^0-9.-]", "", gsub(",", ".", streets$cycleway_right_width)) %>% as.numeric()
  streets$cycleway_left_width <- gsub("[^0-9.-]", "", gsub(",", ".", streets$cycleway_left_width)) %>% as.numeric()
  streets$cycleway_both_width <- gsub("[^0-9.-]", "", gsub(",", ".", streets$cycleway_both_width)) %>% as.numeric()
  streets$maxspeed <- gsub("[^0-9.-]", "", gsub(",", ".", streets$maxspeed)) %>% as.numeric()
  
  
  barriers[!is.na(barriers$traffic_calming), "barrier"] <- "traffic_calming"
  barriers$kerb <- factor(barriers$kerb, levels = c("flush", "lowered", "raised"), ordered = T)
  barriers$maxwidth <- gsub("[^0-9.-]", "", barriers$maxwidth) %>% as.numeric()
  barriers$maxwidth_physical <- gsub("[^0-9.-]", "", barriers$maxwidth_physical) %>% as.numeric()
  barriers$height <- gsub("[^0-9.-]", "", barriers$height) %>% as.numeric()
  barriers <- barriers %>%
    mutate(maxwidth_barriers_combined = pmin(maxwidth, maxwidth_physical, na.rm = T)) %>% 
    mutate(maxwidth_barriers_combined = ifelse(barrier == "cycle_barrier", 0, maxwidth_barriers_combined)) # set maxwidth to 0 to indicate not passible
  
  
  # filter streets -------------------------------------------------
  
  # all road types not allowed for cargo bikes
  streets <- streets %>%
    filter(
      highway != "proposed", # street doesnt exist yet
      ! motorroad  %in% "yes", # no motorroad
      #!bicycle %in% c("no", "private") | is.na(bicycle), # bikes not allowed
      (!access %in% c( # special purpose access not allowed for bikes - except they are explicitly allowed
        "agricultural", "customers", "delivery", "private",
        "permit", "bus", "public_transport", "emergency", "forestry" # destination (Anlieger frei?)
      )) | (bicycle %in% c("yes", "designated", "permissive", "dismount")),
      (!highway %in% c( # exclude all street types not usable for bikes - except they are explicitly allowed
        "motorway", "motorway_link", "trunk", "trunk_link",
        "bus_guideway", "escape", "bridleway", "corridor", "busway"
      )) |
        (bicycle %in% c("yes", "designated", "permissive", "dismount")),
    )
  
  # barriers ----------------------------------------------------------------
  # TODO: get intersection from database
  if (source == "pbf") {
    if(!is.na(sub_regions_path)) {
      sub_regions <- read_sf(sub_regions_path)
      
      # takes 35 sec for Baden-Württemberg
      streets_sub_regions <- st_intersects(st_transform(sub_regions, 3035), st_transform(streets, 3035), sparse =F)
      
      # takes 1 sec for Baden-Württemberg
      barriers_sub_regions <- st_intersects(st_transform(sub_regions, 3035), st_transform(barriers, 3035), sparse = F)
    } else {
      # TODO: implement if no sub regions are given
    }
    
    combined_streets <- c()
    combined_barriers <- c()
    
    for(i in c(1:nrow(sub_regions))){
      streets_region <- streets[streets_sub_regions[i,], ] 
      if(nrow(streets_region) > 0) {
        
        barriers_region <- barriers[barriers_sub_regions[i,], ] 
        
        street_barrier_intersection <- st_intersects(st_transform(streets_region, 3035), 
                                                     st_transform(barriers_region, 3035), sparse = F)
        
        barriers_region$on_street <- colSums(street_barrier_intersection) > 0
        
        reduced_barriers <- barriers_region[, c("barrier", "maxwidth_barriers_combined", "kerb", "height")] %>% st_drop_geometry()
        dummy_barriers <- tibble("n_block" = 0, "n_bollard" = 0, "n_cycle_barrier" = 0, "n_kerb" = 0, "n_lift_gate" = 0, "n_traffic_calming" = 0,
                                 "maxwidth_block" = NA, "maxwidth_bollard" = NA, "maxwidth_cycle_barrier" = NA, "maxwidth_lift_gate" = NA, 
                                 "maxwidth_traffic_calming" = NA, "kerb" = NA, "kerb_height" = NA)
        
        create_barrier_count <- function(barrier_ids) { # barriers without geometry
          if(length(barrier_ids) == 0) {
            return (dummy_barriers)
          }
          barriers_table <- reduced_barriers[barrier_ids, ] %>% 
            mutate(maxwidth_barriers_combined = ifelse(barrier == "cycle_barrier", 0, maxwidth_barriers_combined)) %>% 
            group_by(barrier) %>% 
            summarise(n = n(),
                      maxwidth = min(maxwidth_barriers_combined),
                      kerb = max(kerb),
                      kerb_height = max(height)) %>% 
            mutate(
              kerb = as.character(kerb),
              kerb = case_when(
                !is.na(kerb) ~ kerb,
                kerb_height > 0.03 ~ "raised",
                kerb_height == 0 ~ "flush",
                kerb_height < 0.03 ~ "lowered"
              ))
          
          barriers_wide <- barriers_table %>% 
            tidyr::pivot_wider(barrier, names_from = barrier, values_from = c(n, maxwidth))
          
          if("kerb" %in% barriers_table$barrier){
            barriers_wide <- barriers_wide %>% 
              cbind(barriers_table[barriers_table$barrier == "kerb", c("kerb", "kerb_height")]) %>% 
              select(-maxwidth_kerb)
          }
          
          barriers_wide <- barriers_wide %>% 
            cbind(select(dummy_barriers, - colnames(barriers_wide))) %>%  # fill non existent barriers
            select(colnames(dummy_barriers))
          return(barriers_wide)
        }
        
        barrier_ids_by_street <- apply(street_barrier_intersection, 1, which)
        barriers_on_streets <- lapply(barrier_ids_by_street, create_barrier_count)
        
        # join barrier info back to streets table
        if(length(barriers_on_streets) > 0) {
          combined_streets <- rbind(combined_streets, cbind(streets_region, rbindlist(barriers_on_streets)))
          combined_barriers <- rbind(combined_barriers, barriers_region)
        }
      }
    }
    
    streets <- combined_streets %>% 
      distinct(osm_id, .keep_all =T) # make sure every street is only present once on borders of regions
    # TODO: don't use distinct but combine properly
    
    barriers <- combined_barriers 
  }
  else if (source == "postgis") {
    # TODO: get intersection from database
    barrier_streets_intersection <- get_postgis_query(con, "SELECT way_id, barrier_id FROM barrier_ways_intersection") %>% 
      mutate(barrier_id = as.character(barrier_id),
             way_id = as.character(way_id))
    
    barriers_table <- barrier_streets_intersection %>% 
      left_join(barriers, by = c("barrier_id" = "osm_id")) %>% 
      group_by(way_id, barrier) %>% 
      summarise(n = n(),
                maxwidth = min(maxwidth_barriers_combined),
                kerb = max(kerb),
                kerb_height = max(height)) %>% 
      mutate(
        kerb = as.character(kerb),
        kerb = case_when(
          !is.na(kerb) ~ kerb,
          kerb_height > 0.03 ~ "raised",
          kerb_height == 0 ~ "flush",
          kerb_height < 0.03 ~ "lowered"
        ))
    
    barriers_wide <- barriers_table %>%
      tidyr::pivot_wider(c(barrier, way_id), names_from = barrier, values_from = c(n, maxwidth, kerb, kerb_height))
    
    # add columns that don't exist
    column_names <- c("n_lift_gate", "n_bollard", "n_cycle_barrier", "n_kerb", "n_block", "n_traffic_calming", "maxwidth_lift_gate",
                      "maxwidth_bollard", "maxwidth_cycle_barrier", "maxwidth_block", "maxwidth_traffic_calming", "kerb_kerb", "kerb_height_kerb")
    missing_col_names <- column_names[!column_names %in% colnames(barriers_wide)]
    barriers_wide[, missing_col_names] <- NA
    
    barriers_wide <- barriers_wide %>% 
      select(way_id, n_lift_gate, n_bollard, n_cycle_barrier, n_kerb, n_block, n_traffic_calming, maxwidth_lift_gate,
             maxwidth_bollard, maxwidth_cycle_barrier, maxwidth_block, maxwidth_traffic_calming, kerb_kerb, kerb_height_kerb) %>% 
      rename(kerb = kerb_kerb, 
             kerb_height = kerb_height_kerb)

    streets <- streets %>% 
      left_join(barriers_wide, by = c("osm_id" = "way_id")) %>% 
      mutate(
        n_cycle_barrier = ifelse(is.na(n_cycle_barrier), 0, n_cycle_barrier),
        n_lift_gate = ifelse(is.na(n_lift_gate), 0, n_lift_gate),
        n_bollard = ifelse(is.na(n_bollard), 0, n_bollard),
        n_kerb = ifelse(is.na(n_kerb), 0, n_kerb),
        n_block = ifelse(is.na(n_block), 0, n_block),
        n_traffic_calming = ifelse(is.na(n_traffic_calming), 0, n_traffic_calming)
      )
  }
  
  # split and duplicate right and left cycleway -------------------------------------------
  
  # get all streets with at a cycleway on at least one side where both directions are allowed to ride on
  streets$two_directions_w_cycleways <- (! streets$oneway %in% "yes" & !streets$oneway_bicycle %in% "no") & 
    ((streets$cycleway_right %in% cycleway_options) |
    (streets$cycleway_left %in% cycleway_options) |
    (streets$cycleway %in% cycleway_options) | 
    ( streets$cycleway_both %in% cycleway_options) |
    (!is.na(streets$bicycle_lanes_forward)) |
    (!is.na(streets$bicycle_lanes_backward)))
  
  # combine different width tagging schemes into single column -------------------------------------------
  
  extract_bicycle_lane_id <- function(bicycle_lanes) {
    return(which(str_split(bicycle_lanes, pattern = "\\|")[[1]] == "designated")[1])
  }
  extract_bicycle_lane_width <- function(width_lanes, id) {
    return(as.numeric(str_split(width_lanes, pattern = "\\|")[[1]][id]))
  }
  streets$bicycle_lane_forward_id <- sapply(streets$bicycle_lanes_forward, extract_bicycle_lane_id)
  streets$bicycle_lane_backward_id <- sapply(streets$bicycle_lanes_backward, extract_bicycle_lane_id)
  streets$bicycle_lane_id <- sapply(streets$bicycle_lanes, extract_bicycle_lane_id)
  streets$cycleway_width_forward_extracted <- mapply(extract_bicycle_lane_width, streets$width_lanes_forward, streets$bicycle_lane_forward_id)
  streets$cycleway_width_backward_extracted <- mapply(extract_bicycle_lane_width, streets$width_lanes_backward, streets$bicycle_lane_backward_id)
  streets$cycleway_width_extracted <- mapply(extract_bicycle_lane_width, streets$width_lanes, streets$bicycle_lane_id)
  
  # combine width tagging schemes
  streets <- streets %>%
    mutate(
      cycleway_combined = coalesce(cycleway_right, cycleway_left, cycleway, cycleway_both),
      cycleway_right = coalesce(cycleway_right, cycleway, cycleway_both),
      cycleway_left = coalesce(cycleway_left, cycleway, cycleway_both),
      cycleway_width_combined = coalesce(
        cycleway_right_width, cycleway_left_width,
        cycleway_width, cycleway_both_width),
      cycleway_right_width = coalesce(cycleway_right_width, cycleway_width_forward_extracted),
      cycleway_left_width = coalesce(cycleway_left_width, cycleway_width_backward_extracted),
      cycleway_oneway_combined = coalesce(
        cycleway_right_oneway, cycleway_left_oneway, cycleway_oneway, cycleway_both_oneway
      )
    )
  
  streets[streets$highway == "cycleway", ]$cycleway_combined <- "track"
  streets[streets$highway == "cycleway", ]$cycleway_width_combined <- streets[streets$highway == "cycleway", ]$width
  streets[streets$highway == "cycleway", ]$cycleway_oneway_combined <- streets[streets$highway == "cycleway", ]$oneway
  
  # combine surface tagging schemes ------------------
  
  # combine all cycleways surface to single variable
  streets <- streets %>%
    mutate(
      cycleway_surface_right = coalesce(cycleway_right_surface, cycleway_surface),
      cycleway_surface_left = coalesce(cycleway_left_surface, cycleway_surface),
      cycleway_surface_combined = coalesce(cycleway_right_surface, cycleway_left_surface, cycleway_surface),
      cycleway_smoothness_right = coalesce(cycleway_right_smoothness, cycleway_smoothness),
      cycleway_smoothness_left = coalesce(cycleway_left_smoothness, cycleway_smoothness),
      cycleway_smoothness_combined = coalesce(cycleway_right_smoothness, cycleway_left_smoothness, cycleway_smoothness)
    ) %>%
    mutate(
      # combine all streets surface to single variable
      surface_right = case_when(
        highway == "cycleway" ~ surface,
        !is.na(cycleway_surface_combined) ~ cycleway_surface_right,
        ! cycleway_right %in% c("track") ~ surface, # if cycleway is not a separate track assume same surface as road surface
      ),
      surface_left = case_when(
        highway == "cycleway" ~ surface,
        !is.na(cycleway_surface_combined) ~ cycleway_surface_left,
        ! cycleway_left %in% c("track") ~ surface, # if cycleway is not a separate track assume same surface as road surface
      ),
      surface_combined = case_when(
        highway == "cycleway" ~ surface,
        !is.na(cycleway_surface_combined) ~ cycleway_surface_combined,
        ! cycleway_combined %in% c("track") ~ surface, # if cycleway is not a separate track assume same surface as road surface
      ),
      smoothness_right= case_when(
        highway == "cycleway" ~ smoothness,
        ! is.na(cycleway_smoothness_right) ~ cycleway_smoothness_right,
        ! cycleway_right %in% c("track") ~ smoothness, # if cycleway is not a separate track assume same smoothness as road smoothness
      ),
      smoothness_left= case_when(
        highway == "cycleway" ~ smoothness,
        ! is.na(cycleway_smoothness_left) ~ cycleway_smoothness_left,
        ! cycleway_left %in% c("track") ~ smoothness, # if cycleway is not a separate track assume same smoothness as road smoothness
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
  
  
  # create additional variables ´-------------------------------
  barriers_labels <- streets %>% 
    # st_drop_geometry() %>% 
    select(n_cycle_barrier, n_bollard, n_block, n_kerb, n_lift_gate, n_traffic_calming) %>% 
    mutate(cycle_barrier = ifelse(n_cycle_barrier > 0, "Umlaufsperre", NA),
           bollard = ifelse(n_bollard > 0, "Poller", NA),
           block = ifelse(n_block > 0, "Block", NA),
           kerb = ifelse(n_kerb > 0, "Bordstein", NA),
           lift_gate = ifelse(n_lift_gate > 0, "Schranke", NA),
           traffic_calming = ifelse(n_traffic_calming > 0, "Bodenwelle", NA)
    ) %>% 
    unite("which_barrier", c(cycle_barrier, bollard, block, kerb, lift_gate, traffic_calming), na.rm = T, sep = ", ")
  
  streets <- streets %>%
    cbind("which_barrier" = barriers_labels$which_barrier) %>% 
    mutate(
      dismount_necessary = ifelse(((highway %in% c("footway", "pedestrian")) & 
                                     (!bicycle %in% bicycle_allowed) & (!segregated %in% c("yes"))) | 
                                    ((highway == "path") & ( bicycle %in% c("no", "dismount"))), T, F),
      # get single variable for maxwidth corresponding to barriers label
      min_maxwidth = pmin(maxwidth_cycle_barrier, maxwidth_bollard, maxwidth_block, 
                          maxwidth_lift_gate, maxwidth_traffic_calming, na.rm = T)
    )
  
  
  # set cargo bikability values ---------------------------------------------
  
  # compute index if only one sided cycleway or no cycleway

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
        cycleway_width_combined >= 1.2 & cycleway_width_combined <= 1.6 & cycleway_combined == "lane" ~ 4,
        cycleway_width_combined >= 1.2 & cycleway_width_combined <= 1.6 & cycleway_combined == "track" ~ 3, # schmalerer track schlechter als schmale lane
        cycleway_width_combined < 1.2 & cycleway_combined == "lane" ~ 2,
        cycleway_width_combined < 1.2 & cycleway_combined == "track" ~ 1,
        cycleway_combined == "lane" ~ 4,
        cycleway_combined == "track" ~ 3,
        cycleway_combined == "opposite_lane" ~ 5, # eigene Spur für Gegenrichtung in Einbahnstraße
        cycleway_combined == "opposite" ~ 3, # keine Spur für Gegenrichtung in der Einbahnstraße
        cycleway_combined == "share_busway" ~ 3,
        highway == "steps" ~ 0, # stairs not passable
        highway == "track" & (bicycle %in% c("no")) ~ 1, # track where cyclists have to dismount
        highway == "track" & tracktype == "grade1" ~ 4,
        highway == "track" & tracktype == "grade2" ~ 3,
        highway == "track" & tracktype == "grade5" ~ 0, # not passable for regular cargobikes
        highway == "track" ~ 1, # track without tracktype or tracktype < grade 2
        (highway %in% pedestrian_street_types) & (segregated == "yes") ~ 3, # is there a separate cycleway?
        (highway %in% pedestrian_street_types) & (bicycle %in% bicycle_allowed) |
          highway == "path" & (! bicycle %in% c("no", "dismount")) ~ 2, # bicycle share street with pedestrians
        (highway %in% pedestrian_street_types) ~ 1, # cyclists have to dismount
        highway == "corridor" & (bicycle %in% bicycle_allowed) ~ 2,
        highway == "corridor" ~ 1,
        highway == "bridleway" & (bicycle %in% bicycle_allowed) ~ 2,
        highway == "busway" & (bicycle %in% bicycle_allowed) ~ 3,
        bicycle %in% c("no") ~ 0, # motorways that do not allow bicycles - not even pushing the bike
        highway == "service" ~ 2,
        highway == "residential" | highway == "living_street" ~ 4, # residential streets
        highway == "unclassified" ~ 4,
        highway == "trunk" & (bicycle %in% bicycle_allowed) ~ 2,
        highway == "trunk_link" & (bicycle %in% bicycle_allowed) ~ 2,
        highway == "primary" ~ 1, # Hauptstraße ohne Radwege
        highway == "primary_link" ~ 1,
        highway == "secondary" ~ 2,
        highway == "secondary_link" ~ 2,
        highway == "tertiary" ~ 3,
        highway == "tertiary_link" ~ 3,
        highway == "road" ~ 2,
        maxspeed >= 50 ~ 1,
        TRUE ~ 3
      ),
      cbindex_cycleways_forward = case_when(
        bicycle_road == "yes" ~ 5,
        cycleway_right_width >= 4 & cycleway_right_oneway == "no" ~ 5,
        (cycleway_right_width >= 3.2 & cycleway_right_width <= 4.0) & cycleway_right_oneway == "no" ~ 4,
        (cycleway_right_width >= 2.4 & cycleway_right_width <= 3.2) & cycleway_right_oneway == "no" ~ 3,
        cycleway_right_width < 2.4 & cycleway_right_oneway == "no" ~ 1,
        cycleway_right_width >= 2 & cycleway_right %in% c("track", "lane") ~ 5,
        cycleway_right_width >= 1.6 & cycleway_right_width <= 2 & cycleway_right %in% c("track", "lane") ~ 4,
        cycleway_right_width >= 1.2 & cycleway_right_width <= 1.6 & cycleway_right == "lane" ~ 4,
        cycleway_right_width >= 1.2 & cycleway_right_width <= 1.6 & cycleway_right == "track" ~ 3, # schmalerer track schlechter als schmale lane
        cycleway_right_width < 1.2 & cycleway_right == "lane" ~ 2,
        cycleway_right_width < 1.2 & cycleway_right == "track" ~ 1,
        cycleway_right == "lane" ~ 4,
        cycleway_right == "track" ~ 3,
        cycleway_right == "opposite_lane" ~ 5, # eigene Spur für Gegenrichtung in Einbahnstraße
        cycleway_right == "opposite" ~ 3, # keine Spur für Gegenrichtung in der Einbahnstraße
        cycleway_right == "share_busway" ~ 3,
        highway == "steps" ~ 0, # stairs not passable
        highway == "track" & (bicycle %in% c("no")) ~ 1, # track where cyclists have to dismount
        highway == "track" & tracktype == "grade1" ~ 4,
        highway == "track" & tracktype == "grade2" ~ 3,
        highway == "track" & tracktype == "grade5" ~ 0, # not passable for regular cargobikes
        highway == "track" ~ 1, # track without tracktype or tracktype < grade 2
        (highway %in% pedestrian_street_types) & (segregated == "yes") ~ 3, # is there a separate cycleway?
        (highway %in% pedestrian_street_types) & (bicycle %in% bicycle_allowed) |
          highway == "path" & (! bicycle %in% c("no", "dismount")) ~ 2, # bicycle share street with pedestrians
        (highway %in% pedestrian_street_types) ~ 1, # cyclists have to dismount
        highway == "corridor" & (bicycle %in% bicycle_allowed) ~ 2,
        highway == "corridor" ~ 1,
        highway == "bridleway" & (bicycle %in% bicycle_allowed) ~ 2,
        highway == "busway" & (bicycle %in% bicycle_allowed) ~ 3,
        bicycle %in% c("no") ~ 0, # motorways that do not allow bicycles - not even pushing the bike
        highway == "service" ~ 2,
        highway == "residential" | highway == "living_street" ~ 4, # residential streets
        highway == "unclassified" ~ 4,
        highway == "trunk" & (bicycle %in% bicycle_allowed) ~ 2,
        highway == "trunk_link" & (bicycle %in% bicycle_allowed) ~ 2,
        highway == "primary" ~ 1, # Hauptstraße ohne Radwege
        highway == "primary_link" ~ 1,
        highway == "secondary" ~ 2,
        highway == "secondary_link" ~ 2,
        highway == "tertiary" ~ 3,
        highway == "tertiary_link" ~ 3,
        highway == "road" ~ 2,
        maxspeed >= 50 ~ 1,
        TRUE ~ 3
      ),
      cbindex_cycleways_backward = case_when(
        bicycle_road == "yes" ~ 5,
        cycleway_left_width >= 4 & cycleway_left_oneway == "no" ~ 5,
        (cycleway_left_width >= 3.2 & cycleway_left_width <= 4.0) & cycleway_left_oneway == "no" ~ 4,
        (cycleway_left_width >= 2.4 & cycleway_left_width <= 3.2) & cycleway_left_oneway == "no" ~ 3,
        cycleway_left_width < 2.4 & cycleway_left_oneway == "no" ~ 1,
        cycleway_left_width >= 2 & cycleway_left %in% c("track", "lane") ~ 5,
        cycleway_left_width >= 1.6 & cycleway_left_width <= 2 & cycleway_left %in% c("track", "lane") ~ 4,
        cycleway_left_width >= 1.2 & cycleway_left_width <= 1.6 & cycleway_left == "lane" ~ 4,
        cycleway_left_width >= 1.2 & cycleway_left_width <= 1.6 & cycleway_left == "track" ~ 3, # schmalerer track schlechter als schmale lane
        cycleway_left_width < 1.2 & cycleway_left == "lane" ~ 2,
        cycleway_left_width < 1.2 & cycleway_left == "track" ~ 1,
        cycleway_left == "lane" ~ 4,
        cycleway_left == "track" ~ 3,
        cycleway_left == "opposite_lane" ~ 5, # eigene Spur für Gegenrichtung in Einbahnstraße
        cycleway_left == "opposite" ~ 3, # keine Spur für Gegenrichtung in der Einbahnstraße
        cycleway_left == "share_busway" ~ 3,
        highway == "steps" ~ 0, # stairs not passable
        highway == "track" & (bicycle %in% c("no")) ~ 1, # track where cyclists have to dismount
        highway == "track" & tracktype == "grade1" ~ 4,
        highway == "track" & tracktype == "grade2" ~ 3,
        highway == "track" & tracktype == "grade5" ~ 0, # not passable for regular cargobikes
        highway == "track" ~ 1, # track without tracktype or tracktype < grade 2
        (highway %in% pedestrian_street_types) & (segregated == "yes") ~ 3, # is there a separate cycleway?
        (highway %in% pedestrian_street_types) & (bicycle %in% bicycle_allowed) |
          highway == "path" & (! bicycle %in% c("no", "dismount")) ~ 2, # bicycle share street with pedestrians
        (highway %in% pedestrian_street_types) ~ 1, # cyclists have to dismount
        highway == "corridor" & (bicycle %in% bicycle_allowed) ~ 2,
        highway == "corridor" ~ 1,
        highway == "bridleway" & (bicycle %in% bicycle_allowed) ~ 2,
        highway == "busway" & (bicycle %in% bicycle_allowed) ~ 3,
        bicycle %in% c("no") ~ 0, # motorways that do not allow bicycles - not even pushing the bike
        highway == "service" ~ 2,
        highway == "residential" | highway == "living_street" ~ 4, # residential streets
        highway == "unclassified" ~ 4,
        highway == "trunk" & (bicycle %in% bicycle_allowed) ~ 2,
        highway == "trunk_link" & (bicycle %in% bicycle_allowed) ~ 2,
        highway == "primary" ~ 1, # Hauptstraße ohne Radwege
        highway == "primary_link" ~ 1,
        highway == "secondary" ~ 2,
        highway == "secondary_link" ~ 2,
        highway == "tertiary" ~ 3,
        highway == "tertiary_link" ~ 3,
        highway == "road" ~ 2,
        maxspeed >= 50 ~ 1,
        TRUE ~ 3
      ),
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
        surface_combined == "concrete:plates" ~ 4,
        surface_combined == "concrete:lanes" ~ 2,
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
      cbindex_surface_forward = case_when( # first: test if cycleway attributes are present. Then check general smoothness
        smoothness_right == "excellent" ~ 5,
        smoothness_right == "good" ~ 4,
        smoothness_right == "intermediate" ~ 3,
        smoothness_right == "bad" ~ 2,
        smoothness_right == "very bad" ~ 1,
        smoothness_right == "horrible" ~ 0,
        smoothness_right == "very horrible" ~ 0,
        smoothness_right == "impassable" ~ 0,
        surface_right == "paved" ~ 4,
        surface_right == "asphalt" ~ 5,
        surface_right == "paving_stones" ~ 4,
        surface_right == "concrete" ~ 4,
        surface_right == "concrete:plates" ~ 4,
        surface_right == "concrete:lanes" ~ 2,
        surface_right == "sett" ~ 2,
        surface_right == "cobblestone" | surface_right == "cobblestone:flattened" ~ 2,
        surface_right == "unhewn_cobblestone" ~ 1,
        surface_right == "compacted" ~ 3,
        surface_right == "fine_gravel" ~ 2,
        surface_right == "metal" ~ 3,
        surface_right == "rock" ~ 0,
        surface_right == "sand" ~ 0,
        surface_right == "mud" ~ 0,
        surface_right %in% c(
          "unpaved", "grass", "ground", "gravel", "dirt",
          "pebblestone", "earth", "grass_paver", "woodchips"
        ) ~ 1
      ) %>% as.numeric(),
      cbindex_surface_backward = case_when( # first: test if cycleway attributes are present. Then check general smoothness
        smoothness_left == "excellent" ~ 5,
        smoothness_left == "good" ~ 4,
        smoothness_left == "intermediate" ~ 3,
        smoothness_left == "bad" ~ 2,
        smoothness_left == "very bad" ~ 1,
        smoothness_left == "horrible" ~ 0,
        smoothness_left == "very horrible" ~ 0,
        smoothness_left == "impassable" ~ 0,
        surface_left == "paved" ~ 4,
        surface_left == "asphalt" ~ 5,
        surface_left == "paving_stones" ~ 4,
        surface_left == "concrete" ~ 4,
        surface_left == "concrete:plates" ~ 4,
        surface_left == "concrete:lanes" ~ 2,
        surface_left == "sett" ~ 2,
        surface_left == "cobblestone" | surface_left == "cobblestone:flattened" ~ 2,
        surface_left == "unhewn_cobblestone" ~ 1,
        surface_left == "compacted" ~ 3,
        surface_left == "fine_gravel" ~ 2,
        surface_left == "metal" ~ 3,
        surface_left == "rock" ~ 0,
        surface_left == "sand" ~ 0,
        surface_left == "mud" ~ 0,
        surface_left %in% c(
          "unpaved", "grass", "ground", "gravel", "dirt",
          "pebblestone", "earth", "grass_paver", "woodchips"
        ) ~ 1
      ) %>% as.numeric())
  
  streets <- streets %>% 
    mutate(
      cbindex_barrier = case_when(
        n_cycle_barrier > 0 ~ 0,
        (n_bollard > 0 & maxwidth_bollard < 0.9) |
          (n_block > 0 & maxwidth_block < 0.9) |
          (n_lift_gate > 0 & maxwidth_lift_gate < 0.9) ~ 0,
        (n_bollard > 0 & maxwidth_bollard < 1.0) |
          (n_block > 0 & maxwidth_block < 1.0) |
          (n_lift_gate > 0 & maxwidth_lift_gate < 1.0) ~ 0.2,
        (n_bollard > 0 & maxwidth_bollard < 1.2) |
          (n_block > 0 & maxwidth_block < 1.2) |
          (n_lift_gate > 0 & maxwidth_lift_gate < 1.2) ~ 0.4,
        (n_bollard > 0 & maxwidth_bollard < 1.5) |
          (n_block > 0 & maxwidth_block < 1.5) |
          (n_lift_gate > 0 & maxwidth_lift_gate < 1.5) ~ 0.8,
        (n_bollard > 0 & maxwidth_bollard >= 1.5) |
          (n_block > 0 & maxwidth_block >= 1.5) |
          (n_lift_gate > 0 & maxwidth_lift_gate >= 1.5) ~ 1,
        n_bollard > 0 | n_block > 0 | n_lift_gate > 0 ~ 0.8,
        n_kerb > 0 & kerb == "flush" ~ 1,
        n_kerb > 0 & kerb == "lowered" ~ 0.6,
        n_kerb > 0 & kerb == "raised" ~ 0.2,
        n_kerb > 0 ~ 0.4,
        n_traffic_calming > 0~ 0.6
        # n_cycle_barrier > 0 & width_cycle_barrier > 1.5 ~ 8
      ),  
      pedestrian_traffic = case_when(
        highway %in% c(pedestrian_street_types, "bridleway", "track") & 
          (segregated != "yes") & (bicycle %in% c("yes", "permissive", "designated")) ~ 0.4, # path shared bike pedestrians, cycling allowed
        highway %in% c(pedestrian_street_types, "bridleway", "track") & 
          (segregated != "yes") & (!bicycle %in% c("yes", "permissive", "designated")) ~ 0.2 # dismount bike
      ),
      car_traffic = case_when(
        (! cycleway_combined %in% cycleway_options) & highway %in% c("primary", "primary_link", "trunk", "trunk_link") & !bicycle %in% c("no") ~ 0.2,
        (! cycleway_combined %in% cycleway_options) & highway %in% c("secondary", "secondary_link") & !bicycle %in% c("no") ~ 0.4,
        (! cycleway_combined %in% cycleway_options) & highway %in% c("tertiary", "tertiary_link", "road", "unclassified") & !bicycle %in% c("no") ~ 0.6,
        (! cycleway_combined %in% cycleway_options) & highway %in% c("living_street", "residential") & !bicycle %in% c("no") ~ 0.8,
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
      cbindex_street_quality_forward = case_when(
        !is.na(cbindex_surface_forward) & !is.na(cbindex_cycleways_forward) ~ round(sqrt(cbindex_surface_forward * cbindex_cycleways_forward), 1),
        is.na(cbindex_surface_forward) ~ cbindex_cycleways_forward,
        is.na(cbindex_cycleways_forward) ~ cbindex_surface_forward
      ),
      cbindex_street_quality_backward = case_when(
        !is.na(cbindex_surface_backward) & !is.na(cbindex_cycleways_backward) ~ round(sqrt(cbindex_surface_backward * cbindex_cycleways_backward), 1),
        is.na(cbindex_surface_backward) ~ cbindex_cycleways_backward,
        is.na(cbindex_cycleways_backward) ~ cbindex_surface_backward
      ),
      cbindex = case_when(
        !is.na(cbindex_street_quality) & !is.na(cbindex_barrier) ~ round(cbindex_street_quality * cbindex_barrier, 1),
        is.na(cbindex_street_quality) ~ cbindex_barrier,
        is.na(cbindex_barrier) ~ cbindex_street_quality
      ),
      cbindex_forward = case_when(
        !is.na(cbindex_street_quality_forward) & !is.na(cbindex_barrier) ~ round(cbindex_street_quality_forward * cbindex_barrier, 1),
        is.na(cbindex_street_quality_forward) ~ cbindex_barrier,
        is.na(cbindex_barrier) ~ cbindex_street_quality_forward
      ),
      cbindex_backward = case_when(
        !is.na(cbindex_street_quality_backward) & !is.na(cbindex_barrier) ~ round(cbindex_street_quality_backward * cbindex_barrier, 1),
        is.na(cbindex_street_quality_backward) ~ cbindex_barrier,
        is.na(cbindex_barrier) ~ cbindex_street_quality_backward
      )
    ) %>% 
    mutate(
      cbindex_surface = ifelse(two_directions_w_cycleways, NA, cbindex_surface),
      cbindex_surface_forward = ifelse(two_directions_w_cycleways, cbindex_surface_forward, NA),
      cbindex_surface_backward = ifelse(two_directions_w_cycleways, cbindex_surface_backward, NA),
      cbindex_cycleways = ifelse(two_directions_w_cycleways, NA, cbindex_cycleways),
      cbindex_cycleways_forward = ifelse(two_directions_w_cycleways, cbindex_cycleways_forward, NA),
      cbindex_cycleways_backward = ifelse(two_directions_w_cycleways, cbindex_cycleways_backward, NA),
      cbindex_street_quality = ifelse(two_directions_w_cycleways, NA, cbindex_street_quality),
      cbindex_street_quality_forward = ifelse(two_directions_w_cycleways, cbindex_street_quality_forward, NA),
      cbindex_street_quality_backward = ifelse(two_directions_w_cycleways, cbindex_street_quality_backward, NA),
      cbindex = ifelse(two_directions_w_cycleways, NA, cbindex),
      cbindex_forward = ifelse(two_directions_w_cycleways, cbindex_forward, NA),
      cbindex_backward = ifelse(two_directions_w_cycleways, cbindex_backward, NA)
    )
  
  if(source == "pbf") {
    return(list(streets %>%
                  select(osm_id, name, highway, direction,
                         cbindex, cbindex_forward, cbindex_backward, 
                         cbindex_street_quality, cbindex_street_quality_forward, cbindex_street_quality_backward,
                         cbindex_cycleways, cbindex_cycleways_forward, cbindex_cycleways_backward,
                         cycleway_combined, cycleway_width_combined, cycleway_oneway_combined,
                         cbindex_surface, surface_combined, smoothness_combined, 
                         cbindex_barrier, which_barrier, min_maxwidth, kerb, kerb_height,
                         pedestrian_traffic, car_traffic,
                         maxspeed, bicycle_road, segregated, dismount_necessary),
                barriers %>% 
                  filter(on_street) %>% 
                  select(barrier, maxwidth_barriers_combined, kerb, height)))
  } else if (source == "postgis") {
    # write index to database
    # write cbi, cbi:forward, cbi:backward, min_maxwidth, kerb_height
    
    streets$tags <- new_hstore(1)
    streets[!is.na(streets$cbindex),]$tags %->% "cbi" <- streets[!is.na(streets$cbindex),]$cbindex
    streets[!is.na(streets$cbindex_forward),]$tags %->% "cbi_forward" <- streets[!is.na(streets$cbindex_forward),]$cbindex_forward
    streets[!is.na(streets$cbindex_backward),]$tags %->% "cbi_backward" <- streets[!is.na(streets$cbindex_backward),]$cbindex_backward
    streets[!is.na(streets$min_maxwidth),]$tags %->% "min_maxwidth" <- streets[!is.na(streets$min_maxwidth),]$min_maxwidth
    streets[!is.na(streets$kerb_height),]$tags %->% "kerb_height" <- streets[!is.na(streets$kerb_height),]$kerb_height
    
    streets <- streets %>% 
      mutate(id = as.numeric(osm_id))
    
    # run query in intervall (works better with db?)
    tic()
    intervall_step <- 5000
    intervall_min <- 1
    intervall_max <- 0
    while(intervall_min < nrow(streets)) {
      intervall_max <- intervall_max + intervall_step
      intervall_max <- ifelse(intervall_max > nrow(streets), nrow(streets), intervall_max)
      #print(intervall_min)
      print(intervall_max)
      index_chunk <- streets[intervall_min:intervall_max, ]
      
      postgis_update(con, index_chunk, "ways", id_cols = "id", 
                     update_cols = "tags",
                     hstore_name = "tags"
                     #, hstore_concat = FALSE
      )
      intervall_min <- intervall_min + intervall_step
    }
    toc()
    
    dbDisconnect(con)
    return(TRUE)
  }
  
}

#TODO: adjust for new function
shift_streets <- function(streets) {
  # # get all streets with a cycleway
  split_streets <- (( streets$cycleway_right %in% cycleway_options) |
                       ( streets$cycleway_left %in% cycleway_options)) |
    (!is.na(streets$bicycle_lanes_forward) |
       !is.na(streets$bicycle_lanes_backward)) |
    ( streets$cycleway %in% cycleway_options & (!streets$oneway %in% c("yes"))) | # only cycleway (without right / left specification) and not oneway
    ( streets$cycleway_both %in% cycleway_options) # cyclway:both

  ### all cycleways "left" or "backward"
  only_left <- streets[split_streets, ] %>%
    filter( cycleway_left %in% cycleway_options|
              cycleway %in% cycleway_options |
              cycleway_both %in% cycleway_options |
              cycleway_left %in% cycleway_options |
              !is.na(bicycle_lanes_backward)) %>%
    mutate( # remove information all information for "right" or "forward"
      direction = "backward",
      cycleway_right = NA,
      cycleway_right_surface = NA,
      cycleway_right_smoothness = NA,
      cycleway_right_width = NA,
      cycleway_right_oneway = NA,
      bicycle_lanes_forward = NA,
      width_lanes_forward = NA
    )

  ### all cycleways "right" or "forward"
  only_right <- streets[split_streets, ] %>%
    filter( cycleway_right %in% cycleway_options|
              cycleway %in% cycleway_options |
              cycleway_both %in% cycleway_options |
              cycleway_left %in% cycleway_options |
              !is.na(bicycle_lanes_forward)) %>%
    mutate(
      direction = "forward",
      cycleway_left = NA,
      cycleway_left_surface = NA,
      cycleway_left_smoothness = NA,
      cycleway_left_width = NA,
      cycleway_left_oneway = NA,
      bicycle_lanes_backward = NA,
      width_lanes_backward = NA
    )

  shift_left <- function(only_left) {
    only_left <- only_left %>%
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
    only_left <- st_transform(only_left, 4326)

    return(only_left)
  }

  shift_right <- function(only_right) {
    only_right <- only_right %>%
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

    # takes 86 sec for Ba-Wü
    for (i in 1:nrow(only_right)) {
      new_geo_r <- c(new_geo_r, st_geometry(only_right[i, ]) + c(diff_r[i, ]$east, diff_r[i, ]$north))
    }
    st_geometry(only_right) <- st_sfc(new_geo_r, crs = 3035)
    only_right <- st_transform(only_right, 4326)


    return(only_right)
  }

  if(with_line_shift){
    only_left <- shift_left(only_left)
    only_right <- shift_right(only_right)
  }

  # join split right and left cycleways back with other streets
  streets <- streets %>%
    # remove
    filter(!osm_id %in% (c(only_right$osm_id, only_left$osm_id))) %>%
    mutate(direction = "both") %>%
    # bind new left cycleways
    rbind(only_left) %>%
    # bind new right cycleways
    rbind(only_right) %>%
    mutate(
      bicycle_lanes_combined = coalesce(bicycle_lanes_forward, bicycle_lanes_backward, bicycle_lanes),
      width_lanes_combined = coalesce(width_lanes_forward, width_lanes_backward, width_lanes)
    )
  
}

pedestrian_traffic_data <- function(pbf_file_name, geo_clip_file_path) {
  ## pedestrian traffic
  # markets <- oe_get(
  #   pbf_file_name,
  #   # force_download = T,
  #   # force_vectortranslate = TRUE,
  #   # quiet = T,
  #   stringsAsFactors = F,
  #   max_file_size = 1000000000, # ~ 1GB - default is too small
  #   extra_tags = c("opening_hours", "amenity"),
  #   layer = "points",
  #   query = "SELECT * FROM 'points' WHERE  amenity = 'marketplace'"
  # )
  # 
  markets <- oe_read(here("data", "berlin-latest.osm.pbf"),
                     stringsAsFactors = F,
                     extra_tags = c("opening_hours", "amenity"),
                     query = "SELECT * FROM 'points' WHERE  amenity = 'marketplace'")
  
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
  
  return (select(markets, name, opening_hours))
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
