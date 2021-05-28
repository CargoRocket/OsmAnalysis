
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