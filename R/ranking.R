# install.packages("librarian", quiet = T)
# librarian::shelf(osmextract, sf, dplyr, stringr, here, leaflet, leafem, htmlwidgets, readr)

library(osmextract)
library(sf)
library(dplyr)
library(stringr)
library(here)
library(leaflet)
library(leafem)
library(htmlwidgets)
library(readr)

mapbox_key <- "pk.eyJ1IjoibHhuZHJrcHAiLCJhIjoiY2tuNHBiaWVhMGt5czJvbzBwZXY3aDg3MCJ9.VvzhWzj9LSz0K68lJoBhng"

landkreise <- read_sf(here("data", "bw_landkreise.geojson")) %>%
  select(-regierungsbezirk_id, -klasse, -beginn, -ende)
destination_path_csv <- here("data", "ranking.csv")
baseline_path_csv <- here("data", "baseline_ranking.csv")
archive_destination_path_csv <- here("data", paste0(Sys.Date(), "_ranking.csv"))
path_to_maps <- here("docs", "datenrennen_maps")
dir.create(path_to_maps, showWarnings = F)

# cycleway ranking: surface, smoothness, width -----------------------------

extra_tags <- c(
  "cycleway", "cycleway:right", "cycleway:left", "cycleway:both",
  "surface", "cycleway:surface", "cycleway:right:surface", "cycleway:left:surface", "cycleway:both:surface", 
  "smoothness", "cycleway:smoothness", "cycleway:right:smoothness","cycleway:left:smoothness", "cycleway:both:smoothness", 
  "width", "cycleway:width", "cycleway:right:width", "cycleway:left:width", "cycleway:both:width", 
  "width:lanes", "width:lanes:forward", "width:lanes:backward",
  "bicycle:lanes", "bicycle:lanes:forward", "bicycle:lanes:backward",
  "oneway", "bicycle_road"
)

options(timeout = 240) # prevent timeout when downloading pbf file
cycleways <- oe_get(
  "Baden-Württemberg",
  force_download = T,
  # force_vectortranslate = TRUE,
  # quiet = T,
  stringsAsFactors = F,
  max_file_size = 1000000000, # ~ 1GB - default is too small
  extra_tags = extra_tags,
  query = "SELECT * FROM 'lines' WHERE highway = 'cycleway' OR cycleway IN ('track', 'lane', 'opposite_lane') OR cycleway_right IN ('track', 'lane', 'opposite_lane') OR cycleway_left IN ('track', 'lane', 'opposite_lane') OR cycleway_both IN ('track', 'lane', 'opposite_lane') OR bicycle_road = 'yes'"
)

bicycle_roads <- filter(cycleways, bicycle_road %in% ("yes"))
cycleways <- filter(cycleways, ! bicycle_road %in% ("yes"))

# if only bicycle_lanes_forward, then remove width backward (bc this is then only applicable to road but not cycleway) and vice versa
cycleways [is.na(cycleways$bicycle_lanes_forward), ]$width_lanes_forward <- NA
cycleways [is.na(cycleways$bicycle_lanes_backward), ]$width_lanes_backward <- NA

cycleways$width_cleaned <- gsub("[^0-9.-]", "", cycleways$width) %>% as.numeric()
cycleways$cycleway_width_cleaned <- gsub("[^0-9.-]", "", cycleways$cycleway_width) %>% as.numeric()
cycleways$cycleway_right_width_cleaned <- gsub("[^0-9.-]", "", cycleways$cycleway_right_width) %>% as.numeric()
cycleways$cycleway_left_width_cleaned <- gsub("[^0-9.-]", "", cycleways$cycleway_left_width) %>% as.numeric()
cycleways$cycleway_both_width_cleaned <- gsub("[^0-9.-]", "", cycleways$cycleway_both_width) %>% as.numeric()

# duplicate streets with cycleway in both directions ---
# - cycleway:right and cycleway:left 
# or bicycle:lanes:forward and bicycle:lanes:backward
# or only cylceway/ cyclway:both (without right / left speficication)
split_streets <- ((!is.na(cycleways$cycleway_right) & !is.na(cycleways$cycleway_left)) |
  (!is.na(cycleways$bicycle_lanes_forward) & !is.na(cycleways$bicycle_lanes_backward)) |
  !is.na(cycleways$cycleway) & (! cycleways$oneway %in% "yes") |
  !is.na(cycleways$cycleway_both))

only_left <- cycleways %>%
  filter(split_streets | ! is.na(cycleway_left) | !is.na(bicycle_lanes_backward)) %>%
  mutate(
    cycleway_right = NA,
    cycleway_right_surface = NA,
    cycleway_right_smoothness = NA,
    cycleway_right_width = NA,
    cycleway_right_width_cleaned = NA,
    # lanes_forward = NA,
    bicycle_lanes_forward = NA,
    width_lanes_forward = NA
  ) %>%
  st_transform(3035)
only_right <- cycleways %>%
  filter(split_streets | ! is.na(cycleway_right) | !is.na(bicycle_lanes_forward)) %>%
  mutate(
    cycleway_left = NA,
    cycleway_left_surface = NA,
    cycleway_left_smoothness = NA,
    cycleway_left_width = NA,
    cycleway_left_width_cleaned = NA,
    # lanes_backward = NA,
    bicycle_lanes_backward = NA,
    width_lanes_backward = NA
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

start_points_right <- st_line_sample(st_geometry(only_right), sample = 0)
end_points_right <- st_line_sample(only_right, sample = 1)
diff_r <- (end_points_right - start_points_right) %>%
  st_coordinates() %>%
  as_tibble() %>%
  mutate(
    north = ifelse(X < -5, 3, 0), # shift north if line goes west
    east = ifelse(Y > 5, 3, 0)
  ) # shift east if line goes north

new_geo_l <- c()
new_geo_r <- c()
for (i in 1:nrow(only_left)) {
  new_geo_l <- c(new_geo_l, st_geometry(only_left[i, ]) + c(diff_l[i, ]$east, diff_l[i, ]$north))
}
for (i in 1:nrow(only_right)) {
  new_geo_r <- c(new_geo_r, st_geometry(only_right[i, ]) + c(diff_r[i, ]$east, diff_r[i, ]$north))
}

st_geometry(only_left) <- st_sfc(new_geo_l, crs = 3035)
st_geometry(only_right) <- st_sfc(new_geo_r, crs = 3035)

# join splitted right and left cycleways back with other cycleways
cycleways <- cycleways %>%
  # remove lines with both cycleway:right and :left
  filter(!osm_id %in% (c(only_right$osm_id, only_left$osm_id))) %>%
  # bind new left cycleways
  rbind(st_transform(only_left, 4326)) %>%
  # bind new right cycleways
  rbind(st_transform(only_right, 4326)) %>% 
  mutate(
    bicycle_lanes_combined = coalesce(bicycle_lanes_forward, bicycle_lanes_backward, bicycle_lanes),
    # lanes_combined = coalesce(lanes_forward, lanes_backward, lanes),
    width_lanes_combined = coalesce(width_lanes_forward, width_lanes_backward, width_lanes)
  )

# extract width from lanes
extract_bicycle_lane_id <- function(bicycle_lanes) {
  return(which(str_split(bicycle_lanes, pattern = "\\|")[[1]] == "designated")[1])
}
extract_bicycle_lane_width <- function(width_lanes, id) {
  return(as.numeric(str_split(width_lanes, pattern = "\\|")[[1]] [id]))
}
cycleways$bicycle_lane_id <- sapply(cycleways$bicycle_lanes_combined, extract_bicycle_lane_id)
cycleways$cycleway_width_extracted <- mapply(extract_bicycle_lane_width, cycleways$width_lanes_combined, cycleways$bicycle_lane_id)

# combine all possible cycleway and width tagging into single column
cycleways <- cycleways %>% 
  mutate(
    cycleway_combined = coalesce(cycleway_right, cycleway_left, cycleway, cycleway_both),
    cycleway_width_combined = coalesce(
      cycleway_width_extracted,
      cycleway_right_width_cleaned, cycleway_left_width_cleaned, 
      cycleway_width_cleaned, cycleway_both_width_cleaned)
  ) %>%
  mutate(
    # also add information for highway = cycleway
    cycleway_combined = ifelse(highway == "cycleway", "track", cycleway_combined), 
    cycleway_width_combined = ifelse(highway == "cycleway", width_cleaned, cycleway_width_combined)) %>% 
  # drop all cycleways "no", "opposite", "shared_busway" and others
  filter(cycleway_combined %in% c("lane", "track", "opposite_lane"))


# combine all surface variables to one
cycleways <- cycleways %>%
  mutate(
    cycleway_surface_combined = coalesce(cycleway_surface, cycleway_right_surface, cycleway_left_surface),
    cycleway_smoothness_combined = coalesce(cycleway_smoothness, cycleway_right_smoothness, cycleway_left_smoothness)
  ) %>%
  mutate(
    surface_combined = case_when(
      highway == "cycleway" ~ surface,
      !is.na(cycleway_surface_combined) ~ cycleway_surface_combined,
      cycleway_combined %in% c("lane", "opposite_lane") ~ surface, # if lane on road assume same surface as street
    ),
    smoothness_combined = case_when(
      highway == "cycleway" ~ smoothness,
      !is.na(cycleway_smoothness_combined) ~ cycleway_smoothness_combined,
      cycleway_combined %in% c("lane", "opposite_lane") ~ smoothness, # if lane on road assume same surface as street
    )
  ) %>%
  select(highway, name, cycleway_combined, cycleway_width_combined, surface_combined, smoothness_combined) # reduce columns

cycleways$missing_values <- rowSums(is.na(cycleways[, c("cycleway_width_combined", "surface_combined", "smoothness_combined")]))
bicycle_roads$missing_values <- rowSums(is.na(bicycle_roads[, c("surface", "smoothness")]))

# intersect with landkreise ---
# intersection takes about 40 seconds
cycleways_intersected <- cycleways %>%
  st_transform(3035) %>% 
  st_intersection(st_transform(landkreise, 3035)) %>% 
  st_transform(4326)

# Todo: include in csv?
bicycle_roads_intersected <- bicycle_roads %>%
  st_transform(3035) %>% 
  st_intersection(st_transform(landkreise, 3035)) %>% 
  st_transform(4326)

cycleway_ranking <- cycleways_intersected %>%
  st_drop_geometry() %>%
  group_by(kreis_name) %>%
  summarize(
    count_cycleways = n(),
    n_with_width = sum(!is.na(cycleway_width_combined)),
    n_with_surface = sum(!is.na(surface_combined)),
    n_with_smoothness = sum(!is.na(smoothness_combined)),
    count_missing = sum(missing_values, na.rm = T)
  ) %>%
  mutate(
    perc_with_width = round(n_with_width / count_cycleways * 100),
    perc_with_surface = round(n_with_surface / count_cycleways * 100),
    perc_with_smoothness = round(n_with_smoothness / count_cycleways * 100)
  )


# barrier ranking: maxwidth or maxwidth:physical --------------------------

barriers <- oe_get(
  "Baden-Württemberg",
  # quiet = F,
  extra_tags = c("maxwidth", "maxwidth:physical", "kerb"),
  layer = 'points',
  query = "SELECT * FROM 'points' WHERE barrier IN ('bollard', 'block', 'cycle_barrier', 'kerb') "
)

barriers$maxwidth_cleaned <- gsub("[^0-9.-]", "", barriers$maxwidth) %>% as.numeric()
barriers$maxwidth_physical_cleaned <- gsub("[^0-9.-]", "", barriers$maxwidth_physical) %>% as.numeric()
barriers <- barriers %>%
  mutate(maxwidth_barriers_combined = pmin(maxwidth_cleaned, maxwidth_physical_cleaned, na.rm = T))

# the intersection with landkreise takes about ~90 seconds
barriers_intersected <- barriers %>%
  select(barrier, kerb, maxwidth_barriers_combined) %>%
  st_transform(3035) %>% 
  st_intersection(st_transform(landkreise, 3035)) %>% 
  st_transform(4326)

barrier_ranking <- barriers_intersected %>%
  filter(barrier != "kerb") %>% # no kerbs - they are listed seperately
  st_drop_geometry() %>%
  group_by(kreis_name) %>%
  summarize(
    count_barriers = n(),
    n_with_barrier_maxwidth = sum(!is.na(maxwidth_barriers_combined))
  ) %>%
  mutate(
    perc_with_barrier_maxwidth = round(n_with_barrier_maxwidth / count_barriers * 100))

kerb_ranking <- barriers_intersected %>%
  filter(barrier == "kerb") %>% # filter only kerbs
  st_drop_geometry() %>%
  group_by(kreis_name) %>%
  summarize(count_kerbs = n()) 

# combine to singe ranking  -----------------------------------------------

combined_ranking <- cycleway_ranking %>%
  left_join(barrier_ranking, by = "kreis_name") %>%
  left_join(kerb_ranking, by = "kreis_name") %>%
  mutate(count_barriers = ifelse(is.na(count_barriers), 0, count_barriers),
    count_kerbs = ifelse(is.na(count_kerbs), 0, count_kerbs)) %>% 
  mutate(perc_total = round(((3 * count_cycleways - count_missing) + n_with_barrier_maxwidth) /
    (count_barriers + 3 * count_cycleways) * 100)) %>% 
  select(
    kreis_name, count_cycleways, count_barriers, count_kerbs, n_with_width, perc_with_width,
    n_with_surface, perc_with_surface, n_with_smoothness, perc_with_smoothness,
    n_with_barrier_maxwidth, perc_with_barrier_maxwidth, perc_total
  ) %>%
  arrange(desc(perc_total))

# get baseline
if(!file.exists(baseline_path_csv)){
  baseline <- combined_ranking
  write_csv(combined_ranking, baseline_path_csv) # if no baseline file present - write new one
} else {
  baseline <- read_csv(baseline_path_csv)
}

# add change from baseline
combined_ranking <- combined_ranking %>% 
  left_join(baseline[,c("kreis_name", "count_cycleways", "count_barriers", "count_kerbs")], 
            suffix = c("", "_base"), by = "kreis_name") %>% 
  mutate(added_cycleways = count_cycleways - count_cycleways_base,
         added_barriers = count_barriers - count_barriers_base,
         added_kerbs = count_kerbs - count_kerbs_base)

# write to csv
if(file.exists(destination_path_csv)){
  file.remove(destination_path_csv) # problems with rights to overwrite(?)
}
write_csv(combined_ranking, destination_path_csv)
write_csv(combined_ranking, archive_destination_path_csv)


# create map --------------------------------------------------------------

palette <- colorBin(c("#479E8F", "#f4d03f", "#E76F51", "#B60202"), domain = c(0:4), bins = 4) # , green, yellow, orange, red
palette_barrier <- colorNumeric(c("#479E8F"), domain = c(0:100), na.color = "#B60202")
palette_kerb <- colorFactor(c("#479E8F"), domain = c("lowered", "raised", "flush"), na.color = "#B60202")

for (lk in landkreise$kreis_name) {
  cw_lk <- cycleways_intersected %>%
    filter(kreis_name == lk)
  
  br_lk <- bicycle_roads_intersected %>%
    filter(kreis_name == lk)
  
  b_lk <- barriers_intersected %>%
    filter(barrier != "kerb") %>% # no kerbs - they are listed seperately
    filter(kreis_name == lk)
  
  k_lk <- barriers_intersected %>%
    filter(barrier == "kerb") %>% # only kerbs
    filter(kreis_name == lk)

  m <- leaflet(cw_lk) %>%
    addLogo("https://cargorocket.de/assets/images/cargorocket-logo.svg",
      src = "remote",
      position = "topleft",
      width = "150px",
    ) %>%
    fitBounds(st_bbox(cw_lk)[[1]], st_bbox(cw_lk)[[2]], st_bbox(cw_lk)[[3]], st_bbox(cw_lk)[[4]]) %>%
    addTiles(
      urlTemplate = paste0("https://api.mapbox.com/styles/v1/mapbox/light-v10/tiles/512/{z}/{x}/{y}?access_token=", mapbox_key),
      attribution = '\u00a9 <a href="https://www.mapbox.com/about/maps/">Mapbox</a> \u00a9 <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>',
      group = "Mapbox Light"
    ) %>%
    addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
    addControl(lk, position = "topright") %>%
    addLayersControl(
      baseGroups = c("Mapbox Light", "OpenStreetMap"),
      overlayGroups = c("Fahrradstraßen", "Radwege", "Barrieren", "Bordsteine"),
      options = layersControlOptions(collapsed = F),
      position = "topright"
    ) %>%
    addPolygons(
      data = filter(landkreise, kreis_name == lk),
      fillColor = "transparent",
      color = "black",
      opacity = 0.5
    ) %>%
    addPolylines(
      data = br_lk,
      group = "Fahrradstraßen",
      popup = ~ paste(
        "Anzahl fehlende Werte:", missing_values,
        "<br>", "Oberfläche (surface):", surface,
        "<br>", "Ebenheit (smoothness):", smoothness
      ),
      color = ~ palette(missing_values),
      opacity = 0.9,
      weight = 4
    ) %>%
    addPolylines(
      data = cw_lk,
      group = "Radwege",
      popup = ~ paste(
        "Anzahl fehlende Werte:", missing_values,
        "<br>", "Radweg:", cycleway_combined,
        "<br>", "Radweg Oberfläche (surface):", surface_combined,
        "<br>", "Radweg Ebenheit (smoothness):", smoothness_combined,
        "<br>", "Radweg Breite (width):", cycleway_width_combined
      ),
      color = ~ palette(missing_values),
      opacity = 0.9,
      weight = 4
    ) %>%
    addCircleMarkers(
      data = b_lk,
      group = "Barrieren",
      popup = ~ paste(
        "Barriere:", barrier,
        "<br>", "Max. Breite:", maxwidth_barriers_combined),
      color = "transparent",
      fillColor = ~ palette_barrier(maxwidth_barriers_combined),
      fillOpacity = 0.8,
      radius = 3
    ) %>%
    addCircleMarkers(
      data = k_lk,
      group = "Bordsteine",
      popup = ~ paste("Bordstein:", kerb),
      color = "transparent",
      fillColor = ~ palette_kerb(kerb),
      fillOpacity = 0.8,
      radius = 3
    ) %>%
    addLegend(
      colors = palette(0:3),
      labels = (0:3),
      opacity = 0.8,
      position = "bottomleft", title = "Fehlende Werte für Radwege (Linien)"
    ) %>%
    addLegend(
      colors = palette_barrier(c(0, NA)),
      labels = c("Breite (bzw. Höhe) vorhanden", "fehlende Breite (bzw. Höhe)"),
      opacity = 0.8,
      position = "bottomright", title = "Barrieren & Bordsteine (Punkte)"
    )

  saveWidget(m, file.path(path_to_maps, paste0(lk, "_map.html")),
             title = paste("CargoRocket Mapathon", lk), selfcontained = T)
}
