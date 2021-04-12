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
archive_destination_path_csv <- here("data", paste0(Sys.Date(), "_ranking.csv"))
path_to_maps <- here("docs", "datenrennen_maps")
dir.create(path_to_maps, showWarnings = F)

# cycleway ranking: surface, smoothness, width -----------------------------

extra_tags <- c(
  "cycleway", "cycleway:right", "cycleway:left", "cycleway:both",
  "surface", "cycleway:surface", "cycleway:right:surface", "cycleway:left:surface", "cycleway:both:surface", 
  "smoothness", "cycleway:smoothness", "cycleway:right:smoothness","cycleway:left:smoothness", "cycleway:both:smoothness", 
  "width", "cycleway:width", "cycleway:right:width", "cycleway:left:width", "cycleway:both:width"
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
  query = "SELECT * FROM 'lines' WHERE highway = 'cycleway' OR cycleway IN ('track', 'lane', 'opposite_lane') OR cycleway_right IN ('track', 'lane', 'opposite_lane') OR cycleway_left IN ('track', 'lane', 'opposite_lane') OR cycleway_both IN ('track', 'lane', 'opposite_lane')"
)

cycleways$width_cleaned <- gsub("[^0-9.-]", "", cycleways$width) %>% as.numeric()
cycleways$cycleway_width_cleaned <- gsub("[^0-9.-]", "", cycleways$cycleway_width) %>% as.numeric()
cycleways$cycleway_right_width_cleaned <- gsub("[^0-9.-]", "", cycleways$cycleway_right_width) %>% as.numeric()
cycleways$cycleway_left_width_cleaned <- gsub("[^0-9.-]", "", cycleways$cycleway_left_width) %>% as.numeric()
cycleways$cycleway_both_width_cleaned <- gsub("[^0-9.-]", "", cycleways$cycleway_both_width) %>% as.numeric()

# duplicate streets with separate cycleway:right and cycleway:left
only_left <- cycleways %>%
  filter(!is.na(cycleway_right) & !is.na(cycleway_left)) %>%
  mutate(
    cycleway_right = NA,
    cycleway_right_surface = NA,
    cycleway_right_smoothness = NA,
    cycleway_right_width = NA,
    cycleway_right_width_cleaned = NA
  ) %>%
  st_transform(3035)
only_right <- cycleways %>%
  filter(!is.na(cycleway_right) & !is.na(cycleway_left)) %>%
  mutate(
    cycleway_left = NA,
    cycleway_left_surface = NA,
    cycleway_left_smoothness = NA,
    cycleway_left_width = NA,
    cycleway_left_width_cleaned = NA
  ) %>%
  st_transform(3035)

start_points <- st_line_sample(st_geometry(only_left), sample = 0)
end_points <- st_line_sample(only_left, sample = 1)

diff_l <- (end_points - start_points) %>%
  st_coordinates() %>%
  as_tibble() %>%
  mutate(
    north = ifelse(X > 5, 3, 0), # shift north if line goes east
    east = ifelse(Y < -5, 3, 0)
  ) # shift east if line goes south
diff_r <- (end_points - start_points) %>%
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
  new_geo_r <- c(new_geo_r, st_geometry(only_right[i, ]) + c(diff_r[i, ]$east, diff_r[i, ]$north))
}
st_geometry(only_left) <- st_sfc(new_geo_l, crs = 3035)
st_geometry(only_right) <- st_sfc(new_geo_r, crs = 3035)

# combine all cycleway variables to one
cycleways <- cycleways %>%
  filter(!(!is.na(cycleway_right) & !is.na(cycleway_left))) %>%
  # remove lines with both cycleway:right and :left
  rbind(st_transform(only_left, 4326)) %>%
  # bind new left cycleways
  rbind(st_transform(only_right, 4326)) %>%
  # bind new right cycleways
  mutate(
    cycleway_combined = coalesce(cycleway, cycleway_right, cycleway_left, cycleway_both),
    cycleway_width_combined = coalesce(
      cycleway_width_cleaned, cycleway_right_width_cleaned,
      cycleway_left_width_cleaned, cycleway_both_width_cleaned
    )
  ) %>%
  mutate(
    cycleway_combined = ifelse(highway == "cycleway", "track", cycleway_combined), # also add information for highway = cycleway
    cycleway_width_combined = ifelse(highway == "cycleway", width_cleaned, cycleway_width_combined)
  )

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

# intersection takes about 40 seconds
cycleways_intersected <- cycleways %>%
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
  extra_tags = c("maxwidth", "maxwidth:physical"),
  layer = 'points',
  query = "SELECT * FROM 'points' WHERE barrier IN ('bollard', 'block', 'cycle_barrier') "
)

barriers$maxwidth_cleaned <- gsub("[^0-9.-]", "", barriers$maxwidth) %>% as.numeric()
barriers$maxwidth_physical_cleaned <- gsub("[^0-9.-]", "", barriers$maxwidth_physical) %>% as.numeric()
barriers <- barriers %>%
  mutate(maxwidth_barriers_combined = pmin(maxwidth_cleaned, maxwidth_physical_cleaned, na.rm = T))

# the intersection with landkreise takes a ~90 seconds
barriers_intersected <- barriers %>%
  select(barrier, maxwidth_barriers_combined) %>%
  st_transform(3035) %>% 
  st_intersection(st_transform(landkreise, 3035)) %>% 
  st_transform(4326)

barrier_ranking <- barriers_intersected %>%
  st_drop_geometry() %>%
  group_by(kreis_name) %>%
  summarize(
    count_barriers = n(),
    n_with_barrier_maxwidth = sum(!is.na(maxwidth_barriers_combined))
  ) %>%
  mutate(perc_with_barrier_maxwidth = round(n_with_barrier_maxwidth / count_barriers * 100))


# combine to singe ranking  -----------------------------------------------

combined_ranking <- cycleway_ranking %>%
  left_join(barrier_ranking, by = "kreis_name") %>%
  mutate(perc_total = round(((3 * count_cycleways - count_missing) + n_with_barrier_maxwidth) /
                              (count_barriers + 3 * count_cycleways) * 100)) %>% 
  select(
    kreis_name, count_cycleways, count_barriers, n_with_width, perc_with_width,
    n_with_surface, perc_with_surface, n_with_smoothness, perc_with_smoothness,
    n_with_barrier_maxwidth, perc_with_barrier_maxwidth, perc_total
  ) %>%
  arrange(desc(perc_total))

# write to csv

if(file.exists(destination_path_csv)){
  file.remove(destination_path_csv) # problems with rights to overwrite(?)
}
write_csv(combined_ranking, destination_path_csv)
write_csv(combined_ranking, archive_destination_path_csv)


# create map --------------------------------------------------------------

palette <- colorBin(c("#479E8F", "#f4d03f", "#E76F51", "#B60202"), domain = c(0:4), bins = 4) # , green, yellow, orange, red
palette_barrier <- colorNumeric(c("#479E8F"), domain = c(0:100), na.color = "#B60202")

for (lk in landkreise$kreis_name) {
  cw_lk <- cycleways_intersected %>%
    filter(kreis_name == lk)
  
  b_lk <- barriers_intersected %>%
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
      overlayGroups = c("Radwege", "Barrieren"),
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
    addLegend(
      colors = palette(0:3),
      labels = (0:3),
      opacity = 0.8,
      position = "bottomleft", title = "Fehlende Werte für Radwege (Linien)"
    ) %>%
    addLegend(
      colors = palette_barrier(c(0, NA)),
      labels = c("Breite vorhanden", "fehlende Breite"),
      opacity = 0.8,
      position = "bottomright", title = "Barrieren (Punkte)"
    )
  
  saveWidget(m, file.path(path_to_maps, paste0(lk, "_map.html")),
             title = paste("Datenrennen", lk), selfcontained = T)
}
