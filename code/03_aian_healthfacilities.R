# Facilities (AI/AN)
# ----------------------------
aian_facilities <- read.csv(file.path(data_dir, "ok_ihs_locations.csv"))

FAC_LON_COL  <- "long"
FAC_LAT_COL  <- "lat"
FAC_NAME_COL <- "Facility_Name"

facility_colors <- c(
  "IHS" = "#7B1FA2",
  "Title 1 (Tribal)" = "#D32F2F",
  "Title 5 (638)" = "#F57C00",
  "Urban" = "#212121"
)

aian_facilities_sf <- aian_facilities %>%
  mutate(
    Affiliation = as.character(Affiliation),
    lon = .data[[FAC_LON_COL]],
    lat = .data[[FAC_LAT_COL]],
    fac_color = unname(facility_colors[Affiliation]),
    fac_color = ifelse(is.na(fac_color), "#999999", fac_color),
    fac_label = if (FAC_NAME_COL %in% names(.)) {
      paste0(.data[[FAC_NAME_COL]], " (", Affiliation, ")")
    } else {
      paste0("Facility (", Affiliation, ")")
    }
  ) %>%
  mutate(
    lon = as.numeric(lon),
    lat = as.numeric(lat)
  ) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

add_facilities <- function(m) {
  m %>%
    addCircleMarkers(
      data = aian_facilities_sf,
      lng = ~lon,
      lat = ~lat,
      radius = 3,
      stroke = TRUE,
      weight = 1,
      color = "black",
      fillColor = ~fac_color,
      fillOpacity = 0.9,
      opacity = 1,
      label = ~fac_label
    )
}
