options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

data_dir <- here::here("data/")

# ----------------------------
# Boundaries (OK)
# ----------------------------
ok <- tigris::states(cb = TRUE, year = 2023) %>%
  filter(STUSPS == "OK") %>%
  st_make_valid() %>%
  st_transform(4326)

ok_bbox <- st_bbox(ok)
ok_bounds <- list(
  xmin = as.numeric(ok_bbox["xmin"]),
  ymin = as.numeric(ok_bbox["ymin"]),
  xmax = as.numeric(ok_bbox["xmax"]),
  ymax = as.numeric(ok_bbox["ymax"])
)

