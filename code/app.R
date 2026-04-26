# ============================================================
# Multidimensional SDOH: 6 domains + domain scores + Shiny MVP
# Revised Sandbox:
# - includes domains + outcomes
# - sandbox uses LOG-SCALE outcomes for stability:
#     log1p(cn_gdm_rate)
#     log1p(aian_gdm_rate)
# - sandbox plot shows BOTH domains and outcomes
# - lowers sandbox model minimum complete rows to 5 so CN GDM rate
#   is less likely to be skipped
#
# IMPORTANT:
# This code assumes your data contain these columns:
#   cn_gdm_rate, aian_gdm_rate, all_birth_rate, AIAN_birth_rate
# ============================================================

library(dplyr)
library(tidyr)
library(sf)
library(stringr)
library(ggplot2)
library(shiny)
library(leaflet)
library(plotly)
library(purrr)
library(DT)
library(scales)
library(htmltools)
library(tidycensus)
library(tigris)
library(janitor)
library(here)

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

# ----------------------------
# Load data
# ----------------------------
merged_clean <- readRDS(file.path(data_dir, "merged_clean.RDS")) %>%
  st_transform(4326) %>%
  mutate(
    ins_aian_ihs_rev = -ins_aian_ihs,
    ins_aian_medicaid_rev = -ins_aian_medicaid
  )

# ----------------------------
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

# ----------------------------
# Define 6 SDOH domains
# ----------------------------
sdoh_domains <- list(
  Economic_Instability = c(
    "pct_population_below_poverty",
    "pct_labor_force_unemployed",
    "prop_SNAP",
    "prop_LOWI"
  ),
  Education_Attainment = c(
    "pct_adults_25plus_hs_or_higher",
    "pct_adults_25plus_bachelors_or_higher",
    "edu_aian_0_8_years",
    "edu_aian_9_11_years",
    "edu_aian_12_years",
    "edu_aian_13_15_years",
    "edu_aian_16plus_years"
  ),
  Housing_Transport = c(
    "pct_occupied_housing_renter",
    "pct_renter_households_severely_cost_burdened",
    "pct_households_overcrowded",
    "pct_households_no_vehicle",
    "pct_households_lacking_complete_plumbing",
    "pct_housing_built_pre_1980"
  ),
  Health_Access = c(
    "pct_population_uninsured",
    "ins_aian_self_pay",
    "ins_aian_ihs_rev",
    "ins_aian_medicaid_rev"
  ),
  Environment_Burden = c(
    "PM2_5",
    "prop_LAPOP1_10"
  ),
  Social_Context = c(
    "pct_population_with_disability",
    "aian_married",
    "pct_population_aian_alone",
    "pct_population_two_or_more_races",
    "pct_population_white_age_0_17",
    "pct_population_white_age_18_64",
    "pct_population_white_age_65_plus",
    "pct_population_black_age_0_17",
    "pct_population_black_age_18_64",
    "pct_population_black_age_65_plus",
    "pct_population_aian_age_0_17",
    "pct_population_aian_age_18_64",
    "pct_population_aian_age_65_plus"
  )
)

# ----------------------------
# Outcomes
# ----------------------------
outcomes_vars <- c(
  "cn_gdm_rate",
  "aian_gdm_rate",
  "all_birth_rate",
  "AIAN_birth_rate"
)

sandbox_outcome_vars_log <- c(
  "cn_gdm_rate_log",
  "aian_gdm_rate_log"
)

id_vars <- c("GEOID", "NAME", "county", "year")

# ----------------------------
# Labels
# ----------------------------
indicator_labels <- c(
  pct_population_below_poverty                 = "Population below poverty (%)",
  pct_labor_force_unemployed                   = "Unemployment rate (%)",
  prop_SNAP                                    = "Households receiving SNAP (%)",
  prop_LOWI                                    = "Low-income households (%)",
  pct_adults_25plus_hs_or_higher               = "Adults 25+ with HS diploma or higher (%)",
  pct_adults_25plus_bachelors_or_higher        = "Adults 25+ with Bachelor’s degree or higher (%)",
  edu_aian_0_8_years                           = "AI/AN: <9 years of education (%)",
  edu_aian_9_11_years                          = "AI/AN: 9–11 years of education (%)",
  edu_aian_12_years                            = "AI/AN: HS graduate (12 years) (%)",
  edu_aian_13_15_years                         = "AI/AN: Some college/Associate (13–15 years) (%)",
  edu_aian_16plus_years                        = "AI/AN: Bachelor’s or higher (16+ years) (%)",
  pct_occupied_housing_renter                  = "Renter-occupied housing (%)",
  pct_renter_households_severely_cost_burdened = "Severely cost-burdened renters (%)",
  pct_households_overcrowded                   = "Overcrowded households (%)",
  pct_households_no_vehicle                    = "Households with no vehicle (%)",
  pct_households_lacking_complete_plumbing     = "Households lacking complete plumbing (%)",
  pct_housing_built_pre_1980                   = "Housing built before 1980 (%)",
  pct_population_uninsured                     = "Uninsured population (%)",
  ins_aian_ihs                                 = "AI/AN: Indian Health Service coverage (%)",
  ins_aian_medicaid                            = "AI/AN: Medicaid coverage (%)",
  ins_aian_self_pay                            = "AI/AN: Self-pay / uninsured (%)",
  PM2_5                                        = "Fine particulate air pollution (PM2.5)",
  prop_LAPOP1_10                               = "Low access to parks/open space (%)",
  pct_population_with_disability               = "Population with disability (%)",
  aian_married                                 = "AI/AN married (%)",
  pct_population_aian_alone                    = "Population AI/AN alone (%)",
  pct_population_two_or_more_races             = "Population two or more races (%)",
  pct_population_white_age_0_17                = "White population age 0–17 (%)",
  pct_population_white_age_18_64               = "White population age 18–64 (%)",
  pct_population_white_age_65_plus             = "White population age 65+ (%)",
  pct_population_black_age_0_17                = "Black population age 0–17 (%)",
  pct_population_black_age_18_64               = "Black population age 18–64 (%)",
  pct_population_black_age_65_plus             = "Black population age 65+ (%)",
  pct_population_aian_age_0_17                 = "AI/AN population age 0–17 (%)",
  pct_population_aian_age_18_64                = "AI/AN population age 18–64 (%)",
  pct_population_aian_age_65_plus              = "AI/AN population age 65+ (%)"
)

outcome_labels <- c(
  "CN gestational diabetes rate"    = "cn_gdm_rate",
  "AI/AN gestational diabetes rate" = "aian_gdm_rate",
  "All births rate"                 = "all_birth_rate",
  "AI/AN births rate"               = "AIAN_birth_rate"
)

sandbox_outcome_labels <- c(
  "CN gestational diabetes rate (log scale)"    = "cn_gdm_rate_log",
  "AI/AN gestational diabetes rate (log scale)" = "aian_gdm_rate_log"
)

label_for_indicator <- function(x) {
  ifelse(x %in% names(indicator_labels), indicator_labels[x], x)
}

pretty_domain <- function(x) {
  x %>%
    str_replace_all("_", " ") %>%
    str_squish() %>%
    str_to_title() %>%
    str_replace("^Housing Transport$", "Housing & Transport Barriers") %>%
    str_replace("^Health Access$", "Health Care Access Barriers") %>%
    str_replace("^Social Context$", "Social & Community Context") %>%
    str_replace("^Economic Instability$", "Economic Instability")
}

wrap_2lines <- function(x) {
  str_replace(x, "^(\\S+)\\s+(.*)$", "\\1<br>\\2")
}

# ----------------------------
# Helpers
# ----------------------------
z_na <- function(x) {
  mu <- mean(x, na.rm = TRUE)
  sdv <- sd(x, na.rm = TRUE)
  if (!is.finite(sdv) || sdv == 0) return(rep(NA_real_, length(x)))
  (x - mu) / sdv
}

pct_rank_0_100 <- function(x) {
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  r <- rank(x, na.last = "keep", ties.method = "average")
  n <- sum(!is.na(x))
  if (n <= 1) return(rep(NA_real_, length(x)))
  100 * (r - 1) / (n - 1)
}

compute_domain_scores <- function(dat, domains, year_filter = NULL) {
  d <- dat
  if (!is.null(year_filter)) d <- d %>% filter(year == year_filter)
  
  all_vars <- unique(unlist(domains))
  missing_vars <- setdiff(all_vars, names(d))
  if (length(missing_vars) > 0) {
    stop("These domain variables are missing from merged_clean: ", paste(missing_vars, collapse = ", "))
  }
  
  z_df <- d %>%
    st_drop_geometry() %>%
    select(any_of(c(id_vars, all_vars))) %>%
    mutate(across(all_of(all_vars), z_na))
  
  dom_scores <- map_dfc(names(domains), function(dom) {
    vars_dom <- domains[[dom]]
    mat <- z_df %>% select(all_of(vars_dom)) %>% as.data.frame()
    tibble(!!paste0("z_", dom) := rowMeans(mat, na.rm = TRUE))
  }) %>%
    mutate(across(everything(), ~ ifelse(is.nan(.x), NA_real_, .x)))
  
  dom_pcts <- dom_scores %>%
    mutate(across(everything(), pct_rank_0_100, .names = "{.col}_pct")) %>%
    select(ends_with("_pct"))
  
  composite <- tibble(
    z_SDOH_Composite = rowMeans(dom_scores, na.rm = TRUE)
  ) %>%
    mutate(
      z_SDOH_Composite = ifelse(is.nan(z_SDOH_Composite), NA_real_, z_SDOH_Composite),
      z_SDOH_Composite_pct = pct_rank_0_100(z_SDOH_Composite)
    )
  
  bind_cols(
    d %>% select(any_of(c("GEOID", "NAME", "county", "year"))) %>% st_drop_geometry(),
    dom_scores,
    dom_pcts,
    composite,
    .name_repair = "check_unique"
  )
}

make_bins <- function(var_name, v) {
  if (grepl("_pct$", var_name)) return(seq(0, 100, by = 20))
  
  v2 <- v[is.finite(v)]
  if (length(v2) < 2) return(NULL)
  
  probs <- seq(0, 1, length.out = 7)
  b <- unique(as.numeric(quantile(v2, probs = probs, na.rm = TRUE)))
  if (length(b) < 3) return(NULL)
  b
}

legend_under_map <- function(var_label, pal, bins, na_color = "#cccccc") {
  labs <- paste0(
    format(round(head(bins, -1), 2), trim = TRUE),
    "–",
    format(round(tail(bins, -1), 2), trim = TRUE)
  )
  cols <- pal((head(bins, -1) + tail(bins, -1)) / 2)
  
  tags$div(
    style = paste(
      "margin-top:6px; padding:6px 8px;",
      "border:1px solid #e5e7eb; border-radius:8px; background:#fff;"
    ),
    tags$div(style = "font-weight:600; margin-bottom:4px; font-size:12px;", var_label),
    tags$div(
      style = "display:flex; flex-wrap:wrap; gap:10px; align-items:center;",
      lapply(seq_along(labs), function(i) {
        tags$div(
          style = "display:flex; align-items:center; gap:5px;",
          tags$span(style = paste0(
            "display:inline-block; width:12px; height:12px; border:1px solid #999; background:",
            cols[i], ";"
          )),
          tags$span(style = "font-size:10px;", labs[i])
        )
      }),
      tags$div(
        style = "display:flex; align-items:center; gap:5px;",
        tags$span(style = paste0(
          "display:inline-block; width:12px; height:12px; border:1px solid #999; background:",
          na_color, ";"
        )),
        tags$span(style = "font-size:10px;", "Missing")
      )
    )
  )
}

# ----------------------------
# Labels
# ----------------------------
indicator_labels <- c(
  pct_population_below_poverty                 = "Population below poverty (%)",
  pct_labor_force_unemployed                   = "Unemployment rate (%)",
  prop_SNAP                                    = "Households receiving SNAP (%)",
  prop_LOWI                                    = "Low-income households (%)",
  pct_adults_25plus_hs_or_higher               = "Adults 25+ with HS diploma or higher (%)",
  pct_adults_25plus_bachelors_or_higher        = "Adults 25+ with Bachelor’s degree or higher (%)",
  edu_aian_0_8_years                           = "AI/AN: <9 years of education (%)",
  edu_aian_9_11_years                          = "AI/AN: 9–11 years of education (%)",
  edu_aian_12_years                            = "AI/AN: HS graduate (12 years) (%)",
  edu_aian_13_15_years                         = "AI/AN: Some college/Associate (13–15 years) (%)",
  edu_aian_16plus_years                        = "AI/AN: Bachelor’s or higher (16+ years) (%)",
  pct_occupied_housing_renter                  = "Renter-occupied housing (%)",
  pct_renter_households_severely_cost_burdened = "Severely cost-burdened renters (%)",
  pct_households_overcrowded                   = "Overcrowded households (%)",
  pct_households_no_vehicle                    = "Households with no vehicle (%)",
  pct_households_lacking_complete_plumbing     = "Households lacking complete plumbing (%)",
  pct_housing_built_pre_1980                   = "Housing built before 1980 (%)",
  pct_population_uninsured                     = "Uninsured population (%)",
  ins_aian_ihs                                 = "AI/AN: Indian Health Service coverage (%)",
  ins_aian_medicaid                            = "AI/AN: Medicaid coverage (%)",
  ins_aian_self_pay                            = "AI/AN: Self-pay / uninsured (%)",
  PM2_5                                        = "Fine particulate air pollution (PM2.5)",
  prop_LAPOP1_10                               = "Low access to parks/open space (%)",
  pct_population_with_disability               = "Population with disability (%)",
  aian_married                                 = "AI/AN married (%)",
  pct_population_aian_alone                    = "Population AI/AN alone (%)",
  pct_population_two_or_more_races             = "Population two or more races (%)",
  pct_population_white_age_0_17                = "White population age 0–17 (%)",
  pct_population_white_age_18_64               = "White population age 18–64 (%)",
  pct_population_white_age_65_plus             = "White population age 65+ (%)",
  pct_population_black_age_0_17                = "Black population age 0–17 (%)",
  pct_population_black_age_18_64               = "Black population age 18–64 (%)",
  pct_population_black_age_65_plus             = "Black population age 65+ (%)",
  pct_population_aian_age_0_17                 = "AI/AN population age 0–17 (%)",
  pct_population_aian_age_18_64                = "AI/AN population age 18–64 (%)",
  pct_population_aian_age_65_plus              = "AI/AN population age 65+ (%)"
)

outcome_labels <- c(
  "CN gestational diabetes rate"    = "cn_gdm_rate",
  "AI/AN gestational diabetes rate" = "aian_gdm_rate",
  "All births rate"                 = "all_birth_rate",
  "AI/AN births rate"               = "AIAN_birth_rate"
)

sandbox_outcome_labels <- c(
  "CN gestational diabetes rate (log scale)"    = "cn_gdm_rate_log",
  "AI/AN gestational diabetes rate (log scale)" = "aian_gdm_rate_log"
)

label_for_indicator <- function(x) {
  ifelse(x %in% names(indicator_labels), indicator_labels[x], x)
}

pretty_domain <- function(x) {
  x %>%
    str_replace_all("_", " ") %>%
    str_squish() %>%
    str_to_title() %>%
    str_replace("^Housing Transport$", "Housing & Transport Barriers") %>%
    str_replace("^Health Access$", "Health Care Access Barriers") %>%
    str_replace("^Social Context$", "Social & Community Context") %>%
    str_replace("^Economic Instability$", "Economic Instability")
}

wrap_2lines <- function(x) {
  str_replace(x, "^(\\S+)\\s+(.*)$", "\\1<br>\\2")
}

# ----------------------------
# Helpers
# ----------------------------
z_na <- function(x) {
  mu <- mean(x, na.rm = TRUE)
  sdv <- sd(x, na.rm = TRUE)
  if (!is.finite(sdv) || sdv == 0) return(rep(NA_real_, length(x)))
  (x - mu) / sdv
}

pct_rank_0_100 <- function(x) {
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  r <- rank(x, na.last = "keep", ties.method = "average")
  n <- sum(!is.na(x))
  if (n <= 1) return(rep(NA_real_, length(x)))
  100 * (r - 1) / (n - 1)
}

compute_domain_scores <- function(dat, domains, year_filter = NULL) {
  d <- dat
  if (!is.null(year_filter)) d <- d %>% filter(year == year_filter)
  
  all_vars <- unique(unlist(domains))
  missing_vars <- setdiff(all_vars, names(d))
  if (length(missing_vars) > 0) {
    stop("These domain variables are missing from merged_clean: ", paste(missing_vars, collapse = ", "))
  }
  
  z_df <- d %>%
    st_drop_geometry() %>%
    select(any_of(c(id_vars, all_vars))) %>%
    mutate(across(all_of(all_vars), z_na))
  
  dom_scores <- map_dfc(names(domains), function(dom) {
    vars_dom <- domains[[dom]]
    mat <- z_df %>% select(all_of(vars_dom)) %>% as.data.frame()
    tibble(!!paste0("z_", dom) := rowMeans(mat, na.rm = TRUE))
  }) %>%
    mutate(across(everything(), ~ ifelse(is.nan(.x), NA_real_, .x)))
  
  dom_pcts <- dom_scores %>%
    mutate(across(everything(), pct_rank_0_100, .names = "{.col}_pct")) %>%
    select(ends_with("_pct"))
  
  composite <- tibble(
    z_SDOH_Composite = rowMeans(dom_scores, na.rm = TRUE)
  ) %>%
    mutate(
      z_SDOH_Composite = ifelse(is.nan(z_SDOH_Composite), NA_real_, z_SDOH_Composite),
      z_SDOH_Composite_pct = pct_rank_0_100(z_SDOH_Composite)
    )
  
  bind_cols(
    d %>% select(any_of(c("GEOID", "NAME", "county", "year"))) %>% st_drop_geometry(),
    dom_scores,
    dom_pcts,
    composite,
    .name_repair = "check_unique"
  )
}

make_bins <- function(var_name, v) {
  if (grepl("_pct$", var_name)) return(seq(0, 100, by = 20))
  
  v2 <- v[is.finite(v)]
  if (length(v2) < 2) return(NULL)
  
  probs <- seq(0, 1, length.out = 7)
  b <- unique(as.numeric(quantile(v2, probs = probs, na.rm = TRUE)))
  if (length(b) < 3) return(NULL)
  b
}

legend_under_map <- function(var_label, pal, bins, na_color = "#cccccc") {
  labs <- paste0(
    format(round(head(bins, -1), 2), trim = TRUE),
    "–",
    format(round(tail(bins, -1), 2), trim = TRUE)
  )
  cols <- pal((head(bins, -1) + tail(bins, -1)) / 2)
  
  tags$div(
    style = paste(
      "margin-top:6px; padding:6px 8px;",
      "border:1px solid #e5e7eb; border-radius:8px; background:#fff;"
    ),
    tags$div(style = "font-weight:600; margin-bottom:4px; font-size:12px;", var_label),
    tags$div(
      style = "display:flex; flex-wrap:wrap; gap:10px; align-items:center;",
      lapply(seq_along(labs), function(i) {
        tags$div(
          style = "display:flex; align-items:center; gap:5px;",
          tags$span(style = paste0(
            "display:inline-block; width:12px; height:12px; border:1px solid #999; background:",
            cols[i], ";"
          )),
          tags$span(style = "font-size:10px;", labs[i])
        )
      }),
      tags$div(
        style = "display:flex; align-items:center; gap:5px;",
        tags$span(style = paste0(
          "display:inline-block; width:12px; height:12px; border:1px solid #999; background:",
          na_color, ";"
        )),
        tags$span(style = "font-size:10px;", "Missing")
      )
    )
  )
}
# ----------------------------
# Dropdown choices
# ----------------------------
domain_pct_vars   <- paste0("z_", names(sdoh_domains), "_pct")
domain_pct_labels <- paste0(pretty_domain(names(sdoh_domains)), " (percentile)")

indicator_vars <- unique(unlist(sdoh_domains))
indicator_labels_pretty <- unname(vapply(indicator_vars, label_for_indicator, character(1)))

choices_all <- c(
  "SDOH Composite (percentile)" = "z_SDOH_Composite_pct",
  stats::setNames(domain_pct_vars, domain_pct_labels),
  stats::setNames(indicator_vars, indicator_labels_pretty),
  outcome_labels
)

# ----------------------------
# Scenario Sandbox helpers
# ----------------------------
domain_z_cols <- paste0("z_", names(sdoh_domains))

pretty_var_for_sandbox <- function(v) {
  if (v %in% domain_z_cols) {
    dom <- str_remove(v, "^z_")
    return(paste0(pretty_domain(dom), " (domain z-score)"))
  }
  if (v == "z_SDOH_Composite") return("SDOH Composite (z-score)")
  if (v %in% sandbox_outcome_vars_log) {
    nm <- names(sandbox_outcome_labels)[match(v, unname(sandbox_outcome_labels))]
    if (!is.na(nm)) return(nm)
  }
  if (v %in% outcomes_vars) {
    nm <- names(outcome_labels)[match(v, unname(outcome_labels))]
    if (!is.na(nm)) return(nm)
  }
  label_for_indicator(v)
}

fit_sandbox_models <- function(d_sf, domain_cols, outcomes_cols) {
  
  fit_one <- function(d, target, preds) {
    preds <- setdiff(preds, target)
    preds <- preds[preds %in% names(d)]
    
    preds <- preds[vapply(preds, function(v) {
      x <- suppressWarnings(as.numeric(d[[v]]))
      ok <- is.finite(x)
      if (sum(ok) < 5) return(FALSE)
      stats::sd(x[ok]) > 0
    }, logical(1))]
    
    if (length(preds) == 0) return(NULL)
    
    fml <- as.formula(paste(target, "~", paste(preds, collapse = " + ")))
    stats::lm(fml, data = d)
  }
  
  d <- d_sf %>% st_drop_geometry()
  
  preds <- intersect(domain_cols, names(d))
  missing_preds <- setdiff(domain_cols, preds)
  if (length(missing_preds) > 0) {
    stop(
      "Scenario Sandbox: these domain z-score columns are missing from dat_scored_sf_sandbox: ",
      paste(missing_preds, collapse = ", ")
    )
  }
  
  outcomes_present <- intersect(outcomes_cols, names(d))
  
  d <- d %>%
    mutate(across(all_of(preds), ~ suppressWarnings(as.numeric(.x)))) %>%
    mutate(across(all_of(outcomes_present), ~ suppressWarnings(as.numeric(.x))))
  
  targets <- unique(c(preds, outcomes_present))
  
  models <- list()
  for (tgt in targets) {
    if (!tgt %in% names(d)) next
    
    df <- d %>%
      select(any_of(c(preds, tgt))) %>%
      filter(if_all(everything(), ~ is.finite(.x)))
    
    # lowered from 10 to 5 so CN rate is less likely to be skipped
    if (nrow(df) < 5) {
      message("Skipping ", tgt, " because only ", nrow(df), " complete rows are available.")
      next
    }
    
    mod <- fit_one(df, target = tgt, preds = preds)
    if (!is.null(mod)) models[[tgt]] <- mod
  }
  
  list(
    models = models,
    preds = preds,
    outcomes_present = outcomes_present
  )
}

county_choices <- sort(unique(na.omit(dat_scored_sf$county)))
stopifnot(length(county_choices) >= 1)

# ----------------------------
# UI
# ----------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .well { background: #f8fafc; border: 1px solid #e5e7eb; border-radius: 10px; }
      .control-card { background:#fff; border:1px solid #e5e7eb; border-radius:12px; padding:10px; }
      .control-title { font-weight:700; font-size:13px; margin-bottom:8px; }
      .subtle { color:#555; font-size:12px; }
      #cn_logo { display:block; max-width:70px; }
      .metric-card { background:#fff; border:1px solid #e5e7eb; border-radius:12px; padding:10px; margin-bottom:10px; }
      .metric-title { font-weight:700; font-size:13px; margin-bottom:4px; }
      .metric-big { font-size:22px; font-weight:800; line-height:1.1; }
      .metric-small { font-size:12px; color:#555; }
    "))
  ),
  
  tags$div(
    style = "display:flex; align-items:center; gap:14px; padding:10px 0;",
    tags$div(style = "flex:0 0 auto; width:70px;", imageOutput("cn_logo", height = "60px")),
    tags$div(
      style = "flex:1 1 auto;",
      tags$div("Cherokee Nation Public Health", style = "font-size:24px; font-weight:700; line-height:1.1; margin:0;"),
      tags$div("Social Determinants of Health Dashboard", style = "font-size:24px; font-weight:700; line-height:1.1; margin:0;"),
      tags$div(paste0("Cherokee Nation Public Health • ", YEAR0), style = "font-size:14px; color:#555; margin-top:2px;")
    )
  ),
  
  tags$div(
    class = "well",
    style = "margin-top:10px;",
    tags$div(
      style = "font-size:14px;",
      tags$b("Welcome. "),
      "This dashboard helps Tribal public health analysts compare Oklahoma counties (2022) across six Social Determinants of Health (SDOH) domains. Use it to identify counties with similar profiles, highlight domain-specific strengths and needs, and explore relationships between SDOH conditions and health outcomes."
    ),
    tags$br(),
    tags$details(
      open = FALSE,
      tags$summary(style = "font-weight:600; font-size:14px;", "What are the 6 SDOH domains in this dashboard?"),
      tags$div(
        style = "margin-top:8px; font-size:13px;",
        tags$ul(
          tags$li(tags$b("Economic Instability: "), "poverty, unemployment, SNAP, low-income households"),
          tags$li(tags$b("Education Attainment: "), "overall educational attainment and AI/AN education distribution"),
          tags$li(tags$b("Housing & Transportation Barriers: "), "renting, cost burden, overcrowding, vehicle access, plumbing, housing age"),
          tags$li(tags$b("Health Care Access Barriers: "), "uninsurance and AI/AN coverage indicators (IHS, Medicaid, self-pay)"),
          tags$li(tags$b("Neighborhood & Environment Burden: "), "air quality (PM2.5) and access to parks/open space"),
          tags$li(tags$b("Social & Community Context: "), "disability, marital status, and population composition indicators")
        )
      )
    ),
    tags$details(
      open = FALSE,
      tags$summary(style = "font-weight:600; font-size:14px;", "What outcomes are included in this dashboard?"),
      tags$div(
        style = "margin-top:8px; font-size:13px;",
        tags$ul(
          tags$li("CN gestational diabetes rate"),
          tags$li("AI/AN gestational diabetes rate"),
          tags$li("All births rate"),
          tags$li("AI/AN births rate")
        )
      )
    ),
    tags$details(
      open = FALSE,
      tags$summary(style = "font-weight:600; font-size:14px;", "How are z-scores, domain scores, and the composite score calculated?"),
      tags$div(
        style = "margin-top:8px; font-size:13px;",
        tags$p(
          tags$b("Indicator z-score: "),
          "For each indicator, we standardize across Oklahoma counties: ",
          tags$code("z = (county value − OK mean) / OK standard deviation."),
          " A z-score of +1 means the county is ~1 standard deviation above the Oklahoma average for that indicator."
        ),
        tags$p(tags$b("Domain score: "), "Within each domain, we take the average of the indicator z-scores (equal weight)."),
        tags$p(tags$b("Composite SDOH score: "), "We take the average of the six domain z-scores (equal weight)."),
        tags$p(tags$b("Percentiles (0–100): "), "For domain and composite scores, we also compute a percentile rank across Oklahoma counties."),
        tags$p(tags$b("Important note: "), "Z-scores and percentiles are descriptive comparisons across Oklahoma counties for 2022; they are not absolute thresholds.")
      )
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      tags$div(class = "control-card",
               tags$div(class = "control-title", "Training"),
               tags$div(class = "subtle", "Pick a lesson and watch the short walkthrough.")
      ),
      
      selectInput(
        "training_video",
        "Choose a lesson",
        choices = c(
          "Welcome (1 min)" = "https://www.youtube.com/embed/YOUR_ID1",
          "Dashboard overview (3 min)" = "https://www.youtube.com/embed/YOUR_ID2",
          "Maps tab (2 min)" = "https://www.youtube.com/embed/YOUR_ID3",
          "County Profile (3 min)" = "https://www.youtube.com/embed/YOUR_ID4",
          "Clusters (3 min)" = "https://www.youtube.com/embed/YOUR_ID5",
          "Tradeoffs (2 min)" = "https://www.youtube.com/embed/YOUR_ID6",
          "Scenario Sandbox (3 min)" = "https://www.youtube.com/embed/YOUR_ID7"
        )
      ),
      uiOutput("training_video_ui"),
      
      tags$details(
        open = FALSE,
        tags$summary(style = "font-weight:700; font-size:13px; margin-top:10px;", "Data sources & definitions"),
        tags$div(
          style = "margin-top:8px; font-size:12px; color:#444;",
          tags$p(tags$b("Sources: "), "ACS 5-year county measures; OK2Share state data; Cherokee Nation health data; federal environmental/food access datasets. All measures are compiled to Oklahoma county level for ", YEAR0, "."),
          tags$p(style="font-size:12px; color:#555;", "Z-scores and percentiles are descriptive comparisons across Oklahoma counties (not causal)."),
          tags$p(style="font-size:12px; color:#555;", "Scenario Sandbox shows model-based predictions on a transformed outcome scale (associational), not guaranteed causal effects.")
        )
      ),
      
      hr(),
      helpText("Data year: ", YEAR0)
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(
        tabPanel(
          "Maps",
          tags$div(
            class = "well",
            style = "margin-top:10px; font-size:13px;",
            tags$b("How to use these maps: "),
            "Select up to four variables to compare across counties. Hover counties to see values. Use “Focus county” to outline one county in red. Zoom/pan are restricted to Oklahoma."
          ),
          
          fluidRow(
            column(
              9,
              fluidRow(
                column(6, leafletOutput("map1", height = 320), uiOutput("legend1")),
                column(6, leafletOutput("map2", height = 320), uiOutput("legend2"))
              ),
              fluidRow(
                column(6, leafletOutput("map3", height = 320), uiOutput("legend3")),
                column(6, leafletOutput("map4", height = 320), uiOutput("legend4"))
              )
            ),
            
            column(
              3,
              tags$div(class = "control-card",
                       tags$div(class = "control-title", "Map options"),
                       selectInput("map_var1", "Map 1 variable", choices = choices_all, selected = "z_SDOH_Composite_pct"),
                       selectInput("map_var2", "Map 2 variable", choices = choices_all, selected = "z_Economic_Instability_pct"),
                       selectInput("map_var3", "Map 3 variable", choices = choices_all, selected = "z_Education_Attainment_pct"),
                       selectInput("map_var4", "Map 4 variable", choices = choices_all, selected = "z_Housing_Transport_pct"),
                       
                       hr(style = "margin:10px 0;"),
                       selectInput("county_pick", "Focus county", choices = county_choices, selected = county_choices[1]),
                       
                       hr(style = "margin:10px 0;"),
                       checkboxInput("show_facilities", "Show AI/AN facility locations", value = FALSE),
                       
                       tags$div(
                         style = "margin-top:8px; font-size:12px;",
                         tags$div(style="font-weight:600; margin-bottom:4px;", "Facility affiliation"),
                         tags$table(
                           style="border-collapse:collapse;",
                           tags$tr(
                             tags$td(style=paste0("width:10px; height:10px; background:", facility_colors["IHS"], "; border:1px solid #000;")),
                             tags$td(style="padding-left:6px; padding-right:14px;", "IHS"),
                             tags$td(style=paste0("width:10px; height:10px; background:", facility_colors["Title 1 (Tribal)"], "; border:1px solid #000;")),
                             tags$td(style="padding-left:6px; padding-right:14px;", "Title 1 (Tribal)"),
                             tags$td(style=paste0("width:10px; height:10px; background:", facility_colors["Title 5 (638)"], "; border:1px solid #000;")),
                             tags$td(style="padding-left:6px; padding-right:14px;", "Title 5 (638)"),
                             tags$td(style=paste0("width:10px; height:10px; background:", facility_colors["Urban"], "; border:1px solid #000;")),
                             tags$td(style="padding-left:6px;", "Urban")
                           )
                         )
                       )
              )
            )
          )
        ),
        
        tabPanel(
          "County Profile",
          tags$div(
            class = "well",
            style = "margin-top:10px; font-size:13px;",
            tags$b("How to use County Profile: "),
            "Select a county to compare domain percentiles (0–100). The drivers table lists indicators where the county differs most from the Oklahoma average; the search box lets you quickly find an indicator by keyword."
          ),
          
          fluidRow(
            column(
              9,
              fluidRow(
                column(6, plotlyOutput("radar", height = 420), uiOutput("profile_interp")),
                column(6, DTOutput("drivers_tbl"))
              )
            ),
            column(
              3,
              tags$div(class = "control-card",
                       tags$div(class = "control-title", "Profile options"),
                       selectInput("county_pick_profile", "County", choices = county_choices, selected = county_choices[1]),
                       checkboxInput("use_domain_view", "Show domain radar", value = TRUE),
                       hr(style = "margin:10px 0;"),
                       tags$div(class = "subtle", "Z-scores: positive = above OK average; negative = below OK average.")
              )
            )
          )
        ),
        
        tabPanel(
          "Profiles / Clusters",
          tags$div(
            class = "well",
            style = "margin-top:10px; font-size:13px;",
            tags$b("How to interpret clusters: "),
            "Clusters group counties with similar domain percentile patterns (k-means). The map shows where similar-profile counties are located; the fingerprint chart shows the average domain percentile profile for each cluster."
          ),
          
          fluidRow(
            column(9,
                   leafletOutput("cluster_map", height = 520),
                   tags$div(style="margin-top:10px;", plotlyOutput("cluster_fingerprint", height = 320))
            ),
            column(
              3,
              tags$div(class = "control-card",
                       tags$div(class = "control-title", "Cluster options"),
                       sliderInput("k", "Number of clusters (k)", min = 3, max = 8, value = 5, step = 1)
              )
            )
          )
        ),
        
        tabPanel(
          "Tradeoffs",
          tags$div(
            class = "well",
            style = "margin-top:10px; font-size:13px;",
            tags$b("How to use Tradeoffs: "),
            "Choose X and Y measures to view a quadrant map (median splits) and identify counties with high/high, low/low, or mixed patterns."
          ),
          
          fluidRow(
            column(9, leafletOutput("trade_map", height = 520), uiOutput("trade_interp")),
            column(
              3,
              tags$div(class = "control-card",
                       tags$div(class = "control-title", "Tradeoff options"),
                       selectInput("xvar", "X variable", choices = choices_all, selected = "z_Housing_Transport_pct"),
                       selectInput("yvar", "Y variable", choices = choices_all, selected = "cn_gdm_rate"),
                       checkboxInput("quad_map", "Quadrant map (median splits)", value = TRUE)
              )
            )
          )
        ),
        
        tabPanel(
          "Scenario Sandbox",
          tags$div(
            class = "well",
            style = "margin-top:10px; font-size:13px;",
            tags$b("What this is: "),
            "This sandbox shows model-based predictions if a domain score changed, holding the county’s other domain scores fixed. Outcomes are modeled on a log scale for stability. These are associational predictions (not guaranteed causal effects)."
          ),
          tags$div(
            tags$h4("Sandbox Instructions"),
            tags$p("Use this sandbox to test how changes in community conditions may influence outcomes."),
            tags$p("Adjust the scenario inputs to simulate improvements or declines in selected factors."),
            tags$p("Results update automatically, allowing you to explore potential impacts before policy or program decisions are made."),
            tags$p("Reset inputs at any time to begin a new scenario.")
          ),
          
          fluidRow(
            column(
              9,
              fluidRow(
                column(4, uiOutput("sandbox_card_baseline")),
                column(4, uiOutput("sandbox_card_scenario")),
                column(4, uiOutput("sandbox_card_delta"))
              ),
              tags$div(class = "control-card",
                       tags$div(class = "control-title", "Predicted changes"),
                       plotlyOutput("sandbox_delta_plot", height = 420),
                       tags$div(style="margin-top:10px;", DTOutput("sandbox_tbl"))
              )
            ),
            column(
              3,
              tags$div(class = "control-card",
                       tags$div(class = "control-title", "Scenario options"),
                       selectInput(
                         "sandbox_county", "County",
                         choices = county_choices,
                         selected = county_choices[1]
                       ),
                       selectInput(
                         "sandbox_lever",
                         "Lever (domain z-score)",
                         choices = stats::setNames(domain_z_cols, pretty_domain(names(sdoh_domains))),
                         selected = "z_Education_Attainment"
                       ),
                       sliderInput("sandbox_delta", "Change in lever (SD units)", min = -2, max = 2, value = 0, step = 0.1),
                       hr(style="margin:10px 0;"),
                       tags$div(
                         class="subtle",
                         "Interpretation: Targets are predicted from the six domain z-scores. Adjusting one lever updates predictions while keeping other domains fixed."
                       )
              )
            )
          )
        )
      )
    )
  )
)

# ----------------------------
# Server
# ----------------------------
server <- function(input, output, session) {
  
  output$cn_logo <- renderImage({
    list(
      src = normalizePath("www/cnimage.png"),
      contentType = "image/png",
      width = 70,
      height = 60,
      alt = "Cherokee Nation Public Health logo"
    )
  }, deleteFile = FALSE)
  
  dat <- reactive(dat_scored_sf)
  
  leaflet_ok_options <- function(sfdat) {
    max_bounds <- list(
      c(ok_bounds$ymin, ok_bounds$xmin),
      c(ok_bounds$ymax, ok_bounds$xmax)
    )
    
    leaflet(
      sfdat,
      options = leafletOptions(
        minZoom = 6,
        maxZoom = 12,
        zoomControl = TRUE,
        dragging = TRUE,
        scrollWheelZoom = TRUE,
        maxBounds = max_bounds,
        maxBoundsViscosity = 1.0
      )
    ) %>% fitBounds(ok_bounds$xmin, ok_bounds$ymin, ok_bounds$xmax, ok_bounds$ymax)
  }
  
  make_map <- function(sfdat, var, focus_county = NULL) {
    v <- sfdat[[var]]
    
    if (is.numeric(v)) {
      pal <- colorNumeric("Blues", domain = v[is.finite(v)], na.color = "#cccccc")
      fill <- pal(v)
    } else {
      pal <- colorFactor("Set2", domain = unique(v), na.color = "#cccccc")
      fill <- pal(v)
    }
    
    pretty_var <- ifelse(var %in% unname(outcome_labels),
                         names(outcome_labels)[match(var, unname(outcome_labels))],
                         label_for_indicator(var))
    
    lbl <- sprintf(
      "<strong>%s</strong><br/>%s: %s",
      sfdat$county,
      pretty_var,
      ifelse(is.na(v), "NA", format(round(v, 3), nsmall = 0))
    ) %>% lapply(htmltools::HTML)
    
    m <- leaflet_ok_options(sfdat) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = fill,
        fillOpacity = 0.8,
        color = "white",
        weight = 1,
        label = lbl,
        highlightOptions = highlightOptions(weight = 3, color = "#444444", bringToFront = TRUE)
      ) %>%
      addPolylines(data = ok, color = "#111111", weight = 2, opacity = 0.8)
    
    if (!is.null(focus_county)) {
      sel <- sfdat %>% filter(county == focus_county)
      if (nrow(sel) == 1) {
        m <- m %>% addPolygons(data = sel, fill = FALSE, color = "red", weight = 4)
      }
    }
    
    if (isTRUE(input$show_facilities)) {
      m <- add_facilities(m)
    }
    
    m
  }
  
  output$map1 <- renderLeaflet({ make_map(dat(), input$map_var1, input$county_pick) })
  output$map2 <- renderLeaflet({ make_map(dat(), input$map_var2, input$county_pick) })
  output$map3 <- renderLeaflet({ make_map(dat(), input$map_var3, input$county_pick) })
  output$map4 <- renderLeaflet({ make_map(dat(), input$map_var4, input$county_pick) })
  
  render_legend_for <- function(var) {
    sfdat <- dat()
    v <- sfdat[[var]]
    if (!is.numeric(v)) return(tags$div())
    bins <- make_bins(var, v)
    if (is.null(bins)) return(tags$div())
    
    pal <- colorBin(
      palette = "Blues",
      domain  = v,
      bins    = bins,
      na.color = "#cccccc",
      pretty   = FALSE
    )
    
    var_label <- ifelse(var %in% unname(outcome_labels),
                        names(outcome_labels)[match(var, unname(outcome_labels))],
                        label_for_indicator(var))
    
    legend_under_map(var_label, pal, bins)
  }
  
  output$legend1 <- renderUI({ render_legend_for(input$map_var1) })
  output$legend2 <- renderUI({ render_legend_for(input$map_var2) })
  output$legend3 <- renderUI({ render_legend_for(input$map_var3) })
  output$legend4 <- renderUI({ render_legend_for(input$map_var4) })
  
  output$radar <- renderPlotly({
    req(input$use_domain_view)
    focus_county <- input$county_pick_profile
    req(focus_county)
    
    d <- dat() %>% st_drop_geometry() %>% filter(county == focus_county)
    req(nrow(d) == 1)
    
    dom_cols <- paste0("z_", names(sdoh_domains), "_pct")
    dom_labels <- names(sdoh_domains) %>% pretty_domain() %>% wrap_2lines()
    
    radar_df <- tibble(
      domain = dom_labels,
      value  = as.numeric(d[1, dom_cols])
    )
    
    plot_ly(
      radar_df,
      type = "scatterpolar",
      r = ~value,
      theta = ~domain,
      fill = "toself",
      hovertemplate = "%{theta}<br>%{r:.1f} percentile<extra></extra>"
    ) %>%
      layout(
        title = list(text = paste0(focus_county, " — Domain percentiles (0–100)"), font = list(size = 14)),
        margin = list(l = 55, r = 55, t = 60, b = 55),
        polar = list(
          radialaxis = list(visible = TRUE, range = c(0, 100), tickfont = list(size = 11)),
          angularaxis = list(tickfont = list(size = 12))
        ),
        showlegend = FALSE
      )
  })
  
  output$drivers_tbl <- renderDT({
    focus <- input$county_pick_profile
    req(focus)
    
    all_vars <- unique(unlist(sdoh_domains))
    
    z_ind <- dat() %>%
      st_drop_geometry() %>%
      select(any_of(c("county", all_vars))) %>%
      mutate(across(all_of(all_vars), z_na))
    
    row_focus <- z_ind %>% filter(county == focus)
    req(nrow(row_focus) == 1)
    
    drivers <- tibble(
      Domain = map_chr(all_vars, ~{
        nm <- names(sdoh_domains)[map_lgl(sdoh_domains, function(vs) .x %in% vs)]
        if (length(nm) == 0) NA_character_ else nm[1]
      }),
      Indicator = label_for_indicator(all_vars),
      `Z-score (vs OK avg)` = as.numeric(row_focus[1, all_vars])
    ) %>%
      mutate(`Abs(z)` = abs(`Z-score (vs OK avg)`)) %>%
      arrange(desc(`Abs(z)`)) %>%
      select(Domain, Indicator, `Z-score (vs OK avg)`) %>%
      mutate(`Z-score (vs OK avg)` = round(`Z-score (vs OK avg)`, 4))
    
    datatable(
      head(drivers, 15),
      options = list(pageLength = 15, searching = TRUE, dom = "ftip"),
      class = "compact stripe hover",
      rownames = FALSE
    )
  })
  
  output$profile_interp <- renderUI({
    focus_county <- input$county_pick_profile
    req(focus_county)
    
    d <- dat() %>% st_drop_geometry() %>% filter(county == focus_county)
    req(nrow(d) == 1)
    
    dom_cols <- paste0("z_", names(sdoh_domains), "_pct")
    vals <- as.numeric(d[1, dom_cols])
    names(vals) <- names(sdoh_domains)
    
    top_high <- names(sort(vals, decreasing = TRUE))[1:2]
    top_low  <- names(sort(vals, decreasing = FALSE))[1:2]
    
    tags$div(
      class = "well",
      tags$h4("How to interpret this County Profile"),
      tags$p("The radar chart shows domain percentiles (0–100) for ", tags$b(focus_county), " compared with other Oklahoma counties."),
      tags$p(tags$b("Highest-ranking domains: "), paste0(pretty_domain(top_high), collapse = ", "), "."),
      tags$p(tags$b("Lowest-ranking domains: "), paste0(pretty_domain(top_low), collapse = ", "), "."),
      tags$p(tags$b("Drivers table: "), "The 15 rows are indicators where this county differs most from the Oklahoma average (largest absolute z-scores)."),
      tags$div(
        style = "margin-top:6px;",
        tags$b("How to interpret z-scores (Drivers table):"),
        tags$ul(
          style = "margin-top:4px; margin-bottom:0px;",
          tags$li(tags$b("z = 0"), " means the county is at the Oklahoma average for that indicator."),
          tags$li(tags$b("z > 0"), " means the county is above the Oklahoma average; ", tags$b("z < 0"), " means below average."),
          tags$li(tags$b("|z| ≈ 1"), " means about 1 standard deviation from the Oklahoma average (a notable difference)."),
          tags$li(tags$b("|z| ≥ 2"), " means about 2+ standard deviations from the Oklahoma average (a large difference).")
        )
      )
    )
  })
  
  clustered <- reactive({
    d <- dat() %>% st_drop_geometry()
    dom_cols <- paste0("z_", names(sdoh_domains), "_pct")
    X <- d %>% select(all_of(dom_cols)) %>% mutate(across(everything(), as.numeric))
    Xmat <- as.matrix(X)
    
    for (j in seq_len(ncol(Xmat))) {
      bad <- !is.finite(Xmat[, j])
      Xmat[bad, j] <- 50
    }
    
    k_use <- min(input$k, nrow(Xmat))
    if (k_use < 2) {
      d$cluster <- NA_integer_
      return(d)
    }
    
    set.seed(1)
    km <- kmeans(Xmat, centers = k_use, nstart = 25)
    d$cluster <- km$cluster
    d
  })
  
  output$cluster_map <- renderLeaflet({
    d_sf <- dat() %>% left_join(clustered() %>% select(GEOID, cluster), by = "GEOID")
    
    pal <- colorFactor("Set3", domain = sort(unique(d_sf$cluster)), na.color = "#cccccc")
    
    lbl <- sprintf(
      "<strong>%s</strong><br/>Cluster: %s",
      d_sf$county,
      ifelse(is.na(d_sf$cluster), "NA", d_sf$cluster)
    ) %>% lapply(htmltools::HTML)
    
    m <- leaflet_ok_options(d_sf) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = pal(d_sf$cluster),
        fillOpacity = 0.8,
        color = "white",
        weight = 1,
        label = lbl,
        highlightOptions = highlightOptions(weight = 3, color = "#444444", bringToFront = TRUE)
      ) %>%
      addPolylines(data = ok, color = "#111111", weight = 2, opacity = 0.8)
    
    if (isTRUE(input$show_facilities)) m <- add_facilities(m)
    m
  })
  
  output$cluster_fingerprint <- renderPlotly({
    d <- clustered()
    dom_cols <- paste0("z_", names(sdoh_domains), "_pct")
    
    fp <- d %>%
      filter(!is.na(cluster)) %>%
      group_by(cluster) %>%
      summarise(across(all_of(dom_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
      pivot_longer(cols = all_of(dom_cols), names_to = "domain", values_to = "mean_pct") %>%
      mutate(domain = str_remove(domain, "^z_") %>% str_remove("_pct$") %>% pretty_domain())
    
    plot_ly(fp, x = ~domain, y = ~mean_pct, color = ~factor(cluster), type = "bar") %>%
      layout(
        title = list(text = "Cluster fingerprints (mean domain percentiles)", font = list(size = 14)),
        margin = list(l = 50, r = 20, t = 60, b = 80),
        xaxis = list(title = "", tickangle = 45),
        yaxis = list(title = "Mean percentile (0–100)", range = c(0, 100))
      )
  })
  
  output$training_video_ui <- renderUI({
    req(input$training_video)
    tags$iframe(
      width = "100%",
      height = "360",
      src = input$training_video,
      frameborder = "0",
      allowfullscreen = NA
    )
  })
  
  output$trade_map <- renderLeaflet({
    d <- dat()
    x <- d[[input$xvar]]
    y <- d[[input$yvar]]
    
    if (!isTRUE(input$quad_map) || !is.numeric(x) || !is.numeric(y)) {
      v <- d[[input$xvar]]
      m <- make_map(d, input$xvar, focus_county = NULL)
      
      if (is.numeric(v)) {
        bins <- make_bins(input$xvar, v)
        if (!is.null(bins)) {
          pal_leg <- colorBin("Blues", domain = v, bins = bins, na.color = "#cccccc", pretty = FALSE)
          pretty_var <- ifelse(input$xvar %in% unname(outcome_labels),
                               names(outcome_labels)[match(input$xvar, unname(outcome_labels))],
                               label_for_indicator(input$xvar))
          m <- m %>% addLegend(
            position = "bottomright",
            pal = pal_leg,
            values = v,
            title = pretty_var,
            opacity = 0.9,
            na.label = "Missing"
          )
        }
      }
      
      return(m)
    }
    
    mx <- median(x, na.rm = TRUE)
    my <- median(y, na.rm = TRUE)
    
    d$quad <- dplyr::case_when(
      is.na(x) | is.na(y) ~ NA_character_,
      x >= mx & y >= my ~ "High X / High Y",
      x >= mx & y <  my ~ "High X / Low Y",
      x <  mx & y >= my ~ "Low X / High Y",
      TRUE ~ "Low X / Low Y"
    )
    
    quad_levels <- c("High X / High Y", "High X / Low Y", "Low X / High Y", "Low X / Low Y")
    pal <- colorFactor("Set2", domain = quad_levels, na.color = "#cccccc")
    
    xlab <- ifelse(input$xvar %in% unname(outcome_labels),
                   names(outcome_labels)[match(input$xvar, unname(outcome_labels))],
                   label_for_indicator(input$xvar))
    ylab <- ifelse(input$yvar %in% unname(outcome_labels),
                   names(outcome_labels)[match(input$yvar, unname(outcome_labels))],
                   label_for_indicator(input$yvar))
    
    lbl <- sprintf(
      "<strong>%s</strong><br/>%s: %s<br/>%s: %s<br/>%s",
      d$county,
      xlab, ifelse(is.na(x), "NA", round(x, 3)),
      ylab, ifelse(is.na(y), "NA", round(y, 3)),
      ifelse(is.na(d$quad), "Quadrant: NA", paste0("Quadrant: ", d$quad))
    ) %>% lapply(htmltools::HTML)
    
    m <- leaflet_ok_options(d) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = pal(d$quad),
        fillOpacity = 0.8,
        color = "white",
        weight = 1,
        label = lbl,
        highlightOptions = highlightOptions(weight = 3, color = "#444444", bringToFront = TRUE)
      ) %>%
      addPolylines(data = ok, color = "#111111", weight = 2, opacity = 0.8) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = factor(d$quad, levels = quad_levels),
        title = "Quadrants (median splits)",
        opacity = 0.9,
        na.label = "Missing"
      )
    
    if (isTRUE(input$show_facilities)) m <- add_facilities(m)
    
    m
  })
  
  output$trade_interp <- renderUI({
    req(input$xvar, input$yvar)
    
    xlab <- ifelse(input$xvar %in% unname(outcome_labels),
                   names(outcome_labels)[match(input$xvar, unname(outcome_labels))],
                   label_for_indicator(input$xvar))
    ylab <- ifelse(input$yvar %in% unname(outcome_labels),
                   names(outcome_labels)[match(input$yvar, unname(outcome_labels))],
                   label_for_indicator(input$yvar))
    
    tags$div(
      class = "well",
      tags$h4("How to interpret this Tradeoffs map"),
      tags$p("This map compares two measures at once using Oklahoma medians for ", YEAR0, "."),
      tags$p(tags$b("X: "), xlab, " • ", tags$b("Y: "), ylab),
      tags$p("Counties are grouped into four quadrants: High/High, High/Low, Low/High, Low/Low (relative to Oklahoma medians).")
    )
  })
  
  # ============================================================
  # Scenario Sandbox
  # ============================================================
  sandbox_fit <- fit_sandbox_models(
    d_sf = dat_scored_sf_sandbox,
    domain_cols = domain_z_cols,
    outcomes_cols = sandbox_outcome_vars_log
  )
  sandbox_models <- sandbox_fit$models
  sandbox_preds  <- sandbox_fit$preds
  sandbox_outcomes_present <- sandbox_fit$outcomes_present
  
  print(sandbox_outcomes_present)
  print(names(sandbox_models))
  
  sandbox_row <- reactive({
    req(input$sandbox_county)
    d0 <- dat_scored_sf_sandbox %>% st_drop_geometry() %>% filter(county == input$sandbox_county)
    req(nrow(d0) == 1)
    
    base <- d0[1, sandbox_preds, drop = FALSE] %>%
      mutate(across(everything(), ~ suppressWarnings(as.numeric(.x))))
    
    lev <- input$sandbox_lever
    req(lev %in% sandbox_preds)
    
    scen <- base
    scen[[lev]] <- scen[[lev]] + input$sandbox_delta
    
    list(base = base, scen = scen)
  })
  
  sandbox_pred_tbl <- reactive({
    req(sandbox_row())
    base <- sandbox_row()$base
    scen <- sandbox_row()$scen
    
    tgts <- unique(c("z_SDOH_Composite", names(sandbox_models)))
    tgts <- setdiff(tgts, input$sandbox_lever)
    
    rows <- lapply(tgts, function(tgt) {
      
      if (tgt == "z_SDOH_Composite") {
        bval <- mean(as.numeric(base[1, sandbox_preds, drop = TRUE]), na.rm = TRUE)
        sval <- mean(as.numeric(scen[1, sandbox_preds, drop = TRUE]), na.rm = TRUE)
        return(data.frame(
          target = tgt,
          type = "Composite",
          baseline = bval,
          scenario = sval,
          delta = sval - bval,
          stringsAsFactors = FALSE
        ))
      }
      
      if (tgt %in% names(sandbox_models)) {
        mod <- sandbox_models[[tgt]]
        if (!is.null(mod)) {
          bval <- suppressWarnings(as.numeric(predict(mod, newdata = base)))
          sval <- suppressWarnings(as.numeric(predict(mod, newdata = scen)))
          
          row_type <- if (tgt %in% sandbox_preds) "Domain" else if (tgt %in% sandbox_outcomes_present) "Outcome" else "Other"
          
          return(data.frame(
            target = tgt,
            type = row_type,
            baseline = bval,
            scenario = sval,
            delta = sval - bval,
            stringsAsFactors = FALSE
          ))
        }
      }
      
      data.frame(
        target = tgt,
        type = "Other",
        baseline = NA_real_,
        scenario = NA_real_,
        delta = NA_real_,
        stringsAsFactors = FALSE
      )
    })
    
    out <- bind_rows(rows)
    
    safe_pretty <- function(x) {
      tryCatch(pretty_var_for_sandbox(x), error = function(e) x)
    }
    
    out %>%
      mutate(
        Target = vapply(target, safe_pretty, character(1)),
        Baseline = round(baseline, 4),
        Scenario = round(scenario, 4),
        `Change (Scenario − Baseline)` = round(delta, 4)
      ) %>%
      arrange(match(type, c("Composite", "Domain", "Outcome", "Other")), Target) %>%
      select(type, Target, Baseline, Scenario, `Change (Scenario − Baseline)`)
  })
  
  output$sandbox_tbl <- renderDT({
    df <- sandbox_pred_tbl()
    datatable(
      df,
      options = list(pageLength = 20, searching = TRUE, dom = "ftip"),
      class = "compact stripe hover",
      rownames = FALSE
    )
  })
  
  # ---- REVISED: show BOTH domains and outcomes in plot ----
  output$sandbox_delta_plot <- renderPlotly({
    df <- sandbox_pred_tbl()
    req(is.data.frame(df), nrow(df) >= 1)
    
    df2 <- df %>%
      filter(type %in% c("Composite", "Domain", "Outcome")) %>%
      arrange(`Change (Scenario − Baseline)`) %>%
      mutate(Target = factor(Target, levels = Target))
    
    plot_ly(
      df2,
      x = ~`Change (Scenario − Baseline)`,
      y = ~Target,
      color = ~type,
      type = "bar",
      orientation = "h",
      customdata = ~type,
      hovertemplate = paste(
        "<b>%{y}</b><br>",
        "Type: %{customdata}<br>",
        "Change: %{x:.4f}<extra></extra>"
      )
    ) %>%
      layout(
        margin = list(l = 280, r = 20, t = 20, b = 40),
        xaxis = list(title = "Predicted change"),
        yaxis = list(title = ""),
        legend = list(title = list(text = "Result type"))
      )
  })
  
  output$sandbox_card_baseline <- renderUI({
    req(sandbox_row())
    base <- sandbox_row()$base
    lev <- input$sandbox_lever
    val <- as.numeric(base[[lev]])
    
    tags$div(class="metric-card",
             tags$div(class="metric-title", "Baseline lever"),
             tags$div(class="metric-big", sprintf("%.2f", val)),
             tags$div(class="metric-small", pretty_var_for_sandbox(lev))
    )
  })
  
  output$sandbox_card_scenario <- renderUI({
    req(sandbox_row())
    scen <- sandbox_row()$scen
    lev <- input$sandbox_lever
    val <- as.numeric(scen[[lev]])
    
    tags$div(class="metric-card",
             tags$div(class="metric-title", "Scenario lever"),
             tags$div(class="metric-big", sprintf("%.2f", val)),
             tags$div(class="metric-small", paste0(pretty_var_for_sandbox(lev), " (shift applied)"))
    )
  })
  
  output$sandbox_card_delta <- renderUI({
    dlt <- input$sandbox_delta
    lev <- input$sandbox_lever
    tags$div(class="metric-card",
             tags$div(class="metric-title", "Applied change"),
             tags$div(class="metric-big", sprintf("%+.2f SD", dlt)),
             tags$div(class="metric-small", paste0("Added to ", pretty_var_for_sandbox(lev)))
    )
  })
}

shinyApp(ui, server)