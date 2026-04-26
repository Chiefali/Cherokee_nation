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
