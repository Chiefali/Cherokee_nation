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

