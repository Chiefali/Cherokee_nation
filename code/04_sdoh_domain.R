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
