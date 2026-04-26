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
