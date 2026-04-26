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
