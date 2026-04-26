# Load data
# ----------------------------
merged_clean <- readRDS(file.path(data_dir, "merged_clean.RDS")) %>%
  st_transform(4326) %>%
  mutate(
    ins_aian_ihs_rev = -ins_aian_ihs,
    ins_aian_medicaid_rev = -ins_aian_medicaid
  )

