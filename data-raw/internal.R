# This script creates internal data used by Tamatoa and amlrPinnipeds functions
#   These include:

#-------------------------------------------------------------------------------
census.cols.phocid <- c(
  "ad_female_count", "ad_male_count", "ad_unk_count",
  "juv_female_count", "juv_male_count", "juv_unk_count",
  "pup_live_count", "pup_dead_count",
  "unk_female_count", "unk_male_count", "unk_unk_count"
)

#-------------------------------------------------------------------------------
usethis::use_data(
  census.cols.phocid,
  internal = TRUE, overwrite = TRUE
)
