# This script creates internal data used by Tamatoa and amlrPinnipeds functions
#   These include:

#-------------------------------------------------------------------------------
.census.cols.phocid <- c(
  "ad_female_count", "ad_male_count", "ad_unk_count",
  "juv_female_count", "juv_male_count", "juv_unk_count",
  "pup_live_count", "pup_dead_count",
  "unk_female_count", "unk_male_count", "unk_unk_count"
)


.summary.timing.choices.list <- list(
  "Multiple seasons - total" = "fs_total",
  "Multiple seasons - by week" = "fs_week",
  "Multiple seasons - date series" = "fs_date_series",
  "Multiple seasons - by date" = "fs_date_single",
  "Single season" = "fs_single",
  "Raw data" = "fs_raw"
)
.summary.timing.choices <- unlist(unname(.summary.timing.choices.list))

.summary.timing.multiple <- c("fs_total", "fs_week", "fs_date_series", "fs_date_single")
.summary.timing.single <- c("fs_single")

.tamatoa.csv.accept <- c(
  "text/csv",
  "text/comma-separated-values,text/plain",
  ".csv"
)


#-------------------------------------------------------------------------------
usethis::use_data(
  .census.cols.phocid, .summary.timing.choices.list, .summary.timing.choices,
  .summary.timing.multiple, .summary.timing.single, .tamatoa.csv.accept,
  internal = TRUE, overwrite = TRUE
)
