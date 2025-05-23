# This script creates internal data used by Tamatoa and other package functions
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
  # "Multiple seasons - by week" = "fs_week",
  # "Multiple seasons - date series" = "fs_date_series",
  "Multiple seasons - by date" = "fs_date_single",
  "Multiple seasons - facet view" = "fs_facet",
  "Individual capture" = "fs_capture_ind",
  "Single season" = "fs_single",
  "Raw data" = "fs_raw"
)
.summary.timing.choices <- unlist(unname(.summary.timing.choices.list))

.summary.timing.multiple <- intersect(
  .summary.timing.choices,
  c("fs_total", "fs_date_single", "fs_facet", "fs_raw")
)
.summary.timing.single <- intersect(.summary.timing.choices, c("fs_single"))

.tamatoa.csv.accept <- c(
  "text/csv",
  "text/comma-separated-values,text/plain",
  ".csv"
)

.id.list <- list(
  "db"       = "db", # Database connections
  "si"       = "si", # Season info
  "info"     = "info", # Info tab
  "dcc"      = "dcc_pinniped",
  "afs_cwpc" = "afs_capewide_pup_census",
  "afs_sbc"  = "afs_study_beach_census",
  "afs_sam"  = "afs_sam_census",
  "captures" = "captures",
  "cpw"      = "ccamlr_pup_weights",
  "csphoc"   = "csphoc",
  "ps"       = "pinniped_season",
  "resights" = "tag_resights",
  "takes"    = "takes", #MMPA takes tab
  "views"    = "views"
)

#-------------------------------------------------------------------------------
usethis::use_data(
  .census.cols.phocid, .summary.timing.choices.list, .summary.timing.choices,
  .summary.timing.multiple, .summary.timing.single, .tamatoa.csv.accept,
  .id.list,
  internal = TRUE, overwrite = TRUE
)
