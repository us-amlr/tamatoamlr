utils::globalVariables(c(
  # https://github.com/r-lib/tidyselect/issues/248
  "where", ".",

  # Commonly used variables
  "observer", "location", "location_group", "beach_id", "species",
  "Beach", "Beaches", "loc_lower",

  # Plotting
  "count_class", "count_value", "species_lty",

  # season_info
  "season_name", "season_open_date", "season_close_date", "season_days",
  "diet_scat_date", "date_median_pupping", "ts",

  # Phocid census
  "census_phocid_header_id", "census_id", "census_type",
  "census_date_start", "census_date_end",
  "census_date", "time_start", "time_end",
  "ad_female_count", "ad_male_count", "ad_unk_count",
  "juv_female_count", "juv_male_count", "juv_unk_count",
  "pup_live_count", "pup_dead_count",
  "unk_female_count", "unk_male_count", "unk_unk_count",
  "census_notes", "census_created_dt"
))

