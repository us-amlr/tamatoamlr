utils::globalVariables(c(
  # https://github.com/r-lib/tidyselect/issues/248
  "where", ".",

  # Commonly used variables
  "observer", "location", "location_group", "beach_id", "species",
  "Beach", "Beaches", "loc_lower", "loc_clean", "week_num", "header_id",

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
  "census_notes", "census_created_dt",


  # AFS Capewide Pup Census
  "pup_count", "pup_live_count", "pup_dead_count", "date_min",
  "count_loc_mean", "count_loc_var", "count_loc_sd", "study_beach_count",
  "count_mean", "count_var", "count_sd", "count_range", "group",

  # AFS Study Beach Census
  "ad_male_count_sum", "adult_male_non_terr_count", "adult_male_terr_count",
  "adult_male_terr_noFem_count", "adult_male_terr_wFem_count",
  "adult_male_unk_count",


  # Misc
  "Code", "Day", "Hr", "ID", "Mass", "Mn", "Sig", "Yr", "action",
  "attendance_study", "body_condition", "box_time",
  "capt_to_release", "capt_to_reunion", "capture_date", "capture_date_rm",
  "capture_datetime", "capture_location", "capture_time", "capture_time_rm",
  "ccamlr_pup_weights_notes", "census_afs_capewide_pup_sort", "cohort",
  "combined_number", "d", "datetime", "datetime_prev", "days_diff",
  "device_inventory_id", "device_num", "device_type", "exclude_count", "freq",
  "frequency", "gas_off", "gas_on", "gas_time", "head", "header_notes", "in_box",
  "location_fctr", "location_lty", "m", "mass_kg", "mass_std_deviation",
  "mass_std_error", "mass_total_kg", "max_dt", "mean_mass_kg", "min_dt", "n_records",
  "n_resights", "n_trips", "n_weights", "name", "notes_tmp", "number_of_captures",
  "parturition", "parturition_date", "pinniped_id", "pinniped_season_id",
  "primary_tag", "pup_alive", "pup_mortality", "pup_mortality_date", "pup_num",
  "pup_total_count", "release_time", "research_program", "resight_date",
  "resight_time", "reunion_time", "round_date", "round_num", "sd", "season_date",
  "season_info_id", "sex", "sig", "station", "std_length_cm", "tag_freq_code", "tag_letter",
  "tag_numeric", "tag_sort", "tag_type", "tagging_date", "tare_check", "tare_kg", "time",
  "time_diff_hr", "trip_length_hr", "trip_length_hr_mean", "trip_num",
  "trip_num_completed", "trip_num_completed_max"
))

