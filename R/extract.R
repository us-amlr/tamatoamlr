#' Extract data from database
#'
#' Functions used to extract pinniped data from database
#'
#' @name extract
#'
#' @param src a data source; likely a \code{\link[pool]{pool}} object
#'   or some other connection to a database
#'
#' @details
#' These functions return the output of \code{\link[dplyr]{tbl}},
#' likely with at least some selecting or renaming.
#' Functions or modules using these functions will need to call
#' \code{link[dplyr]{collect}} to retrieve the data to R.
#'
#' These functions provide a single place in the package where functions
#' that extract data from a database are located, making it easier to update
#' said functions if some aspect of the database structure changes.
#'
#' @export
tbl_vCensus_Phocid <- function(src) {
  tbl(src, "vCensus_Phocid") %>%
    select(season_name, census_phocid_header_id, census_id,
           census_type, observer,
           census_date_start, census_date_end,
           census_date, time_start, time_end,
           location, location_group, beach_id, species,
           ad_female_count, ad_male_count, ad_unk_count,
           juv_female_count, juv_male_count, juv_unk_count,
           pup_live_count, pup_dead_count,
           unk_female_count, unk_male_count, unk_unk_count,
           census_notes, census_created_dt) %>%
    collect() %>%
    mutate(species = str_to_sentence(species))
}

#' @name extract
#' @export
tbl_vCensus_AFS_Study_Beach <- function(src) {
  tbl(src, "vCensus_AFS_Study_Beach") %>%
    select(season_name, census_id, census_type,
           observer, census_date, time_start, time_end,
           location, location_group, beach_id, species,
           ad_female_count, pup_live_count, pup_dead_count,
           ad_male_count_sum,
           juv_female_count, juv_male_count, juv_unk_count,
           adult_male_terr_count,
           adult_male_terr_wFem_count, adult_male_terr_noFem_count,
           adult_male_non_terr_count, adult_male_unk_count, ad_unk_count,
           census_notes, census_created_dt) %>%
    collect() %>%
    mutate(species = str_to_sentence(species))
}

#' @name extract
#' @export
tbl_vCensus_AFS_Capewide_Pup <- function(src) {
  tbl(src, "vCensus_AFS_Capewide_Pup") %>%
    select(season_name, census_id, census_type,
           observer, census_date, time_start, time_end,
           location, beach_id, census_afs_capewide_pup_sort, species,
           pup_count, pup_live_count, pup_dead_count, exclude_count,
           research_program, census_notes, census_created_dt) %>%
    collect() %>%
    mutate(species = str_to_sentence(species))
}

#' @name extract
#' @export
tbl_vCCAMLR_Pup_Weights <- function(src) {
  tbl(src, "vCCAMLR_Pup_Weights") %>%
    select(season_name, round_num, round_date, time_start, time_end,
           location, location_group, beach_id,
           pup_num, sex, mass_total_kg, tare_kg, mass_kg, tare_check,
           # pinniped_id, tag_unqiue_mother_primary, attendance_pup_id,
           ccamlr_pup_weights_notes, header_notes, research_program) %>%
    collect()
}

#' @name extract
#' @export
tbl_beaches_capewide <- function(src) {
  tbl(src, "vBeaches") %>%
    filter(!is.na(census_afs_capewide_pup_sort)) %>%
    arrange(census_afs_capewide_pup_sort) %>%
    select(beach_id, location = name, census_afs_capewide_pup_sort) %>%
    collect()
}

#' @name extract
#' @export
tbl_vTag_Resights_Season_Summary <- function(src) {
  tbl(src, "vTag_Resights_Season_Summary") %>%
    arrange(season_open_date, species, tag_sort) %>%
    collect()
}

#' @name extract
#' @export
tbl_pinniped_season <- function(src) {
  tbl(src, "pinniped_season") %>%
    select(pinniped_season_id, pinniped_id, season_info_id,
           attendance_study, parturition, parturition_date,
           pup_mortality, pup_mortality_date) %>%
    collect()
}
