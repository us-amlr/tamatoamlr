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
tbl_beaches <- function(src) {
  tbl(src, "beaches") %>%
    select(beach_id = ID, everything()) %>%
    collect()
}

#' @name extract
#' @export
tbl_location_name_general <- function(src) {
  tbl_beaches(src) %>%
    filter(general) %>%
    select(name) %>%
    unname() %>%
    unlist() %>%
    as.list()
}

#' @name extract
#' @export
tbl_location_name_group <- function(src) {
  tbl(src, "beach_groups") %>%
    filter(beach_group_level == 1) %>%
    select(beach_group_name) %>%
    collect() %>%
    unname() %>%
    unlist() %>%
    as.list()
}
