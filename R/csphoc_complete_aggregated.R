#' Complete aggregated US AMLR CS-PHOC data
#'
#' Complete aggregated CS-PHOC data collected byt he US AMLR program
#'
#' @param x data frame, phocid census data. See Details for more info
#' @param x.header data frame, phocid census header data.
#'   Must contain at least column 'header_id' and 'census_date_start'
#' @param fill.location character; value with which to fill the 'location'
#'   column. Default is `NULL`
#'
#' @details
#' Because the Cape Shirreff Phocid Census (CS-PHOC) data
#' collected by the US AMLR program is not explicit,
#' meaning it does not always include zero records for when a beach
#' was surveyed and no animals were seen, these data
#' usually must be completed before being shared or used in analyses.
#' This function uses \code{\link[tidyr:complete]{complete}} with US AMLR
#' CS-PHOC data that has already been aggregated by header ID and species,
#' i.e. there is only one record for every header record and species combo.
#' When completing, count columns are filled with zeroes to match
#' PI instruction on when certain counts were recorded. Specifically:
#'
#' - The following counts are filled with zeroes for all US AMLR CS-PHOC data: ad_female_count, ad_male_count, ad_unk_count, juv_female_count, juv_male_count, juv_unk_count, pup_live_count
#' - unk_unk_count is filled with zeroes for all elephant seal counts, and beginning in the 2014-15 season for all other species
#' - unk_female_count and unk_male_count are filled with zeroes beginning in the 2017-18 seasons
#'
#' The header data frame must be passed as an argument to get dates.
#'
#' \code{x} is completed across all header_id values in \code{x},
#' and all values of \code{\link{pinniped.phocid.sp}}
#'
#' The CS-PHOC repo (link below) contains an example use case
#'
#' @return
#' A data frame, completed by 'header_id' and 'species'. The data frame will
#' have the same column names and types as \code{x}
#'
#' @seealso \url{https://github.com/us-amlr/cs-phoc}
#'
#' @export
csphoc_complete_aggregated <- function(x, x.header, fill.location = NULL) {
  # Checks
  stopifnot(
    all(c("header_id", "species", "research_program") %in% names(x)),
    all(c("header_id", "census_date_start") %in% names(x.header)),
    all(x$research_program == "USAMLR")
  )
  if ("research_program" %in% names(x.header))
    stopifnot(all(x.header$research_program == "USAMLR"))

  uniq.check <- x %>%
    group_by(header_id, species) %>%
    filter(n() > 1)
  if (nrow(uniq.check) > 0)
    stop("x must be already grouped and summed by header ID and species")


  # Header mgmt - ensure same class is returned
  header.id.class <- class(x.header$header_id)
  as_header_id_func <- if (header.id.class == "character") {
    as.character
  } else if (header.id.class == "integer") {
    as.integer
  } else {
    stop("Invalid header_id type")
  }


  # Fill variables
  cs.fill <- list(
    ad_female_count = 0, ad_male_count = 0, ad_unk_count = 0,
    juv_female_count = 0, juv_male_count = 0, juv_unk_count = 0,
    pup_live_count = 0, research_program = "USAMLR"
  )
  cs.fill <- c(cs.fill, location = fill.location)

  if (!all(names(cs.fill) %in% names(x)))
    stop("All fill variable names must be present in x:\n",
         paste(names(cs.fill), collapse = ", "))


  # Complete, and do additional processing
  x.out <- x %>%
    # Make to-complete columns factors to ensure all are created
    mutate(header_id = fct(as.character(header_id),
                           as.character(x.header$header_id)),
           species = fct(species, amlrPinnipeds::pinniped.phocid.sp)) %>%
    complete(header_id, species, fill = cs.fill, explicit = FALSE) %>%
    mutate(header_id = as_header_id_func(header_id),
           species = as.character(species)) %>%
    left_join(select(x.header, header_id, census_date_start),
              by = "header_id") %>%
    # These fills are based on PI info about when these columns were used
    mutate(species = species, #hack for rstudio line formatting
           unk_female_count = if_else(
             census_date_start > ymd("2017-07-01") & is.na(unk_female_count),
             as.integer(0), unk_female_count),
           unk_male_count = if_else(
             census_date_start > ymd("2017-07-01") & is.na(unk_male_count),
             as.integer(0), unk_male_count),
           unk_unk_count = if_else(
             (census_date_start > ymd("2014-07-01") & is.na(unk_unk_count)) |
               (species == "Elephant seal" & is.na(unk_unk_count)),
             as.integer(0), unk_unk_count)) %>%
    select(-census_date_start) %>%
    select(header_id, location, species, everything())

  stopifnot(
    identical(names(x.out), names(x)),
    identical(vapply(x.out, class, as.character(1)),
              vapply(x, class, as.character(1)))
  )

  x.out
}
