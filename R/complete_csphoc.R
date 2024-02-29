#' Complete aggregated US AMLR CS-PHOC data
#'
#' Complete aggregated CS-PHOC data collected by the US AMLR program
#'
#' @param x data frame, phocid census data. See Details for more info
#'
#' @details Because the Cape Shirreff Phocid Census (CS-PHOC) data collected by
#'   the US AMLR program is not explicit, meaning it does not always include
#'   zero records for when a beach was surveyed and no animals were seen, these
#'   data usually must be completed before being shared or used in analyses.
#'   This function uses [tidyr:complete()] with US AMLR CS-PHOC data that has
#'   already been aggregated by header ID and species, i.e. there is only one
#'   record for every header record and species combo.
#'
#'   When completing, count columns are filled with zeroes to match PI
#'   instruction on when certain counts were recorded. Specifically:
#'
#' - The following counts are filled with zeroes for all US AMLR CS-PHOC data:
#'   ad_female_count, ad_male_count, ad_unk_count, juv_female_count,
#'   juv_male_count, juv_unk_count, pup_live_count
#' - unk_unk_count is filled with zeroes for all elephant seal counts,
#'   and beginning in the 2014-15 season for all other species
#' - unk_female_count and unk_male_count are filled with zeroes
#'   beginning in the 2017-18 seasons
#'
#'   This logic is why census_date_start is a required.
#'
#'   In addition, \code{x} must meet the following requirements:
#'
#' - Contains the following columns:
#'   header_id, census_date_start, location, species, ad_female_count,
#'   ad_male_count, ad_unk_count, juv_female_count, juv_male_count,
#'   juv_unk_count, pup_live_count, research_program
#' - None of the following columns contain \code{NA} values:
#'   header_id, census_date_start, location, species, research_program
#' - The location column only has one unique value
#'   (i.e., the location column values are all the same)
#' - All records are USAMLR records,
#'   i.e. \code{all(x$research_program == "USAMLR")}
#' - \code{x} is already grouped and summed by header_id and species
#' - No header ID has multiple start dates,
#'   and no 2 distinct header IDs have the same start date
#' - All species column values are one of \code{\link{pinniped.phocid.sp}}
#'
#'   Note that the columns by which the data frame is completed are left as is,
#'   meaning that users can make these columns factors if desired.
#'
#'   The CS-PHOC repo (link below) contains an example use case
#'
#' @return A data frame, completed by \code{(\link[tidyr:expand]{nesting}}
#'   \code{(header_id, census_date_start), species)}. The data frame will have
#'   the same column names and types as \code{x}
#'
#' @examples
#' header.id <- c(1, 1, 2, 2, 3, 2)
#' census.dates <- as.Date(c("2000-01-01", "2000-01-08", "2000-01-15"))
#'
#' count.df <- data.frame(
#'   header_id = header.id,
#'   species = tamatoamlr::pinniped.phocid.sp[c(1, 2, 1, 3, 4, 2)],
#'   location = "test",
#'   ad_female_count = c(5, 3, 7, 3, 6, 3),
#'   ad_male_count = c(NA, 4, 2, NA, 0, 3),
#'   ad_unk_count = 0,
#'   juv_female_count = 0,
#'   juv_male_count = 0,
#'   juv_unk_count = 0,
#'   pup_live_count = 0,
#'   unk_female_count = NA_integer_,
#'   unk_male_count = NA_integer_,
#'   unk_unk_count = NA_integer_,
#'   research_program = "USAMLR",
#'   census_date_start = census.dates[header.id]
#' )
#'
#' complete_csphoc(count.df)
#'
#' @seealso \url{https://github.com/us-amlr/cs-phoc}
#'
#' @export
complete_csphoc <- function(x) {
  #--------------------------------------------------------
  x.names <- c("header_id", "census_date_start", "location", "species")
  cs.fill <- list(
    ad_female_count = 0, ad_male_count = 0, ad_unk_count = 0,
    juv_female_count = 0, juv_male_count = 0, juv_unk_count = 0,
    pup_live_count = 0, research_program = "USAMLR"
  )

  ### Checks
  if (!all(x.names %in% names(x)))
    stop("x must have at least the following columns:\n",
         paste(c(x.names, names(cs.fill)), collapse = ", "))

  if (n_distinct(x$location) != 1)
    stop("x can only have one distinct location value")

  na.check <- vapply(c(x.names, "research_program"), function(i) {
    all(!is.na(x[[i]]))
  }, as.logical(1))
  if (!all(na.check))
    stop("The following columns cannot have NA values:\n")

  if (!all(x$research_program == "USAMLR"))
    stop("csphoc_complete_aggregated is only intended for USAMLR records")

  if (!all(x$species %in% tamatoamlr::pinniped.phocid.sp))
    stop("All species values must be one of tamatoamlr::pinniped.phocid.sp")


  # Check that x is already grouped and summed by header ID and species
  uniq.check.agg <- x %>%
    group_by(header_id, species) %>%
    filter(n() > 1)
  if (nrow(uniq.check.agg) > 0)
    stop("x must be already grouped and summed by header ID and species, ",
         "meaning that there can be no rows with duplicate pairs of: ",
         "(header_id, species)")

  # Make sure no header ID has multiple start dates,
  #   and no 2 distinct header IDs have the same start date
  uniq.check.date1 <- x %>%
    group_by(header_id) %>%
    filter(n_distinct(census_date_start) > 1)
  uniq.check.date2 <- x %>%
    group_by(census_date_start) %>%
    filter(n_distinct(header_id) > 1)
  if (nrow(uniq.check.date1) > 0 || nrow(uniq.check.date2) > 0)
    stop("Please ensure that no header ID has multiple start dates, ",
         "and no 2 distinct header IDs have the same start date")


  #--------------------------------------------------------
  ### Complete, with additional 'filling'
  cs.fill <- c(cs.fill, location = unique(x$location))

  x.out <- x %>%
    complete(nesting(header_id, census_date_start), species,
             fill = cs.fill, explicit = FALSE) %>%
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
    select(!!names(x), everything()) %>%
    arrange(header_id, location, species)

  stopifnot(
    identical(names(x.out), names(x)),
    identical(vapply(x.out, class, as.character(1)),
              vapply(x, class, as.character(1)))
  )

  x.out
}
