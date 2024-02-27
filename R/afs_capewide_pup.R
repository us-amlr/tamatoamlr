#' Summarize AFS Capewide Pup Census Data
#'
#' Summarize AFS Capewide Pup (CWP) Census Data
#'
#' @param x data frame; output of [tbl_vCensus_AFS_Capewide_Pup()] or
#'   equivalent. The exception is if `x.byloc == TRUE`; in this case then `x`
#'   must be the output of `cwp_total_by_loc()`
#' @param loc.agg logical; indicates if `cwp_review()` should aggregate records
#'   across location (by observer) before summarizing (`TRUE`), or if records
#'   should be summarized by season/location as-is (`FALSE`; default).
#' @param x.byloc logical; indicates if `x` is the output of
#'   `cwp_total_by_loc()`. This argument is intended to minimize work if a user
#'   needs both the output of `cwp_total_by_loc()` and `cwp_total()`
#'
#' @details `cwp_loc_agg()`: Aggregate pup counts and related info across
#'   locations, and by season/observer. This function is intended for situations
#'   where a user has filtered CWP data for a specific subset of locations and
#'   wants to summarize the counts across all of these locations, effectively
#'   treating all of the locations as one location.
#'
#'   `cwp_review()`: Summarize pup counts and ancillary information for review
#'   after grouping by season_name and location. `loc.agg == TRUE` then all
#'   counts will be summed by observer across locations, i.e., `x` will be
#'   passed to `cwp_loc_agg()`, before being summarized. The output data frame
#'   is various pasted conglomerations that are intended for review to help
#'   determine if recounts need to happen.
#'
#'   `cwp_total_by_loc()`: group by season_name and location; return count mean,
#'   variance, SD, and min date. Calculating the variance and SD follow
#'   CS/AMLR-specific rules about when it must be NA based on historical
#'   knowledge about past survey data; see the 'assumptions' section for
#'   details. census_date_min is the earliest census date for that
#'   season/location group.
#'
#'   `cwp_total()`: after running cwp_total_by_loc, group by season name and
#'   return counts and standard deviations
#'
#'   Note that none of these functions do any rounding. To round values, for
#'   instance to get a whole number for pup counts, use either [base::round()]
#'   or [amlrDatabases::round_logical()]
#'
#'   These functions make several assumptions specific to US AMLR Pinniped Data:
#'   * Assumes that the research program is unique by season
#'   * Study beaches (locations: Copihue, Maderas, Cachorros, Chungungo)
#'   censused between 2008-07-01 and 2011-07-01 are single counts and thus do
#'   not have variance values.
#'   * All counts before July 2008 (i.e., before the 2008/09 season) do not have variance values.
#'
#' @return An ungrouped data frame with the summarized AFS Capewide Pup Census
#'   data. The summary types are described in 'Details'.
#'
#' @examplesIf FALSE
#'   # Not run; examples only will run if on SWFSC network
#'   x <- tbl_vCensus_AFS_Capewide_Pup(amlr_dbConnect("***REMOVED***_Test"))
#'
#'   x.201617 <- x[x$season_name == "2016/17", ]
#'   cwp_review(x.201617)
#'
#'   x.201617.ballena <- x.201617[grepl("Ballena", x.201617$location), ]
#'   cwp_loc_agg(x.201617.ballena)
#'   cwp_review(x.201617.ballena, loc.agg = TRUE)
#'
#'   cwp_total(x)
#'   cwp_total(x.201617.ballena, loc.agg = TRUE)
#'
#'   x.byloc <- cwp_total_by_loc(x)
#'   cwp_total(x.byloc, x.byloc = TRUE)
#'
#' @name afs_capewide_pup
#' @export
cwp_loc_agg <- function(x) {
  columns.names <- c(
    "season_name", "census_afs_capewide_pup_sort", "location", "census_date",
    "observer", "census_notes", "exclude_count",
    "pup_count", "pup_live_count", "pup_dead_count"
  )
  stopifnot(
    all(c(columns.names %in% names(x)))
  )

  if (any(x$exclude_count)) {
    warning("x contains some records with exclude_count == TRUE; ",
            "note these records will be removed to aggreagate across location",
            immediate. = TRUE)
  }

  x %>%
    arrange(census_afs_capewide_pup_sort) %>%
    filter(!exclude_count) %>%
    group_by(season_name, observer) %>%
    summarise(across(c(pup_count, pup_live_count, pup_dead_count),
                     sum, na.rm = TRUE, .names = "{.col}"),
              location = paste(unique(location), collapse = ", "),
              census_date_min = min(census_date),
              exclude_count = unique(exclude_count),
              census_notes = if_else(
                all(is.na(census_notes)),
                NA_character_,
                paste(as.character(na.omit(census_notes)), collapse = "; ")),
              research_program = unique(research_program),
              .groups = "drop") %>%
    mutate(census_afs_capewide_pup_sort = 1,
           .after = season_name)
}



#' @name afs_capewide_pup
#' @export
cwp_review <- function(x, loc.agg = FALSE) {
  columns.names <- c(
    "season_name", "census_afs_capewide_pup_sort", "location", "census_date",
    "observer", "census_notes", "exclude_count",
    "pup_count", "pup_live_count", "pup_dead_count"
  )
  stopifnot(
    all(c(columns.names %in% names(x)))
  )

  # Aggregate across locations, if specified
  if (loc.agg) x <- cwp_loc_agg(x) %>% rename(census_date = census_date_min)

  x %>%
    group_by(season_name, census_afs_capewide_pup_sort, location) %>%
    arrange(observer) %>% #so that collapsed data are always in the same order
    summarise(n_records = n(),
              count_mean = mean(pup_count),
              count_range = diff(range(pup_count)),
              count_range_perc_diff = if_else(
                count_mean == 0, 0, count_range / count_mean * 100),
              observers = paste(observer, collapse = "; "),
              counts = paste(pup_count, collapse = "; "),
              exclude_count = paste(as.integer(exclude_count), collapse = "; "),
              notes = paste(census_notes, collapse = "; "),
              counts_live = paste(pup_live_count, collapse = "; "),
              counts_dead = paste(pup_dead_count, collapse = "; "),
              .groups = "drop") %>%
    relocate(census_afs_capewide_pup_sort, .after = last_col())
}

#' @name afs_capewide_pup
#' @export
cwp_total_by_loc <- function(x, loc.agg = FALSE) {
  columns.names <- c(
    "season_name", "census_afs_capewide_pup_sort", "location",
    "census_date", "pup_count", "research_program"
  )
  stopifnot(all(c(columns.names %in% names(x))))
  if (any(x$exclude_count)) {
    warning("x contains some records with exclude_count == TRUE; ",
            "note these records will be removed to summarize totals by location",
            immediate. = TRUE)
  }

  # Aggregate across locations, if specified
  if (loc.agg) x <- cwp_loc_agg(x) %>% rename(census_date = census_date_min)

  x %>%
    group_by(season_name, census_afs_capewide_pup_sort, location) %>%
    summarise(num_records = n(),
              count_loc_mean = mean(pup_count),
              count_loc_var = var(pup_count),
              census_date_min = min(census_date),
              research_program = unique(research_program),
              .groups = "drop") %>%
    mutate(study_beach_count = between(census_date_min,
                                       ymd("2008-07-01"), ymd("2011-07-01")) &
             location %in% c("Copihue", "Maderas", "Cachorros", "Chungungo"),
           count_loc_var = case_when(
             census_date_min < as.Date("2008-07-01") ~ NA_real_,
             study_beach_count ~ NA_real_,
             .default = count_loc_var
           ),
           count_loc_sd = sqrt(count_loc_var)) %>%
    relocate(count_loc_sd, .before = count_loc_var) %>%
    select(-c(census_afs_capewide_pup_sort, study_beach_count))
}

#' @name afs_capewide_pup
#' @export
cwp_total <- function(x, x.byloc = FALSE, loc.agg = FALSE) {
  x.df.byloc <- if (x.byloc) {
    stopifnot(identical(
      c("season_name", "location",
        "num_records", "count_loc_mean", "count_loc_sd", "count_loc_var",
        "census_date_min", "research_program"),
      names(x)
    ))
    x
  } else {
    cwp_total_by_loc(x, loc.agg = loc.agg)
  }

  x.df.byloc %>%
    group_by(season_name) %>%
    summarise(count_mean = sum(count_loc_mean),
              count_var = if_else(min(census_date_min) < as.Date("2011-07-01"),
                                  NA_real_, sum(count_loc_var, na.rm = TRUE)),
              count_sd = sqrt(count_var),
              research_program = unique(research_program),
              .groups = "drop") %>%
    select(-count_var)
}
