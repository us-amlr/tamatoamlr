#' Summarize AFS Capewide Pup Census Data
#'
#' Summarize AFS Capewide Pup Census Data
#'
#' @param x data frame; output of \code{\link{tbl_vCensus_AFS_Capewide_Pup}}
#'   or equivalent
#' @param x.bylocation logical; for function \code{afs_cwp_totals},
#'   indicates if \code{x} is the output of \code{afs_cwp_totals_bylocation}.
#'   Default is \code{FALSE}
#'
#' @details
#' afs_cwp_single: group by census_date and location;
#' output is lots of conglomerations that are
#' intended to help determine if recounts need to happen
#'
#' afs_cwp_totals_bylocation: group by season_name and location;
#' return count mean, variance, SD, and min date.
#' Var and SD follow CS/AMLR-specific rules about when it must be NA
#'
#' afs_cwp_totals: after running afs_cwp_totals_bylocation,
#' group by season name and return counts and standard deviations
#'
#' @return
#' An ungrouped data frame with the summarized AFS Capewide Pup Census data.
#' The summary types are described in 'Details'.
#'
#' @examplesIf FALSE
#' # Only will run if on SWFSC network
#' x <- tbl_vCensus_AFS_Capewide_Pup(amlr_dbConnect("***REMOVED***_Test"))
#'
#' afs_cwp_single(x[x$season_name == "2016/17", ])
#' afs_cwp_totals(x)
#'
#' x.byloc <- afs_cwp_totals_bylocation(x)
#' afs_cwp_totals(x.byloc, x.bylocation = TRUE)
#'
#' @name afs_capewide_pup
#' @export
afs_cwp_single <- function(x) { #}, by.observer) {
  columns.names <- c(
    "season_name", "census_afs_capewide_pup_sort", "location", "census_date",
    "observer", "census_notes", "exclude_count",
    "pup_count", "pup_live_count", "pup_dead_count"
  )

  stopifnot(
    all(c(columns.names %in% names(x))),
    n_distinct(x$season_name) == 1
  )

  x %>%
    group_by(season_name, census_afs_capewide_pup_sort, location) %>%
    arrange(observer) %>% #so that collapsed data are always in the same order
    summarise(n_records = n(),
              count_mean = round(mean(pup_count), 1),
              count_range = diff(range(pup_count)),
              count_range_perc_diff = if_else(
                count_mean == 0, 0,
                round(count_range / count_mean * 100, 2)),
              observers = paste(observer, collapse = "; "),
              counts = paste(pup_count, collapse = "; "),
              exclude_count = paste(as.integer(exclude_count), collapse = "; "),
              notes = paste(census_notes, collapse = "; "),
              counts_live = paste(pup_live_count, collapse = "; "),
              counts_dead = paste(pup_dead_count, collapse = "; "),
              # counts = paste(paste(observer, pup_count, sep = ": "),
              #                collapse = "; "),
              # exclude_count = paste(as.integer(exclude_count), collapse = "; "),
              # notes = paste(na.omit(
              #   if_else(
              #     is.na(census_notes), NA_character_,
              #     paste(observer, census_notes, sep = ": ")
              #   )
              # ), collapse = "; "),
              # counts_live = paste(paste(observer, pup_dead_count, sep = ": "),
              #                     collapse = "; "),
              # counts_dead = paste(paste(observer, pup_live_count, sep = ": "),
              #                     collapse = "; "),
              .groups = "drop") %>%
    arrange(census_afs_capewide_pup_sort) %>%
    select(-c(census_afs_capewide_pup_sort))
}

#' @name afs_capewide_pup
#' @export
afs_cwp_totals_bylocation <- function(x) {
  columns.names <- c(
    "season_name", "census_afs_capewide_pup_sort", "location",
    "census_date", "pup_count", "research_program"
  )

  stopifnot(all(c(columns.names %in% names(x))))
  if (any(x$exclude_count))
    warning("Some rows in x have a TRUE value for the 'exclude_count' flag",
            immediate. = TRUE)

  x %>%
    group_by(season_name, census_afs_capewide_pup_sort, location) %>%
    summarise(num_records = n(),
              count_loc_mean = mean(pup_count),
              count_loc_var = var(pup_count),
              date_min = min(census_date),
              research_program = unique(research_program),
              .groups = "drop") %>%
    mutate(study_beach_count = between(date_min,
                                       ymd("2008-07-01"), ymd("2011-07-01")) &
             location %in% c("Copihue", "Maderas", "Cachorros", "Chungungo"),
           count_loc_var = case_when(
             date_min < as.Date("2008-07-01") ~ NA_real_,
             study_beach_count ~ NA_real_,
             .default = count_loc_var
           ),
           count_loc_sd = sqrt(count_loc_var)) %>%
    relocate(count_loc_sd, .before = count_loc_var) %>%
    select(-c(census_afs_capewide_pup_sort, study_beach_count))
}

#' @name afs_capewide_pup
#' @export
afs_cwp_totals <- function(x, x.bylocation = FALSE) {
  y <- if (x.bylocation) {
    stopifnot(identical(
      c("season_name", "location",
        "num_records", "count_loc_mean", "count_loc_sd", "count_loc_var",
        "date_min", "research_program"),
      names(x)
    ))
    x
  } else {
    afs_cwp_totals_bylocation(x)
  }

  y %>%
    group_by(season_name) %>%
    summarise(count_mean = round(sum(count_loc_mean), 0),
              count_var = if_else(min(date_min) < as.Date("2011-07-01"),
                                  NA_real_, sum(count_loc_var, na.rm = TRUE)),
              count_sd = round(sqrt(count_var), 0),
              research_program = unique(research_program),
              .groups = "drop") %>%
    select(-count_var)
}
