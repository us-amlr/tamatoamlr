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
#' A data frame with the summarized AFS Capewide Pup Census data.
#' The summary types are described in 'Details'.
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
    all(c(columns.names %in% names(x)))
  )

  # if (by.observer) {
  #   x %>%
  #     group_by(season_name, observer, census_date,
  #              census_afs_capewide_pup_sort, location) %>%
  #     summarise(n_records = n(),
  #               pup_count = paste(pup_count, sep = "; "),
  #               pup_count = paste(pup_live_count, sep = "; "),
  #               pup_count = paste(pup_dead_count, sep = "; "),
  #
  #               count_mean = round(mean(pup_count), 1),
  #               count_live_mean = round(mean(pup_live_count), 1),
  #               count_dead_mean = round(mean(pup_dead_count), 1),
  #               count_range = diff(range(pup_count)),
  #               counts = paste(paste(observer, pup_count, sep = ": "),
  #                              collapse = "; "),
  #               exclude_count = paste(as.integer(exclude_count), sep = "; "),
  #               notes_tmp = list(if_else(
  #                 is.na(census_notes), NA_character_,
  #                 paste(observer, census_notes, sep = ": ")
  #               )),
  #               notes = paste(na.omit(unlist(notes_tmp)), collapse = "; "),
  #               counts_live = paste(paste(observer, pup_live_count, sep = ": "),
  #                                   collapse = "; "),
  #               counts_dead = paste(paste(observer, pup_dead_count, sep = ": "),
  #                                   collapse = "; "),
  #               .groups = "drop") %>%
  #     arrange(census_afs_capewide_pup_sort) %>%
  #     select(-c(census_afs_capewide_pup_sort, notes_tmp))
  #
  # } else {
  x %>%
    group_by(season_name, census_date,
             census_afs_capewide_pup_sort, location) %>%
    summarise(n_records = n(),
              count_mean = round(mean(pup_count), 1),
              count_live_mean = round(mean(pup_live_count), 1),
              count_dead_mean = round(mean(pup_dead_count), 1),
              count_range = diff(range(pup_count)),
              counts = paste(paste(observer, pup_count, sep = ": "),
                             collapse = "; "),
              exclude_count = paste(as.integer(exclude_count), sep = "; "),
              notes_tmp = list(if_else(
                is.na(census_notes), NA_character_,
                paste(observer, census_notes, sep = ": ")
              )),
              notes = paste(na.omit(unlist(notes_tmp)), collapse = "; "),
              counts_live = paste(paste(observer, pup_dead_count, sep = ": "),
                                  collapse = "; "),
              counts_dead = paste(paste(observer, pup_live_count, sep = ": "),
                                  collapse = "; "),
              .groups = "drop") %>%
    arrange(census_afs_capewide_pup_sort) %>%
    select(-c(census_afs_capewide_pup_sort, notes_tmp))
  # }
}

#' @name afs_capewide_pup
#' @export
afs_cwp_totals_bylocation <- function(x) {
  columns.names <- c(
    "season_name", "census_afs_capewide_pup_sort", "location", "census_date",
    "pup_count"
  )

  stopifnot(
    !any(x$exclude_count),
    all(c(columns.names %in% names(x)))
  )

  x %>%
    group_by(season_name, census_afs_capewide_pup_sort, location) %>%
    summarise(num_records = n(),
              count_loc_mean = mean(pup_count),
              count_loc_var = var(pup_count),
              date_min = min(census_date),
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
    select(-c(census_afs_capewide_pup_sort, study_beach_count))
}

#' @name afs_capewide_pup
#' @export
afs_cwp_totals <- function(x, x.bylocation = FALSE) {
  y <- if (x.bylocation) {
    stopifnot(identical(
      c("season_name", "location",
        "num_records", "count_loc_mean", "count_loc_var", "date_min"),
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
              .groups = "drop") %>%
    select(-count_var)
}
