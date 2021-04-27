#' Sort data frame by season
#'
#' Sort data frame by season, specifically by the season_open_date
#'
#' @param x data frame that at minimum contains a 'season_name' column
#' @param season.info season.info data frame
#' @param ... additional tidyselect arguments passed to arrange(), after desc(season_open_date)
#' @param .desc logical; indicates if the data should be sorted by the descending season open date
#'
#' @details Sort x by corresponding season_open_date (descending), then other user-provided columns
#'
#' @return \code{x}, sorted by season open date and then \code{...}
#'
#' @export
arrange_season <- function(x, season.info, ..., .desc = TRUE) {
  stopifnot(
    inherits(x, "data.frame"),
    "season_name" %in% names(x),
    inherits(season.info, "data.frame"),
    all(c("season_name", "season_open_date") %in% names(season.info)),
    inherits(.desc, "logical")
  )

  season.info.sel <- season.info %>% select(season_name, season_open_date)
  x.out <- left_join(x, season.info.sel, by = "season_name") %>%
    arrange(if (.desc) desc(season_open_date) else season_open_date, ...) %>%
    select(-season_open_date)

  if (nrow(x) != nrow(x.out)) stop("Error in arrange_season_info() - additional rows")

  x.out
}
