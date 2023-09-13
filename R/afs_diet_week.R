#' Fur seal diet week calculation
#'
#' Calculate the week number of the AFS diet study given the diet study start date and a scat collection date
#'
#' @param x data frame
#' @param col.diet.scat.date tidy-select (one unquoted expression) column with the (corresponding) diet study start date
#' @param col.collection.date tidy-select (one unquoted expression) column with the scat collection date
#'
#' @return
#' The data frame \code{x}, with two additional columns: 'week_num' and 'week_start_date'.
#' 'week_num' is the week number in which this scat was collected,
#' calculated using \code{(\link[base:Round]{floor}(col.collection.date - col.diet.scat.date) / 7) + 1},
#' and 'week_start_date' is the date of the start of the 'week_num' week,
#' specifically \code{col.diet.scat.date + \link[lubridate:period]{days}((week_num - 1) * 7)}
#'
#' @examples
#' x <- data.frame(
#'   ID = 1:3,
#'   diet_scat_start = c(as.Date("2016-12-17"), as.Date("2016-12-17"), as.Date("2017-12-23")),
#'   collection_date = c(as.Date("2016-12-17"), as.Date("2016-12-24"), as.Date("2018-01-15"))
#' )
#' afs_diet_week(x, diet_scat_start, collection_date)
#'
#' @export
afs_diet_week <- function(x, col.diet.scat.date, col.collection.date) {
  col.diet.scat.date <- enquo(col.diet.scat.date)
  col.collection.date <- enquo(col.collection.date)

  # # This is inefficient
  # x.tmp <- x %>%
  #   select(!!col.collection.date, !!col.diet.scat.date) %>%
  #   select(where(is.Date))
  #
  # if (ncol(x.tmp) != 2) {
  #   stop("The columns specified by col.diet.scat.date and col.collection.date must be of class Date")
  # }

  x.out <- x %>%
    mutate(diff_days = as.numeric(difftime(!!col.collection.date, !!col.diet.scat.date,
                                           units = "days")))

  if (any(x.out$diff_days < 0)) {
    stop("The col.collection.date date cannot come before the col.diet.scat.date date")
  } else if (any(x.out$diff_days > 365)) {
    stop("The col.collection.date date cannot come more than a year (365 days) after the col.diet.scat.date date")
  }

  x.out %>%
    mutate(week_num = floor(.data$diff_days / 7) + 1,
           week_start_date = ymd(!!col.diet.scat.date) + days((.data$week_num - 1) * 7)) %>%
    select(-.data$diff_days)
}
