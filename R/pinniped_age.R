#' Pinniped age calculation
#'
#' Calculate the age of a pinniped based on a given date and cohort year
#'
#' @param event.date object of class \code{Date}; the date at which you wish to determine the pinniped's age
#' @param cohort object that can be interpreted as number(s),
#'   meaning \code{\link[base:numeric]{is.numeric}(cohort)} must be \code{TRUE}.
#'   The cohort year of the pinniped.
#'
#' @details \code{event.date} (event date) and \code{cohort} must be the same length.
#'
#'   The age of the pinniped is calculated by taking the year of the event date,
#'   subtracting the cohort year, and subtracting an additional 1 if the month is before October
#'   (i.e., if the month of the event date is less than 10).
#'   For instance, if the event date is 15 Nov 2010 and the cohort is 2005, the pinniped age is 2010 - 2005 - 0 = 5
#'   If the event date is 15 Feb 2010 and the cohort is 2005, then the pinniped age is 2010 - 2005 - 1 = 4.
#'
#'   If either the event date or the cohort is \code{NA}, then the pinniped age will be \code{NA}
#'
#' @return A numeric vector of the same length as the input vectors
#'
#' @examples
#' x <- data.frame(
#'   resight_date = as.Date(c("2012-11-01", "2012-03-01", "2014-02-01")),
#'   pinniped_id = c(1, 1, 2),
#'   cohort = c(2010, 2010, 2011)
#' )
#'
#' pinniped_age(x$resight_date, x$cohort)
#'
#' @export
pinniped_age <- function(event.date, cohort) {
  stopifnot(
    (length(event.date) == length(cohort)) | length(event.date) == 1,
    inherits(event.date, "Date"),
    is.numeric(cohort)
  )

  as.numeric( #in case of all NAs
    lubridate::year(event.date) - cohort - if_else(lubridate::month(event.date) < 10, 1, 0)
  )
}
