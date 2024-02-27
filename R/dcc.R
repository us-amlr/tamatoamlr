#' DCC data processing
#'
#' DCC data processing
#'
#' @name dcc
#'
#' @param files character; the name of the file(s) which the
#'   DCC data are to be read from
#' @param station character; DCC station name. Will be added
#'   as a character string in a 'station' column to the raw DCC data
#' @param x data frame to process.
#'   For \code{dcc_format}, see details for required columns
#' @param trip.hours numeric; minimum number of hours for a time gap
#'   to be considered a trip
#' @param tz character; time zone for DCC datetime.
#'   See \code{\link[base:timezones]{timezones}} for more details.
#'   Default is \code{"America/Punta_Arenas"}
#'
#' @details
#' General functions for processing DCC data. Sometime in the future, these may
#' be moved to their own package.
#'
#' @return
#' \code{dcc_read_files}: data frame with raw data row-binded together,
#' and a column added for 'station'
#'
#' \code{dcc_format}: data frame with the following columns: freq (transmitter
#' frequency), code (transmitter code, if relevant), sig (ping signal strength),
#' datetime, station
#'
#' \code{dcc_calc_trips}:
#'
#' @examples
#' # dcc_read_files(file.path, "CABO")
#'
#' @export
dcc_read_files <- function(files, station) {
  bind_rows(lapply(files, function(i) {
    read.csv(i, skip = 6) %>%
      mutate(station = station)
  }))
}


#' @name dcc
#' @export
dcc_format <- function(x, tz = "America/Punta_Arenas") {
  dcc.columns <- c("Yr", "Day", "Hr", "Mn", "Fr", "Sig")
  dcc.names <- names(x)

  if (!all(dcc.columns %in% dcc.names))
    stop("The provided DCC data frame is missing the following required columns:\n",
         paste(setdiff(dcc.columns, dcc.names), collapse = ", "))

  x.proc <- x %>%
    mutate(
      datetime = as.POSIXct(
        strptime(paste(paste0("20", Yr), Day, Hr, Mn, sep = "-"),
                 format = "%Y-%j-%H-%M")),
      freq = as.numeric(case_when(
        str_length(Fr) == 4 ~ paste0("164.", str_sub(Fr, 2, 4)),
        str_length(Fr) == 6 ~ paste0("164.", str_sub(Fr, 4, 6)),
        .default = NA_character_
      )))

  if ("Code" %in% dcc.names) {
    x.proc %>%
      select(freq, code = Code, sig = Sig, datetime, station) %>%
      arrange(freq, code, datetime)
  } else {
    x.proc %>%
      select(freq, sig = Sig, datetime, station) %>%
      arrange(freq, datetime)
  }
}


#' @name dcc
#' @export
dcc_calc_trips <- function(x, trip.hours) {
  dcc.columns <- c("freq", "code", "sig", "datetime")
  dcc.names <- names(x)

  if (!all(dcc.columns %in% dcc.names))
    stop("The provided DCC data frame is not properly formatted. ",
         "Have you used 'dcc_format'? ",
         "The data frame is missing the following required columns:\n",
         paste(setdiff(dcc.columns, dcc.names), collapse = ", "))

  x %>%
    arrange(freq, code, datetime) %>%
    group_by(freq, code) %>%
    mutate(datetime_prev = lag(datetime),
           time_diff_hr = round(as.numeric(
             difftime(datetime, datetime_prev, units = "hours")), 2),
           trip_num_completed = c(NA, cumsum(head(lead(time_diff_hr), -1) > trip.hours))) %>%
    ungroup() %>%
    select(freq, code, sig, datetime,
           datetime_prev, time_diff_hr, trip_num_completed,
           everything())
}


#' @name dcc
#' @export
mutate_tag_freq_code <- function(x) {
  x %>% mutate(tag_freq_code = paste(tag, freq, code, sep = " | "))
}
