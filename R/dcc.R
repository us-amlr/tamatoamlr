#' DCC Stuff
#'
#' DCC stuff
#'
#' @name dcc
#'
#' @param files character; the name of the file(s) which the
#'   DCC data are to be read from
#' @param station character; DCC station name. Will be added
#'   as a character string in a 'station' column to the raw DCC data
#' @param x data frame to process; see details for required columns
#' @param trip.hours numeric; minimum number of hours for a time gap
#'   to be considered a trip
#'
#' @details
#' General functions for processing DCC data
#'
#' @return
#' \code{dcc_read_files}: data frame with raw data row-binded together,
#' with a column added for 'station'
#'
#' \code{dcc_raw_process}:
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
dcc_format <- function(x) {
  dcc.columns <- c("Yr", "Day", "Hr", "Mn", "Fr", "Sig")
  dcc.names <- names(x)

  if (!all(dcc.columns %in% dcc.names))
    stop("The provided DCC data frame is missing the following required columns:\n",
         paste(setdiff(dcc.names, dcc.columns), collapse = ", "))

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
dcc_trips <- function(x, trip.hours) {
  dcc.columns <- c("freq", "code", "sig", "datetime")
  dcc.names <- names(x)

  if (!all(dcc.columns %in% dcc.names))
    stop("The provided DCC data frame is not properly formatted. ",
         "Have you used 'dcc_format'? ",
         "The data frame is missing the following required columns:\n",
         paste(setdiff(dcc.names, dcc.columns), collapse = ", "))

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
