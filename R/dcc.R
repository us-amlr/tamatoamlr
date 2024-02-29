#' DCC data processing
#'
#' Generalized functions for processing and summarizing DCC data. Functions are
#' designed to perform common DCC processing tasks, while leaving space for
#' users to do custom filtering, etc.
#'
#' @name dcc
#'
#' @param files character; the name of the file(s) which the DCC data are to be
#'   read from
#' @param station character; DCC station name. Will be added as a character
#'   string in a 'station' column to the raw DCC data
#' @param skip see [read.csv()]; number of lines to skip when reading each DCC
#'   data file
#' @param x data frame
#' @param tz see [timezones()]; timezone for the DCC data datetimes. Default is
#'   `"America/Punta_Arenas"`, which is the timezone at Cape Shirreff.
#' @param yr.pre character; the two digits to append onto the front of the
#'   two-digit value in the 'Yr' (year) column in the DCC data to make a
#'   four-digit year value. Should be either "20" (default) or "19" for e.g.
#'   2005 or 1999, respectively
#' @param trip.hours numeric; minimum number of hours for a time gap to be
#'   considered a trip
#' @param trips.only logical; indicates if `dcc_calc_trips()` should return a
#'   data frame with all pings with their associated `time_diff_hr` values
#'   (`FALSE`, default), or a data frame filtered for records where
#'   `time_diff_hr > trip.hours`
#'
#' @details Note: these generalized functions for processing DCC data likely
#'   will be moved to their own package sometime in the future.
#'
#' `dcc_read_files()`: Reads data from the provided CSV file paths, and row-bind
#' them into a single data frame with a character column added for 'station'
#'
#' `dcc_format()`: Takes raw DCC data, ideally the output(s) of
#' `dcc_read_files()`, and process these data to a standardized format.
#' Specifically: renames columns, and uses the Yr/Day/Hr/Mn columns to create a
#' single 'datetime' column with the user-provided timezone (`tz`). Returns a
#' data frame with the following columns: freq (transmitter frequency), code
#' (transmitter code, if present in the raw data), sig (signal strength of
#' ping), datetime, station. Assumptions made by this function:
#'
#' * The Yr/Day/Hr/Mn columns, when pasted together with
#' `paste(paste0(yr.pre, Yr), Day, Hr, Mn, sep = "-")`, follow the [strptime()]
#' format `"%Y-%j-%H-%M"` .
#' * All values in the 'Fr' column are either 4- or 6-digit integers
#' representing the frequency value (e.g., 4105 or 164105). The code '16' is
#' pasted onto the front of 4-digit codes. Then all codes are divided by 1000 to
#' generate the true numeric frequency (e.g., 164.105)
#'
#' `dcc_calc_trips()`: Takes processed/formatted DCC data, ideally the output(s)
#' of `dcc_format()`, and calculates time_diff_hr: for each ping, the time
#' difference from the previous ping. This function also calculates
#' trip_num_completed: for each ping, the number of trips that have been
#' completed up to that point in time. Both calculations are done after grouping
#' by freq and (if applicable) code. Trips are identified by `time_diff_hr>=trip.hours`
#'
#' @return For all functions: an ungrouped data frame, as described in Details.
#'
#' @examples
#' dcc_read_files(system.file("extdata", "dcc-coded.csv", package = "tamatoamlr"),
#'                station = "TEST")
#' dcc_read_files(system.file("extdata", "dcc-standard.csv", package = "tamatoamlr"),
#'                station = "TEST")
#'
#' y.coded <- dcc_format(dcc.coded)
#' dcc_calc_trips(y.coded, trip.hours = 24)
#'
#' y.standard <- dcc_format(dcc.standard)
#' dcc_calc_trips(y.standard, trip.hours = 8)
#' dcc_calc_trips(y.standard, trip.hours = 8, trips.only = TRUE)
#' dcc_calc_trips(y.standard, trip.hours = 24, trips.only = TRUE)
#'
#' @export
dcc_read_files <- function(files, station, skip = 6) {
  bind_rows(lapply(files, read.csv, skip = skip)) %>%
    mutate(station = station)
}


#' @name dcc
#' @export
dcc_format <- function(x, tz = "America/Punta_Arenas", yr.pre = "20") {
  # Error checking
  stopifnot(
    yr.pre %in% c("19", "20")
  )

  dcc.columns <- c("Yr", "Day", "Hr", "Mn", "Fr", "Sig")
  dcc.names <- names(x)
  if (!all(dcc.columns %in% dcc.names))
    stop("The provided data frame x is missing the following required columns:\n",
         paste(setdiff(dcc.columns, dcc.names), collapse = ", "))

  # Use raw columns to create datetime and freq columns
  x.proc <- x %>%
    mutate(
      datetime = as.POSIXct(
        strptime(paste(paste0(yr.pre, Yr), Day, Hr, Mn, sep = "-"),
                 format = "%Y-%j-%H-%M")),
      freq_chr = case_when(
        str_length(Fr) == 4 ~ paste0(16, Fr),
        str_length(Fr) == 6 ~ as.character(Fr),
        .default = NA_character_
      ),
      freq = as.numeric(freq_chr)/1000) %>%
    rename_with(tolower)


  # Return data frame, with renamed code if applicable
  code.syms <- syms(intersect(c("code"), names(x.proc)))
  x.proc %>%
    select(freq, !!!code.syms, sig, datetime, station) %>%
    arrange(freq, !!!code.syms, datetime)
}


#' @name dcc
#' @export
dcc_calc_trips <- function(x, trip.hours, trips.only = FALSE) {
  # Error checking
  dcc.columns <- c("freq", "sig", "datetime")
  dcc.names <- names(x)
  if (!all(dcc.columns %in% dcc.names))
    stop("The provided data frame x is not formatted as required. ",
         "Have you used 'dcc_format'? ",
         "x is missing the following required columns:\n",
         paste(setdiff(dcc.columns, dcc.names), collapse = ", "))

  # Make code robust to non-coded DCC data, aka data frames w/o code column
  code.syms <- syms(intersect(c("code"), dcc.names))

  # Do calculations
  x.out <- x %>%
    arrange(freq, !!!code.syms, datetime) %>%
    group_by(freq, !!!code.syms) %>%
    mutate(datetime_prev = lag(datetime),
           time_diff_hr = round(as.numeric(
             difftime(datetime, datetime_prev, units = "hours")), 2),
           trip_num_completed = cumsum(replace_na(time_diff_hr, 0) >= trip.hours)) %>%
    ungroup() %>%
    select(freq, !!!code.syms, sig, datetime,
           datetime_prev, time_diff_hr, trip_num_completed,
           everything())

  # If requested by the user, filter only for the rows capturing the trips
  if (trips.only) {
    x.out %>% filter(time_diff_hr >= trip.hours)
  } else {
    x.out
  }
}
