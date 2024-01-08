#' DCC Stuff
#'
#' DCC stuff
#'
#' @name dcc
#'
#' @param x todo
#'
#' @details
#'
#' @return
#' data
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
dcc_raw_process <- function(x) {
  dcc.columns <- c("Yr", "Day", "Hr", "Mn", "Fr", "Sig", "Code")
  dcc.names <- names(x)

  if (!all(dcc.columns %in% dcc.names))
    stop("The loaded DCC file(s) are missing the following expected columns:\n",
         paste(setdiff(dcc.names, dcc.columns), collapse = ", "))

  x %>%
    mutate(
      datetime = as.POSIXct(
        strptime(paste(paste0("20", Yr), Day, Hr, Mn, sep = "-"),
                 format = "%Y-%j-%H-%M")),
      freq = as.numeric(case_when(
        str_length(Fr) == 4 ~ paste0("164.", str_sub(Fr, 2, 4)),
        str_length(Fr) == 6 ~ paste0("164.", str_sub(Fr, 4, 6)),
        .default = NA_character_
      ))) %>%
    select(freq, code = Code, sig = Sig, datetime, station) %>%
    arrange(freq, code, datetime)
}


