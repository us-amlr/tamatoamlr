#' amlrPinnipeds: A package and Shiny app to analyze and visualize data from the ***REMOVED*** database
#'
#' This package contains \code{\link{tamatoa}},
#' a Shiny app that connects to the ***REMOVED*** database,
#' and allows users to analyze and visualize data from said database.
#' There are also several stand-alone functions,
#' such as for calculating krill lengths from carapace measurements.
#'
#' @name amlrPinnipeds-package
#' @aliases amlrPinnipeds
#' @docType package
#' @title AMLR Pinniped Program data processing and analysis
#' @author Sam Woodman \email{sam.woodman@@noaa.gov}
#'
#' @import amlrDatabases
#' @import dplyr
#' @import ggplot2
#' @import shiny
#'
#' @importFrom DT DTOutput renderDT
#' @importFrom glue glue
#' @importFrom lubridate days is.Date month week year ymd
#' @importFrom purrr pmap_dbl pmap_lgl set_names
#' @importFrom rlang .data enquo
#' @importFrom scales hue_pal
#' @importFrom shinydashboard box
#' @importFrom stringr str_to_sentence
#' @importFrom tidyr nest pivot_longer pivot_wider unnest
#' @importFrom utils write.csv
#'
#' @keywords package
NULL
