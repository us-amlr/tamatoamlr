#' amlrPinnipeds: A to to analyze and visualize data from the AMLR_PINNIPEDS database
#'
#' This package contains a Shiny app that connects to the AMLR_PINNIPEDS database,
#'   and allows users to analyze and visualize data from said database.
#'   This can be done manually in R, or using the built-in R Shiny app opened through \code{\link{amlr_pinnipeds_gui}}
#'
#' @name amlrPinnipeds-package
#' @aliases amlrPinnipeds
#' @docType package
#' @title AMLR Pinniped Program data processing and analysis
#' @author Sam Woodman \email{sam.woodman@@noaa.gov}
#'
#' @import dplyr
#' @import ggplot2
#' @import shiny
#'
#' @importFrom DT DTOutput renderDT
#' @importFrom glue glue
#' @importFrom lubridate days is.Date month week year ymd
#' @importFrom purrr pmap_dbl pmap_lgl set_names
#' @importFrom rlang .data enquo
#' @importFrom shinydashboard box
#' @importFrom stringr str_to_sentence
#' @importFrom tidyr nest pivot_longer pivot_wider unnest
#' @importFrom utils write.csv
#'
#' @keywords package
NULL
