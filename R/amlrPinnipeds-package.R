#' amlrPinnipeds: Analyze and Visualize US AMLR Pinniped Data
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
#' @title US AMLR Pinniped Program data processing and analysis
#' @author Sam Woodman \email{sam.woodman@@noaa.gov}
#' @author Karen Snyder \email{ksnyder@@g.hmc.edu}
#'
#' @import amlrDatabases
#' @import dplyr
#' @import ggplot2
#' @import shiny
#'
#' @importFrom DT DTOutput renderDT
#' @importFrom forcats fct
#' @importFrom glue glue
#' @importFrom lubridate days days_in_month is.Date month week year mdy mdy_hms ymd ymd_hms
#' @importFrom pool dbIsValid dbGetQuery poolClose
#' @importFrom purrr pmap_dbl pmap_lgl set_names
#' @importFrom rlang .data enquo
#' @importFrom scales hue_pal
#' @importFrom shinybusy add_busy_spinner
#' @importFrom shinydashboard box dashboardBody tabItems tabItem
#'   dashboardHeader dashboardPage dashboardSidebar sidebarMenu menuItem
#' @importFrom shinyjs useShinyjs extendShinyjs js
#' @importFrom stats na.omit var
#' @importFrom stringi stri_escape_unicode
#' @importFrom stringr str_length str_match str_remove_all str_replace_all
#' str_replace_na str_sub str_to_lower str_to_sentence
#' @importFrom tidyr complete nest nesting unnest pivot_longer pivot_wider replace_na
#' @importFrom utils globalVariables read.csv write.csv
#'
#' @keywords package
NULL
