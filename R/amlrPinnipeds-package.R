#' amlrPinnipeds: Analyze and Visualize US AMLR Pinniped Data
#'
#' This package contains \code{\link{tamatoa}},
#' a Shiny app that connects to the AMLR_PINNIPEDS database,
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
#' @importFrom glue glue
#' @importFrom lubridate days is.Date month week year ymd
#' @importFrom pool dbIsValid dbGetQuery poolClose
#' @importFrom purrr pmap_dbl pmap_lgl set_names
#' @importFrom rlang .data enquo
#' @importFrom scales hue_pal
#' @importFrom shinybusy add_busy_spinner
#' @importFrom shinydashboard box dashboardBody tabItems tabItem
#'   dashboardHeader dashboardPage dashboardSidebar sidebarMenu menuItem
#' @importFrom shinyjs useShinyjs extendShinyjs js
#' @importFrom stringr str_to_sentence str_to_lower
#' @importFrom tidyr complete nest unnest pivot_longer pivot_wider replace_na
#' @importFrom utils globalVariables write.csv
#'
#' @keywords package
NULL
