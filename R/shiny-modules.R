#' Tamatoa data modules
#'
#' Modules for the different tabs of Tamatoa, the `tamatoamlr` Shiny app
#'
#' @name shiny_modules
#'
#' @param id character used to specify namespace, see [shiny::NS()]
#' @param src reactive; a DBI database connection. Intended to be the output of
#'   [mod_database_server()], i.e. a pool object
#' @param season.df reactive; the season info data frame. Intended to be the
#'   first element (`season.df`) of the (list) output of
#'   [mod_filter_season_server()]
#' @param tab reactive; the selected tab. Used by the modules to only collect
#'   data from database if the relevant tab is chosen
NULL
