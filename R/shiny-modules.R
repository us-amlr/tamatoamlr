#' Modules for the different tabs of Tamatoa, the tamatoamlr Shiny app
#'
#' Modules for the different tabs of Tamatoa, the tamatoamlr Shiny app
#'
#' @name shiny_modules
#'
#' @param id character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#' @param pool reactive; a DBI database connection pool.
#'   Intended to be the output of \code{\link{mod_database_server}}
#' @param season.df reactive; the season info data frame.
#'   Intended to be the first element (\code{season.df}) of
#'   the (list) output of \code{\link{mod_filter_season_server}}
#'
NULL
