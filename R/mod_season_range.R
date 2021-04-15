#' Season range selection
#'
#' Season range selection
#'
#' @name mod_season_range
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#' @param col.width integer; column width of each selectInput widget
#' @param single.season logical; a boolean flag indicating...
#' @param ...
#'
#' @export
mod_season_range_ui <- function(id, col.width) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    column(col.width, selectInput(ns("season_min"), label = tags$h5("Minimum season"), choices = NULL)),
    column(col.width, selectInput(ns("season_max"), label = tags$h5("Maximum season"), choices = NULL))
  )
}


#' @name mod_season_range
#'
#' @param season.id.list, a reactive of season info data, including at least a season.list argument
#'
#' @return list with following components
#' \describe{
#'   \item{minvar}{reactive character string indicating ID of minimum season selection}
#'   \item{maxvar}{reactive character string indicating ID of maximum season selection}
#' }
#'
#' @export
mod_season_range_server <- function(id, season.id.list) {
  stopifnot(is.reactive(season.id.list))

  moduleServer(
    id,
    function(input, output, session) {
      # Set choices and selected for the season selectors
      observe({
        season.list <- season.id.list()
        if (!identical(season.list, list())) {
          updateSelectInput(session, "season_min", choices = season.list, selected = min(unlist(season.list)))
          updateSelectInput(session, "season_max", choices = season.list, selected = max(unlist(season.list)))
        } else {
          updateSelectInput(session, "season_min", choices = NULL)
          updateSelectInput(session, "season_max", choices = NULL)
        }
      })

      # Return values
      list(
        minvar = reactive(as.integer(input$season_min)),
        maxvar = reactive(as.integer(input$season_max))
      )
    }
  )
}
