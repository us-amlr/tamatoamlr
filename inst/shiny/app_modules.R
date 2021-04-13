
#'Season selection
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
seasoninfo_mod_ui <- function(id, col.width) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    column(col.width, selectInput(ns("season_min"), label = tags$h5("Minimum season"), choices = NULL)),
    column(col.width, selectInput(ns("season_max"), label = tags$h5("Maximum season"), choices = NULL))
  )
}


#' Variable selection module server-side processing
#'
#' @param input, output, session standard \code{shiny} boilerplate
#'
#' @return list with following components
#' \describe{
#'   \item{minvar}{reactive character string indicating ID of minimum season selection}
#'   \item{maxvar}{reactive character string indicating ID of maximum season selection}
#' }
seasoninfo_mod_server <- function(id, vals.si) {
  moduleServer(
    id,
    function(input, output, session) {
      # Set choices and selected
      print("season_info mod server")
      observe({
        season.list <- vals.si()$season.list
        if (!identical(season.list, list())) {
          updateSelectInput(session, "season_min", choices = season.list, selected = min(unlist(season.list)))
          updateSelectInput(session, "season_max", choices = season.list, selected = max(unlist(season.list)))
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
