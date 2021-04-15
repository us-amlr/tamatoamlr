#' Filter
#'
#' Filters - currently only used in census tab
#'
#' @name mod_filter
#'
#' @param id, character used to specify namespace, see \code{\link[shiny]{NS}}
#'
#' @export
mod_filter_ui <- function(id) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    conditionalPanel(
      condition = "input.summary_level_1 != 'fs_single'", ns = ns,
      column(
        width = 4,
        selectInput("season_min", label = tags$h5("Minimum season"),
                    choices = season.list, selected = season.list.id.min),
        conditionalPanel(
          condition = "input.summary_level_1 == 'fs_multiple_week'",
          selectInput("week_num", tags$h5("Week number"), choices = list(), selected = NULL)
        )
      ),
      column(4, selectInput("season_max", label = tags$h5("Maximum season"),
                            choices = season.list, selected = season.list.id.max))
    ),
    conditionalPanel(
      condition = "input.summary_level_1 == 'fs_single'", ns = ns,
      column(4, selectInput(ns("season_select"), label = tags$h5("Select season"), choices = NULL)),
      column(4, dateRangeInput(ns("date_range"), label = tags$h5("Date range"))) #Updated in observe() based on selected season
    ),

    column(
      width = 3, offset = 1,
      conditionalPanel(
        condition = "input.type == 'phocid'", ns = ns,
        checkboxGroupInput(ns("species"), label = tags$h5("Species"),
                           choices = pinniped.sp.list.phocid,
                           selected = unname(unlist(pinniped.sp.list.phocid)))
      )
    )
  )
}


#' @name mod_filter
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
mod_filter_server <- function(id, summary.level.1) {
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
