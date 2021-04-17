#' Season filter selection
#'
#' Module for filtering data by multiple seasons (total or by week) or a single season
#'
#' @name mod_season_filter
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @export
mod_season_filter_ui <- function(id) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    fluidRow(
      column(
        width = 6,
        uiOutput(ns("season_min_uiOut_select")),
        uiOutput(ns("season_select_uiOut_select")),
        uiOutput(ns("week_num_uiOut_select"))
      ),
      column(
        width = 6,
        uiOutput(ns("season_max_uiOut_select")),
        uiOutput(ns("date_range_uiOut_dateRange"))
      )
    )
  )
  #   conditionalPanel(
  #     condition = "input.summary_level_1 != 'fs_single'", ns = ns,
  #     column(
  #       width = 4,
  #       selectInput(ns("season_min"), label = tags$h5("Minimum season"), choices = list()),
  #       conditionalPanel(
  #         condition = "input.summary_level_1 == 'fs_multiple_week'", ns = ns,
  #         selectInput(ns("week_num"), tags$h5("Week number"), choices = list())
  #       )
  #     ),
  #     column(4, selectInput(ns("season_max"), label = tags$h5("Maximum season"), choices = list()))
  #   ),
  #   conditionalPanel(
  #     condition = "input.summary_level_1 == 'fs_single'", ns = ns,
  #     column(4, selectInput(ns("season_select"), label = tags$h5("Select season"), choices = NULL)),
  #     column(4, dateRangeInput(ns("date_range"), label = tags$h5("Date range")))
  #   ),
  # )
}


#' @name mod_season_filter
#'
#' @param summ.level a reactive of the 'summary level one' selection. Value must be one of:
#'   "fs_multiple_total", "fs_multiple_week", "fs_single", or "raw"
#' @param season.df reactive; the season info data frame.
#'   Intended to be the first element (\code{season.df}) of the (list) output of \code{\link{mod_season_info_server}}
#' @param season.id.list a reactive of the season info list of season_info table IDs,  with names (season_info.season_name)
#' @param week.list a reactive of the weeks in the dataset where this module was called from
#'
#' @return A list with following components:
#' \describe{
#'   \item{season_min}{reactive character string indicating ID of minimum season selection}
#'   \item{season_max}{reactive character string indicating ID of maximum season selection}
#' }
#'
#' @export
mod_season_filter_server <- function(id, summ.level, season.df, season.id.list, week.list) {
  stopifnot(
    is.reactive(summ.level),
    summ.level() %in% c("fs_multiple_total", "fs_multiple_week", "fs_single", "raw"),
    is.reactive(summ.level),
    is.reactive(season.df),
    is.reactive(season.id.list),
    is.reactive(week.list)
  )

  moduleServer(
    id,
    function(input, output, session) {
      #------------------------------------------------------------------------
      # Get/set reactive values. We use these so that we don't have to change variable names in the code below
      vals <- reactiveValues(
        season.df = NULL,
        season.list = NULL,
        week.list = NULL
      )

      observe({
        vals$season.df = season.df()
        vals$season.list = season.id.list()
        vals$week.list = week.list()
      })

      #------------------------------------------------------------------------
      # Min season dropdown
      output$season_min_uiOut_select <- renderUI({
        req(summ.level != "fs_single", season.list)
        selectInput(
          session$ns("season_min"), tags$h5("Minimum season"),
          choices = season.list, selected = min(unlist(season.list))
        )
      })

      # Max season dropdown
      output$season_max_uiOut_select <- renderUI({
        req(summ.level != "fs_single", season.list)
        selectInput(
          session$ns("season_max"), tags$h5("Maximum season"),
          choices = season.list, selected = max(unlist(season.list))
        )
      })

      # Week number dropdown
      output$week_num_uiOut_select <- renderUI({
        req(summ.level == "fs_multiple_week", vals$week.list)
        selectInput(
          session$ns("week_num"), tags$h5("Week number"),
          choices = vals$week.list, selected = min(unlist(vals$week.list))
        )
      })

      # Select season dropdown - could combine with min season, but left separate for now
      output$season_select_uiOut_select <- renderUI({
        req(summ.level == "fs_single", season.list)
        selectInput(
          session$ns("season_select"), tags$h5("Select season"),
          choices = season.list, selected = max(unlist(season.list))
        )
      })

      # Date range - for single season only
      output$season_select_uiOut_select <- renderUI({
        req(summ.level == "fs_single")
        season.curr <- req(vals$season.df) %>%
          filter(ID == as.numeric(req(input$season_select)))

        validate(
          need(nrow(season.curr) == 1,
               "Error in mod_season_range_server.season_select_uiOut_select")
        )

        start <- min <- season.curr[["season_open_date"]]
        end <- max <- season.curr[["season_close_date"]]

        dateRangeInput(
          session$ns("date_range"), tags$h5("Date range"),
          start = start, end = end, min = min, max = max
        )
      })


      #------------------------------------------------------------------------
      ### Return values
      list(
        season_min = reactive(as.integer(input$season_min)),
        season_max = reactive(as.integer(input$season_max)),
        season_select = reactive(as.integer(input$season_select)),
        week_num = reactive(as.integer(input$week_num)),
        date_range = reactive(input$date_range)
      )
    }
  )
}
