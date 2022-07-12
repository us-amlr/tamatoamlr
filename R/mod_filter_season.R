#' Season filter selection
#'
#' Module for filtering data by multiple seasons (total or by week) or a single season
#'
#' @name mod_filter_season
#'
#' @param id character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @export
mod_filter_season_ui <- function(id) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    fluidRow(
      uiOutput(ns("season_select_uiOut_select")),
      uiOutput(ns("date_range_uiOut_dateRange"))
    )
  )
}


#' @name mod_filter_season
#'
#' @param summ.level a reactive of the 'summary level one' selection. Value must
#'   be one of: "fs_multiple_total", "fs_multiple_week", "fs_single", or "raw"
#' @param season.df reactive; the season info data frame. Intended to be the
#'   first element (\code{season.df}) of the (list) output of
#'   \code{\link{mod_season_info_server}}
#'
#' @return A list with following components:
#' \itemize{
#'   \item{season_select: reactive character, the name(s) of the selected season(s)}
#'   \item{date_range: reactive Date vector of length 2; the date range for a single season}
#' }
#'
#' @export
mod_filter_season_server <- function(id, summ.level, season.df) {
  #browser()
  stopifnot(
    is.reactive(summ.level),
    is.reactive(season.df)
  )

  moduleServer(
    id,
    function(input, output, session) {
      #browser()
      #------------------------------------------------------------------------
      ### Generate season list to use in reactive
      season_list <- reactive({
        as.list(season.df()$season_name)
      })

      #------------------------------------------------------------------------
      # Select season dropdown - could combine with min season, but left separate for now
      output$season_select_uiOut_select <- renderUI({
        req(season.df())

        ### Keep this list up to date with choices arg of .summaryTimingUI
        summ.levels.vals <- c(
          "fs_total", "fs_week", "fs_date_series", "fs_date_single",
          "fs_single", "fs_raw",

          "fs_multiple_total", "fs_multiple_date", "fs_multiple_week", "raw"
        )

        validate(
          need(summ.level() %in% summ.levels.vals,
               "Invalid summ.level value - please contact Sam")
        )

        if (summ.level() == "fs_single") {
          multi <- FALSE
          choices.sel <- max(unlist(season_list()))
          column.width = 6
        } else if (summ.level() == "fs_date_series") {
          multi <- TRUE
          choices.sel <- utils::head(unlist(season_list()), 6)
          column.width = 12
        } else {
          multi <- TRUE
          choices.sel <- unlist(season_list())
          column.width = 12
        }

        column(
          width = column.width,
          selectInput(
            session$ns("season_select"), tags$h5("Select season"),
            choices = season_list(), selected = choices.sel,
            multiple = multi, selectize = TRUE
          )
        )
      })


      # Date range - for single season only
      output$date_range_uiOut_dateRange <- renderUI({
        req(summ.level() == "fs_single", season.df(), input$season_select)
        season.curr <- season.df() %>%
          filter(season_name == input$season_select)

        validate(
          need(nrow(season.curr) == 1,
               "Error in mod_season_range_server.date_range_uiOut_dateRange")
        )

        start <- min <- season.curr[["season_open_date"]]
        end <- max <- if (is.na(season.curr[["season_close_date"]])) {
          Sys.Date()
        } else {
          season.curr[["season_close_date"]]
        }

        column(
          width = 6,
          dateRangeInput(
            session$ns("date_range"), tags$h5("Date range"),
            start = start, end = end, min = min, max = max
          )
        )
      })


      #------------------------------------------------------------------------
      ### Return values
      list(
        season_select = reactive(input$season_select),
        date_range = reactive(input$date_range)
      )
    }
  )
}
