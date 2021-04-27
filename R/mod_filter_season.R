#' Season filter selection
#'
#' Module for filtering data by multiple seasons (total or by week) or a single season
#'
#' @name mod_filter_season
#'
#' @param id character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#' @param col.width integer; column width of column of UI widgets
#'
#' @export
mod_filter_season_ui <- function(id, col.width = 4) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    # No fluidRow() call so that these can be incorporated in a single fluidRow call, e.g. with a species group checkbox
    column(
      width = col.width,
      uiOutput(ns("season_min_uiOut_select")),
      uiOutput(ns("season_select_uiOut_select")),
      uiOutput(ns("week_num_uiOut_select"))
    ),
    column(
      width = col.width,
      uiOutput(ns("season_max_uiOut_select")),
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
#' @param season.id.list a reactive of the season info list of season_info table
#'   IDs,  with names (season_info.season_name)
#' @param tbl.df a reactive lazy data frame; see details for more info
#' @param week.type a non-reactive character string; indicates how the week number should be calculated
#'
#' @details \code{tbl.df} should be a lazy data frame from a \code{\link[dplyr]{tbl}(pool, "table_name")} call.
#'   This data frame is retrieved using \code{\link[dplyr:compute]{collect}} in this module,
#'   and must have a column named 'date_column',
#'   If \code{week.type} is 'temporal', then the week numbers are calculated via
#'   \code{week_num = lubridate::\link[lubridate]{week}(date_column)}
#'   If \code{week.type} is 'diet', then the week numbers are determined relative to the diet_scat_date value
#'
#' @return A list with following components:
#' \itemize{
#'   \item{season_min: reactive integer, the ID of minimum season selection}
#'   \item{season_max: reactive integer, the ID of maximum season selection}
#'   \item{season_select: reactive integer, the ID of the single season selection}
#'   \item{week_num: reactive integer, the week number to plot}
#'   \item{date_range: reactive Date vector of length 2, the date range for a single season}
#' }
#'
#' @export
mod_filter_season_server <- function(id, summ.level, season.df, season.id.list, tbl.df, week.type = "temporal") {
  stopifnot(
    is.reactive(summ.level),
    summ.level() %in% c("fs_multiple_total", "fs_multiple_week", "fs_single", "raw"),
    is.reactive(season.df),
    is.reactive(season.id.list),
    is.reactive(tbl.df) | is.null(tbl.df),
    !is.reactive(week.type),
    week.type %in% c("temporal", "diet")
  )

  moduleServer(
    id,
    function(input, output, session) {
      #------------------------------------------------------------------------
      # Min season dropdown
      output$season_min_uiOut_select <- renderUI({
        req(summ.level() != "fs_single", season.id.list())
        selectInput(
          session$ns("season_min"), tags$h5("Minimum season"),
          choices = season.id.list(), selected = min(unlist(season.id.list()))
        )
      })


      # Max season dropdown
      output$season_max_uiOut_select <- renderUI({
        req(summ.level() != "fs_single", season.id.list())
        selectInput(
          session$ns("season_max"), tags$h5("Maximum season"),
          choices = season.id.list(), selected = max(unlist(season.id.list()))
        )
      })


      # Select season dropdown - could combine with min season, but left separate for now
      output$season_select_uiOut_select <- renderUI({
        req(summ.level() == "fs_single", season.id.list())
        selectInput(
          session$ns("season_select"), tags$h5("Select season"),
          choices = season.id.list(), selected = max(unlist(season.id.list()))
        )
      })


      # Week number dropdown
      output$week_num_uiOut_select <- renderUI({
        req(summ.level() == "fs_multiple_week", tbl.df(), input$season_min, input$season_max)

        tbl.df <- tbl.df() %>%
          filter(between(season_info_id, !!input$season_min, !!input$season_max)) %>%
          collect()

        tbl.df.wk <- if (week.type == "diet") {
          tbl.df %>%
            amlrPinnipeds::afs_diet_week(diet_scat_date, date_column)
        } else if (week.type == "temporal") {
          tbl.df %>% mutate(week_num = lubridate::week(date_column))
        }

        wk.list <- as.list(sort(unique(tbl.df.wk$week_num)))

        selectInput(
          session$ns("week_num"), tags$h5("Week number"),
          choices = wk.list, selected = min(unlist(wk.list))
        )
      })


      # Date range - for single season only
      output$date_range_uiOut_dateRange <- renderUI({
        req(summ.level() == "fs_single", season.df(), input$season_select)
        season.curr <- season.df() %>%
          filter(ID == as.numeric(input$season_select))

        validate(
          need(nrow(season.curr) == 1,
               "Error in mod_season_range_server.date_range_uiOut_dateRange")
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
