#' Shiny module for season information
#'
#' Shiny module for season information
#'
#' @name mod_season_info
#'
#' @param id character used to specify namespace, see \code{\link[shiny]{NS}}
#' @param col.width integer; column width of column of UI widgets
#'
#' @export
mod_season_info_ui <- function(id, col.width = 7) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    box(
      title = "Season information", status = "warning", solidHeader = FALSE,
      width = col.width, collapsible = TRUE,
      tableOutput(ns("tbl_season")),
      uiOutput(ns("tbl_download_uiOut"))
    )
  )
}


#' @name mod_season_info
#'
#' @param pool reactive; a DBI database connection pool,
#'   intended to be the output of \code{\link{mod_database_server}}
#' @param si.name character; name of season information table.
#'   Default is 'season_info'
#'
#' @returns
#' \code{mod_season_server} returns a list with one component:
#' a reactive of the season information data frame
#'
#' @export
mod_season_info_server <- function(id, pool, si.name = "season_info") {
  stopifnot(is.reactive(pool))

  moduleServer(
    id,
    function(input, output, session) {
      # Make download button a UI
      output$tbl_download_uiOut <- renderUI({
        req(season_info_out())
        downloadButton(session$ns("tbl_download"), "Download table as CSV")
      })


      # Get data from table
      season_info <- reactive({
        df.out <- tbl(req(pool()), si.name) %>%
          select(-ts) %>%
          arrange(desc(season_open_date)) %>%
          collect()

        df.out
      })

      # Season info display table
      season_info_out <- reactive({
        season_info() %>%
          mutate(across(where(is.Date), as.character)) %>%
          select(`Season name` = season_name,
                 `Opening date` = season_open_date,
                 `Closing date` = season_close_date,
                 `Season days` = season_days,
                 `Diet study start date` = diet_scat_date,
                 `Date of median pupping` = date_median_pupping)
      })

      output$tbl_season <- renderTable(season_info_out())

      # Download table
      output$tbl_download <- downloadHandler(
        filename = function() "season_info_table.csv",
        content = function(file) {
          write.csv(season_info_out(), file = file, row.names = FALSE, na = "")
        }
      )

      ### Return values
      list(
        season.df = season_info
        # season.list = reactive(as.list(season_info()$season_name))
      )
    }
  )
}
