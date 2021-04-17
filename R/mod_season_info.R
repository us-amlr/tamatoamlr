#' Shiny module for season information
#'
#' Shiny module for season information
#'
#' @name mod_season_info
#'
#' @param id character used to specify namespace, see \code{\link[shiny]{NS}}
#'
#' @export
mod_season_info_ui <- function(id) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    box(
      title = "Season information", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
      tableOutput(ns("tbl_season")),
      downloadButton(ns("tbl_download"), "Download table as CSV")
    )
  )
}


#' @name mod_season_info
#'
#' @param pool reactive; a DBI database connection pool, intended to be the output of \code{\link{mod_database_server}}
#' @param si.name character; name of season information table. Default is 'season_info'
#'
#' @returns
#' \code{mod_season_server} returns a list of two reactives:
#' 1) \code{season.df}, the season information data frame and
#' 2) \code{season.id.list}, a list of the ID values from the season information table,
#' with the 'season_name' values as names
#'
#' @export
mod_season_info_server <- function(id, pool, si.name = "season_info") {
  stopifnot(is.reactive(pool))

  moduleServer(
    id,
    function(input, output, session) {
      # Get data from table
      season_info <- reactive({
        tbl(req(pool()), si.name) %>%
          select(-ts) %>%
          arrange(desc(season_open_date)) %>%
          collect() %>%
          mutate(season_open_date = as.Date(season_open_date),
                 season_close_date = as.Date(season_close_date),
                 diet_scat_date = as.Date(diet_scat_date))
      })

      season_info_out <- reactive({
        season_info() %>%
          mutate(across(where(is.Date), as.character)) %>%
          select(`Season name` = season_name,
                 `Opening date` = season_open_date,
                 `Closing date` = season_close_date,
                 `Season days` = season_days,
                 `Diet study start date` = diet_scat_date)
      })

      # Season info display table
      output$tbl_season <- renderTable(season_info_out())

      # Download table
      output$tbl_download <- downloadHandler(
        filename = function() {
          "season_info_table.csv"
        },
        content = function(file) {
          write.csv(season_info_out(), file = file, row.names = FALSE, na = "")
        }
      )

      ### Return values
      list(
        season.df = season_info,
        season.id.list = reactive(set_names(as.list(season_info()$ID), season_info()$season_name))
      )
    }
  )
}


##############################################################################


##############################################################################
