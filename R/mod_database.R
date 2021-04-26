#' Database module for the amlrPinnipeds shiny app
#'
#' Database module for the amlrPinnipeds shiny app
#'
#' @name mod_database
#'
#' @param id character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#' @param col.width integer; column width of column of UI widgets
#'
#' @export
mod_database_ui <- function(id, col.width = 5) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    box(
      title = "Database connection information", status = "warning", solidHeader = FALSE, width = col.width, collapsible = TRUE,
      tableOutput(ns("pool_db_conn")),
      tags$br(),
      selectInput(ns("db_name"), tags$h5("Database..."), width = "200px",
                  choices = list("AMLR_PINNIPEDS" = "AMLR_PINNIPEDS",
                                 "AMLR_PINNIPEDS_Test" = "AMLR_PINNIPEDS_Test",
                                 "Local database" = "local"),
                  selected = "AMLR_PINNIPEDS")
    )
  )
}

#' @name mod_database
#'
#' @param pool.remote.prod output of a \code{\link[pool]{dbPool}} call.
#'   A DBI database connection pool connected to the remote PRODUCTION database, e.g. 'AMLR_PINNIPEDS'
#' @param pool.remote.test output of a \code{\link[pool]{dbPool}} call.
#'   A DBI database connection pool connected to the remote TEST database, e.g. 'AMLR_PINNIPEDS_Test'
#' @param db.driver character; name of driver used to connect to remote database
#' @param db.server character; name of server where remote database is hosted
#'
#' @returns \code{mod_database_server} returns a reactive of the pool connection specified by the user
#'
#' @export
mod_database_server <- function(id, pool.remote.prod, pool.remote.test, db.driver, db.server) {
  moduleServer(
    id,
    function(input, output, session) {
      vals.db <- reactiveValues(
        pool = NULL,
        db.name = "",
        system.user = ""
      )

      #----------------------------------------------------------------------------
      # Which database to use?
      observeEvent(input$db_name, {
        if (input$db_name == "AMLR_PINNIPEDS") {
          vals.db$pool <- pool.remote.prod
          vals.db$db.name <- input$db_name
          vals.db$system.user <- pool::dbGetQuery(vals.db$pool, "SELECT SYSTEM_USER")

        } else if (input$db_name == "AMLR_PINNIPEDS_Test") {
          vals.db$pool <- pool.remote.test
          vals.db$db.name <- input$db_name
          vals.db$system.user <- pool::dbGetQuery(vals.db$pool, "SELECT SYSTEM_USER")

        } else if (input$db_name == "local") {
          vals.db$pool <- NULL
          vals.db$db.name <- ""
          vals.db$system.user <- ""

        } else {
          vals.db$pool <- NULL
          vals.db$db.name <- ""
          vals.db$system.user <- ""
        }
      })


      # Info about db connection
      output$pool_db_conn <- renderTable({
        validate(
          need(input$db_name != "local",
               "Cannot connect to a local database at this time; please select another option")
        )

        validate(
          need(inherits(vals.db$pool, "Pool"),
               paste("The amlrPinnipeds Shiny app was not able to connect to the specified database -",
                     "are you connected to VPN?"))
        )

        data.frame(
          Label = c("Driver", "Server", "Username", "Database name"),
          Value = unlist(c(db.driver, db.server, vals.db$system.user, vals.db$db.name))
        )
      })

      ### Return values
      return(reactive(vals.db$pool))
      # return(reactive(reactiveValuesToList(vals.db)))
    }
  )
}
