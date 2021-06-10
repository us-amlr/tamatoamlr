#' Database module for AMLR shiny apps
#'
#' Database module for AMLR shiny apps
#'
#' @name mod_database
#'
#' @param id character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#' @param name.prod character; name of production database on SWFSC server
#' @param name.test character; name of test database on SWFSC server
#' @param col.width integer; column width of column of UI widgets
#'
#' @export
mod_database_ui <- function(id, name.prod, name.test, col.width = 5) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    box(
      title = "Database connection information", status = "warning",
      solidHeader = FALSE, width = col.width, collapsible = TRUE,
      tableOutput(ns("pool_db_conn")),
      tags$br(),
      selectInput(ns("db_name"), tags$h5("Database..."), width = "200px",
                  choices = list(name.prod, name.test, "Local database" = "local"),
                  selected = name.prod)
    )
  )
}

#' @name mod_database
#'
#' @param pool.remote.prod output of a \code{\link[pool]{dbPool}} call.
#'   A DBI database connection pool connected to the remote PRODUCTION database, e.g. '***REMOVED***'
#' @param pool.remote.test output of a \code{\link[pool]{dbPool}} call.
#'   A DBI database connection pool connected to the remote TEST database, e.g. '***REMOVED***_Test'
#' @param db.driver character; name of driver used to connect to remote database
#' @param db.server character; name of server where remote database is hosted
#'
#' @returns \code{mod_database_server} returns a reactive of the pool connection specified by the user
#'
#' @export
mod_database_server <- function(id, name.prod, name.test, pool.remote.prod, pool.remote.test,
                                db.driver, db.server) {
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
        if (input$db_name == name.prod) {
          vals.db$pool <- pool.remote.prod
          vals.db$db.name <- input$db_name
          vals.db$system.user <- pool::dbGetQuery(vals.db$pool, "SELECT SYSTEM_USER")

        } else if (input$db_name == name.test) {
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
               paste("The Shiny app was not able to connect to the specified database -",
                     "are you connected to VPN?"))
        )

        data.frame(
          Label = c("Driver", "Server", "Username", "Database name"),
          Value = unlist(c(db.driver, db.server, vals.db$system.user, vals.db$db.name))
        )
      })

      ### Return values
      return(reactive(vals.db$pool))
    }
  )
}
