# Modules used across the app

##############################################################################
### Module for
db_mod_ui <- function(id) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    fluidRow(
      box(
        title = "Database connection information", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
        tableOutput(ns("pool_db_conn")),
        tags$br(),
        selectInput(ns("db_name"), tags$h5("Database..."), width = "200px",
                    choices = list("***REMOVED***" = "remote_prod",
                                   "***REMOVED***_Test" = "remote_test",
                                   "Local database" = "local"),
                    selected = "remote_prod")
      ),
      box(
        title = "Season information", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
        tableOutput(ns("info_season_info"))
      )
    )
  )
}

db_mod_server <- function(id, pool.remote.prod, pool.remote.test, db.driver, db.server, si.name = "season_info") {
  moduleServer(
    id,
    function(input, output, session) {
      # Make reactiveValues for tables, in case whole table has been brought into memory?
      vals.db <- reactiveValues(
        pool = NULL,
        db.name = NULL
      )

      vals.si <- reactiveValues(
        df = NULL,
        season.list = list(),
        season.id.min = NULL,
        season.id.max = NULL
      )

      #----------------------------------------------------------------------------
      # Which database to use?
      observeEvent(input$info_db_name, {
        if (input$info_db_name == "remote_prod") {
          vals.db$pool <- pool.remote.prod
          vals.db$db.name <- db.name.prod

        } else if (input$info_db_name == "remote_test") {
          vals.db$pool <- pool.remote.test
          vals.db$db.name <- db.name.test

        } else if (input$info_db_name == "local") {
          vals.db$pool <- NULL
          vals.db$db.name <- NULL

        } else {
          vals.db$pool <- NULL
          vals.db$db.name <- NULL
        }

        vals.si$df <- tbl(vals.db$pool, si.name) %>%
          select(-ts) %>%
          arrange(desc(season_open_date)) %>%
          collect() %>%
          mutate(season_open_date = as.Date(season_open_date),
                 season_close_date = as.Date(season_close_date),
                 diet_scat_date = as.Date(diet_scat_date))

        # # Used in displays/filters
        # vals.si$season.list <- set_names(as.list(vals.si$df$ID), vals.si$df$season_name)
        # vals.si$season.id.min <- min(unlist(vals.si$season.list))
        # vals.si$season.id.max <- max(unlist(vals.si$season.list))
      })


      # Info about db connection
      output$pool_db_conn <- renderTable({
        validate(
          need(inherits(vals.db$pool, "Pool"),
               paste("The amlrPinnipeds Shiny app was not able to connect to the specified database -",
                     "are you connected to VPN?"))
        )

        validate(
          need(input$db_name != "local",
               "Cannot connect to a local database at this time; please select another option")
        )

        data.frame( #dbGetInfo(pool) is not useful atm
          Label = c("Driver", "Server", "Database name"),
          Value = c(db.driver, db.server, vals.db$db.name)
        )
      })

      # Season info display table
      output$info_season_info <- renderTable({
        req(vals.si$df) %>%
          mutate(across(where(is.Date), as.character)) %>%
          select(`Season name` = season_name,
                 `Opening date` = season_open_date,
                 `Closing date` = season_close_date,
                 `Season days` = season_days,
                 `Diet study start date` = diet_scat_date)
      })
    }
  )
}


##############################################################################


##############################################################################
