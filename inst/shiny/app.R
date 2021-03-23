# app.R for CruzPlot

###############################################################################
# Check for and attach packages
list.packages <- list(
  "DT", "shiny", "shinydashboard", "shinyjs",
  "DBI", "pool",
  "dbplyr", "dplyr", "ggplot2", "lubridate", "purrr", "stringr", "tidyr"
)

if (!require(amlrPinnipeds))
  stop("Error attaching amlrPinnipeds package - please reinstall amlrPinnipeds package")
if (!all(sapply(list.packages, require, character.only = TRUE)))
  stop("Error attaching packages - please reinstall amlrPinnipeds package")



###############################################################################
### Set up db connection, with error checking
# Based on https://github.com/rstudio/pool
db.driver <- "SQL Server"
db.server <- "swc-estrella-s"
db.name <- "AMLR_PINNIPEDS_Test"

pool <- try(dbPool(
  drv = odbc::odbc(),
  Driver = db.driver,
  Server = db.server,
  Database = db.name,
  Trusted_Connection = "True",
  idleTimeout = 3600000  # 1 hour
), silent = TRUE)


# Check for connection to db, then get some broadly used season_info data
if (!isTruthy(pool)) {
  stop("The Shiny app was unable to connect to the ", db.name, " database on the ",
       db.server, " server via a trusted connection - are you logged in to VPN?")

} else {
  season.info <- tbl(pool, "season_info") %>%
    select(-ts) %>%
    arrange(desc(season_open_date)) %>%
    collect()

  # Used in displays/filters
  season.list <- set_names(as.list(season.info$ID), season.info$season_name)
  season.list.id.min <- min(unlist(season.list))
  season.list.id.max <- max(unlist(season.list))
}

onStop(function() {
  poolClose(pool)
})




# ###############################################################################
# ##### Assorted other stuff...
# old <- options()
# on.exit(options(old))
#
# options(shiny.maxRequestSize = 50 * 1024^2) # Max file size is 50MB
# options("digits" = 5)   # for proper display of sighting and effort coordinates

jscode <- "shinyjs.closeWindow = function() { window.close(); }"



###############################################################################
##### UI
ui.new.line <- function() helpText(HTML("<br/>"))


# Load files with UI code
source(file.path("ui_function.R"), local = TRUE, chdir = TRUE)


# UI function
ui <- dashboardPage(
  dashboardHeader(title = "AMLR Pinnipeds Database", titleWidth = "220"),

  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("General info", tabName = "tab_info", icon = icon("th", lib = "font-awesome")),
      menuItem("AFS Natality and Pup Fate", tabName = "tab_afs_natal", icon = icon("th")),
      menuItem("Census", tabName = "tab_census", icon = icon("th", lib = "font-awesome")),
      menuItem("Tag resights", tabName = "tab_tr", icon = icon("th", lib = "font-awesome")),
      tags$br(), tags$br(), tags$br(),
      actionButton("stop", "Close Shiny app")
    ), width = "220"
  ),

  dashboardBody(
    useShinyjs(),
    # See https://stackoverflow.com/questions/35306295/how-to-stop-running-shiny-app-by-closing-the-browser-window
    extendShinyjs(text = jscode, functions = c("closeWindow")),

    # See https://stackoverflow.com/questions/59760316/change-the-color-of-text-in-validate-in-a-shiny-app
    tags$head( #validation text
      tags$style(HTML("
                      .shiny-output-error-validation {
                      color: red; font-weight: bold;
                      }
                      "))
    ),

    # ui.createMap()
    ui.tabs()
  )
)


###############################################################################
##### server
server <- function(input, output, session) {
  #----------------------------------------------------------------------------
  ### Quit GUI
  session$onSessionEnded(function() {
    stopApp(returnValue = "amlrPinnipeds Shiny app was closed")
  })

  observeEvent(input$stop, {
    stopApp(returnValue = "amlrPinnipeds Shiny app was closed")
    js$closeWindow()
  })


  #----------------------------------------------------------------------------
  ### Reactive values
  # tbl_season_info <- reactive({
  #   tbl(pool, "season_info") %>%
  #     select(-ts) %>%
  #     arrange(desc(season_open_date)) %>%
  #     collect()
  # })
  #
  # # Reactive val for list of season names and IDs in the db - used in displays/filters
  # val.season.list <- reactiveVal(value = list())
  # observe(val.season.list <- set_names(as.list(tbl_season_info()$ID), tbl_season_info()$season_name))

  # # Reactive values of tables, in case whole table has been brought into memory
  # vals <- reactiveValues(
  #   season.list = list()
  # )


  #----------------------------------------------------------------------------
  ### General info tab

  # Info about db connection
  output$pool_db_conn <- renderTable({
    validate(
      need(dbIsValid(pool),
           paste("The amlrPinnipeds Shiny app was not able to connect to the database -",
                 "are you connected to VPN?"))
    )
    # pool.info <- dbGetInfo(pool) #None of this is useful atm

    data.frame(
      Label = c("Driver", "Server", "Database name"),
      Value = c(db.driver, db.server, db.name)
    )
  })

  # Season info display table
  output$info_season_info <- renderTable({
    season.info %>%
      select(season_name, season_open_date, season_close_date, season_days, diet_scat_date) %>%
      set_names(c("Season name", "Opening date", "Closing date", "Season days", "Diet study start date"))
    # tbl(pool, "season_info") %>%
    #   select(season_name, season_open_date, season_close_date, season_days, diet_scat_date) %>%
    #   arrange(desc(season_open_date)) %>%
    #   collect() %>%
    #   set_names(c("Season name", "Opening date", "Closing date", "Season days", "Diet study start date"))
  })


  #----------------------------------------------------------------------------
  ### Server files
  source(file.path("server_files", "server_afs_natality_pup_fate.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_files", "server_census.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_files", "server_tag_resights.R"), local = TRUE, chdir = TRUE)


  #----------------------------------------------------------------------------
  ### Other



}

shiny::shinyApp(ui = ui, server = server)
