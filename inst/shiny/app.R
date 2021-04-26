# app.R for amlrPinnipeds

###############################################################################
# Check for and attach packages
list.packages <- list(
  "DBI", "odbc", "pool",
  "DT", "shiny", "shinybusy", "shinydashboard", "shinyjs",
  "dbplyr", "dplyr", "ggplot2", "lubridate", "purrr", "stringr", "tidyr"
)

if (!require(amlrPinnipeds))
  stop("Error attaching amlrPinnipeds package - please reinstall amlrPinnipeds package")
if (!all(sapply(list.packages, require, character.only = TRUE, warn.conflicts = FALSE)))
  stop("Error attaching packages - please reinstall amlrPinnipeds package")



###############################################################################
### Set up db connections, with error checking
db.driver <- "SQL Server"
db.server <- "swc-estrella-s"
db.name.prod <- "AMLR_PINNIPEDS"
db.name.test <- "AMLR_PINNIPEDS_Test"

# Based on https://github.com/rstudio/pool
pool.remote.prod <- try(pool::dbPool(
  drv = odbc::odbc(),
  Driver = db.driver,
  Server = db.server,
  Database = db.name.prod,
  Trusted_Connection = "True",
  idleTimeout = 3600000  # 1 hour
), silent = TRUE)

# TODO: make these nicer i.e. via NULLS + validates
#   Really, this should all happen in mod_database_server, with NULLs being returned if it can't connect.
#   That way everything would be self-contained
#   HOWEVER, this then violates the dbPool call being outside of the server function.?

db_stop_txt <- function(x, y) {
  paste0(
    "The Shiny app was unable to connect to the ", x, " database on the ",
    y, " server via a trusted connection - are you logged in to VPN? ",
    "Please close the app, log into VPN, and then open the app again"
  )
}

# Test connection to the production db
if (!isTruthy(pool.remote.prod)) {
  stop(db_stop_txt(db.name.prod, db.server))
} else if (!dbIsValid(pool.remote.prod)) {
  stop(db_stop_txt(db.name.prod, db.server))


} else {
  # If there is a valid connection to the production database, connect to the test db as well.
  pool.remote.test <- try(pool::dbPool(
    drv = odbc::odbc(),
    Driver = db.driver,
    Server = db.server,
    Database = db.name.test,
    Trusted_Connection = "True",
    idleTimeout = 3600000  # 1 hour
  ), silent = TRUE)

  # Check for connection to Test db
  if (!isTruthy(pool.remote.test)) {
    stop(db_stop_txt(db.name.test, db.server))
  } else if (!dbIsValid(pool.remote.test)) {
    stop(db_stop_txt(db.name.test, db.server))
  }
}


onStop(function() {
  poolClose(pool.remote.prod)
  poolClose(pool.remote.test)
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

# Load files with UI code
# source(file.path("ui_tabs.R"), local = TRUE, chdir = TRUE)
# source(file.path("modules", "ui_modules.R"), local = TRUE, chdir = TRUE)

# UI function
ui <- dashboardPage(
  dashboardHeader(title = "AMLR Pinnipeds Database Summaries", titleWidth = "400"),

  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Database and season info", tabName = "tab_info", icon = icon("th", lib = "font-awesome")),
      menuItem("AFS Diet", tabName = "tab_afs_diet", icon = icon("th", lib = "font-awesome")),
      menuItem("AFS Natality and Pup Mortality", tabName = "tab_afs_pinniped_season", icon = icon("th")),
      menuItem("Census", tabName = "tab_census", icon = icon("th", lib = "font-awesome")),
      menuItem("Tag resights", tabName = "tab_tr", icon = icon("th", lib = "font-awesome")),
      menuItem("Pinnipeds + Tags", tabName = "tab_pt", icon = icon("th", lib = "font-awesome")),
      tags$br(), tags$br(),
      uiOutput("tabs_warning"),
      actionButton("stop", "Close Shiny app")
    ), width = "230"
  ),

  dashboardBody(
    useShinyjs(),
    # See https://stackoverflow.com/questions/35306295/how-to-stop-running-shiny-app-by-closing-the-browser-window
    extendShinyjs(text = jscode, functions = c("closeWindow")),

    # Use shinybusy to indicate when plot work is being done
    shinybusy::add_busy_spinner(
      spin = "double-bounce", position = "top-right", margin = c(20, 20),
      height = "100px", width = "100px"
    ),


    # See https://stackoverflow.com/questions/59760316/change-the-color-of-text-in-validate-in-a-shiny-app
    tags$head(tags$style(HTML("
      .shiny-output-error-validation {
      color: red; font-weight: bold;
      }
    "))),

    tabItems(
      tabItem(tabName = "tab_info", fluidRow(mod_database_ui("db"), mod_season_info_ui("si"))),
      tabItem(tabName = "tab_afs_diet", mod_afs_diet_ui("afs_diet")),
      tabItem(tabName = "tab_afs_pinniped_season", mod_afs_pinniped_season_ui("afs_pinniped_season")),
      tabItem(tabName = "tab_census", mod_census_ui("census")),
      tabItem(tabName = "tab_tr", mod_tag_resights_ui("tag_resights")),
      tabItem(tabName = "tab_pt", mod_pinnipeds_tags_ui("pinnipeds_tags"))
    )
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

  output$tabs_warning <- renderUI({
    req(pool())
    df <- dbGetQuery(pool(), "SELECT DB_NAME() AS db_name")
    if (df$db_name == "AMLR_PINNIPEDS_Test") {
      tags$h5(
        tags$span(
          "Warning: app is connected to the", tags$br(), "Test database",
          style = "color: red;"
        ),
        tags$br()
      )
    } else {
      NULL
    }
  })


  #----------------------------------------------------------------------------
  ### Modules
  pool <- mod_database_server("db", pool.remote.prod, pool.remote.test, db.driver, db.server)
  si.list <- mod_season_info_server("si", pool)

  mod_afs_diet_server("afs_diet", pool, si.list$season.df, si.list$season.id.list)
  mod_afs_pinniped_season_server("afs_pinniped_season", pool, si.list$season.df, si.list$season.id.list)
  mod_census_server("census", pool, si.list$season.df, si.list$season.id.list)
  mod_tag_resights_server("tag_resights", pool, si.list$season.df, si.list$season.id.list)
  mod_pinnipeds_tags_server("pinnipeds_tags", pool)
}

shiny::shinyApp(ui = ui, server = server)
