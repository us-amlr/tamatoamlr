# app.R for amlrPinnipeds

###############################################################################
##### Check for and attach packages
list.packages <- list(
  "amlrDatabases", "DBI", "pool",
  "DT", "shiny", "shinybusy", "shinydashboard", "shinyjs",
  "dbplyr", "dplyr", "glue", "ggplot2", "lubridate", "purrr", "stringr", "tidyr"
)

if (!require(amlrPinnipeds))
  stop("Error attaching amlrPinnipeds - please reinstall the amlrPinnipeds package")
if (!all(sapply(list.packages, require, character.only = TRUE, warn.conflicts = FALSE)))
  stop("Error attaching packages - please reinstall amlrPinnipeds package")



###############################################################################
##### Set connections to remote dbs
db.driver <- "ODBC Driver 18 for SQL Server"
db.server.remote <- "swc-***REMOVED***-s"

db.name.prod <- "***REMOVED***"
db.name.test <- "***REMOVED***_Test"

pool.remote.prod <- amlrDatabases::amlr_dbPool(db.name.prod, db.driver, db.server.remote)
remote.prod.valid <- isTruthy(pool.remote.prod)

if (remote.prod.valid) {
  pool.remote.test <- amlrDatabases::amlr_dbPool(db.name.test, db.driver, db.server.remote)
  remote.prod.valid <- DBI::dbIsValid(pool.remote.prod)
} else {
  pool.remote.test <- NULL
}


onStop(function() {
  if (isTruthy(pool.remote.prod))
    if (pool::dbIsValid(pool.remote.prod)) poolClose(pool.remote.prod)
  if (isTruthy(pool.remote.test))
    if (pool::dbIsValid(pool.remote.test)) poolClose(pool.remote.test)
})



###############################################################################
##### Assorted other stuff...

# old <- options()
# on.exit(options(old))
#
# options(shiny.maxRequestSize = 50 * 1024^2) # Max file size is 50MB
# options("digits" = 5)   # for proper display of decimals

jscode <- "shinyjs.closeWindow = function() { window.close(); }"



###############################################################################
##### UI

# Load files with UI code
# source(file.path("ui_tabs.R"), local = TRUE, chdir = TRUE)
# source(file.path("modules", "ui_modules.R"), local = TRUE, chdir = TRUE)

# UI function
ui <- dashboardPage(
  title = "Tamatoa",
  dashboardHeader(title = "Tamatoa: Summaries for the AMLR Pinniped Database",
                  titleWidth = "520"),

  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Database and Season Info", tabName = "tab_info", icon = icon("th", lib = "font-awesome")),
      menuItem("AFS Diet", tabName = "tab_afs_diet", icon = icon("th", lib = "font-awesome")),
      # menuItem("AFS Natality and Pup Mortality", tabName = "tab_afs_pinniped_season", icon = icon("th")),
      menuItem("AFS Capewide Pup Census", tabName = "tab_afs_capewide_pup_census", icon = icon("th", lib = "font-awesome")),
      menuItem("AFS Study Beach Census", tabName = "tab_afs_study_beach_census", icon = icon("th", lib = "font-awesome")),
      menuItem("Phocid Census", tabName = "tab_phocid_census", icon = icon("th", lib = "font-awesome")),
      menuItem("Tag Resights", tabName = "tab_tr", icon = icon("th", lib = "font-awesome")),
      menuItem("Pinnipeds + Tags", tabName = "tab_pt", icon = icon("th", lib = "font-awesome")),
      tags$br(), tags$br(),
      column(12, uiOutput("tabs_warning")),
      actionButton("stop", "Close Tamatoa")
    ), width = "230"
  ),

  dashboardBody(
    useShinyjs(),
    # https://stackoverflow.com/questions/35306295/how-to-stop-running-shiny-app-by-closing-the-browser-window
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
      tabItem(
        "tab_info",
        fluidRow(
          mod_database_ui("db", db.name.prod, db.name.test, remote.prod.valid,
                          db.remote.default = "remote_test"),
          mod_season_info_ui("si")
        )
      ),
      # tabItem("tab_afs_diet", mod_afs_diet_ui("afs_diet")),
      # tabItem("tab_afs_pinniped_season", mod_afs_pinniped_season_ui("afs_pinniped_season")),
      # tabItem("tab_afs_capewide_pup_census", mod_afs_capewide_census_ui("afs_capewide_pup_census")),
      # tabItem("tab_afs_study_beach_census", mod_afs_study_beach_census_ui("afs_study_beach_census")),
      tabItem("tab_phocid_census", mod_phocid_census_ui("phocid_census"))
      # tabItem("tab_tr", mod_tag_resights_ui("tag_resights")),
      # tabItem("tab_pt", mod_pinnipeds_tags_ui("pinnipeds_tags"))
    )
  )
)


###############################################################################
##### server
server <- function(input, output, session) {
  #----------------------------------------------------------------------------
  ### Quit GUI
  session$onSessionEnded(function() {
    # Close current pool object. Needed here in case working off 'other' db
    isolate({
      if (inherits(db_pool(), "Pool")) {
        if (pool::dbIsValid(db_pool())) {
          pool::poolClose(db_pool())
        }
      }
    })
    stopApp(returnValue = "Tamatoa was closed")
  })

  observeEvent(input$stop, {
    stopApp(returnValue = "Tamatoa was closed")
    js$closeWindow()
  })

  #----------------------------------------------------------------------------
  ### Modules
  db_pool <- mod_database_server(
    "db", pool.remote.prod, pool.remote.test, db.driver
  )

  si.list <- mod_season_info_server("si", db_pool)

  # mod_afs_diet_server("afs_diet", pool, si.list$season.df, si.list$season.id.list)
  # mod_afs_pinniped_season_server("afs_pinniped_season", pool, si.list$season.df, si.list$season.id.list)
  # mod_afs_capewide_pup_census_server("afs_capewide_pup_census", pool, si.list$season.df, si.list$season.id.list)
  # mod_afs_study_beach_census_server("afs_study_beach_census", db_pool, si.list$season.df)
  mod_phocid_census_server("phocid_census", db_pool, si.list$season.df)
  # # mod_tag_resights_server("tag_resights", pool, si.list$season.df, si.list$season.id.list)
  # mod_pinnipeds_tags_server("pinnipeds_tags", db_pool)

  #-----------------------------------------------------------------------------
  output$tabs_warning <- renderUI({
    validate(
      need(inherits(db_pool(), "Pool"),
           "The Shiny app is not connected to a database")
    )

    df <- dbGetQuery(req(db_pool()), "SELECT DB_NAME() AS db_name")
    if (df$db_name == "***REMOVED***_Test") {
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
}


###############################################################################
##### Start it up
shiny::shinyApp(ui = ui, server = server)
