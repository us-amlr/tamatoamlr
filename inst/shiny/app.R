# app.R for amlrPinnipeds

###############################################################################
# Check for and attach packages
list.packages <- list(
  "amlrDatabases", "DBI", "pool",
  "DT", "shiny", "shinybusy", "shinydashboard", "shinyjs",
  "dbplyr", "dplyr", "glue", "ggplot2", "lubridate", "purrr", "stringr", "tidyr"
)

if (!require(amlrPinnipeds))
  stop("Error attaching amlrPinnipeds package - please reinstall amlrPinnipeds package")
if (!all(sapply(list.packages, require, character.only = TRUE, warn.conflicts = FALSE)))
  stop("Error attaching packages - please reinstall amlrPinnipeds package")



###############################################################################
### Set up db connections, with error checking
db.driver <- "SQL Server"
db.server.remote <- "swc-estrella-s"
db.server.local <- paste0(Sys.info()[["nodename"]], "\\SQLEXPRESS")

db.name.prod <- "AMLR_PINNIPEDS"
db.name.test <- "AMLR_PINNIPEDS_Test"

# pool.remote.prod <- amlr_dbPool(db.name.prod, db.driver, db.server.remote)
pool.remote.prod <- NULL
pool.local.prod <- amlr_dbPool(db.name.prod, db.driver, db.server.local)

# TODO: make these nicer i.e. via NULLS + validates
#   Really, this should all happen in mod_database_server,
#   with NULLs being returned if it can't connect.
#   That way everything would be self-contained
#   HOWEVER, this then violates the dbPool call being outside of the server function.?

remote.prod.valid <- isTruthy(pool.remote.prod)
local.prod.valid  <- isTruthy(pool.local.prod)

if (remote.prod.valid) {
  pool.remote.test <- amlr_dbPool(db.name.test, db.driver, db.server.remote)
  remote.prod.valid <- DBI::dbIsValid(pool.remote.prod)
} else {
  pool.remote.test <- NULL
}

if (local.prod.valid) {
  pool.local.test <- amlr_dbPool(db.name.test, db.driver, db.server.local)
  local.prod.valid <- DBI::dbIsValid(pool.local.prod)
} else {
  pool.local.test <- NULL
}

# db_stop_txt <- function(x, y) {
#   paste0(
#     "The Shiny app was unable to connect to the ", x, " database on the ",
#     y, " server via a trusted connection - are you logged in to VPN? ",
#     "Please close the app, log into VPN, and then open the app again"
#   )
# }

# # Test connection to the production db
# if (!(isTruthy(pool.remote.prod) | isTruthy(pool.local.prod))) {
#   stop(db_stop_txt(db.name.prod, db.server.server))
# } else if (!(DBI::dbIsValid(pool.remote.prod) | DBI::dbIsValid(pool.local.prod))) {
#   stop(db_stop_txt(db.name.prod, db.server.server))
#
#
# } else {
#   # If there is a valid connection to the prod db, connect to the test db as well.
#   pool.remote.test <- amlr_dbPool(db.name.test, db.driver, db.server.server)
#
#   # Check for connection to Test db
#   if (!isTruthy(pool.remote.test)) {
#     stop(db_stop_txt(db.name.test, db.server.server))
#   } else if (!DBI::dbIsValid(pool.remote.test)) {
#     stop(db_stop_txt(db.name.test, db.server.server))
#   }
# }


onStop(function() {
  if (isTruthy(pool.remote.prod)) poolClose(pool.remote.prod)
  if (isTruthy(pool.remote.test)) poolClose(pool.remote.test)
  if (isTruthy(pool.local.prod)) poolClose(pool.local.prod)
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
      tabItem("tab_info", fluidRow(mod_database_ui("db", db.name.prod, db.name.test, remote.prod.valid),
                                   mod_season_info_ui("si"))),
      tabItem("tab_afs_diet", mod_afs_diet_ui("afs_diet")),
      tabItem("tab_afs_pinniped_season", mod_afs_pinniped_season_ui("afs_pinniped_season")),
      tabItem("tab_census", mod_census_ui("census")),
      tabItem("tab_tr", mod_tag_resights_ui("tag_resights")),
      tabItem("tab_pt", mod_pinnipeds_tags_ui("pinnipeds_tags"))
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
    validate(
      need(inherits(pool(), "Pool"),
           "The Shiny app is not connected to a database")
    )

    df <- dbGetQuery(req(pool()), "SELECT DB_NAME() AS db_name")
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
  pool <- mod_database_server(
    "db", db.name.prod, db.name.test,
    pool.remote.prod, pool.remote.test, pool.local.prod, pool.local.test,
    db.driver, db.server.remote, db.server.local
  )
  # pool <- mod_database_server(
  #   "db", db.name.prod, db.name.test, pool.remote.prod, pool.remote.test, db.driver, db.server
  # )

  si.list <- mod_season_info_server("si", pool)

    mod_afs_diet_server("afs_diet", pool, si.list$season.df, si.list$season.id.list)
    mod_afs_pinniped_season_server("afs_pinniped_season", pool, si.list$season.df, si.list$season.id.list)
    mod_census_server("census", pool, si.list$season.df, si.list$season.id.list)
    mod_tag_resights_server("tag_resights", pool, si.list$season.df, si.list$season.id.list)
    mod_pinnipeds_tags_server("pinnipeds_tags", pool)
}

shiny::shinyApp(ui = ui, server = server)
