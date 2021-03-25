# app.R for CruzPlot

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
### Set up db connection, with error checking
# TODO: pass these arguments in via the amlr_pinnipeds_gui function since they're outside of the session?
db.driver <- "SQL Server"
db.server <- "swc-***REMOVED***-s"
db.name <- "***REMOVED***_Test"

# Based on https://github.com/rstudio/pool
pool <- try(pool::dbPool(
  drv = odbc::odbc(),
  Driver = db.driver,
  Server = db.server,
  Database = db.name,
  Trusted_Connection = "True",
  idleTimeout = 3600000  # 1 hour
), silent = TRUE)


# Check for connection to db, then get/save broadly used data
if (!isTruthy(pool)) {
  # TODO: should this be displayed in some uiOutput.?
  stop("The Shiny app was unable to connect to the ", db.name, " database on the ",
       db.server, " server via a trusted connection - are you logged in to VPN?")

} else {
  season.info <- tbl(pool, "season_info") %>%
    select(-ts) %>%
    arrange(desc(season_open_date)) %>%
    collect() %>%
    mutate(season_open_date = as.Date(season_open_date),
           season_close_date = as.Date(season_close_date),
           diet_scat_date = as.Date(diet_scat_date))

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

pinniped.sp.list.all <- list(
  "Fur seal" = "fur seal",
  "Crabeater seal" = "crabeater seal",
  "Elephant seal" = "elephant seal",
  "Leopard seal" = "leopard seal",
  "Weddell seal" = "weddell seal"
)
pinniped.sp.levels <- names(pinniped.sp.list.all) #levels for factor

pinniped.sp.list.tr <- pinniped.sp.list.all[
  c("Fur seal", "Elephant seal", "Leopard seal", "Weddell seal")
]
pinniped.sp.list.phocid <- pinniped.sp.list.all[
  c("Crabeater seal", "Elephant seal", "Leopard seal", "Weddell seal")
]

# Colors for pinnipeds in plots
pinniped.sp.colors <- purrr::set_names(
  scales::hue_pal()(5), names(pinniped.sp.list.all)
)


###############################################################################
##### UI

# Load files with UI code
source(file.path("ui_tabs.R"), local = TRUE, chdir = TRUE)

# UI function
ui <- dashboardPage(
  dashboardHeader(title = "AMLR Pinnipeds Database Summaries", titleWidth = "400"),

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
      ui_tab_info(),
      ui_tab_afs_natal(),
      ui_tab_census(),
      ui_tab_tr()
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


  #----------------------------------------------------------------------------
  ### Reactive values

  # Make reactiveValues for tables, in case whole table has been brought into memory?

  # Catch-all reactiveValues
  vals <- reactiveValues(
    census.beaches = NULL,
    census.cols = NULL,
    census.warning.na.records = NULL
  )


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
      mutate(across(where(is.Date), as.character)) %>%
      select(`Season name` = season_name,
             `Opening date` = season_open_date,
             `Closing date` = season_close_date,
             `Season days` = season_days,
             `Diet study start date` = diet_scat_date)
  })


  #----------------------------------------------------------------------------
  ### Server files
  source(file.path("server_files", "server_afs_natality_pup_fate.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_files", "server_census.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_files", "server_tag_resights.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_files", "server_funcs.R"), local = TRUE, chdir = TRUE)


  #----------------------------------------------------------------------------
  ### Other



}

shiny::shinyApp(ui = ui, server = server)
