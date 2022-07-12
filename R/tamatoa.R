#' Open Tamatoa, the amlrPinnipeds Shiny app
#'
#' Open Tamatoa, the amlrPinnipeds Shiny app
#'
#' @param ... passed to \code{\link[shiny]{shinyApp}}
#' @param remote.prod logical; default is TRUE. Should tamatoa create an
#'   automatic connection to the AMLR_PINNIPEDS database on estrella?
#' @param remote.test logical; default is TRUE. Should tamatoa create an
#'   automatic connection to the AMLR_PINNIPEDS_Test database on estrella?
#' @param local.prod logical; default is FALSE Should tamatoa create an
#'   automatic connection to the AMLR_PINNIPEDS database on
#'   the local SQL Express (see Details for more info)?
#'
#' @details
#' If \code{local.prod} is TRUE, then Tamatoa will attempt to connect to the
#' AMLR_PINNIPEDS database on the following server:
#' \code{\link[base]{paste0}(\link[base]{Sys.info}()[["nodename"]], "\\SQLEXPRESS")}
#'
#'
#' @examples
#' if (interactive()) tamatoa()
#'
#' @seealso \url{https://www.fisheries.noaa.gov/about/antarctic-ecosystem-research-division-southwest-fisheries-science-center}
#'
#' @export
tamatoa <- function(...,
                    remote.prod = TRUE, remote.test = TRUE,
                    local.prod = FALSE) {
  ##############################################################################
  ##### Set connections to dbs, as specified by the user
  db.driver <- "ODBC Driver 18 for SQL Server"

  db.server.remote <- "swc-estrella-s"
  db.server.local <- paste0(Sys.info()[["nodename"]], "\\SQLEXPRESS")

  db.name.prod <- "AMLR_PINNIPEDS"
  db.name.test <- "AMLR_PINNIPEDS_Test"

  pool.remote.prod <- if (remote.prod) {
    message("Attempting to connect to ", db.name.prod, " on ", db.server.remote)
    amlr_dbPool(db.name.prod, db.driver, db.server.remote)
  } else {
    NULL
  }

  if (remote.prod && remote.test && !isTruthy(pool.remote.prod))
    warning("Could not connect to ", db.name.prod, " on ", db.server.remote,
            ", and thus not attempting to connect to ", db.name.test)
  pool.remote.test <- if (remote.test && (!remote.prod || isTruthy(pool.remote.prod))) {
    message("Attempting to connect to ", db.name.test, " on ", db.server.remote)
    amlr_dbPool(db.name.test, db.driver, db.server.remote)
  } else {
    NULL
  }

  pool.local.prod <- if (local.prod) {
    message("Attempting to connect to ", db.name.prod, " on ", db.server.local)
    amlr_dbPool(db.name.prod, db.driver, db.server.local)
  } else {
    NULL
  }

  onStop(function() {
    if (isTruthy(pool.remote.prod))
      if (dbIsValid(pool.remote.prod)) poolClose(pool.remote.prod)
    if (isTruthy(pool.remote.test))
      if (dbIsValid(pool.remote.test)) poolClose(pool.remote.test)
  })


  ##############################################################################
  ##### Assorted other stuff...

  # old <- options()
  # on.exit(options(old), add = TRUE)

  # options(shiny.maxRequestSize = 50 * 1024^2) # Max file size is 50MB
  # options("digits" = 1)   # for proper display of decimals



  ##############################################################################
  ##### UI
  ui <- dashboardPage(
    title = "Tamatoa",
    dashboardHeader(title = "Tamatoa: Analyze and Visualize US AMLR Pinniped Data",
                    titleWidth = "540"),

    dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem("Database and Season Info", tabName = "tab_info", icon = icon("th")),
        # menuItem("AFS Diet", tabName = "tab_afs_diet", icon = icon("th", lib = "font-awesome")),
        # menuItem("AFS Natality and Pup Mortality", tabName = "tab_afs_pinniped_season", icon = icon("th")),
        # menuItem("AFS Capewide Pup Census", tabName = "tab_afs_capewide_pup_census", icon = icon("th", lib = "font-awesome")),
        # menuItem("AFS Study Beach Census", tabName = "tab_afs_study_beach_census", icon = icon("th", lib = "font-awesome")),
        menuItem("Phocid Census", tabName = "tab_phocid_census", icon = icon("th")),
        # menuItem("Tag Resights", tabName = "tab_tr", icon = icon("th", lib = "font-awesome")),
        # menuItem("Pinnipeds + Tags", tabName = "tab_pt", icon = icon("th", lib = "font-awesome")),
        menuItem("Captures Data", tabName = "tab_captures_summaries", icon = icon("th")),
        tags$br(), tags$br(),
        column(12, uiOutput("tabs_warning")),
        actionButton("stop", "Close")
      ), width = "230"
    ),

    dashboardBody(
      useShinyjs(),
      # https://stackoverflow.com/questions/35306295
      extendShinyjs(
        text = "shinyjs.closeWindow = function() { window.close(); }",
        functions = c("closeWindow")
      ),

      add_busy_spinner(
        spin = "double-bounce", position = "top-right", margins = c(20, 20),
        height = "100px", width = "100px"
      ),


      # https://stackoverflow.com/questions/59760316
      tags$head(tags$style(HTML("
        .shiny-output-error-validation {
        color: red; font-weight: bold;
        }
      "))),

      tabItems(
        tabItem(
          "tab_info",
          fluidRow(mod_database_ui("db"), mod_season_info_ui("si"))
        ),
        # tabItem("tab_afs_diet", mod_afs_diet_ui("afs_diet")),
        # tabItem("tab_afs_pinniped_season", mod_afs_pinniped_season_ui("afs_pinniped_season")),
        # tabItem("tab_afs_capewide_pup_census", mod_afs_capewide_census_ui("afs_capewide_pup_census")),
        # tabItem("tab_afs_study_beach_census", mod_afs_study_beach_census_ui("afs_study_beach_census")),
        tabItem("tab_phocid_census", mod_phocid_census_ui("phocid_census")),
        # tabItem("tab_tr", mod_tag_resights_ui("tag_resights")),
        # tabItem("tab_pt", mod_pinnipeds_tags_ui("pinnipeds_tags"))
        tabItem("tab_captures_summaries", captures_ui("captures"))
      )
    )
  )


  ##############################################################################
  ##### server
  server <- function(input, output, session) {
    #---------------------------------------------------------------------------
    ### Quit GUI
    session$onSessionEnded(function() {
      # Close current pool object. Needed here in case working off 'other' db
      isolate({
        if (inherits(db.pool(), "Pool")) {
          if (dbIsValid(db.pool())) {
            poolClose(db.pool())
          }
        }
      })
      stopApp(returnValue = "Tamatoa was closed")
    })

    observeEvent(input$stop, {
      stopApp(returnValue = "Tamatoa was closed")
      js$closeWindow()
    })

    #---------------------------------------------------------------------------
    ### Modules
    pool.list <- purrr::compact(list(
      `AMLR_PINNIPEDS - estrella` = if (remote.prod) pool.remote.prod else NULL,
      `AMLR_PINNIPEDS_Test - estrella` = if (remote.test) pool.remote.test else NULL,
      `AMLR_PINNIPEDS - SQLExpress` = if (local.prod) pool.local.prod else NULL
    ))

    db.pool <- mod_database_server("db", pool.list, db.driver)

    si.list <- mod_season_info_server("si", db.pool)

    # mod_afs_diet_server("afs_diet", pool, si.list$season.df, si.list$season.id.list)
    # mod_afs_pinniped_season_server("afs_pinniped_season", pool, si.list$season.df, si.list$season.id.list)
    # mod_afs_capewide_pup_census_server("afs_capewide_pup_census", pool, si.list$season.df, si.list$season.id.list)
    # mod_afs_study_beach_census_server("afs_study_beach_census", db.pool, si.list$season.df)
    mod_phocid_census_server("phocid_census", db.pool, si.list$season.df)
    # mod_tag_resights_server("tag_resights", pool, si.list$season.df, si.list$season.id.list)
    # mod_pinnipeds_tags_server("pinnipeds_tags", db.pool)
    captures_server("captures", db.pool)
    #----------------------------------------------------------------------------
    output$tabs_warning <- renderUI({
      validate(
        need(inherits(db.pool(), "Pool"),
             "The Shiny app is not connected to a database")
      )

      df <- dbGetQuery(req(db.pool()), "SELECT DB_NAME() AS db_name")

      if (grepl("Test", df$db_name)) {
        tags$h5(
          tags$span(
            "Warning: connected to the", tags$br(), "Test database",
            style = "color: red;"
          ),
          tags$br()
        )
      } else {
        NULL
      }
    })
  }


  ##############################################################################
  ##### Start it up
  shiny::shinyApp(ui = ui, server = server, ...)
}
