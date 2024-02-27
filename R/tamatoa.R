#' Open Tamatoa, the package Shiny app
#'
#' Open Tamatoa, the package Shiny app
#'
#' @param ... passed to [shiny::shinyApp()]
#' @param remote.prod logical; default is TRUE. Should Tamatoa create an
#'   automatic connection to the ***REMOVED*** database on ***REMOVED***?
#' @param remote.test logical; default is TRUE. Should Tamatoa create an
#'   automatic connection to the ***REMOVED***_Test database on ***REMOVED***?
#' @param local.prod logical; default is FALSE Should Tamatoa create an
#'   automatic connection to the ***REMOVED*** database on the local SQL
#'   Express? (see Details for more info)
#' @param local.server.sql character; SQL server suffix. Default is
#'   `"\\SQLEXPRESS"`
#'
#' @details If `local.prod == TRUE`, then Tamatoa will attempt to connect to the
#'   ***REMOVED*** database on the following server:
#'   \code{\link[base]{paste0}(\link[base]{Sys.info}()[["nodename"]],
#'   local.server.sql)}
#'
#' @examplesIf interactive()
#'   tamatoa()
#'
#'   # Testing
#'   tamatoa(remote.prod = FALSE, remote.test = TRUE)
#'
#'   # In the field
#'   tamatoa(remote.prod = FALSE, remote.test = FALSE, local.prod = TRUE)
#'
#' @seealso
#'   \url{https://www.fisheries.noaa.gov/about/antarctic-ecosystem-research-division-southwest-fisheries-science-center}
#'
#' @export
tamatoa <- function(...,
                    remote.prod = TRUE, remote.test = TRUE,
                    local.prod = FALSE, local.server.sql = "\\SQLEXPRESS") {
  ##############################################################################
  ##### Set connections to dbs, as specified by the user
  db.driver <- "ODBC Driver 18 for SQL Server"

  db.server.remote <- "swc-***REMOVED***-s"
  db.server.local <- paste0(Sys.info()[["nodename"]], local.server.sql)

  db.name.prod <- "***REMOVED***"
  db.name.test <- "***REMOVED***_Test"

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
    dashboardHeader(title = "Tamatoa: Analyze and Visualize U.S. AMLR Pinniped Data",
                    titleWidth = "540"),

    dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem("Database and Season Info", tabName = .id.list$info, icon = icon("th")),
        # menuItem("AFS Diet", tabName = "tab_afs_diet", icon = icon("th", lib = "font-awesome")),
        # menuItem("AFS Natality and Pup Mortality", tabName = "tab_afs_pinniped_season", icon = icon("th")),
        menuItem("AFS DCC", tabName = .id.list$dcc, icon = icon("th")),
        menuItem("AFS Capewide Pup Census", tabName = .id.list$afs_cwpc, icon = icon("th", lib = "font-awesome")),
        menuItem("AFS Study Beach Census", tabName = .id.list$afs_sbc, icon = icon("th", lib = "font-awesome")),
        # menuItem("Pinnipeds + Tags", tabName = "tab_pt", icon = icon("th", lib = "font-awesome")),
        # menuItem("Captures", tabName = .id.list$captures, icon = icon("th")),
        menuItem("CCAMLR Pup Weights", tabName = .id.list$cpw, icon = icon("th")),
        menuItem("CS-PHOC: Phocid Census", tabName = .id.list$csphoc, icon = icon("th")),
        menuItem("Tag Resights", tabName = .id.list$resights, icon = icon("th", lib = "font-awesome")),
        menuItem("Views", tabName = .id.list$views, icon = icon("th", lib = "font-awesome")),
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
        tabItem(.id.list$info,
                fluidRow(mod_database_ui(.id.list$db), mod_season_info_ui(.id.list$si))),
        tabItem(.id.list$dcc, mod_dcc_pinniped_ui(.id.list$dcc)),
        # tabItem("tab_afs_diet", mod_afs_diet_ui("afs_diet")),
        # tabItem("tab_afs_pinniped_season", mod_afs_pinniped_season_ui("afs_pinniped_season")),
        tabItem(.id.list$afs_cwpc, mod_afs_capewide_pup_census_ui(.id.list$afs_cwpc)),
        tabItem(.id.list$afs_sbc, mod_afs_study_beach_census_ui(.id.list$afs_sbc)),
        tabItem(.id.list$captures, mod_captures_ui(.id.list$captures)),
        tabItem(.id.list$cpw, mod_ccamlr_pup_weights_ui(.id.list$cpw)),
        tabItem(.id.list$csphoc, mod_phocid_census_ui(.id.list$csphoc)),
        tabItem(.id.list$resights, mod_tag_resights_ui(.id.list$resights)),
        tabItem(.id.list$views, mod_views_ui(.id.list$views))
        # tabItem("tab_pt", mod_pinnipeds_tags_ui("pinnipeds_tags"))
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
      `***REMOVED*** - ***REMOVED***` = if (remote.prod) pool.remote.prod else NULL,
      `***REMOVED***_Test - ***REMOVED***` = if (remote.test) pool.remote.test else NULL,
      `***REMOVED*** - SQLExpress` = if (local.prod) pool.local.prod else NULL
    ))

    db.pool <- mod_database_server(.id.list$db, pool.list, db.driver)
    si.list <- mod_season_info_server(.id.list$si, db.pool)
    tab <- reactive(input$tabs)

    # mod_afs_diet_server("afs_diet", pool, si.list$season.df)
    # mod_afs_pinniped_season_server("afs_pinniped_season", pool, si.list$season.df)
    mod_dcc_pinniped_server(
      .id.list$dcc, db.pool, si.list$season.df, tab)
    mod_afs_capewide_pup_census_server(
      .id.list$afs_cwpc, db.pool, si.list$season.df, tab)
    mod_afs_study_beach_census_server(
      .id.list$afs_sbc, db.pool, si.list$season.df, tab)
    # mod_captures_server(
    #   .id.list$captures, db.pool, si.list$season.df, tab)
    mod_ccamlr_pup_weights_server(
      .id.list$cpw, db.pool, si.list$season.df, tab)
    mod_phocid_census_server(
      .id.list$csphoc, db.pool, si.list$season.df, tab)
    mod_tag_resights_server(
      .id.list$resights, db.pool, si.list$season.df, tab)
    mod_views_server(
      .id.list$views, db.pool, si.list$season.df, tab)
    # mod_pinnipeds_tags_server("pinnipeds_tags", db.pool)
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
