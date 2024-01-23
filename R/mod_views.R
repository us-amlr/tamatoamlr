#' @name shiny_modules
#' @export
mod_views_ui <- function(id) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    fluidRow(
      box(
        title = "Filters", status = "warning", solidHeader = FALSE,
        width = 6, collapsible = TRUE,
        # mod_filter_season_ui(ns("filter_season"))
        fluidRow(
          column(6, uiOutput(ns("season")))
        )
      ),
      box(
        title = "Summary options", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
        helpText("This tab allows you to view and download certain Pinniped views"),
        fluidRow(
          column(
            width = 6,
            selectInput(ns("view"), tags$h5("View to display"),
                        choices = c("Sample inventory" = "sample_inventory",
                                    "Attendance pup weights" = "apw"),
                        selected = "sample_inventory")
          ),
          column(
            width = 6,
            conditionalPanel(
              condition = "input.view == 'sample_inventory'", ns = ns,
              radioButtons(ns("summary_type"), tags$h5("Summarize by"),
                           choices = c("Sample type" = "sample_type",
                                       "All samples" = "all"),
                           selected = "all")
            )
          )
          # column(4, .summaryTimingUI(ns, c("fs_single"))),
          # column(4, .summaryLocationUI(ns, c("by_capewide", "by_beach"), "by_capewide")),
          # column(4, .summarySpAgeSexUI(ns, c("by_sp", "by_sp_age_sex"), "by_sp"))
        )
      )
    ),
    mod_output_ui(ns("out"))
  )
}



#' @name shiny_modules
#' @export
mod_views_server <- function(id, src, season.df, tab) {
  .mod_check(src, season.df, tab)

  moduleServer(
    id,
    function(input, output, session) {
      ##########################################################################
      # General

      ### Season
      output$season <- renderUI({
        selectInput(session$ns("season"), tags$h5("Season"),
                    choices = req(season.df())$season_name)
      })


      # ### Get filter_season values
      # filter_season <- reactive({
      #   mod_filter_season_server(
      #     "filter_season",  reactive(input$summary_timing), season.df
      #   )
      # })


      #-------------------------------------------------------------------------
      # Sample inventory view

      sample_inventory_collect <- reactive({
        sample.inventory <- try(
          tbl(src(), "vSample_Inventory") %>% collect(),
          silent = TRUE
        )
        validate(
          need(sample.inventory,
               "Unable to find and load vSample_Inventory from specified database")
        )
        sample.inventory
      })

      sample_inventory <- reactive({
        if (input$summary_type == "all") {
          sample_inventory_collect()

        } else if (input$summary_type == "sample_type") {
          sample_inventory_collect() %>%
            filter(season_name == req(input$season)) %>%
            group_by(species, sample_type) %>%
            summarise(package_count = n())
          # individul_seals = n_distinct(paste(pinniped_id, unk_group_id)))

        } else {
          validate("invalid summary_type - please contact the database manager")
        }
      })


      #-------------------------------------------------------------------------
      # Attendance pup weights

      apw_collect <- reactive({
        apw <- try(
          tbl(src(), "vAttendance_Pup_Weights") %>% collect(),
          silent = TRUE
        )
        validate(
          need(apw,
               "Unable to find and load vAttendance_Pup_Weights from specified database")
        )
        apw
      })


      apw <- reactive({
        apw_collect() %>% filter(season_name == req(input$season))
      })

      #-------------------------------------------------------------------------
      tbl_output <- reactive({
        tbl.df <- if (input$view == "sample_inventory") {
          sample_inventory()
        } else if (input$view == "apw") {
          apw()
        } else {
          validate("invalid view - please contact the database manager")
        }
      })

      plot_output <- reactive({
        validate("There are no plots for the views")
      })


      #-------------------------------------------------------------------------
      ### Send off
      observe(mod_output_server("out", tbl_output, plot_output))
    }
  )
}
