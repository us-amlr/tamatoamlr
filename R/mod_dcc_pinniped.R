#' @name shiny_modules
#' @export
mod_dcc_pinniped_ui <- function(id) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    fluidRow(
      box(
        title = "Load Files", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
        fluidRow(
          column(
            width = 6,
            radioButtons(ns("tx_key_type"), tags$h5("Key source"),
                         choices = c("Database"), #, "File"),
                         selected = "Database"),
            conditionalPanel(
              condition = "input.tx_key_type == 'Database'", ns = ns,

              helpText("Tamatoa will pull the DCC key from the database"),
              downloadButton(ns("key_download"), "Download database key")
            )
          ),
          column(
            width = 6,
            fileInput(ns("dcc_files_cabo"),
                      tags$h5("Load raw DCC files from CABO station"),
                      multiple = TRUE, accept = .tamatoa.csv.accept),
            fileInput(ns("dcc_files_mad"),
                      tags$h5("Load raw DCC files from MAD station"),
                      multiple = TRUE, accept = .tamatoa.csv.accept)
          )
        )
      ),
      box(
        title = "Summary and Filter Options", status = "warning",
        solidHeader = FALSE, width = 6, collapsible = TRUE,
        fluidRow(
          column(
            width = 6,
            radioButtons(ns("summary_type"), tags$h5("Summary type"),
                         choices = c("Trips" = "trips",
                                     "Trip lengths" = "trip_lengths",
                                     "Pings, by individual" = "pings"),
                         selected = "trips"),
            tags$br(),
            conditionalPanel(
              condition = "input.summary_type == 'trips'", ns = ns,
              checkboxInput(ns("trips_completed"), "Show only maximum completed trips")
            ),
            conditionalPanel(
              condition = "input.summary_type == 'trip_lengths'", ns = ns,
              radioButtons(ns("trip_lengths_summary_type"),
                           tags$h5("Trip length summary type"),
                           choices = c("For each seal, average across trips" = "by_pinniped",
                                       "For each trip number, average across seals" = "by_trip",
                                       "Average across all seals and trips" = "all"),
                           selected = "by_pinniped"),
              numericInput(ns("trip_num_max"), tags$h5("Maximum trip number to include"),
                           value = 6, min = 1, step = 1)
            )
          ),
          column(
            width = 6,
            uiOutput(ns("season")),
            # uiOutput(ns("date_range")),
            numericInput(ns("trip_hours"), tags$h5("Trip time gap (hours)"),
                         value = 24, min = 1, step = 1),
            checkboxInput(ns("include_resights"), "Include resights as tx pings")
          )
        ),
        uiOutput(ns("tag_freq_code_uiOut_select"))
      ),
      mod_output_ui(ns("out"), tags$br(), uiOutput(ns("warning_na_records")))
    )
  )
}



#' @name shiny_modules
#' @export
mod_dcc_pinniped_server <- function(id, src, season.df, tab) {
  .mod_check(src, season.df, tab)

  # How to structure this to pull key (tx and other info) from db. Thoughts:
  #   Will need to make user select single season.
  #   When they do that, then the code pulls capture data and checks devices
  #   for all 'micro-vhf tx' devices that were still deployed at the start of the selected season.
  #   Then for those pinnipeds, pull in pinniped_season data to get pup_mortality_date.
  #   The end date used in the (individual) filter is pup_mortality_date or device recovered date


  moduleServer(
    id,
    function(input, output, session) {
      ##########################################################################
      # General
      vals <- reactiveValues(
        warning_na_records = NULL,
        tag_freq_code_single = NULL,
        tag_freq_code_multiple = NULL
      )

      observeEvent(input$tag_freq_code, {
        if (input$summary_type != "pings") {
          vals$tag_freq_code_multiple <- input$tag_freq_code
        } else {
          vals$tag_freq_code_single <- input$tag_freq_code
        }
      })

      ### Warning messages
      output$warning_na_records <- renderUI({
        span(req(vals$warning_na_records), style = "color:red;")
      })

      ### Season
      output$season <- renderUI({
        req(input$tx_key_type == "Database")
        selectInput(session$ns("season"), tags$h5("Season"),
                    choices = req(season.df())$season_name)
      })

      # ### Date range
      # output$date_range <- renderUI({
      #   req(input$tx_key_type == "Database")
      #
      #   dateRangeInput(session$ns("date_range"), tags$h5("Date range"),
      #                  start = max(req(season.df())$season_open_date))
      # })

      ### Tag-freq-code selection
      mutate_tag_freq_code <- function(x) {
        x %>% mutate(tag_freq_code = paste(tag, freq, code, sep = " | "))
      }

      output$tag_freq_code_uiOut_select <- renderUI({
        x <- dcc_df() %>%
          mutate_tag_freq_code()

        tag.freq.code.part <- x %>%
          distinct(tag_freq_code, parturition) %>%
          arrange(tag_freq_code)

        choices.pupped <- tag.freq.code.part %>%
          filter(parturition) %>%
          select(tag_freq_code) %>%
          unlist() %>% unname()

        choices.all <- tag.freq.code.part %>%
          select(tag_freq_code) %>%
          unlist() %>% unname()

        multiple <- input$summary_type != "pings"
        select.name <- if_else(
          multiple,
          "Select at least one 'tag | frequency | code'",
          "Select exactly one 'tag | frequency | code'",
        )
        selected <- isolate({
          if (multiple) {
            if (is.null(vals$tag_freq_code_multiple)) choices.pupped else vals$tag_freq_code_multiple
          } else {
            if (is.null(vals$tag_freq_code_single)) NULL else vals$tag_freq_code_single
          }
        })

        selectInput(session$ns("tag_freq_code"), tags$h5(select.name),
                    choices = choices.all, selected = selected,
                    multiple = multiple)
      })

      ##########################################################################
      # Load and do initial formatting of DCC data

      ### Shiny-specific validate function
      dcc_validate <- function(dcc.df) {
        dcc.columns <- c("Yr", "Day", "Hr", "Mn", "Fr", "Sig", "Code")

        validate(
          need(all(dcc.columns %in% names(dcc.df)),
               paste("The loaded DCC file(s) do not have all of the expected columns:",
                     paste(dcc.columns, collapse = ", ")))
        )

        dcc.df
      }

      ### DCC data - load CABO files
      dcc_files_cabo <- reactive({
        dcc_read_files(req(input$dcc_files_cabo$datapath), "CABO") %>%
          dcc_validate()
      })

      ### DCC data - load MAD files
      dcc_files_mad <- reactive({
        dcc_read_files(req(input$dcc_files_mad$datapath), "MAD") %>%
          dcc_validate()
      })

      ### Join files
      dcc_files <- reactive({
        bind_rows(dcc_files_cabo(), dcc_files_mad()) %>%
          dcc_format() %>%
          arrange(freq, code, datetime)
      })


      ##########################################################################
      # Collect relevant table from the database

      ### Pinniped season
      ps_df_collect <- reactive({
        req(src(), tab() == .id.list$dcc, input$season)
        ps.df.collect <- try(tbl_vPinniped_Season(src()), silent = TRUE)

        validate(
          need(ps.df.collect,
               "Unable to collect vPinniped_Season from the specified database")
        )

        ps.df.collect %>%
          filter(season_name == req(input$season)) %>%
          select(pinniped_id, parturition, pup_mortality, pup_mortality_date,
                 attendance_study) %>%
          collect()
      })

      ### Micro-VHF key, joined with pinniped season
      microvhf_key <- reactive({
        req(src(), tab() == .id.list$dcc)
        microvhf <- try(tbl_vMicroVHF_Deployed(src()), silent = TRUE)

        validate(
          need(microvhf,
               "Unable to collect vPinniped_Season from the specified database")
        )

        microvhf %>%
          left_join(ps_df_collect(), by = join_by(pinniped_id))
      })


      ##########################################################################
      # TODO: Incorporate resights as 'pings', if specified by user


      ##########################################################################
      # Join key and dcc data, and finish DCC data processing

      dcc_df <- reactive({
        microvhf_key(); dcc_files()

        inner_join(microvhf_key(), dcc_files(),
                   by = join_by(freq, code), relationship = "many-to-many") %>%
          mutate(end_date = min(recovery_date, pup_mortality_date, today(),
                                na.rm = TRUE)) %>%
          filter(between(datetime, deployment_date, end_date)) %>%
          # filter(parturition) %>%
          rename(tag = tag_primary) %>%
          dcc_trips(trip.hours = input$trip_hours) %>%
          # arrange(freq, code, tag, datetime) %>%
          # group_by(freq, code) %>%
          # mutate(datetime_prev = lag(datetime),
          #        time_diff_hr = round(as.numeric(
          #          difftime(datetime, datetime_prev, units = "hours")), 2),
          #        trip_num_completed = c(NA, cumsum(head(lead(time_diff_hr), -1) > input.trip.hours))) %>%
          # ungroup() %>%
          # select(tag, freq, code, sig, datetime,
          #        datetime_prev, time_diff_hr, trip_num_completed,
          #        everything()) %>%
          relocate(tag, .before = freq) %>%
          mutate(tag_freq_code = paste(tag, freq, code, sep = " | "))
      })


      ##########################################################################
      # Outputs

      #-------------------------------------------------------------------------
      ### Table
      tbl_output <- reactive({
        # if (input$summary_type == "trips") {
        #   if (input$trips_completed) dcc_df_trips_completed() else dcc_df_trips()
        # } else if (input$summary_type == "trip_lengths") {
        #   dcc_df_trip_lengths()
        # } else if (input$summary_type == "pings") {
        #   dcc_df_pings()
        # } else {
        #   validate("invalid summary_type - please contact the database manager")
        # }
        dcc_df()
      })


      #-------------------------------------------------------------------------
      # Output plot
      plot_output <- reactive({
        # if (input$summary_type == "trips") {
        #   dcc_plot_trips()
        # } else if (input$summary_type == "trip_lengths") {
        #   dcc_plot_trip_lengths()
        # } else if (input$summary_type == "pings") {
        #   dcc_plot_pings()
        # } else {
        #   validate("invalid summary_type - please contact the database manager")
        # }
        validate("plot in the works..")
      })


      #-------------------------------------------------------------------------
      ### Send off
      observe(mod_output_server("out", tbl_output, plot_output))


      ##########################################################################
      ### Download database key
      output$key_download <- downloadHandler(
        filename = function() paste0("database_key", ".csv"),
        content = function(file) write.csv(microvhf_key(), file)
      )
    }
  )
}
