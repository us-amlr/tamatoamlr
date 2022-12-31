#' @name shiny_modules
#' @export
mod_dcc_raw_ui <- function(id) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    fluidRow(
      box(
        title = "Load Files", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
        fluidRow(
          column(
            width = 6,
            fileInput(ns("tx_key"), tags$h5("Load female-transmitter key"),
                      accept = .tamatoa.csv.accept),
            dateRangeInput(ns("dcc_date_range"), tags$h5("Date range"),
                           start = "2022-11-28")
          ),
          column(
            width = 6,
            fileInput(ns("dcc_files_cabo"), tags$h5("Load raw DCC files from CABO station"),
                      multiple = TRUE, accept = .tamatoa.csv.accept),
            fileInput(ns("dcc_files_mad"), tags$h5("Load raw DCC files from MAD station"),
                      multiple = TRUE, accept = .tamatoa.csv.accept)
          )
        )
      ),
      box(
        title = "Summary options", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
        fluidRow(
          column(
            width = 6,
            selectInput(ns("summary_type"), tags$h5("Summary type"),
                        choices = c("Trips" = "trips",
                                    "Pings, by individual" = "pings"),
                        selected = "trips"),
            uiOutput(ns("individual_tag_tx_uiOut_select"))
          ),
          column(
            width = 6,
            numericInput(ns("trip_hours"), tags$h5("Trip time gap (hours)"),
                         value = 8),
            checkboxInput(ns("include_resights"), "Include resights as tx pings - TODO")
          )
        )
      ),
      mod_output_ui(ns("out"), tags$br(), uiOutput(ns("warning_na_records")))
    )
  )
}



#' @name shiny_modules
#' @export
mod_dcc_raw_server <- function(id, pool, season.df) {
  stopifnot(
    is.reactive(pool),
    is.reactive(season.df)
  )

  moduleServer(
    id,
    function(input, output, session) {
      ##########################################################################
      # General
      vals <- reactiveValues(
        warning_na_records = NULL
      )

      output$individual_tag_tx_uiOut_select <- renderUI({
        req(input$summary_type == 'pings')

        choices.list <- dcc_df_join() %>%
          mutate(tag_device = paste(tag, device_num, sep = " | ")) %>%
          distinct(tag_device) %>%
          arrange(tag_device) %>%
          unlist() %>% unname()

        selectInput(session$ns("individual_tag_tx"),
                    tags$h5("Select individual 'tag | tx'"),
                    choices = choices.list)
      })


      ##########################################################################
      # Input data

      ### Female-transmitter key
      tx_key_df <- reactive({
        tx.key <- read.csv(req(input$tx_key$datapath))
        tx.key.columns <- c("location", "tag", "pinniped_id")

        validate(
          need(all(tx.key.columns %in% names(tx.key)),
               paste("The female-transmitter key file does not have all of the expected columns: ",
                     paste(tx.key.columns, collapse = ", ")))
        )

        tx.key
      })

      ### DCC data - general functions
      dcc_read_file <- function(x, y) {
        lapply(x, function(i) {
          read.csv(i, skip = 6) %>%
            mutate(station = y)
        })
      }

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
      dcc_files_cabo_load <- reactive({
        dcc.cabo <- bind_rows(dcc_read_file(req(input$dcc_files_cabo$datapath), "CABO"))
        dcc_validate(dcc.cabo)
      })

      ### DCC data - load MAD files
      dcc_files_mad_load <- reactive({
        dcc.mad <- bind_rows(dcc_read_file(req(input$dcc_files_mad$datapath), "MAD"))
        dcc_validate(dcc.mad)
      })


      ##########################################################################
      # Process

      ### Combine DCC data, and process
      dcc_raw_df <- reactive({
        bind_rows(dcc_files_cabo_load(), dcc_files_mad_load()) %>%
          mutate(
            datetime = as.POSIXct(strptime(paste(paste0("20", Yr), Day, Hr, Mn,
                                                 sep = "-"),
                                           format = "%Y-%j-%H-%M")),
            freq = as.numeric(case_when(
              str_length(Fr) == 4 ~ paste0("164.", str_sub(Fr, 2, 4)),
              str_length(Fr) == 6 ~ paste0("164.", str_sub(Fr, 4, 6)),
              TRUE ~ NA_character_
            ))) %>%
          select(freq, code = Code, sig = Sig, datetime, station) %>%
          arrange(freq, code, datetime)
      })

      ### Join DCC data with with tx key
      dcc_df_join <- reactive({
        dcc_raw_df() %>%
          inner_join(tx_key_df(), by = c("freq", "code"))
      })

      ### Filter DCC data based on capture and provided dates, and calculate trips
      dcc_df <- reactive({
        dcc <- dcc_df_join() %>%
          filter(datetime > capture_datetime,
                 between(as.Date(datetime),
                         input$dcc_date_range[1], input$dcc_date_range[2]))

        if (input$include_resights) {
          tr.key.join <- tbl(pool(), "vTag_Resights") %>%
            filter(pinniped_id %in% !!dcc$pinniped_id,
                   between(resight_date, !!input$dcc_date_range[1],
                           !!input$dcc_date_range[2])) %>%
            collect() %>%
            inner_join(select(tx_key_df(), -tag), by = c("pinniped_id")) %>%
            mutate(sig = NA_integer_,
                   station = "RESIGHT")

          if (any(is.na(tr.key.join$resight_time))) {
            vals$warning_na_records <- paste(
              "There were", sum(is.na(tr.key.join$resight_time)),
              "resights with a NULL resight_time.",
              "These times will be assumed to be 12:00 (noon)"
            )
            tr.key.join <- tr.key.join %>%
              mutate(resight_time = replace_na(resight_time, "12:00:00"))
          } else {
            vals$warning_na_records <- NULL
          }

          dcc.tmp <- dcc %>%
            group_by(pinniped_id) %>%
            summarise(min_dt = min(datetime),
                      max_dt = max(datetime))

          tr.ping.resights <- tr.key.join %>%
            mutate(datetime = lubridate::ymd_hms(paste(resight_date, resight_time))) %>%
            left_join(dcc.tmp, by = "pinniped_id") %>%
            filter(datetime >= min_dt, datetime <= max_dt) %>%
            select(!!names(dcc))


          dcc <- bind_rows(dcc, tr.ping.resights)
        }

        dcc %>%
          arrange(freq, code, datetime) %>%
          arrange(tag, datetime) %>%
          group_by(freq, code) %>%
          mutate(datetime_prev = lag(datetime),
                 time_diff_hr = round(as.numeric(
                   difftime(datetime, datetime_prev, units = "hours")), 2),
                 trip_num = c(NA, cumsum(head(lead(time_diff_hr), -1) > input$trip_hours))) %>%
          ungroup()
      })



      ##########################################################################
      # Outputs

      ### Customized output for trips
      dcc_df_trip <- reactive({
        dcc_df() %>%
          filter(time_diff_hr > input$trip_hours) %>%
          select(tag, freq, code, trip_num, trip_length_hr = time_diff_hr,
                 departure_dt = datetime_prev, arrival_dt = datetime,
                 capture_location = location)
      })

      ### Customized output for pings
      dcc_df_pings <- reactive({
        tag.tx.split <- strsplit(req(input$individual_tag_tx), " | ")[[1]]

        dcc_df() %>%
          filter(tag == tag.tx.split[1],
                 device_num == tag.tx.split[3]) %>%
          mutate(date = as.Date(datetime),
                 time = format(datetime, "%H:%M:%S")) %>%
          select(-datetime_prev) %>%
          select(tag, freq, code, sig, station,
                 datetime, date, time, time_diff_hr, trip_num, everything())
      })

      #-------------------------------------------------------------------------
      ### Output table
      tbl_output <- reactive({
        if (input$summary_type == "trips") {
          dcc_df_trip()
        } else if (input$summary_type == "pings") {
          dcc_df_pings()
        } else {
          validate("invalid summary_type - please contact the database manager")
        }
      })


      #-------------------------------------------------------------------------
      ### Output plot
      plot_output <- reactive({
        # census_df()
        # print("plot1")
        # print(input$summary_timing)
        # if (input$summary_timing == "fs_single") {
        #   validate("No plot for single season AFS Cape-wide Pup Census data")
        # }
        # # validate(
        # #   need(input$summary_timing != "fs_single",
        # #        "No plot for single season AFS Cape-wide Pup Census data")
        # # )
        # print("plot2")

        ggplot(data.frame(x = 1:2, y = 1:2), aes(x, y)) +
          geom_point() +
          ggtitle("Ignore this plot - DCC")
      })#-------------------------------------------------------------------------
      ### Send off
      observe(mod_output_server("out", session, tbl_output, plot_output))
    }
  )
}
