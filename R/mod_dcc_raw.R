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
            radioButtons(ns("tx_key_type"), tags$h5("Key source"),
                         choices = c("Database", "File"),
                         selected = "Database"),
            conditionalPanel(
              condition = "input.tx_key_type == 'Database'", ns = ns,

              helpText("Tamatoa will now pull...science"),
              downloadButton(ns("tx_key_database_download"), "Download database key")
            ),
            conditionalPanel(
              condition = "input.tx_key_type == 'File'", ns = ns,
              fileInput(ns("tx_key"), tags$h5("Load female-transmitter key"),
                        accept = .tamatoa.csv.accept)
            )
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
        title = "Summary and Filter Options", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
        fluidRow(
          column(
            width = 6,
            radioButtons(ns("summary_type"), tags$h5("Summary type"),
                         choices = c("Trips" = "trips",
                                     "Trip lengths" = "trip_lengths",
                                     "Pings, by individual" = "pings"),
                         selected = "trips"),
            # column(
            #   width = 12,
            tags$br(),
            conditionalPanel(
              condition = "input.summary_type == 'trips'", ns = ns,
              checkboxInput(ns("trips_completed"), "Show only maximum completed trips")
            ),
            conditionalPanel(
              condition = "input.summary_type == 'trip_lengths'", ns = ns,
              radioButtons(ns("trip_lengths_summary_type"), tags$h5("Trip length summary type"),
                           choices = c("For each seal, average across trips" = "by_pinniped",
                                       "For each trip number, average across seals" = "by_trip",
                                       "Average across all seals and trips" = "all"),
                           selected = "by_pinniped"),
              numericInput(ns("trip_num_max"), tags$h5("Maximum trip number to include"),
                           value = 6, min = 1, step = 1)
            )
            # )
          ),
          column(
            width = 6,
            uiOutput(ns("season")),
            dateRangeInput(ns("dcc_date_range"), tags$h5("Date range"),
                           start = "2022-11-28"),
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
mod_dcc_raw_server <- function(id, pool, season.df) {
  stopifnot(
    is.reactive(pool),
    is.reactive(season.df)
  )

  # TODO
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
        warning_na_records = NULL
      )

      ### Warning messages
      output$warning_na_records <- renderUI({
        span(req(vals$warning_na_records), style = "color:red;")
      })

      ### TODO
      mutate_tag_freq_code <- function(x) {
        x %>% mutate(tag_freq_code = paste(tag, freq, code, sep = " | "))
      }

      output$tag_freq_code_uiOut_select <- renderUI({
        x <- dcc_df_join() %>%
          mutate_tag_freq_code()

        choices.attendance <- x %>%
          filter(attendance_study) %>%
          arrange(tag_freq_code) %>%
          unlist() %>% unname()

        choices.list <- x %>%
          distinct(tag_freq_code) %>%
          arrange(tag_freq_code) %>%
          unlist() %>% unname()

        multiple <- input$summary_type != "pings"
        selected <- if (multiple) choices.attendance else NULL
        select.name <- if_else(
          multiple,
          "Select at least one 'tag | frequency | code'",
          "Select one 'tag | frequency | code'",
        )

        selectInput(session$ns("tag_freq_code"), tags$h5(select.name),
                    choices = choices.list, selected = selected,
                    multiple = multiple)
      })

      ### Season dropdown
      output$season <- renderUI({
        req(input$tx_key_type == "Database")
        selectInput(session$ns("season"), tags$h5("Season"),
                    choices = req(season.df())$season_name)
      })


      ##########################################################################
      # Input data

      #-------------------------------------------------------------------------
      # Female-transmitter key...

      ### ...from database
      tx_key_database <- reactive({
        ### Read in info
        # Device info
        devices <- tbl(req(pool()), "vCaptures_Devices") %>%
          filter(device_type == "Transmitter - Micro-VHF") %>%
          mutate(code = as.numeric(str_sub(device_num, 3, 4))) %>%
          collect()

        devices.removed <- devices %>%
          filter(action == "Removed") %>%
          select(device_inventory_id, pinniped_id, capture_date_rm = capture_date,
                 capture_time_rm = capture_time)

        # Pinniped_season
        ps <- tbl(req(pool()), "pinniped_season") %>%
          select(pinniped_season_id, pinniped_id, season_info_id,
                 attendance_study, parturition, parturition_date,
                 pup_mortality, pup_mortality_date) %>%
          collect()

        # Season info
        si <- season.info.id <- tbl(req(pool()), "season_info") %>%
          filter(season_name == !!req(input$season)) %>%
          collect()

        season.info.id <- si$ID
        season.open.date <- si$season_open_date

        # Resights - number of resights by seal
        tr.summ <- tbl(req(pool()), "vTag_Resights_Season_Summary") %>%
          filter(season_info_id == season.info.id,
                 species == "Fur seal") %>%
          select(pinniped_id, sex, n_resights) %>%
          collect()

        ### Final processing, and return the female-tx key
        devices %>%
          filter(action == "Deployed") %>%
          left_join(devices.removed, by = c("pinniped_id", "device_inventory_id")) %>%
          mutate(season_info_id = season.info.id,
                 capture_datetime = lubridate::ymd_hms(paste(capture_date, capture_time))) %>%
          select(season_info_id, capture_season_name = season_name,
                 capture_date, capture_time, capture_datetime, location, location_group,
                 pinniped_id, species, tag,
                 device_type, device_num, freq = frequency, code, combined_number,
                 capture_date_rm, capture_time_rm) %>%
          left_join(ps, by = c("season_info_id", "pinniped_id")) %>%
          inner_join(tr.summ, by = "pinniped_id") %>%
          filter(is.na(capture_date_rm) | capture_date_rm > season.open.date)
      })


      ### ...from file
      tx_key_file <- reactive({
        tx.key <- read.csv(req(input$tx_key$datapath))
        tx.key.columns <- c("capture_date", "capture_datetime",
                            "pup_mortality_date")

        validate(
          need(all(tx.key.columns %in% names(tx.key)),
               paste("The key file does not have the required columns: ",
                     paste(tx.key.columns, collapse = ", ")))
        )

        tx.key %>%
          mutate(capture_date = mdy(capture_date),
                 capture_datetime = mdy_hms(capture_datetime),
                 pup_mortality_date = mdy(pup_mortality_date))
      })

      ### Pick key and check for columns
      tx_key_df <- reactive({
        tx.key.df <- if (input$tx_key_type == "Database") {
          tx_key_database()
        } else if (input$tx_key_type == "File") {
          tx_key_file()
        } else {
          validate("Invalid input$tx_key_type value - please report this issue")
        }

        tx.key.columns <- c("capture_date", "capture_time", "location",
                            "tag", "pinniped_id", "attendance_study",
                            "capture_datetime", "pup_mortality_date")
        validate(
          need(all(tx.key.columns %in% names(tx.key.df)),
               paste("The female-transmitter key file does not",
                     "have all of the expected columns: ",
                     paste(tx.key.columns, collapse = ", ")))
        )

        tx.key.df
      })

      ### Download database key
      output$tx_key_database_download <- downloadHandler(
        filename = function() paste0("d atabase_key", ".csv"),
        content = function(file) write.csv(tx_key_database(), file)
      )

      #-------------------------------------------------------------------------
      # DCC data


      ### Read in data from DCC file
      dcc_read_file <- function(x, y) {
        bind_rows(lapply(x, function(i) {
          read.csv(i, skip = 6) %>%
            mutate(station = y)
        }))
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
        dcc_read_file(req(input$dcc_files_cabo$datapath), "CABO") %>%
          dcc_validate()
      })

      ### DCC data - load MAD files
      dcc_files_mad_load <- reactive({
        dcc_read_file(req(input$dcc_files_mad$datapath), "MAD") %>%
          dcc_validate()
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

      ### Join DCC data with with tx key, after filtering for user-provided date range
      dcc_df_join <- reactive({
        dcc_raw_df() %>%
          filter(between(as.Date(datetime),
                         input$dcc_date_range[1], input$dcc_date_range[2])) %>%
          inner_join(tx_key_df(), by = c("freq", "code")) %>%
          filter(datetime >= capture_datetime)
      })

      ### Add resight data to dcc data, if specified
      dcc_resight_df <- reactive({
        dcc <- dcc_df_join()

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
            mutate(datetime = ymd_hms(paste(resight_date, resight_time))) %>%
            left_join(dcc.tmp, by = "pinniped_id") %>%
            filter(datetime >= min_dt, datetime <= max_dt) %>%
            select(!!names(dcc))


          dcc <- bind_rows(dcc, tr.ping.resights)
        }

        dcc
      })


      ### Calculate trips
      dcc_calc_trips_df <- reactive({
        dcc_resight_df() %>%
          arrange(freq, code, datetime) %>%
          arrange(tag, datetime) %>%
          group_by(freq, code) %>%
          mutate(datetime_prev = lag(datetime),
                 time_diff_hr = round(as.numeric(
                   difftime(datetime, datetime_prev, units = "hours")), 2),
                 trip_num_completed = c(NA, cumsum(head(lead(time_diff_hr), -1) > input$trip_hours))) %>%
          ungroup()
      })


      ### Filter for selected tag/tx
      dcc_df <- reactive({
        validate(
          need(input$tag_freq_code,
               "Please select a 'tag | freq | code'")
        )

        dcc_calc_trips_df() %>%
          mutate_tag_freq_code() %>%
          filter(tag_freq_code %in% input$tag_freq_code) %>%
          select(-tag_freq_code)
      })



      ##########################################################################
      # Outputs

      #-------------------------------------------------------------------------
      #-------------------------------------------------------------------------
      ### Customized output for trips
      dcc_df_trips <- reactive({
        dcc.trips.out <- dcc_df() %>%
          filter(time_diff_hr > input$trip_hours,
                 is.na(pup_mortality_date) |
                   as.Date(datetime_prev) < pup_mortality_date) %>%
          select(tag, freq, code,
                 trip_num = trip_num_completed, trip_length_hr = time_diff_hr,
                 departure_dt = datetime_prev, arrival_dt = datetime,
                 capture_location = location, pinniped_id)

        validate(
          need(nrow(dcc.trips.out) > 0,
               "There are no trips for the selected filters")
        )

        dcc.trips.out
      })

      ### Number of completed trips for seals
      dcc_df_trips_completed <- reactive({
        dcc_df_trips() %>%
          group_by(freq, code) %>%
          filter(trip_num == max(trip_num)) %>%
          ungroup() %>%
          select(tag, freq, code, trip_num_completed_max = trip_num,
                 capture_location)
      })

      ### Mean trip lengths
      dcc_df_trip_lengths <- reactive({
        dcc.trips <- dcc_df_trips() %>%
          filter(trip_num <= input$trip_num_max)

        summarise_trip_lengths <- function(.data, ...) {
          .data %>%
            group_by(...) %>%
            summarise(n_trips = n(),
                      # {{n_records_name}} := n(),
                      trip_length_hr_mean = round(mean(trip_length_hr), 2),
                      trip_length_day_mean = round(trip_length_hr_mean/24, 2),
                      .groups = "drop")
        }

        if (input$trip_lengths_summary_type == "by_pinniped") {
          dcc.trips %>% summarise_trip_lengths(tag, freq, code)
        } else if (input$trip_lengths_summary_type == "by_trip") {
          dcc.trips %>% summarise_trip_lengths(trip_num)
        } else if (input$trip_lengths_summary_type == "all") {
          dcc.trips %>% summarise_trip_lengths()
        } else {
          .validate_else("trip_lengths_summary_type")
        }
      })

      ### Customized output for pings
      dcc_df_pings <- reactive({
        dcc <- dcc_df()
        req(n_distinct(paste(dcc$tag, dcc$code)) == 1)

        pings.out <- dcc_df() %>%
          mutate(date = as.Date(datetime),
                 time = format(datetime, "%H:%M:%S"),
                 pup_alive = is.na(pup_mortality_date) | date < pup_mortality_date) %>%
          select(-datetime_prev) %>%
          select(tag, freq, code, sig, station, datetime, date, time,
                 time_diff_hr, trip_num_completed, pup_alive, everything())

        validate(
          need(nrow(pings.out) > 0,
               "There are no pings for the selected individual")
        )

        pings.out
      })

      #-------------------------------------------------------------------------
      ### Output table
      tbl_output <- reactive({
        if (input$summary_type == "trips") {
          if (input$trips_completed) dcc_df_trips_completed() else dcc_df_trips()
        } else if (input$summary_type == "trip_lengths") {
          dcc_df_trip_lengths()
        } else if (input$summary_type == "pings") {
          dcc_df_pings()
        } else {
          validate("invalid summary_type - please contact the database manager")
        }
      })


      #-------------------------------------------------------------------------
      #-------------------------------------------------------------------------
      # Plot outputs

      ### Plot for trips summary type
      dcc_plot_trips <- reactive({
        dcc.trip.max <- max(dcc_df_trips()$trip_num)
        dcc.triplen.max <- max(dcc_df_trips()$trip_length_hr)

        # Make plot depending on trips_completed checkbox
        if (input$trips_completed) {
          dcc_df_trips_completed() %>%
            mutate_tag_freq_code() %>%
            ggplot(aes(tag_freq_code, trip_num_completed_max)) +
            geom_col() +
            scale_y_continuous(breaks = seq(0, dcc.trip.max, by = 1),
                               minor_breaks = NULL) +
            theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1)) +
            ggtitle(paste("Number of completed trips:",
                          paste(format(input$dcc_date_range, "%d %b %Y"),
                                collapse = " to "))) +
            xlab("Tag | Frequency | Code") +
            ylab(NULL)


        } else {
          ggplot(dcc_df_trips(), aes(trip_num, trip_length_hr)) +
            geom_point(aes(color = tag), size = 3) +
            geom_line(aes(group = tag, color = tag)) +
            guides(color = guide_legend(title = "Tag")) +
            scale_x_continuous(breaks = seq_len(dcc.trip.max),
                               minor_breaks = NULL) +
            scale_y_continuous(breaks = seq(0, dcc.triplen.max, by = 20),
                               minor_breaks = seq(0, dcc.triplen.max, by = 10)) +
            expand_limits(y = 0) +
            ggtitle(paste("AFS female trips:",
                          paste(format(input$dcc_date_range, "%d %b %Y"),
                                collapse = " to "))) +
            xlab("Trip number") +
            ylab("Trip length (hours)")
        }
      })


      ### Plot for trip length summary type
      dcc_plot_trip_lengths <- reactive({
        # Helper plot function
        dcc_trip_length_plot <- function(df, x, y, fill, xlab) {
          ggplot.out <-  df %>%
            ggplot(aes({{x}}, {{y}}, fill = {{fill}})) +
            geom_col() +
            scale_y_continuous(breaks = seq(0,  max(df$trip_length_hr_mean),
                                            by = 20),
                               minor_breaks = NULL) +
            ggtitle(paste("Average trip length through up to",
                          input$trip_num_max, "trips:",
                          paste(format(input$dcc_date_range, "%d %b %Y"),
                                collapse = " to "))) +
            xlab(xlab) +
            ylab("Trip length (hours)")

          if (xlab == "Tag | Frequency | Code") {
            ggplot.out +
              theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
          } else {
            ggplot.out
          }
        }

        # Make plot depending on trip_lengths_summary_type
        if (input$trip_lengths_summary_type == "by_pinniped") {
          dcc_trip_length_plot(
            dcc_df_trip_lengths() %>% mutate_tag_freq_code(),
            tag_freq_code, trip_length_hr_mean, n_trips,
            "Tag | Frequency | Code"
          )
        } else if (input$trip_lengths_summary_type == "by_trip") {
          dcc_trip_length_plot(
            dcc_df_trip_lengths(), trip_num, trip_length_hr_mean, n_trips,
            "Trip number"
          )
        } else if (input$trip_lengths_summary_type == "all") {
          validate("No plot for all trips trip length summary")
        } else {
          .validate_else("trip_lengths_summary_type")
        }
      })


      ### Plot for pings summary type
      dcc_plot_pings <- reactive({
        dcc_df_pings() %>%
          filter(time_diff_hr < input$trip_hours) %>%
          ggplot(aes(datetime)) +
          geom_point(aes(y = time_diff_hr, size = sig, color = pup_alive)) +
          geom_line(aes(y = time_diff_hr, color = pup_alive)) +
          guides(color = guide_legend(title = "Pup alive?"),
                 size = guide_legend(title = "Signal")) +
          scale_x_datetime(date_breaks = "1 day", date_minor_breaks = "6 hours",
                           date_labels = "%d %b %Y") +
          theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1)) +
          ggtitle(paste0(dcc_df_pings()$tag[1],
                         ": time gap and signal strength")) +
          xlab("Datetime") +
          ylab("Time gap (hours)")
      })


      #-------------------------------------------------------------------------
      ### Output plot
      plot_output <- reactive({
        if (input$summary_type == "trips") {
          dcc_plot_trips()
        } else if (input$summary_type == "trip_lengths") {
          dcc_plot_trip_lengths()
        } else if (input$summary_type == "pings") {
          dcc_plot_pings()
        } else {
          validate("invalid summary_type - please contact the database manager")
        }
      })


      #-------------------------------------------------------------------------
      ### Send off
      observe(mod_output_server("out", session, tbl_output, plot_output))
    }
  )
}
