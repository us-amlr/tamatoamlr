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
                         choices = c("Database", "File"),
                         selected = "Database"),
            conditionalPanel(
              condition = "input.tx_key_type == 'Database'", ns = ns,

              helpText("Tamatoa will pull the DCC key from the database"),
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
mod_dcc_pinniped_server <- function(id, src, season.df) {
  stopifnot(
    is.reactive(src),
    is.reactive(season.df)
  )

  # How to structure this to pull key (tx and other info) from db. Thoughts:
  #   Will need to make user select single season.
  #   When they do that, then the code pulls capture data and checks devices
  #   for all 'micro-vhf tx' devices that were still deployed at the start of the selected season.
  #   Then for those pinnipeds, pull in pinniped_season data to get pup_mortality_date.
  #   The end date used in the (individual) filter is pup_mortality_date or device recovered date


  moduleServer(
    id,
    function(input, output, session) {

    }
  )
}
