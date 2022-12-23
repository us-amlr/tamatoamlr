#' @name shiny_modules
#' @export
mod_afs_study_beach_census_ui <- function(id) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    fluidRow(
      box(
        title = "Filters", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
        fluidRow(
          column(12, mod_filter_season_ui(ns("filter_season"))),
          # column(
          #   width = 3, offset = 1,
          #   checkboxGroupInput(ns("species"), label = tags$h5("Species"),
          #                      choices = amlrPinnipeds::pinniped.phocid.sp,
          #                      selected = amlrPinnipeds::pinniped.phocid.sp)
          # )
        ),

        # uiOutput(ns("week_num_uiOut_select")),
        uiOutput(ns("age_sex_uiOut_selectize")),
        uiOutput(ns("location_uiOut_selectize"))
      ),
      box(
        title = "Summary options", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
        helpText("This tab allows you to summarize and visualize AFS Study Beach census data. ",
                 "Select how you wish to summarize this data, ",
                 "and then specify any filters you would like to apply"),
        fluidRow(
          column(
            width = 4,
            .summaryTimingUI(ns, c("fs_total", "fs_date_single", "fs_single")),
            helpText("Cumulative sum for dead pups? In some years")
            # conditionalPanel(
            #   condition = "input.summary_timing == 'fs_single'", ns = ns,
            #   checkboxInput(ns("plot_cumsum"), "Plot cumulative sum", value = FALSE)
            # )
          ),
          column(4, .summaryLocationUI(ns, c("by_amlr", "by_capewide", "by_beach"), "by_amlr", FALSE)),
          column(4, .summarySpAgeSexUI(ns, c("by_sp_age_sex"), "by_sp_age_sex"))
        ),
        helpText("Note that locations (i.e., the 'location' column in the",
                 "table output) are always grouped", tags$br(),
                 "helptext todo")
      )
    ),
    mod_output_ui(ns("out"), tags$br(), uiOutput(ns("warning_na_records")))
  )
}



#' @name shiny_modules
#' @export
mod_afs_study_beach_census_server <- function(id, pool, season.df) {
  stopifnot(
    is.reactive(pool),
    is.reactive(season.df)
  )

  moduleServer(
    id,
    function(input, output, session) {
      ##########################################################################
      # General

      ### Get filter_season values
      filter_season <- reactive({
        mod_filter_season_server(
          "filter_season",  reactive(input$summary_timing), season.df
        )
      })

      # AFS Study Beach Census data is always filtered by location_group
      # ### Get location column
      # loc_column <- reactive({
      #   if_else(input$location_aggregate, "location_group", "location")
      # })


      ##########################################################################
      # Census-specific common values
      vals <- reactiveValues(
        warning_na_records = NULL
      )


      ##########################################################################
      # RenderUIs

      ### Warning messages
      output$warning_na_records <- renderUI({
        span(req(vals$warning_na_records), style = "color:red;")
      })

      ### Locations dropdown
      output$location_uiOut_selectize <- renderUI({
        req(input$summary_location == "by_beach")
        census.df <- census_df_filter_season()

        beaches.list <- if (input$location_aggregate){
          sort(unique(census.df$location_group))
        } else {
          sort(unique(census.df$location))
        }

        selectInput(
          session$ns("location"), tags$h5("Location(s)"),
          choices = beaches.list, multiple = TRUE
        )
      })

      ### Columns dropdown
      output$age_sex_uiOut_selectize <- renderUI({
        req(input$summary_sas == "by_sp_age_sex", pool())

        selectInput(
          session$ns("age_sex"), tags$h5("Columns to plot"),
          choices = c("pup_live_count", "pup_dead_count",
                      "ad_female_count", "ad_male_count_sum",
                      "juv_female_count", "juv_male_count", "juv_unk_count"),
          selected = c("pup_live_count", "pup_dead_count"),
          multiple = TRUE, selectize = TRUE
        )
      })


      ##########################################################################
      ##########################################################################
      ##########################################################################
      # Collect all census data - one time run, then all data is collected
      census_df_collect <- reactive({
        vals$warning_na_records <- NULL

        validate(
          need(try(tbl(req(pool()), "vCensus_AFS_Study_Beach"), silent = TRUE),
               "Unable to find vCensus_AFS_Study_Beach on specified database")
        )

        census.df.collect <- tbl_vCensus_AFS_Study_Beach(pool())

        #----------------------------------------------
        # Filter records for non-NA values, verbosely as appropriate
        census.df.nona <- census.df.collect %>%
          filter(!is.na(season_name), !is.na(location),
                 !is.na(census_date), !is.na(species))

        nrow.diff <- nrow(census.df.collect) - nrow(census.df.nona)
        vals$warning_na_records <- if (nrow.diff != 0) {
          paste(
            nrow.diff,
            ifelse(nrow.diff == 1, "row was", "rows were"),
            "removed because of a NULL season_name, species,",
            "location, and/or census_date value.",
            "Please tell the database manager."
          )
        } else {
          NULL
        }

        validate(
          need(nrow(census.df.nona) > 0,
               "No data to process after removing rows with NA values")
        )

        census.df.nona
      })



      ##########################################################################
      # Filter collected data

      #-------------------------------------------------------------------------
      ### Filter data by species, season/date, and remove NA values
      census_df_filter_season <- reactive({
        census.df.orig <- census_df_collect()
        #----------------------------------------------
        # Filter by season/date/week num
        fs <- filter_season()

        census.df <- if (input$summary_timing %in% .summary.timing.multiple) {
          census.df.orig %>%
            filter(season_name %in% !!req(fs$season()))
        } else if (input$summary_timing %in% .summary.timing.single) {
          census.df.orig %>%
            filter(season_name == !!req(fs$season()),
                   between(census_date,
                           !!req(fs$date_range())[1], !!req(fs$date_range())[2]))
        } else {
          validate("invalid input$summary_timing value")
        }

        # if (input$summary_timing == "fs_week") {
        #   census.df <- census.df %>%
        #     filter(week_num == as.numeric(!!req(fs$week())))
        # }

        validate(
          need(nrow(census.df) > 0,
               "There are no data for the given season filter(s)")
        )

        census.df
      })


      #-------------------------------------------------------------------------
      ### Filter data by location
      census_df_filter_location <- reactive({
        census.df <- census_df_filter_season()

        if (input$summary_location == "by_beach") {
          validate(need(input$location, "Please select at least one beach name"))
          census.df <- census.df %>% filter(location_group %in% input$location)
        } else if (input$summary_location == "by_amlr") {
          # TODO: AMLR study beach start dates, etc
          amlr.beaches <- c("Chungungo", "Cachorros", "Maderas", "Copi", "Hue",
                            "Copihue", "Modulo",  "Daniel", "Marko")
          # tbl(pool(), "beaches") %>%
          #   collect() %>%
          #   filter(!is.na(study_beach_season_start_id)) %>%
          #   select(name) %>%
          #   arrange(name) %>%
          #   unlist() %>% unname()
          census.df <- census.df %>% filter(location_group %in% amlr.beaches)
        }

        validate(
          need(nrow(census.df) > 0,
               "There are no data for the given location filter")
        )

        census.df
      })


      ##########################################################################
      # Process collected and filtered census data

      ### Process collected census data, part 1
      #-------------------------------------------------------------------------
      census_df <- reactive({
        grp.chr <- if (req(input$summary_location) == "by_beach") {
          c("season_name", "census_date", "species", "location")
        } else {
          c("season_name", "census_date", "species")
        }

        census_df_filter_location() %>%
          mutate(location = location_group) %>%
          select(!!grp.chr, !!!as.list(input$age_sex)) %>%
          group_by(!!!syms(grp.chr)) %>%
          summarise(across(where(is.numeric), sum, na.rm = TRUE),
                    .groups = "drop") %>%
          # complete(census_date) %>%
          arrange_season(season.df(), desc(census_date))
      })



      ##########################################################################
      # Outputs

      #-------------------------------------------------------------------------
      ### Output table
      tbl_output <- reactive({
        # df.out <- census_df() %>%
        #   mutate(species = str_to_sentence(species)) %>%
        #   nest(data_lc = where(is.numeric)) %>%
        #   mutate(flag0 = pmap_lgl(list(.data$data_lc), function(i) all(i == 0))) %>%
        #   filter(!.data$flag0) %>%
        #   unnest(cols = c(.data$data_lc)) %>%
        #   select(-.data$flag0) %>%
        #   arrange(if("census_date" %in% names(.)) census_date else season_name,
        #           species)
        #
        # if (input$summary_timing %in% .summary.timing.multiple) {
        #   census_df_filter_location() %>%
        #     group_by(season_name) %>%
        #     summarise(n_census_header = n_distinct(census_phocid_header_id)) %>%
        #     right_join(df.out, by = "season_name") %>%
        #     select(season_name, .data$n_census_header, everything())
        # } else {
        #   df.out %>% select(season_name, census_date, everything())
        # }
        census_df()
      })


      #-------------------------------------------------------------------------
      ### Output plot
      plot_output <- reactive({
        ggplot(data.frame(x = 1:10, y = 10:19), aes(x, y)) +
          geom_point()
      })

      #-------------------------------------------------------------------------
      ### Send off
      observe(mod_output_server("out", session, tbl_output, plot_output))
    }
  )
}
