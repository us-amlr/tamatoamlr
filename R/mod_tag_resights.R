#' @name shiny_modules
#' @export
mod_tag_resights_ui <- function(id) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    fluidRow(
      box(
        title = "Filters", status = "warning", solidHeader = FALSE,
        width = 6, collapsible = TRUE,
        mod_filter_season_ui(ns("filter_season")),
        fluidRow(
          column(6, selectInput(ns("species"), tags$h5("Species"), #inline = TRUE,
                                choices = tamatoamlr::pinniped.sp.study,
                                selected = tamatoamlr::pinniped.sp.study,
                                multiple = TRUE, selectize = TRUE)),
          column(
            width = 6,
            tags$br(), tags$br(),
            checkboxInput(ns("ka"), "Include only resights of known-age pinnipeds")
          )
        )
        # checkboxInput(ns("all_pinnipeds"),
        #               "Summarize all pinnipeds. Uncheck to select individuals",
        #               value = TRUE),
        # conditionalPanel(
        #   condition = "input.all_pinnipeds == false", ns = ns,
        #   uiOutput(ns("pinniped_uiOut_selectize"))
        # )
      ),
      box(
        title = "Summary options", status = "warning", solidHeader = FALSE,
        width = 6, collapsible = TRUE,
        helpText("This tab allows you to summarize and visualize tag resight data. ",
                 "Select how you wish to summarize this data, ",
                 "and then specify any filters you would like to apply"),
        fluidRow(
          column(4, .summaryTimingUI(ns, c("fs_single"), "fs_single")), #"fs_total",
          column(4, radioButtons(ns("summary_type"), tags$h5("Summary type"),
                                 choices = list("Resight summary" = "summ"),
                                 selected = "summ"))
          # column(4, )
        )
      )
    ),
    mod_output_ui(
      ns("out"),
      tags$br(), uiOutput(ns("warning_na_records"))
      # uiOutput(ns("warning_date_single_filter"))
    )
  )
}



#' @name shiny_modules
#' @export
mod_tag_resights_server <- function(id, src, season.df, tab) {
  stopifnot(
    is.reactive(src),
    is.reactive(season.df),
    is.reactive(tab)
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

      ### Census-specific common values
      vals <- reactiveValues(
        warning_na_records = NULL,
        warning_date_single_filter = NULL
      )


      ##########################################################################
      # Observe events

      # ### Store the selected beaches and column names
      # # observe(vals$beaches_selected <- input$location)
      # observe(vals$census_tbl_columns_selected <- input$age_sex)
      #
      # observeEvent(input$tabs, {
      #   # vals$beaches_selected  <- NULL
      #   vals$census_tbl_columns_selected <- NULL
      # })


      ##########################################################################
      # RenderUIs

      ### Warning messages
      output$warning_na_records <- renderUI({
        span(req(vals$warning_na_records), style = "color:red;")
      })

      # output$warning_date_single_filter <- renderUI({
      #   span(req(vals$warning_date_single_filter), style = "color:red;")
      # })


      ### Pinniped selection dropdown
      output$pinniped_uiOut_selectize <- renderUI({
        req(input$summary_timing == "fs_single")

        x <- tr_df_filter_ka() %>%
          distinct(pinniped_id, species, tag_primary, tag_type_primary) %>%
          mutate(name = paste(species, paste(tag_primary, tag_type_primary),
                              sep = " | "))

        choices.list <- as.list(x$pinniped_id) %>% set_names(x$name)

        selectInput(
          session$ns("pinniped"),
          tags$h5("Pinnipeds to include (Species | Primary Tag"),
          choices = choices.list, selected = choices.list,
          multiple = TRUE, selectize = TRUE
        )
      })


      ##########################################################################
      ##########################################################################
      ##########################################################################
      # Collect all phocid census data - one time run, then all data is collected
      tr_df_collect <- reactive({
        req(src(), tab() == .ids$tag_resights)
        vals$warning_na_records <- NULL

        tr.df.collect.orig <- try(tbl_vTag_Resights(src()), silent = TRUE)

        validate(
          need(tr.df.collect.orig,
               "Unable to collect vTag_Resights from the specified database")
        )

        #--------------------------------------------------
        ### TMP: bring in leops separately, for now
        leop.collect.orig <- try(tbl_vTag_Resights_Leopards(src()), silent = TRUE)
        validate(
          need(leop.collect.orig,
               "Unable to collect vTag_Resights_Leopards from the specified database")
        )
        #--------------------------------------------------

        tr.df.collect <- bind_rows(tr.df.collect.orig, leop.collect.orig)

        #----------------------------------------------
        # Filter records for non-NA values, verbosely as appropriate
        # NOTE: no need to remove location NAs
        tr.df.nona <- tr.df.collect %>%
          filter(!is.na(season_name), !is.na(resight_date), !is.na(species))

        nrow.diff <- nrow(tr.df.collect) - nrow(tr.df.nona)
        vals$warning_na_records <- if (nrow.diff != 0) {
          paste(
            nrow.diff,
            ifelse(nrow.diff == 1, "row was", "rows were"),
            "removed because of a NULL season_name, species,",
            "and/or resight_date value.",
            "Please tell the database manager."
          )
        } else {
          NULL
        }

        validate(
          need(nrow(tr.df.nona) > 0,
               "No data to process after removing rows with NA values")
        )

        tr.df.nona
      })


      ##########################################################################
      # Filter collected data

      #-------------------------------------------------------------------------
      ### Filter data by season/date
      tr_df_filter_season <- reactive({
        tr.df.orig <- tr_df_collect()

        #----------------------------------------------
        # Filter by season/date/week num
        fs <- filter_season()

        # TODO: remove eventually
        validate(
          need(input$summary_timing == "fs_single",
               "Only single season summaries are currently available")
        )

        tr.df <- if (input$summary_timing %in% .summary.timing.multiple) {
          tr.df.orig %>%
            filter(season_name %in% !!req(fs$season()))
        } else if (input$summary_timing %in% .summary.timing.single) {
          tr.df.orig %>%
            filter(season_name == !!req(fs$season()),
                   between(resight_date,
                           !!req(fs$date_range())[1], !!req(fs$date_range())[2]))
        } else {
          validate("invalid input$summary_timing value")
        }

        validate(
          need(nrow(tr.df) > 0,
               "There are no data for the given season/date filter(s)")
        )

        tr.df
      })


      #-------------------------------------------------------------------------
      ### Filter data by species
      tr_df_filter_species <- reactive({
        tr.df <- tr_df_filter_season() %>%
          filter(species %in% !!input$species)

        validate(
          need(nrow(tr.df) > 0,
               "There are no data for the given species filter")
        )

        tr.df
      })


      #-------------------------------------------------------------------------
      ### Filter data by non-NA cohort
      tr_df_filter_ka <- reactive({
        tr.df <- tr_df_filter_species()
        if (input$ka) tr.df <- tr.df %>% filter(!is.na(cohort))

        validate(
          need(nrow(tr.df) > 0,
               "There are no data for the given known-age filter")
        )

        tr.df
      })


      # #-------------------------------------------------------------------------
      # ### Filter data by selected pinniped(s), if necessary
      # tr_df_filter_pinniped <- reactive({
      #   tr.df <- tr_df_filter_ka()
      #
      #   if (!input$all_pinnipeds){
      #     validate(
      #       need(input$pinniped, "Please select at least one pinniped")
      #     )
      #     tr.df <- tr.df %>% filter(pinniped_id %in% as.numeric(input$pinniped))
      #   }
      #
      #   validate(
      #     need(nrow(tr.df) > 0,
      #          "There are no data for the given pinniped selection(s)")
      #   )
      #
      #   tr.df
      # })


      ##########################################################################

      #------------------------------------------------------------------------
      ### Summarize tag resight data to display
      tr_df_summ <- reactive({
        if (input$summary_type == "summ") {
          tr_df_filter_ka() %>%
            mutate(species = as.character(species)) %>%
            group_by(season_name, pinniped_id, species,
                     tag_primary, tag_type_primary, sex = pinniped_sex, cohort,
                     tag_sort_primary) %>%
            # summarise(tag_primary = unique(tag_primary),
            #           tag_type_primary = unique(tag_type_primary),
            #           cohort = unique(cohort),
            summarise(n_resights = n(),
                      resight_date_first = min(resight_date),
                      resight_date_last = max(resight_date),
                      locations = paste(unique(location_group), collapse = ", "),
                      statuses = paste(unique(status), collapse = ", "),
                      amlr_tag_primary = !unique(non_amlr_tag_primary),
                      # nonamlr_tag_primary = unique(nonamlr_tag_primary),
                      # tag_sort = unique(tag_sort),
                      # tag_sort_primary = unique(tag_sort_primary),
                      .groups = "drop") %>%
            mutate(age = pinniped_age(today(), cohort)) %>%
            relocate(pinniped_id, tag_sort_primary,
                     .after = last_col()) %>%
            relocate(age, amlr_tag_primary, .after = cohort) %>%
            arrange(species, tag_sort_primary)

        } else {
          validate("Invalid summary_type input - please contact the database manager")
        }
      })


      ##########################################################################
      # Outputs

      #-------------------------------------------------------------------------
      ### Output table
      tbl_output <- reactive({
        tr_df_summ()
      })


      #-------------------------------------------------------------------------
      ### Output plot
      plot_output <- reactive({
        #--------------------------------------------------------
        fs <- filter_season()
        ggplot.out <- NULL
        validate("No plot for tag resights")

        # Output
        ggplot.out
      })


      #-------------------------------------------------------------------------
      ### Send off
      observe(mod_output_server("out", tbl_output, plot_output))
    }
  )
}
