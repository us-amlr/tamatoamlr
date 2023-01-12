#' @name shiny_modules
#' @export
mod_ccamlr_pup_weights_ui <- function(id) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    fluidRow(
      box(
        title = "Filters", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
        mod_filter_season_ui(ns("filter_season"))
      ),
      box(
        title = "Summary options", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
        helpText("This tab allows you to summarize and visualize CCAMLR Pup Weights data. ",
                 "Select how you wish to summarize this data, ",
                 "and then specify any filters you would like to apply"),
        fluidRow(
          column(
            width = 4,
            .summaryTimingUI(ns, c("fs_single")) #"fs_facet",
          ),
          column(4, uiOutput(ns("round_num_uiOut_select"))),
          # column(4, .summarySpAgeSexUI(ns, c("by_sp", "by_sp_age_sex"), "by_sp"))
        )
      )
    ),
    mod_output_ui(ns("out"))
  )
}



#' @name shiny_modules
#' @export
mod_ccamlr_pup_weights_server <- function(id, pool, season.df) {
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

      ### Get filter_season values
      filter_season <- reactive({
        mod_filter_season_server(
          "filter_season",  reactive(input$summary_timing), season.df
        )
      })


      ##########################################################################
      # RenderUIs

      # ### Warning messages
      # output$warning_na_records <- renderUI({
      #   span(req(vals$warning_na_records), style = "color:red;")
      # })

      ### Round number
      output$round_num_uiOut_select <- renderUI({
        selectInput(session$ns("round_num"), tags$h5("Pup Weight Round(s)"),
                    choices = sort(unique(req(cpw_df_collect())$round_num)),
                    selected = c(1:4), multiple = TRUE)
      })


      ##########################################################################
      ##########################################################################
      ##########################################################################
      # Collect all cpw data - one time run, then all data is collected
      cpw_df_collect <- reactive({
        vals$warning_na_records <- NULL

        validate(
          need(try(tbl(req(pool()), "vCCAMLR_Pup_Weights"), silent = TRUE),
               "Unable to find vCCAMLR_Pup_Weights on specified database")
        )

        tbl_vCCAMLR_Pup_Weights(pool()) %>%
          arrange(round_date, round_num, pup_num)
      })


      ##########################################################################
      # Filter collected data

      #-------------------------------------------------------------------------
      ### Filter data by species, season/date, and remove NA values
      cpw_df_filter_season <- reactive({
        cpw.df.orig <- cpw_df_collect()
        #----------------------------------------------
        # Filter by season/date/week num
        fs <- filter_season()

        cpw.df <- if (input$summary_timing %in% .summary.timing.multiple) {
          cpw.df.orig %>%
            filter(season_name %in% !!req(fs$season()))
        } else if (input$summary_timing %in% .summary.timing.single) {
          cpw.df.orig %>%
            filter(season_name == !!req(fs$season()),
                   between(round_date,
                           !!req(fs$date_range())[1], !!req(fs$date_range())[2]))
        } else {
          validate("invalid input$summary_timing value")
        }

        validate(
          need(nrow(cpw.df) > 0,
               "There are no data for the given season filter(s)")
        )

        cpw.df
      })

      #-------------------------------------------------------------------------
      cpw_df_round <- reactive({
        validate(
          need(input$round_num, "Please select at least one round number")
        )
        cpw_df_filter_season() %>%
          filter(round_num %in% input$round_num)
      })


      ##########################################################################
      # Summarize
      cpw_df <- reactive({
        cpw_df_round() %>%
          group_by(season_name, round_num, round_date, time_start, time_end) %>%
          summarise(mass_kg_female = round(mean(mass_kg[sex == "F"]), 2),
                    mass_kg_male = round(mean(mass_kg[sex == "M"]), 2),
                    n_female_weights = sum(sex == "F"),
                    n_male_weights = sum(sex == "M"),
                    locations = paste(unique(location_group), collapse = ", "),
                    .groups = "drop")
      })

      ##########################################################################
      # Outputs

      #-------------------------------------------------------------------------
      ### Output table
      tbl_output <- reactive({
        si.dmp <- season.df() %>% select(season_name, date_median_pupping)
        cpw_df() %>%
          left_join(si.dmp, by = "season_name") %>%
          mutate(days_since_date_median_pupping = as.numeric(
            difftime(round_date, date_median_pupping, units = "days"))) %>%
          select(-date_median_pupping)
      })


      #-------------------------------------------------------------------------
      ### Output plot
      plot_output <- reactive({
        cpw.df <- cpw_df() %>%
          pivot_longer(c(mass_kg_female, mass_kg_male),
                       names_to = "sex", values_to = "mass_kg") %>%
          mutate(sex = case_when(
            sex == "mass_kg_female" ~ "F",
            sex == "mass_kg_male" ~ "M"
          ))

        ggplot(cpw.df, aes(round_num, mass_kg)) +
          geom_point(aes(color = sex)) +
          # geom_smooth(method = lm, se = FALSE) +
          geom_line(aes(group = sex, color = sex)) +
          ggtitle(paste("CCAMLR Pup Weights:", unique(cpw.df$season_name))) +
          xlab("Round number") +
          ylab("Mass (kg)") +
          ylim(0, NA) +
          scale_x_continuous(breaks = as.numeric(input$round_num))
      })


      #-------------------------------------------------------------------------
      ### Send off
      observe(mod_output_server("out", session, tbl_output, plot_output))
    }
  )
}
