#' @name shiny_modules
#' @export
mod_afs_capewide_pup_census_ui <- function(id) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    fluidRow(
      box(
        title = "Filters", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
        fluidRow(
          column(12, mod_filter_season_ui(ns("filter_season")))
        ),
        uiOutput(ns("location_uiOut_selectize"))
      ),
      box(
        title = "Summary options", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
        helpText("This tab allows you to summarize and visualize AFS Capewide Pup Census data. ",
                 "Select how you wish to summarize this data, ",
                 "and then specify any filters you would like to apply"),
        fluidRow(
          column(4, .summaryTimingUI(ns, c("fs_total", "fs_single", "fs_raw"))),
          column(4, .summaryLocationUI(ns, c("by_capewide", "by_beach"), "by_capewide", FALSE))
        ),
        conditionalPanel(
          condition = "input.summary_timing == 'fs_total'", ns = ns,
          checkboxInput(ns("all_seasons"), "Include all all seasons", value = TRUE),
          helpText("THe 'all seasons' checkbox will override the seaosn filter")
        ),
        conditionalPanel(
          condition = "input.summary_timing == 'fs_single'", ns = ns,
          checkboxInput(ns("exclude_count"), "Remove data with the 'exclude_count' flag",
                        value = TRUE)
        ),
        uiOutput(ns("exclude_count_uiOut_helptext"))
      )
    ),
    mod_output_ui(ns("out"), tags$br(), uiOutput(ns("warning_na_records")))
  )
}



#' @name shiny_modules
#' @export
mod_afs_capewide_pup_census_server <- function(id, pool, season.df) {
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

      ### Census-specific common values
      vals <- reactiveValues(
        warning_na_records = NULL
      )

      ### Warning messages
      output$warning_na_records <- renderUI({
        span(req(vals$warning_na_records), style = "color:red;")
      })

      ### Locations dropdown
      output$location_uiOut_selectize <- renderUI({
        req(input$summary_location == "by_beach", pool())
        # beaches.list <- tbl_beaches_capewide(pool())$location
        beaches.list <-  sort(unique(census_df_collect()$location))

        selectInput(
          session$ns("location"), tags$h5("Location(s)"),
          choices = beaches.list, multiple = TRUE
        )
      })

      ### Exclude count block
      output$exclude_count_uiOut_helptext <- renderUI({
        req(input$summary_timing)

        if (input$summary_timing == "fs_single") {
          helpText("This 'Single Season' ",
                   "view is for checking data for count consistency")
        } else if (input$summary_timing == "fs_total") {
          helpText("Data with the 'exclude_count' flag will always",
                   "be removed when plotting over multiple seasons")
        }  else if (input$summary_timing == "fs_raw") {
          helpText("All data, including data with the 'exclude_count'",
                   "flag, are included in the raw data output")
        } else {
          stop("Invalid input$summary_timing")
        }
      })


      ###################################################################################
      ##########################################################################
      ##########################################################################
      # Collect all census data - one time run, then all data is collected
      census_df_collect <- reactive({
        vals$warning_na_records <- NULL

        census.df.collect <- try(tbl_vCensus_AFS_Capewide_Pup(pool()),
                                 silent = TRUE)

        validate(
          need(census.df.collect,
               "Unable to find vCensus_AFS_Capewide_Pup on specified database")
        )


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

        validate(
          need(nrow(census.df) > 0,
               "There are no data for the given season filter(s)")
        )

        census.df %>%
          mutate(season_name = factor(season_name))
      })


      #-------------------------------------------------------------------------
      ### Filter data that has been specified to be excluded
      census_df_filter_exclude <- reactive({
        census.df <- census_df_filter_season()

        if (input$summary_timing == "fs_total") {
          census.df <- census.df %>% filter(!exclude_count)
          vals$warning_na_records <- NULL

        } else if (input$summary_timing == "fs_single" && input$exclude_count) {
          census.df.orig <- census.df
          census.df <- census.df.orig %>% filter(!exclude_count)

          nrow.diff <- nrow(census.df.orig) - nrow(census.df)
          vals$warning_na_records <- if (nrow.diff != 0) {
            paste(
              "Note:", nrow.diff,
              ifelse(nrow.diff == 1, "row was", "rows were"),
              "removed because of a exclude_count flag"
            )
          } else {
            NULL
          }

        } else {
          vals$warning_na_records <- NULL
        }

        validate(
          need(nrow(census.df) > 0,
               "No data to process after removing exclude_count rows")
        )

        census.df
      })


      #-------------------------------------------------------------------------
      ### Filter data by location
      census_df_filter_location <- reactive({
        census.df <- census_df_filter_exclude()

        if (input$summary_location == "by_beach") {
          validate(need(input$location, "Please select at least one beach name"))
          census.df <- census.df %>% filter(location %in% input$location)
        }

        validate(
          need(nrow(census.df) > 0,
               "There are no data for the given location filter")
        )

        census.df%>%
          mutate(location = factor(location, levels = input$location))
      })


      # ##########################################################################
      # # Process data
      # census_df_fs_single <- reactive({
      #   census.df.out <- census_df_filter_location() %>%
      #     # mutate(pup_total_count = pup_live_count + pup_dead_count) %>%
      #     group_by(census_date, census_afs_capewide_pup_sort, location) %>%
      #     summarise(n_records = n(),
      #               count_mean = round(mean(pup_count), 1),
      #               count_live_mean = round(mean(pup_live_count), 1),
      #               count_dead_mean = round(mean(pup_dead_count), 1),
      #               count_range = diff(range(pup_count)),
      #               counts = paste(paste(observer, pup_count, sep = ": "),
      #                              collapse = "; "),
      #               notes_tmp = list(if_else(
      #                 is.na(census_notes), NA_character_,
      #                 paste(observer, census_notes, sep = ": ")
      #               )),
      #               notes = paste(na.omit(unlist(notes_tmp)), collapse = "; "),
      #               counts_live = paste(paste(observer, pup_dead_count, sep = ": "),
      #                                   collapse = "; "),
      #               counts_dead = paste(paste(observer, pup_live_count, sep = ": "),
      #                                   collapse = "; "),
      #               .groups = "drop") %>%
      #     # left_join(tbl_beaches_capewide(req(pool())), by = "location") %>%
      #     arrange(census_afs_capewide_pup_sort) %>%
      #     select(-c(census_afs_capewide_pup_sort, notes_tmp)) #, beach_id))
      #
      #   bind_rows(
      #     data.frame(location = "Capewide",
      #                count_mean = sum(census.df.out$count_mean),
      #                count_live_mean = sum(census.df.out$count_live_mean),
      #                count_dead_mean = sum(census.df.out$count_dead_mean)),
      #     census.df.out
      #   )
      # })
      #
      #
      # census_df_fs_total <- reactive({
      #   census.df.out <- census_df_filter_location() %>%
      #     filter(!exclude_count) %>%
      #     # mutate(season_name = amlr_season_from_date(census_date))
      #     # z.summ.loc <- z %>%
      #     group_by(season_name, census_afs_capewide_pup_sort, location) %>%
      #     summarise(num_records = n(),
      #               count_loc_mean = mean(pup_count),
      #               count_loc_var = var(pup_count),
      #               tmp_date = min(census_date),
      #               .groups = "drop")
      #
      #   if (input$summary_location == "by_beach") {
      #     census.df.out %>%
      #       rename(count_mean = count_loc_mean,
      #              count_var = count_loc_var) %>%
      #       select(-tmp_date)
      #
      #   } else {
      #     census.df.out %>%
      #       group_by(season_name) %>%
      #       summarise(count_mean = round(sum(count_loc_mean), 0),
      #                 count_var = if_else(min(tmp_date) < as.Date("2011-07-01"),
      #                                     NA_real_, sum(count_loc_var, na.rm = TRUE)),
      #                 count_sd = round(sqrt(count_var), 0),
      #                 .groups = "drop") %>%
      #       select(-count_var)
      #   }
      # })

      ### Table and plot for fs_total summaries
      census_df_fs_total <- reactive({
        census.df <- census_df_filter_location()

        if (input$summary_location == "by_beach") {
          afs_cwp_totals_bylocation(census.df) %>%
            rename(count_mean = count_loc_mean,
                   count_var = count_loc_var,
                   count_sd = count_loc_sd) %>%
            select(-date_min)

        } else {
          afs_cwp_totals(census.df)
        }
      })

      plot_fs_total <- reactive({
        x <- census_df_fs_total() %>% mutate(group = 1)

        g.out <- if (input$summary_location == "by_beach") {
          ggplot(x, aes(season_name, count_mean,
                        group = location, color = location)) +
            guides(color = guide_legend(title = "Location"))
        } else {
          ggplot(x, aes(season_name, count_mean, group = group))
        }

        g.out +
          geom_point() +
          geom_line() +
          geom_errorbar(aes(ymin = count_mean-count_sd,
                            ymax = count_mean+count_sd),
                        width = 0.2) +
          scale_x_discrete(drop = FALSE) +
          theme(axis.text.x = element_text(angle = 90)) +
          ggtitle("Cape Shirreff AFS Capewide Pup Census") +
          xlab("Season") +
          ylab("Count (mean)") +
          expand_limits(y = 0)
      })



      ##########################################################################
      # Outputs

      # output$mean_count_uiOut_text <- renderUI({
      #   census.df.summ <- census_df_fs_single()
      #   paste(
      #     "Total mean count:", round(sum(census.df.summ$count_mean), 0),
      #     paste0("(", round(sum(census.df.summ$count_live_mean), 0), " live)")
      #   )
      # })

      #-------------------------------------------------------------------------
      ### Output table
      tbl_output <- reactive({
        req(input$summary_timing)
        census.df <- census_df_filter_location()

        if (input$summary_timing == "fs_raw") {
          census.df %>% select(-c(census_afs_capewide_pup_sort, pup_count))
        } else if (input$summary_timing == "fs_single") {
          afs_cwp_single(census.df)
        } else if (input$summary_timing == "fs_total") {
          census_df_fs_total()
        } else {
          validate(
            "This summary is in development"
          )
        }
      })


      #-------------------------------------------------------------------------
      ### Output plot
      plot_output <- reactive({
        # ggplot(data.frame(x = 1:2, y = 1:2), aes(x, y)) +
        #   geom_point() +
        #   ggtitle("Ignore this plot")
        # NULL
        if (input$summary_timing == "fs_raw") {
          validate("No plot for raw data summary")

          # } else if (input$summary_timing == "fs_single") {
          #   afs_cwp_single(census.df)

        } else if (input$summary_timing == "fs_total") {
          plot_fs_total()


        } else {
          validate("This plot is in development")
        }
      })

      #-------------------------------------------------------------------------
      ### Send off
      observe(mod_output_server("out", session, tbl_output, plot_output))
    }
  )
}
