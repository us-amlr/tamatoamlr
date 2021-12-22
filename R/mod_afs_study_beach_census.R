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
          column(4, uiOutput(ns("on_date_uiOut_select"))),
          column(
            width = 8,
            conditionalPanel(
              condition = "input.summary_timing == 'fs_date_single'", ns = ns,
              helpText("TODO: describe census date control functionality"))
          )
        ),
        uiOutput(ns("age_sex_uiOut_selectize")),
        uiOutput(ns("beach_uiOut_selectize"))
      ),
      box(
        title = "Summary options", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
        tags$h5("This tab allows you to summarize and visualize AFS study beach census data. ",
                "Select how you wish to summarize this data, and then any filters you would like to apply"),
        fluidRow(
          column(4, .summaryTimingUI(ns, c("fs_date_series", "fs_date_single", "fs_single", "fs_raw"))),
          column(4, .summaryLocationUI(ns, c("by_capewide", "by_amlr", "by_beach"), "by_amlr")),
          column(
            width = 4,
            conditionalPanel(
              condition = "input.summary_location == 'by_amlr'", ns = ns,
              checkboxGroupInput(ns("amlr_beach_dmm"), tags$h5("Include these beaches"),
                                 choices = list("Daniel", "Marko", "Modulo"),
                                 selected = c("Daniel", "Marko"))
            )
          )
        ),
        checkboxInput(ns("pup_dead_cumulative"), "Display dead pup counts as cumulative",
                      value = TRUE),
        conditionalPanel(
          condition = "input.summary_location == 'by_amlr'", ns = ns,
          helpText("Records will be included if they are from one of the checked beaches, ",
                   "or from Chungungo, Cachorros, Maderas, Copi, or Hue")
        )
      )
    ),
    mod_output_ui(ns("afs_study_beach_census_out"), tags$br(), uiOutput(ns("warning_na_records")))
  )
}



#' @name shiny_modules
#' @export
mod_afs_study_beach_census_server <- function(id, pool, season.df) { #season.df, season.list
  stopifnot(
    is.reactive(pool),
    is.reactive(season.df)
  )

  moduleServer(
    id,
    function(input, output, session) {
      ###############################################################################
      # Census-specific common values

      vals <- reactiveValues(
        beaches_selected = NULL,
        census_tbl_columns_selected = NULL,
        warning_na_records = NULL
      )


      ### Column names specific to each census type
      census.names <- list(
        "ad_female_count", "pup_live_count", "pup_dead_count",
        "ad_male_count", "ad_unk_count",
        "adult_male_non_terr_count", "adult_male_terr_count",
        "adult_male_terr_noFem_count", "adult_male_terr_wFem_count", "adult_male_unk_count",
        "juv_female_count", "juv_male_count", "juv_unk_count"
      )


      ###############################################################################
      # Observe events

      ### Store the selected beaches and column names
      observe(vals$beaches_selected <- input$beach)
      observe(vals$census_tbl_columns_selected <- input$age_sex)

      observeEvent(input$tabs, {
        vals$beaches_selected  <- NULL
        vals$census_tbl_columns_selected <- NULL
      })


      ###############################################################################
      # RenderUIs

      ### Warning messages
      output$warning_na_records <- renderUI({
        span(req(vals$warning_na_records), style = "color:red;")
      })

      ### Census date selector
      output$on_date_uiOut_select <- renderUI({
        req(input$summary_timing == "fs_date_single")

        date.curr <- census_df_filter_season()$census_date
        year(date.curr)[month(date.curr) >= 7] <- 2000
        year(date.curr)[month(date.curr) < 7] <- 2001
        date.curr <- sort(unique(date.curr))

        date.list <- purrr::set_names(
          as.list(paste(month(date.curr), day(date.curr), sep = "-")),
          paste(month(date.curr, label = TRUE),
                str_pad(day(date.curr), width = 2, pad = "0"))
        )

        date.today <- paste(month(Sys.Date()), day(Sys.Date()), sep = "-")
        date.sel <- if (date.today %in% date.list) date.today else date.list[[1]]

        selectInput(
          session$ns("on_date"), tags$h5("Census date"),
          choices = date.list, selected = date.sel
        )
      })

      ### Beaches dropdown
      output$beach_uiOut_selectize <- renderUI({
        req(input$summary_location == "by_beach")
        beaches.list <- as.list(sort(unique(census_df_filter_season()$Beach)))

        beaches.sel <- if (is.null(vals$beaches_selected)) {
          beaches.list[[1]]
        } else {
          vals$beaches_selected
        }

        selectInput(
          session$ns("beach"), tags$h5("Beach(es)"),
          choices = beaches.list, selected = beaches.sel,
          multiple = TRUE, selectize = TRUE
        )
      })

      ### Columns dropdown
      output$age_sex_uiOut_selectize <- renderUI({
        req(input$summary_timing != "fs_raw")

        choices.selected <- if (input$summary_timing == "fs_date_series") {
          c("pup_live_count")
        } else {
          c("ad_female_count", "pup_live_count", "pup_dead_count")
        }

        selectInput(
          session$ns("age_sex"), tags$h5("Columns to plot"),
          choices = census.names, selected = choices.selected,
          multiple = TRUE, selectize = TRUE
        )
      })


      ###############################################################################
      # Collect all phocid census data - one time run, then all data is collected
      census_df_collect <- reactive({
        vals$warning_na_records <- NULL

        tbl(req(pool()), "vCensus_AFS_Study_Beach") %>%
          select(census_id, season_name, observer,
                 species, census_date, time_start, time_end, Beach,
                 !!as.character(census.names), census_notes, census_created_dt) %>%
          collect() %>%
          mutate(census_date = lubridate::ymd(census_date),
                 species = str_to_sentence(species))
      })


      ###############################################################################
      ### Get filter_season values
      filter_season <- reactive({
        mod_filter_season_server(
          "filter_season",  reactive(input$summary_timing), season.df
        )
      })


      ### Filter data by species, season/date, and remove NA values
      census_df_filter_season <- reactive({
        #----------------------------------------------
        # Filter by season/date
        x <- census_df_collect()
        z <- filter_season()

        census.df <- if (input$summary_timing == "fs_single") {
          req(length(z$season_select()) == 1)
          x %>%
            filter(season_name == !!req(z$season_select()),
                   between(census_date, !!req(z$date_range())[1], !!req(z$date_range())[2]))
        } else if (input$summary_timing %in% c("fs_date_series", "fs_date_single", "fs_raw")) {
          x %>%
            filter(season_name %in% !!req(z$season_select()))
        } else {
          validate("This timing summary option is under development")
        }

        #----------------------------------------------
        # Filter records for non-NA values, verbosely as appropriate
        if (input$summary_timing == "fs_raw") {
          census.df.nona <- census.df %>%
            filter(!is.na(season_name), !is.na(Beach), !is.na(census_date), !is.na(species))

          nrow.diff <- nrow(census.df) - nrow(census.df.nona)
          vals$warning_na_records <- if (nrow.diff != 0) {
            paste(
              "When processing census records,", nrow.diff,
              ifelse(nrow.diff == 1, "row was", "rows were"),
              "removed because of a NULL season_name, species, Beach, and/or census_date value"
            )
          } else {
            NULL
          }

          validate(
            need(nrow(census.df.nona) > 0,
                 "No data to process after removing rows with NA values")
          )

          census.df.nona

        } else {
          census.df
        }
      })


      ### Filter by date for multiple seasons+by date, if needed
      census_df_filter_datesingle <- reactive({
        x <- census_df_filter_season()

        if (input$summary_timing == "fs_date_single") {
          tmp <- x %>%
            mutate(date_max = ymd(paste(
              if_else(month(census_date) > 7, str_sub(season_name, 1, 4),
                      paste0("20", str_sub(season_name, 6, 7))),
              !!req(input$on_date), sep = "-"
            )),
            date_diff = as.numeric(difftime(date_max, census_date, units = "days"))) %>%
            filter(census_date < date_max)

          tmp.summ <- tmp %>%
            group_by(season_name) %>%
            summarise(date_diff_min = min(date_diff),
                      census_date = unique(census_date[date_diff == date_diff_min]))

          tmp %>%
            inner_join(tmp.summ, by = c("season_name", "census_date")) %>%
            select(-c(date_max, date_diff, date_diff_min))

        } else {
          x
        }
      })


      ### Final filter function
      census_df_filter <- reactive({
        x <- census_df_filter_datesingle()

        #----------------------------------------------
        # Filter by Beach
        census.df <- if (req(input$summary_location) == "by_beach") {
          validate(need(input$beach, "Please select at least one beach name"))
          x %>% filter(Beach %in% input$beach)

        } else if (input$summary_location == "by_amlr") {
          beach.amlr.regex <- c(
            "Chungungo", "Cachorros", "Maderas", "Copi", "Hue",
            input$amlr_beach_dmm
          )
          x %>%
            filter(pmap_lgl(list(x$Beach), function(i, j) {
              any(str_detect(i, j))
            }, j = beach.amlr.regex))

        } else {
          x
        }
      })


      ###############################################################################
      # Process collected census data

      ### Process collected census data, part 1 (summary level 2)
      #------------------------------------------------------------------------------
      census_df_summ <- reactive({
        x <- census_df_filter()

        #----------------------------------------------
        # Make dead pup count cumulative if specified
        if (input$pup_dead_cumulative) {
          x <- x %>%
            arrange(Beach, census_date) %>%
            group_by(season_name, Beach) %>%
            mutate(pup_dead_count = cumsum(pup_dead_count)) %>%
            ungroup()
        }

        #----------------------------------------------
        # Summarize as specified, and output
        if (input$summary_location == "by_beach") {
          .vcs_summ_func(x, season_name, census_date, species, Beach,
                         season.df = season.df()) %>%
            mutate(Beach = factor(Beach, levels == input$beach))
        } else { #if (input$summary_location %in% c("by_capewide", "by_amlr")) {
          .vcs_summ_func(x, season_name, census_date, species,
                         season.df = season.df(), beach.chr = TRUE) %>%
            mutate(Beach = "CS")
        }
      })


      #------------------------------------------------------------------------------
      ### Process collected census data, part 2 (summary level 3)
      census_df <- reactive({
        # Get the names of the applicable census columns, and then summarize
        grp.names.all <- c("season_name", "census_date", "species", "Beach")

        validate(
          need(all(input$age_sex %in% unlist(census.names)),
               "Issue in census_df")
        )

        # Summarize by species, sex, and age class
        census_df_summ() %>%
          select(!!dplyr::intersect(grp.names.all, names(.)),
                 !!!as.list(input$age_sex),
                 !!dplyr::intersect("Beaches", names(.))) %>%
          arrange_season(season.df(),
                         !!!syms(dplyr::intersect(grp.names.all[-1], names(.))))
      })


      ###############################################################################
      # Outputs

      #------------------------------------------------------------------------------
      ### Output table
      tbl_output <- reactive({
        if (input$summary_timing == "fs_raw") {
          census_df_filter() %>%
            select(-season_info_id) %>%
            arrange(desc(census_date), observer, Beach)

        } else {
          df.out <- census_df() %>% arrange(desc(census_date))
          if (input$summary_location != "by_beach") {
            df.out %>% select(-Beach)
          } else {
            df.out
          }
        }
      })


      #------------------------------------------------------------------------------
      ### Output plot
      plot_output <- reactive({
        #--------------------------------------------------------
        x <- census_df()

        validate(
          need(input$summary_timing != "fs_raw",
               "There is no plot to show for raw data"),
          need(between(length(input$age_sex), 1, 6),
               "Please select between one and six 'columns to plot' to display a graph")
        )

        # #--------------------------------------------------------
        # # Set plot variable depending on user selections
        # gg.color <- as.name(case_when(
        #   input$summary_timing == "fs_multiple_total" ~ "season_name",
        #   input$summary_timing != "fs_multiple_total" && input$summary_location == "by_beach" ~ "Beach",
        #   input$summary_timing != "fs_multiple_total" && input$summary_location != "by_beach" ~ "count_class"
        # ))
        # x.val <- as.name("census_date")
        # x.lab <- "Date"
        # y.lab <- "Count"
        # gg.title <- "AFS Study Beach Census"


        #--------------------------------------------------------
        # This processing is done here so that output$tbl is wide
        grp.names.all <- c("Beach", "census_date")
        grp.syms <- syms(dplyr::intersect(grp.names.all, names(x)))

        census.df <- x %>%
          pivot_longer(cols = where(is.numeric),
                       names_to = "count_class", values_to = "count_value") %>%
          filter(!is.na(count_value)) %>%
          mutate(season_name = factor(season_name, levels = filter_season()$season_select()),
                 count_class = factor(count_class, levels = input$age_sex)) %>%
          arrange_season(season.df(), !!!grp.syms, count_class)

        validate(need(nrow(census.df) > 0, "No data to plot"))

        if (input$summary_timing == "fs_date_series") {
          year(census.df$census_date) <- if_else(month(census.df$census_date) > 7, 1900, 1901)
        }

        #--------------------------------------------------------
        # Plotting
        ggplot.pre <- ggplot(census.df, aes(x = census_date, y = count_value))

        # Generate initial plot pieces that depend on user selection
        if (input$summary_timing == "fs_date_series") {
          ###
          ggplot.out <- ggplot.pre +
            geom_point(aes(color = season_name, shape = count_class)) +
            geom_line(aes(group = interaction(season_name, Beach, count_class),
                          color = season_name, linetype = Beach)) +
            guides(color = guide_legend(title = "Season"),
                   shape = guide_legend(title = "Age + sex class", order = 1))

          if (input$summary_location != "by_beach") {
            ggplot.out <- ggplot.out + guides(linetype = "none")
          }

        } else {
          if (input$summary_location == "by_beach") {
            ### Color-code lines and points by beach
            ggplot.out <- ggplot.pre +
              geom_point(aes(color = Beach, shape = count_class)) +
              geom_line(aes(group = interaction(Beach, count_class),
                            color = Beach)) +
              guides(shape = guide_legend(title = "Age + sex class", order = 1))

          } else {
            ### Color-code lines and points by age+sex class
            ggplot.out <- ggplot.pre +
              geom_point(aes(color = count_class)) +
              geom_line(aes(group = count_class, color = count_class)) +
              guides(color = guide_legend(title = "Age + sex class", order = 1))
          }
        }

        # Add in more general parts of the plot
        ggplot.out +
          expand_limits(y = 0) +
          xlab("Date") +
          ylab("Count") +
          ggtitle("AFS Study Beach Census") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      })


      ### Send off
      observe(mod_output_server("afs_study_beach_census_out", id, tbl_output, plot_output))
    }
  )
}
