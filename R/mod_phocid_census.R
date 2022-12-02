#' @name shiny_modules
#' @export
mod_phocid_census_ui <- function(id) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    fluidRow(
      box(
        title = "Filters", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
        fluidRow(
          column(8, mod_filter_season_ui(ns("filter_season"))),
          column(
            width = 3, offset = 1,
            checkboxGroupInput(ns("species"), label = tags$h5("Species"),
                               choices = amlrPinnipeds::pinniped.phocid.sp,
                               selected = amlrPinnipeds::pinniped.phocid.sp)
          )
        ),
        uiOutput(ns("week_num_uiOut_select")),
        uiOutput(ns("age_sex_uiOut_selectize")),
        uiOutput(ns("location_uiOut_selectize"))
      ),
      box(
        title = "Summary options", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
        tags$h5("This tab allows you to summarize and visualize phocid census data. ",
                "Select how you wish to summarize this data, ",
                "and then specify any filters you would like to apply"),
        fluidRow(
          column(
            width = 4,
            .summaryTimingUI(ns, c("fs_total", "fs_date_single", "fs_single")),
            conditionalPanel(
              condition = "input.summary_timing == 'fs_single'", ns = ns,
              checkboxInput(ns("plot_cumsum"), "Plot cumulative sum", value = FALSE)
            )
          ),
          column(4, .summaryLocationUI(ns, c("by_capewide", "by_beach"), "by_capewide")),
          column(4, .summarySpAgeSexUI(ns, c("by_sp", "by_sp_age_sex"), "by_sp"))
        )
      )
    ),
    mod_output_ui(ns("out"), tags$br(), uiOutput(ns("warning_na_records")))
  )
}



#' @name shiny_modules
#' @export
mod_phocid_census_server <- function(id, pool, season.df) {
  stopifnot(
    is.reactive(pool),
    is.reactive(season.df)
  )

  moduleServer(
    id,
    function(input, output, session) {
      ##########################################################################
      ### Get filter_season values
      filter_season <- reactive({
        mod_filter_season_server(
          "filter_season",  reactive(input$summary_timing), season.df
        )
      })


      ##########################################################################
      # Census-specific common values

      vals <- reactiveValues(
        beaches_selected = NULL,
        census_tbl_columns_selected = NULL,
        warning_na_records = NULL
      )

      ### Names of summary level 1 inputs for multiple seasons
      census.summ1.mult <- c("fs_total", "fs_date_single")


      ##########################################################################
      # Observe events

      ### Store the selected beaches and column names
      observe(vals$beaches_selected <- input$location)
      observe(vals$census_tbl_columns_selected <- input$age_sex)

      observeEvent(input$tabs, {
        vals$beaches_selected  <- NULL
        vals$census_tbl_columns_selected <- NULL
      })


      ##########################################################################
      # RenderUIs

      ### Warning messages
      output$warning_na_records <- renderUI({
        span(req(vals$warning_na_records), style = "color:red;")
      })
#
#       ### Week number dropdown
#       output$week_num_uiOut_select <- renderUI({
#         req(input$summary_timing == "fs_date_single")
#
#         validate("todo")
#
#         # wk.list <- sort(unique(census_df_filter_season()$week_num))
#         #
#         # # browser()
#         # # census_df_filter_season() %>%
#         # #   mutate(md = paste(stringr::str_pad(month(census_date), 2, side = "left", pad = "0"),
#         # #                     stringr::str_pad(day(census_date), 2, side = "left", pad = "0"),
#         # #                     sep = "-")) %>%
#         # #   group_by(week_num) %>%
#         # #   summarise(date_min = min(md),
#         # #             date_max = max(md))
#         #
#         # selectInput(
#         #   session$ns("week_num"), tags$h5("Week number"),
#         #   choices = wk.list, selected = min(unlist(wk.list))
#         # )
#       })
#
#       ### locations dropdown
#       output$location_uiOut_selectize <- renderUI({
#         req(input$summary_location == "by_beach")
#         beaches.list <- if (input$location_aggregate) {
#           as.list(sort(unique(census_df_filter_season()$location_group)))
#         } else {
#           as.list(sort(unique(census_df_filter_season()$location)))
#         }
#
#         beaches.sel <- if (is.null(vals$beaches_selected)) {
#           beaches.list[[1]]
#         } else {
#           vals$beaches_selected
#         }
#
#         selectInput(
#           session$ns("location"), tags$h5("Location(s)"),
#           choices = beaches.list, selected = beaches.sel,
#           multiple = TRUE, selectize = TRUE
#         )
#       })

      ### Columns dropdown
      output$age_sex_uiOut_selectize <- renderUI({
        req(input$summary_sas == "by_sp_age_sex")

        selectInput(
          session$ns("age_sex"), tags$h5("Columns to plot"),
          choices = census.cols.phocid, selected = census.cols.phocid[[1]],
          multiple = TRUE, selectize = TRUE
        )
      })


      ##########################################################################
      ##########################################################################
      ##########################################################################
      # Collect all phocid census data - one time run, then all data is collected

      census_df_collect <- reactive({
        vals$warning_na_records <- NULL

        validate(
          need(try(tbl(req(pool()), "vCensus_Phocid"), silent = TRUE),
               "Unable to find vCensus_Phocid on specified database")
        )

        # tbl_vCensus_Phocid(req(pool())) %>%
        tbl(req(pool()), "vCensus_Phocid") %>%
          select(season_name, census_phocid_header_id, census_id,
                 census_type, observer,
                 census_date_start, census_date_end,
                 census_date, time_start, time_end,
                 location, location_group, beach_id, species,
                 ad_female_count, ad_male_count, ad_unk_count,
                 juv_female_count, juv_male_count, juv_unk_count,
                 pup_live_count, pup_dead_count,
                 unk_female_count, unk_male_count, unk_unk_count,
                 census_notes, census_created_dt) %>%
          collect() %>%
          mutate(week_num = lubridate::week(census_date),
                 species = str_to_sentence(species))
        # census_date = factor(census_date),
        # species = factor(species, levels = sort(unique(species))))
      })


      # ##########################################################################
      # ### Filter data by species, season/date, and remove NA values
      # census_df_filter_season <- reactive({
      #   #----------------------------------------------
      #   # Filter by species
      #   census.df <- census_df_collect() %>%
      #     filter(species %in% !!input$species)
      #
      #   #----------------------------------------------
      #   # Filter by season/date/week num
      #   z <- filter_season()
      #
      #   census.df <- if (input$summary_timing %in% census.summ1.mult) {
      #     census.df %>%
      #       filter(season_name %in% !!req(z$season_select()))
      #   } else if (input$summary_timing == "fs_single") {
      #     census.df %>%
      #       filter(season_name == !!req(z$season_select()),
      #              between(census_date, !!req(z$date_range())[1], !!req(z$date_range())[2]))
      #   } else {
      #     validate("invalid input$summary_timing value")
      #   }
      #
      #   validate(
      #     need(nrow(census.df) > 0,
      #          "No data for selected season filter")
      #   )
      #
      #   #----------------------------------------------
      #   # Filter records for non-NA values, verbosely as appropriate
      #   census.df.nona <- census.df %>%
      #     filter(!is.na(season_name), !is.na(location),
      #            !is.na(census_date), !is.na(species))
      #
      #   nrow.diff <- nrow(census.df) - nrow(census.df.nona)
      #   vals$warning_na_records <- if (nrow.diff != 0) {
      #     paste(
      #       "When processing census records,", nrow.diff,
      #       ifelse(nrow.diff == 1, "row was", "rows were"),
      #       "removed because of a NULL season_name, species,",
      #       "location, and/or census_date value"
      #     )
      #   } else {
      #     NULL
      #   }
      #
      #   validate(
      #     need(nrow(census.df.nona) > 0,
      #          "No data to process after removing rows with NA values")
      #   )
      #
      #   census.df.nona
      # })
      #
      #
      # ##########################################################################
      # census_df_filter <- reactive({
      #   #----------------------------------------------
      #   # Filter by location
      #   census.df <- if (input$summary_location == "by_beach") {
      #     validate(need(input$location, "Please select at least one beach name"))
      #     census_df_filter_season() %>% filter(location %in% input$location)
      #   } else {
      #     census_df_filter_season()
      #   }
      #
      #   if (input$location_aggregate)
      #     census.df <- census.df %>% mutate(location = location_group)
      #
      #   # #----------------------------------------------
      #   # # Filter by week num
      #   # census.df.out <- if (input$summary_timing == "fs_week") {
      #   #   census.df %>% filter(.data$week_num == !!req(input$week_num))
      #   # } else {
      #   #   census.df
      #   # }
      #
      #   #----------------------------------------------
      #   validate(
      #     need(nrow(census.df.out) > 0,
      #          "There are no census data to plot based on the procided filters")
      #   )
      #
      #   census.df.out
      # })
      #
      #
      # ##########################################################################
      # # Process collected census data
      #
      # ### Process collected census data, part 1 (summary level 2)
      # #-------------------------------------------------------------------------
      # census_df_summ <- reactive({
      #   vcs <- census_df_filter()
      #
      #   #----------------------------------------------
      #   # Summarize as specified, and output
      #   vcs_summ_func <- function(y, ...) {
      #     y %>%
      #       group_by(...) %>%
      #       summarise(across(where(is.numeric), sum, na.rm = TRUE),
      #                 .groups = "drop") %>%
      #       complete(...) %>%
      #       mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
      #       arrange_season(season.df(), species)
      #   }
      #
      #   if (input$summary_location == "by_capewide" && input$summary_timing == "fs_single") {
      #     vcs_summ_func(vcs, season_name, census_date, species)
      #   } else if (input$summary_location == "by_capewide" && input$summary_timing %in% census.summ1.mult) {
      #     vcs_summ_func(vcs, season_name, species)
      #   } else if (input$summary_location == "by_beach" && input$summary_timing == "fs_single") {
      #     vcs_summ_func(vcs, season_name, census_date, species, location)
      #   } else if (input$summary_location == "by_beach" && input$summary_timing %in% census.summ1.mult) {
      #     vcs_summ_func(vcs, season_name, species, location)
      #   } else {
      #     validate("invalid input$summary_timing + input$summary_location combo")
      #   }
      # })
      #
      #
      # #------------------------------------------------------------------------------
      # ### Process collected census data, part 2 (summary level 3)
      # census_df <- reactive({
      #   # Get the names of the applicable census columns, and then summarize
      #   grp.names.all <- c("season_name", "census_date", "species", "location")
      #
      #   if (input$summary_sas == "by_sp") {
      #     # Summarize by species only
      #     vcs.summ <- census_df_summ() %>%
      #       select(where(is.character), where(is.Date), !!!census.cols.phocid)
      #
      #     grp.syms <- syms(dplyr::intersect(grp.names.all, names(vcs.summ)))
      #
      #     vcs.summ <- vcs.summ %>%
      #       pivot_longer(cols = where(is.numeric), names_to = "count_class", values_to = "count_value") %>%
      #       group_by(!!!grp.syms) %>%
      #       summarise(count_value = sum(count_value),
      #                 .groups = "drop") %>%
      #       arrange_season(season.df(), !!!syms(dplyr::intersect(grp.names.all[-1], names(vcs.summ))))
      #
      #     # } else if (input$summary_sas == "by_sp_age") {
      #     #   validate("Cannot summarize by species+age (yet?)")
      #
      #   } else if (input$summary_sas == "by_sp_age_sex") {
      #     # Summarize by species, sex, and age class
      #     req(all(input$age_sex %in% unlist(census.cols.phocid)))
      #
      #     vcs.summ <- census_df_summ() %>%
      #       select(where(is.character), where(is.Date), !!!as.list(input$age_sex)) %>%
      #       arrange_season(season.df(), !!!syms(dplyr::intersect(grp.names.all[-1], names(.))))
      #
      #   } else {
      #     validate("Invalid input$summary_sas value")
      #   }
      #
      #
      #   # If necessary, calculate cumsums of census columns
      #   if (input$summary_timing == "fs_single" && input$plot_cumsum) {
      #     grp.names.all <- c("season_name", "species", "location")
      #     grp.syms <- syms(dplyr::intersect(grp.names.all, names(vcs.summ)))
      #
      #     vcs.summ <- vcs.summ %>%
      #       group_by(!!!grp.syms) %>%
      #       mutate(across(where(is.numeric), cumsum)) %>%
      #       ungroup()
      #   }
      #
      #   vcs.summ
      # })


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
        # if (input$summary_timing %in% census.summ1.mult) {
        #   census_df_filter() %>%
        #     group_by(season_name) %>%
        #     summarise(n_census_date = n_distinct(census_date)) %>%
        #     right_join(df.out, by = "season_name") %>%
        #     select(season_name, .data$n_census_date, everything())
        # } else {
        #   df.out %>% select(season_name, census_date, everything())
        # }

        census_df_collect()
      })


      #-------------------------------------------------------------------------
      ### Output plot
      plot_output <- reactive({
        # #--------------------------------------------------------
        # census_df() #This is here to 'get to' upstream validate() messages before any req() calls are hit
        #
        # # TODO: fix
        # if (input$summary_sas == "by_sp_age_sex") {
        #   validate(
        #     need(between(length(input$age_sex), 1, 6),
        #          "Please select between one and six 'columns to plot' to display a graph")
        #   )
        # }
        #
        # validate(
        #   need(input$summary_location != "by_beach" || length(input$species) == 1,
        #        "When plotting phocid census data by beach, please select exactly one species")
        # )
        #
        # #--------------------------------------------------------
        # # Set plot variable depending on user selections
        # if (input$summary_timing == "fs_single") {
        #   x.val <- as.name("census_date")
        #   x.lab <- "Date"
        # } else if (input$summary_timing %in% census.summ1.mult) {
        #   x.val <- as.name("season_name")
        #   x.lab <- "Season"
        # } else {
        #   validate("census plot - invalid input$summary_timing value")
        # }
        #
        # # if (input$summary_timing == "fs_week") {
        # #   x.lab <- paste("Season, for week number", req(input$week_num))
        # # }
        #
        # y.lab <- if (input$plot_cumsum) "Count (cumulative sum)" else "Count"
        # gg.title <- case_when(
        #   input$summary_timing == "fs_total" ~ "Phocid Census - Totals by Season",
        #   input$summary_timing == "fs_date_single" ~ "Phocid Census - Closest to Date",
        #   # input$summary_timing == "fs_week" ~ paste("Phocid Census - Week", input$week_num, "by Season"),
        #   input$summary_timing == "fs_single" ~ paste("Phocid Census -", filter_season()$season_select())
        # )
        #
        #
        # #--------------------------------------------------------
        # # This processing is done here so that output$tbl is wide
        # grp.names.all <- c("species", "location", "census_date")
        # grp.syms <- syms(dplyr::intersect(grp.names.all, names(census_df())))
        #
        # census.df <- if (input$summary_sas == "by_sp") {
        #   census_df() %>%
        #     mutate_factor_species() %>%
        #     arrange_season(season.df(), !!!grp.syms)
        # } else {
        #   census_df() %>%
        #     pivot_longer(cols = where(is.numeric), names_to = "count_class", values_to = "count_value") %>%
        #     mutate_factor_species() %>%
        #     arrange_season(season.df(), !!!grp.syms, count_class)
        # }
        #
        # validate(need(nrow(census.df) > 0, "No data to plot"))
        #
        # #--------------------------------------------------------
        # # Plotting
        #
        # # Generate initial plot pieces that depend on user selection
        # if (input$summary_location == "by_beach") {
        #   ### Color-code lines and points by beach, require one species
        #   ggplot.out <- census.df %>%
        #     mutate(species_lty = as.character(unique(species))) %>%
        #     ggplot(aes(x = !!x.val, y = count_value, linetype = species_lty)) +
        #     guides(
        #       size = "none",
        #       linetype = guide_legend(title = "Species", order = 1)
        #     )
        #
        #   validate(
        #     need("location" %in% names(census.df), "census plot: beach name error"),
        #     need(n_distinct(census.df$species) == 1, "census plot: beach-species error")
        #   )
        #   ggplot.out <- if (input$summary_sas == "by_sp") {
        #     ggplot.out +
        #       geom_point(aes(color = location)) +
        #       geom_line(aes(group = location, color = location))
        #   } else {
        #     ggplot.out +
        #       geom_point(aes(shape = count_class, color = location)) +
        #       geom_line(aes(group = interaction(location, count_class), color = location)) +
        #       guides(shape = guide_legend(title = "Age+sex class"))
        #   }
        #
        # } else {
        #   ### Color-code lines and points by species
        #   ggplot.out <- ggplot(census.df, aes(x = !!x.val, y = count_value)) +
        #     guides(size = "none") +
        #     scale_color_manual(values = pinniped.sp.colors)
        #
        #   ggplot.out <- if (input$summary_sas == "by_sp") {
        #     ggplot.out +
        #       geom_point(aes(color = species)) +
        #       geom_line(aes(group = species, color = species))
        #   } else {
        #     ggplot.out +
        #       geom_point(aes(shape = count_class, color = species)) +
        #       geom_line(aes(group = interaction(species, count_class), color = species)) +
        #       guides(shape = guide_legend(title = "Sex+age class"))
        #   }
        #   colors.all <- amlrPinnipeds::pinniped.sp.colors
        #   color.values <- colors.all[names(colors.all) %in% census.df$species]
        #   ggplot.out <- ggplot.out +
        #     scale_color_manual(values = color.values)
        #   guides(color = guide_legend(title = "Species", order = 1))
        # }
        #
        # # Add in more general parts of the plot
        # ggplot.out <- ggplot.out +
        #   expand_limits(y = 0) +
        #   xlab(x.lab) +
        #   ylab(y.lab) +
        #   ggtitle(gg.title) +
        #   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        #
        #
        # # Output
        # ggplot.out

        ggplot(data.frame(x = 1:10, y = 1:10)) + geom_point(aes(x, y))
      })


      #-------------------------------------------------------------------------
      ### Send off
      observe(mod_output_server("out", session, tbl_output, plot_output))
    }
  )
}
