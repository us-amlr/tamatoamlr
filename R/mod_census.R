#' @name shiny_modules
#' @export
mod_census_ui <- function(id) {
  ns <- NS(id)

  pinniped.sp.list.phocid <- amlrPinnipeds::pinniped.sp.list[
    c("Crabeater seal", "Elephant seal", "Leopard seal", "Weddell seal")
  ]

  # assemble UI elements
  tagList(
    fluidRow(
      box(
        title = "Filters", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
        fluidRow(
          mod_season_filter_ui(ns("season_filter"), col.width = 4),
          column(
            width = 3, offset = 1,
            conditionalPanel(
              condition = "input.type == 'phocid'", ns = ns,
              checkboxGroupInput(ns("species"), label = tags$h5("Species"),
                                 choices = pinniped.sp.list.phocid,
                                 selected = unname(unlist(pinniped.sp.list.phocid)))
            )
          )
        ),
        uiOutput(ns("age_sex_uiOut_selectize")),
        uiOutput(ns("beach_uiOut_selectize"))
      ),
      box(
        title = "Summary ...", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
        tags$h5("This tab allows you to summarize and visualize census data. Select your census type and ",
                "how you wish to summarize this data, and then any filters you would like to apply"),
        fluidRow(
          column(
            width = 4,
            radioButtons(ns("type"), label = NULL, #tags$h5("Census type"),
                         choices = list("AFS pup census" = "afs_pup",
                                        "AFS study beach census" = "afs_study_beach",
                                        "Phocid census" = "phocid"),
                         selected = "afs_pup")
          )
        ),
        fluidRow(
          column(
            width = 4,
            radioButtons(ns("summary_level_1"), label = tags$h5("Summary level 1"),
                         choices = list("Multiple seasons - total" = "fs_multiple_total",
                                        "Multiple seasons - weekly" = "fs_multiple_week",
                                        "Single season" = "fs_single"),
                         selected = "fs_multiple_total")
          ),
          column(
            width = 4,
            radioButtons(ns("summary_level_2"), label = tags$h5("Summary level 2"),
                         choices = list("By beach" = "by_beach",
                                        "Cape - wide" = "by_capewide"),
                         selected = "by_capewide")
          ),
          column(
            width = 4,
            radioButtons(ns("summary_level_3"), label = tags$h5("Summary level 3"),
                         choices = list("By species and sex+age class" = "by_sp_age_sex",
                                        "By species" = "by_sp"),
                         selected = "by_sp_age_sex")
          )
        ),
        conditionalPanel(
          condition = "input.summary_level_1 == 'fs_single'", ns = ns,
          checkboxInput(ns("plot_cumsum"), "Plot cumulative sum", value = FALSE)
        )
        # tags$br(), tags$br(),
        # tags$h5("Todo?: descriptive text about what the above choices 'mean' in terms of what is plotted"),
      )
    ),
    mod_output_ui(ns("census_out"), tags$br(), uiOutput(ns("warning_na_records")))
  )
}



#' @name shiny_modules
#' @export
mod_census_server <- function(id, pool, season.df, season.id.list) {
  stopifnot(
    is.reactive(pool),
    is.reactive(season.df),
    is.reactive(season.id.list)
  )

  moduleServer(
    id,
    function(input, output, session) {
      ###############################################################################
      # Census-specific common values

      vals <- reactiveValues(
        beaches_selected = NULL,
        census_tbl_columns_selected = NULL,
        census_tbl_columns_all = list(),
        warning_na_records = NULL
      )

      ### Names of summary level 1 inputs for multiple seasons
      census.summ1.mult <- c("fs_multiple_total", "fs_multiple_week")


      ### Column names specific to each census type
      census.cols.all <- list(
        "ad_female_count", "ad_male_count", "ad_unk_count",
        "adult_male_non_terr", "adult_male_terr", "adult_male_terr_noFem", "adult_male_terr_wFem", "adult_male_unk",
        "juv_female_count", "juv_male_count", "juv_unk_count",
        "pup_dead_count", "pup_live_count",
        "unk_female_count", "unk_male_count", "unk_unk_count", "unknownMF_count"
      )


      census.cols.afs.pup <- list("pup_live_count", "pup_dead_count")
      census.cols.afs.study.beach <- list(
        "ad_female_count", "ad_male_count", "ad_unk_count",
        "adult_male_non_terr", "adult_male_terr", "adult_male_terr_noFem", "adult_male_terr_wFem", "adult_male_unk",
        "juv_female_count", "juv_male_count", "juv_unk_count",
        "pup_dead_count", "pup_live_count"
      )
      census.cols.phocid <- list(
        "ad_female_count", "ad_male_count", "ad_unk_count",
        "juv_female_count", "juv_male_count", "juv_unk_count",
        "pup_dead_count", "pup_live_count",
        "unk_female_count", "unk_male_count", "unk_unk_count", "unknownMF_count"
      )


      ###############################################################################
      # Observe events

      ### Store the selected beaches and column names
      observe(vals$beaches_selected <- input$beach)
      observe(vals$census_tbl_columns_selected <- input$age_sex)

      observe({
        input$tabs
        input$type

        isolate({
          vals$beaches_selected  <- NULL
          vals$census_tbl_columns_selected <- NULL
        })
      })

      observeEvent(input$type, {
        vals$cols.list <- switch(
          input$type,
          afs_pup = census.cols.afs.pup,
          afs_study_beach = census.cols.afs.study.beach,
          phocid = census.cols.phocid
        )
      })


      ###############################################################################
      # RenderUIs

      ### Warning messages
      output$warning_na_records <- renderUI({
        span(req(vals$warning_na_records), style = "color:red;")
      })

      ### Beaches dropdown
      output$beach_uiOut_selectize <- renderUI({
        req(input$summary_level_2 == "by_beach")
        beaches.list <- as.list(sort(unique(census_df_collect()$Beach)))

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
        req(input$summary_level_3 == "by_sp_age_sex")

        choices.list <- req(vals$cols.list)

        selected.vals <- switch(
          input$type,
          afs_pup = choices.list,
          choices.list[[1]]
        )

        validate(need(choices.list, "invalid input$summary_level_1 value"))

        selectInput(
          session$ns("age_sex"), tags$h5("Columns to plot"),
          choices = choices.list, selected = selected.vals,
          multiple = TRUE, selectize = TRUE
        )
      })


      ###############################################################################
      # Generate base sql query, passed to future reactives and season_filter module
      census_df_collect_pre <- reactive({
        vals$warning_na_records <- NULL

        # Validate checks - req() here and validate() in census_df_collect() so UIs don't get validate
        req(!(input$type != "phocid" && input$summary_level_3 == "by_sp"))
        req(!(input$type != "phocid" && input$summary_level_1 == "fs_multiple_week"))

        # Set values
        census.type <- switch(
          input$type,
          afs_pup = "Capewide",
          afs_study_beach = "AFS Study Beach",
          phocid = "Phocid"
        )

        # Generate base sql query, passed to future reactives and season_filter module
        tbl(req(pool()), "vCensus_Season") %>%
          filter(census_type == census.type)
      })


      ###############################################################################
      ### Get season filter values
      census_filter <- reactive({
        tbl.sql <- census_df_collect_pre() %>%
          select(season_info_id, date_column = census_date)

        mod_season_filter_server(
          "season_filter",  reactive(input$summary_level_1), season.df, season.id.list,
          reactive(tbl.sql)
        )
      })


      ### Generate the rest of the SQL query, and collect data from census table
      census_df_collect <- reactive({
        # Validate checks - these are here to stop the processing as soon as possible
        validate(
          need(!(input$type != "phocid" && input$summary_level_3 == "by_sp"),
               "You can only summarize phocid census data 'by species'"),
          need(!(input$type != "phocid" && input$summary_level_1 == "fs_multiple_week"),
               "You can only summarize phocid census data by 'multiple seasons - weekly' (by weeks across multiple seasons)")
        )

        vcs.sql.pre <- census_df_collect_pre()
        z <- census_filter()

        # Add on season/date filters. Week num filtering done below to not requery whole thing
        vcs.sql <- if (input$summary_level_1 %in% census.summ1.mult) {
          vcs.sql.pre %>%
            filter(between(season_info_id, !!req(z$season_min()), !!req(z$season_max())))

        } else if (input$summary_level_1 == "fs_single") {
          vcs.sql.pre %>%
            filter(season_info_id == !!req(z$season_select()),
                   between(census_date, !!req(z$date_range())[1], !!req(z$date_range())[2]))

        } else {
          validate("invalid input$summary_level_1 value")
        }


        # Add on species filters, if applicable
        if (input$type == "phocid") {
          vcs.sql <- vcs.sql %>% filter(tolower(species) %in% !!input$species)
        }

        # Collect query
        vcs <-  vcs.sql %>%
          collect() %>%
          mutate(species = str_to_sentence(species),
                 week_num = lubridate::week(census_date))
      })


      ### Filter for week number. Separate because it is done after the collect call
      census_df_collect_wk <- reactive({
        if (input$summary_level_1 == "fs_multiple_week") {
          census_df_collect() %>% filter(week_num == !!req(census_filter()$week_num()))
        } else {
          census_df_collect()
        }
      })


      ###############################################################################
      # Process collected census data

      ### Process collected census data, part 1 (summary level 2)
      #------------------------------------------------------------------------------
      census_df_summ <- reactive({
        stopifnot(
          c("season_name", "species", "Beach", "census_date", "week_num") %in% names(census_df_collect_wk())
        )

        #----------------------------------------------
        # Filter records, verbosely as appropriate
        vcs <- census_df_collect_wk() %>%
          filter(!is.na(season_name),
                 !is.na(Beach),
                 !is.na(census_date),
                 !is.na(species))

        vcs.nrow.diff <- nrow(census_df_collect_wk()) - nrow(vcs)
        vals$warning_na_records <- if (vcs.nrow.diff != 0) {
          paste(
            "When processing census records,", vcs.nrow.diff,
            ifelse(vcs.nrow.diff == 1, "row was", "rows were"),
            "removed because of a NULL season_name, species, Beach, and/or census_date value"
          )
        } else {
          NULL
        }

        validate(
          need(nrow(vcs) > 0, "No data to process after removing rows with NA values")
        )

        # Filter for Beach
        if (input$summary_level_2 == "by_beach") {
          validate(
            need(input$beach, "Please select at least one beach name")
          )
          vcs <- vcs %>% filter(Beach %in% input$beach)
        }


        #----------------------------------------------
        # For AFS Capewide pup census data, average across observer
        #   This feels like it should be in census_df(), but is here before summary functions
        if (input$type == "afs_pup") {
          vcs %>%
            group_by(season_name, species, Beach, census_date, week_num) %>%
            summarise(across(pup_live_count:pup_dead_count, ~round(mean(.x, na.rm = TRUE), 0), na.rm = TRUE),
                      .groups = "drop")

        } else {
          vcs
        }


        #----------------------------------------------
        # Summarize as specified, and output
        vcs_summ_func <- function(y, ...) {
          y %>%
            group_by(...) %>%
            summarise(across(where(is.numeric), sum, na.rm = TRUE),
                      .groups = "drop") %>%
            complete(...) %>%
            mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
            arrange_season(season.df(), species)
        }

        if (input$summary_level_2 == "by_capewide" && input$summary_level_1 == "fs_single") {
          vcs %>% vcs_summ_func(season_name, species, census_date)

        } else if (input$summary_level_2 == "by_capewide" && input$summary_level_1 %in% census.summ1.mult) {
          vcs %>% vcs_summ_func(season_name, species)

        } else if (input$summary_level_2 == "by_beach" && input$summary_level_1 == "fs_single") {
          vcs %>% vcs_summ_func(season_name, species, Beach, census_date)

        } else if (input$summary_level_2 == "by_beach" && input$summary_level_1 %in% census.summ1.mult) {
          vcs %>% vcs_summ_func(season_name, species, Beach)

        } else {
          validate("invalid input$summary_level_1 + input$summary_level_2 combo")
        }
      })


      #------------------------------------------------------------------------------
      ### Process collected census data, part 2 (summary level 3)
      census_df <- reactive({
        validate(
          need(between(length(input$age_sex), 1, 6),
               "Please select between one and six columns to plot")
        )

        # Get the names of the applicable census columns, and then summarize
        grp.names.all <- c("season_name", "species", "Beach", "census_date")
        if (input$summary_level_3 == "by_sp") {
          # Summarize by species only
          vcs.summ <- census_df_summ() %>%
            select(where(is.character), where(is.Date), !!!vals$cols.list)

          grp.syms <- syms(dplyr::intersect(grp.names.all, names(vcs.summ)))

          vcs.summ <- vcs.summ %>%
            pivot_longer(cols = where(is.numeric), names_to = "count_class", values_to = "count_value") %>%
            group_by(!!!grp.syms) %>%
            summarise(count_value = sum(count_value),
                      .groups = "drop") %>%
            arrange_season(season.df(), !!!syms(dplyr::intersect(grp.names.all[-1], names(vcs.summ))))

        } else if (input$summary_level_3 == "by_sp_age_sex") {
          # Summarize by species, sex, and age class
          req(
            all(input$age_sex %in% unlist(switch(
              input$type,
              afs_pup = census.cols.afs.pup,
              afs_study_beach = census.cols.afs.study.beach,
              phocid = census.cols.phocid
            )))
          )

          vcs.summ <- census_df_summ() %>%
            select(where(is.character), where(is.Date), !!!as.list(input$age_sex))

          grp.syms <- syms(dplyr::intersect(grp.names.all, names(vcs.summ)))

          vcs.summ <- vcs.summ %>%
            arrange_season(season.df(), !!!syms(dplyr::intersect(grp.names.all[-1], names(vcs.summ))))

        } else {
          validate("Invalid input$summary_level_3 value")
        }



        # If necessary, calculate cumsums of census columns
        if (input$summary_level_1 == "fs_single" && input$plot_cumsum) {
          grp.names.all <- c("season_name", "species", "Beach")
          grp.syms <- syms(dplyr::intersect(grp.names.all, names(vcs.summ)))

          vcs.summ <- vcs.summ %>%
            group_by(!!!grp.syms) %>%
            mutate(across(where(is.numeric), cumsum)) %>%
            ungroup()
        }

        vcs.summ
      })


      ###############################################################################
      # Outputs

      #------------------------------------------------------------------------------
      ### Output table
      tbl_output <- reactive({
        census_df() %>%
          nest(data = where(is.numeric)) %>%
          mutate(flag0 = pmap_lgl(list(data), function(i) all(i == 0))) %>%
          filter(!flag0) %>%
          unnest(cols = c(data)) %>%
          select(-flag0)
      })


      #------------------------------------------------------------------------------
      ### Output plot
      # output$plot <- renderPlot({
      plot_output <- reactive({
        #--------------------------------------------------------
        census_df() #This is here to 'get to' upstream validate() messages before any req() calls are hit

        if (input$type == "phocid") {
          validate(
            need(input$summary_level_2 != "by_beach" || length(input$species) == 1,
                 "When plotting phocid census data by beach, please select exactly one species")
          )
        }

        #--------------------------------------------------------
        # Set plot variable depending on user selections
        if (input$summary_level_1 == "fs_single") {
          x.val <- as.name("census_date")
          x.lab <- "Date"
        } else if (input$summary_level_1 %in% census.summ1.mult) {
          x.val <- as.name("season_name")
          x.lab <- "Season"
        } else {
          validate("census plot - invalid input$summary_level_1 value")
        }

        if (input$summary_level_1 == "fs_multiple_week") {
          x.lab <- paste("Season, for week number", req(census_filter()$week_num()))
        }

        y.lab <- if (input$plot_cumsum) "Count (cumulative sum)" else "Count"

        gg.title <- switch(
          input$type,
          afs_pup = "AFS Capewide Pup Census",
          afs_study_beach = "AFS Study Beach Census",
          phocid = "Weekly Phocid Census"
        )

        # size.val <- 1.2


        #--------------------------------------------------------
        # This processing is done here so that output$tbl is wide
        grp.names.all <- c("species", "Beach", "census_date")
        grp.syms <- syms(dplyr::intersect(grp.names.all, names(census_df())))

        census.df <- if (input$summary_level_3 == "by_sp") {
          census_df() %>%
            mutate_factor_species() %>%
            arrange_season(season.df(), !!!grp.syms)
        } else {
          census_df() %>%
            pivot_longer(cols = where(is.numeric), names_to = "count_class", values_to = "count_value") %>%
            mutate_factor_species() %>%
            arrange_season(season.df(), !!!grp.syms, count_class)
        }

        validate(need(nrow(census.df) > 0, "No data to plot"))

        #--------------------------------------------------------
        # Plotting

        # Generate initial plot pieces that depend on user selection
        if (input$summary_level_2 == "by_beach") {
          ### Color-code lines and points by beach, require one species
          ggplot.out <-  census.df %>%
            mutate(species_lty = as.character(unique(species))) %>%
            ggplot(aes(x = !!x.val, y = count_value, linetype = species_lty)) +
            guides(
              size = FALSE,
              linetype = guide_legend(title = "Species", order = 1)
            )

          validate(
            need("Beach" %in% names(census.df), "census plot: beach name error"),
            need(n_distinct(census.df$species) == 1, "census plot: beach-species error")
          )
          if (input$summary_level_3 == "by_sp") {
            ggplot.out <- ggplot.out +
              geom_point(aes(color = Beach)) + #, size = size.val)) +
              geom_line(aes(group = Beach, color = Beach))
          } else {
            ggplot.out <- ggplot.out +
              geom_point(aes(shape = count_class, color = Beach)) + #, size = size.val)) +
              geom_line(aes(group = interaction(Beach, count_class), color = Beach)) +
              guides(shape = guide_legend(title = "Age+sex class"))
          }

        } else {
          ### Color-code lines and points by species
          ggplot.out <- ggplot(census.df, aes(x = !!x.val, y = count_value)) +
            guides(size = FALSE)

          if (input$summary_level_3 == "by_sp") {
            ggplot.out <- ggplot.out +
              geom_point(aes(color = species)) + #, size = size.val)) +
              geom_line(aes(group = species, color = species))
          } else {
            ggplot.out <- ggplot.out +
              geom_point(aes(shape = count_class, color = species)) + #, size = size.val)) +
              geom_line(aes(group = interaction(species, count_class), color = species)) +
              guides(shape = guide_legend(title = "Sex+age class"))
          }
          ggplot.out <- ggplot.out +
            guides(color = guide_legend(title = "Species", order = 1))
        }

        # Add in more general parts of the plot
        ggplot.out <- ggplot.out +
          expand_limits(y = 0) +
          xlab(x.lab) +
          ylab(y.lab) +
          ggtitle(gg.title) +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

        # Output
        ggplot.out
      })


      ### Send off
      observe(mod_output_server("census_out", id, tbl_output, plot_output))
    }
  )
}
