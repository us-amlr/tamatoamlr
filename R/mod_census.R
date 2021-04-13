#' Shiny module for the Census tab
#'
#' Shiny module for the Census tab
#'
#' @name mod_census
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @export
mod_census_ui <- function(id) {
  ns <- NS(id)

  pinniped.sp.list.phocid <- amlrPinnipeds::pinniped.sp.list[
    c("Crabeater seal", "Elephant seal", "Leopard seal", "Weddell seal")
  ]

  # assemble UI elements
  tagList(
    fluidRow(
      column(
        width = 6,
        fluidRow(
          box(
            status = "primary", width = 12,
            plotOutput(ns("plot")),
            tags$br(), tags$br(),
            uiOutput(ns("warning_na_records"))
          ),
          box(
            status = "primary", width = 12,
            tags$h5("This table shows the data displayed in the plot above.",
                    "Note that all rows with only counts of zero for the selected columns have been filtered out."),
            DTOutput(ns("tbl"))
          )
        )
      ),
      column(
        width = 6,
        fluidRow(
          box(
            title = "Summary ...", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
            tags$h5("This tab allows you to summarize and visualize census data. Select your census type and ",
                    "how you wish to summarize this data, and then any filters you would like to apply"),
            fluidRow(
              column(
                width = 4,
                radioButtons(ns("type"), label = NULL, #tags$h5("Census type"),
                             choices = list("AFS pup census" = "afs_pup",
                                            "AFS study beach census" = "afs_study_beach",
                                            "Phocid census" = "phocid")
                )
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
                             choices = list("By species and age+sex class" = "by_sp_age_sex",
                                            "By species" = "by_sp"),
                             selected = "by_sp_age_sex")
              )
            ),
            conditionalPanel(
              condition = "input.summary_level_1 == 'fs_single'", ns = ns,
              checkboxInput(ns("plot_cumsum"), "Plot cumulative sum", value = FALSE)
            ),
            tags$br(), tags$br(),
            tags$h5("Todo?: descriptive text about what the above choices 'mean' in terms of what is plotted"),
          ),

          box(
            title = "Filters", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
            fluidRow(
              conditionalPanel(
                condition = "input.summary_level_1 != 'fs_single'", ns = ns,
                column(
                  width = 12,
                  fluidRow(mod_season_range_ui(ns("census"), 4)),
                  # column(
                  #   width = 4,
                  #   selectInput("census_season_min", label = tags$h5("Minimum season"),
                  #               choices = season.list, selected = season.list.id.min),
                  #   conditionalPanel(
                  #     condition = "input.census_summary_level_1 == 'fs_multiple_week'",
                  #     selectInput("census_week_num", tags$h5("Week number"), choices = list(), selected = NULL)
                  #   )
                  # ),
                  # column(4, selectInput("census_season_max", label = tags$h5("Maximum season"),
                  #                       choices = season.list, selected = season.list.id.max))
                  fluidRow(
                    column(
                      width = 4,
                      conditionalPanel(
                        condition = "input.summary_level_1 == 'fs_multiple_week'", ns = ns,
                        selectInput(ns("week_num"), tags$h5("Week number"), choices = list(), selected = NULL)
                      )
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "input.summary_level_1 == 'fs_single'", ns = ns,
                column(4, selectInput(ns("season_select"), label = tags$h5("Select season"), choices = NULL)),
                column(4, dateRangeInput(ns("date_range"), label = tags$h5("Date range"))) #Updated in observe() based on selected season
              ),

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
          )
        )
      )
    )

  )
}



#' @name mod_census
#'
#' @param pool reactive; a DBI database connection pool. Intended to be the output of \code{\link{mod_database_server}}
#' @param season.df reactive; the season info data frame.
#'   Intended to be the first element (\code{season.df}) of the (list) output of \code{\link{mod_season_server}}
#' @param season.id.list reactive; the (named)list of the season info ID values.
#'   Intended to be the second element (\code{season.id.list}) of the (list) output of \code{\link{mod_season_server}}
#'
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
        beaches = NULL,
        cols = NULL,
        warning.na.records = NULL
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
      census_si <- mod_season_range_server("census", season.id.list)


      ### Store the selected beaches and column names
      observe(vals$beaches <- input$beach)
      observe(vals$cols <- input$age_sex)

      observe({
        input$tabs
        input$type

        isolate({
          vals$beaches <- NULL
          vals$cols <- NULL
        })
      })



      ### Update week num dropdown depending on the currently filtered for data
      observe({
        week.list <- if (input$summary_level_1 == "fs_multiple_week") {
          as.list(sort(unique(census_df_collect()$week_num)))
        } else {
          NULL
        }

        updateSelectInput(session, "week_num", choices = week.list)
      })


      ### Update season selection
      observe({
        season.list <- season.id.list()
        if (!identical(season.list, list())) {
          updateSelectInput(session, "season_select", choices = season.list, selected = max(unlist(season.list)))
        } else {
          updateSelectInput(session, "season_select", choices = NULL)
        }
      })


      ### Update the date range limits based on the selected field season
      observe({
        if (input$summary_level_1 == "fs_single") {
          season.curr <- season.df() %>% filter(ID == as.numeric(req(input$season_select)))

          stopifnot(nrow(season.curr) == 1)
          start <- min <- season.curr[["season_open_date"]]
          end <- max <- season.curr[["season_close_date"]]

        } else {
          start <- min <- end <- max <- NULL
        }

        updateDateRangeInput(session, "date_range", start = start, end = end, min = min, max = max)
      })



      ###############################################################################
      # RenderUIs

      ### Warning messages
      output$warning_na_records <- renderUI({
        span(req(vals$warning.na.records), style = "color:red")
      })

      ### Beaches dropdown
      output$beach_uiOut_selectize <- renderUI({
        req(input$summary_level_2 == "by_beach")
        beaches.list <- as.list(sort(unique(census_df_collect()$Beach)))

        selectInput(
          session$ns("beach"), tags$h5("Beach(es)"),
          choices = beaches.list, selected = vals$beaches,
          multiple = TRUE, selectize = TRUE
        )
      })

      ### Columns dropdown
      output$age_sex_uiOut_selectize <- renderUI({
        req(input$summary_level_3 == "by_sp_age_sex")

        tmp <- tbl(pool(), "vCensus_Season") %>%
          head(1) %>%
          collect() %>%
          select(ad_female_count:unk_unk_count)

        choices.list <- switch(
          input$type,
          afs_pup = census.cols.afs.pup,
          afs_study_beach = census.cols.afs.study.beach,
          phocid = census.cols.phocid
        )
        selected.vals <- switch(
          input$type,
          afs_pup = census.cols.afs.pup,
          census.cols.afs.study.beach[[1]]
        )

        validate(need(choices.list, "invalid input$summary_level_1 value"))
        validate(
          need(all(unlist(choices.list) %in% names(tmp)),
               "Invalid column names, please report to Sam")
        )

        selectInput(
          session$ns("age_sex"), tags$h5("Columns to plot"),
          choices = choices.list, selected = selected.vals,
          multiple = TRUE, selectize = TRUE
        )
      })


      ###############################################################################
      # Generate SQL query, and collect data from census table
      census_df_collect <- reactive({
        vals$warning.na.records <- NULL

        # Validate checks
        validate(
          need(!(input$type != "phocid" && input$summary_level_3 == "by_sp"),
               "You can only summarize phocid census data 'by species'"),
          need(!(input$type != "phocid" && input$summary_level_1 == "fs_multiple_week"),
               "You can only summarize phocid census data by 'multiple seasons - weekly' (by weeks across multiple seasons)")
        )

        #----------------------------------------------
        # Set values
        census.type <- switch(
          input$type,
          afs_pup = "Capewide",
          afs_study_beach = "AFS Study Beach",
          phocid = "Phocid"
        )


        season.id.min <- census_si$minvar()
        season.id.max <- census_si$maxvar()
        # season.id.min <- as.integer(input$season_min)
        # season.id.max <- as.integer(input$season_max)
        season.id.select <- as.integer(input$season_select)

        #----------------------------------------------
        # Generate base sql query
        vcs.sql <- tbl(pool(), "vCensus_Season") %>%
          filter(census_type == census.type)

        # Add on season/date filters. Week num filtering done below to not requery whole thing (??)
        if (input$summary_level_1 %in% census.summ1.mult) {
          vcs.sql <- vcs.sql %>%
            filter(between(season_info_id, season.id.min, season.id.max))

        } else if (input$summary_level_1 == "fs_single") {
          vcs.sql <- vcs.sql %>%
            filter(season_info_id == season.id.select,
                   between(census_date, !!input$date_range[1], !!input$date_range[2]))

        } else {
          validate("invalid input$summary_level_1 value")
        }


        # Add on species filters, if applicable
        if (input$type == "phocid") {
          vcs.sql <- vcs.sql %>%
            filter(tolower(species) %in% !!input$species)
        }

        # Collect query
        vcs <-  vcs.sql %>%
          collect() %>%
          mutate(species = str_to_sentence(species),
                 week_num = lubridate::week(census_date))

        #----------------------------------------------
        # For AFS Capewide pup census data, average across observer
        #   This feels like it should be in census_df(), but is here b/c of input$type reference
        if (input$type == "afs_pup") {
          # validate("Cannot process Capewide pup census data yet") #TODO
          vcs %>%
            group_by(season_name, species, Beach, census_date, week_num) %>%
            summarise(across(pup_live_count:pup_dead_count, ~round(mean(.x, na.rm = TRUE), 0), na.rm = TRUE),
                      .groups = "drop")

        } else {
          vcs
        }
      })


      ###############################################################################
      # Process collected census data

      ### Process collected census data, part 1 (summary level 2)
      #------------------------------------------------------------------------------
      census_df_summ <- reactive({
        stopifnot(
          c("season_name", "species", "Beach", "census_date", "week_num") %in% names(census_df_collect())
        )

        #----------------------------------------------
        # Filter records, verbosely as appropriate
        vcs <- census_df_collect() %>%
          filter(!is.na(season_name),
                 !is.na(Beach),
                 !is.na(census_date),
                 !is.na(species))

        vcs.nrow.diff <- nrow(census_df_collect()) - nrow(vcs)
        vals$warning.na.records <- if (vcs.nrow.diff != 0) {
          paste(
            "When processing census records,", vcs.nrow.diff,
            ifelse(vcs.nrow.diff == 1, "row was", "rows were"),
            "removed because of a NULL season_name, species, Beach, and/or census_date value"
          )

          # warning("When processing census records, ", vcs.nrow.diff,
          #         " rows were removed because of an NA species, Beach, and/or census_date value",
          #         immediate. = TRUE)
        } else {
          NULL
        }

        # Filter for week number
        if (input$summary_level_1 == "fs_multiple_week")
          vcs <- vcs %>% filter(week_num == input$week_num)

        # Filter for Beach
        if (input$summary_level_2 == "by_beach") {
          validate(
            need(input$beach, "Please select at least one beach name")
          )
          vcs <- vcs %>% filter(Beach %in% input$beach)
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

        vcs.summ <- census_df_summ() %>%
          select(where(is.character), where(is.Date), !!!as.list(input$age_sex))

        if (input$summary_level_3 == "by_sp") {
          grp.names.all <- c("season_name", "species", "Beach", "census_date")
          grp.syms <- syms(dplyr::intersect(grp.names.all, names(vcs.summ)))

          vcs.summ <- vcs.summ %>%
            pivot_longer(cols = where(is.numeric), names_to = "count_class", values_to = "count_value") %>%
            group_by(!!!grp.syms) %>%
            summarise(count_value = sum(count_value),
                      .groups = "drop") %>%
            arrange_season(season.df(), !!!syms(dplyr::intersect(grp.names.all[-1], names(vcs.summ))))
        }

        if (input$summary_level_1 == "fs_single" && input$plot_cumsum) {
          grp.names.all <- c("season_name", "species", "Beach")
          grp.syms <- syms(dplyr::intersect(grp.names.all, names(vcs.summ)))

          vcs.summ <- vcs.summ %>%
            group_by(!!!grp.syms) %>%
            mutate(across(where(is.numeric), cumsum)) %>%
            ungroup()
          # View(cbind(vcs.summ, vcs.summ2))
        }

        vcs.summ
      })


      ###############################################################################
      # Outputs

      #------------------------------------------------------------------------------
      ### Output table
      output$tbl <- renderDT({
        census_df() %>%
          nest(data = where(is.numeric)) %>%
          mutate(flag0 = pmap_lgl(list(data), function(i) all(i == 0))) %>%
          filter(!flag0) %>%
          unnest(cols = c(data)) %>%
          select(-flag0)
      }, options = list(scrollX = TRUE))


      #------------------------------------------------------------------------------
      ### Output plot
      output$plot <- renderPlot({
        #--------------------------------------------------------
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
          x.lab <- paste("Season, for week number", input$week_num)
        }

        y.lab <- if (input$plot_cumsum) "Count (cumulative sum)" else "Count"

        gg.title <- switch(
          input$type,
          afs_pup = "AFS Capewide Pup Census",
          afs_study_beach = "AFS Study Beach Census",
          phocid = "Weekly Phocid Census"
        )

        size.val <- 1.2


        #--------------------------------------------------------
        # This processing is done here so that output$tbl is wide
        grp.names.all <- c("species", "Beach", "census_date")
        grp.syms <- syms(dplyr::intersect(grp.names.all, names(census_df())))

        census.df <- if (input$summary_level_3 == "by_sp") {
          census_df() %>%
            factor_species() %>%
            arrange_season(season.df(), !!!grp.syms)
        } else {
          census_df() %>%
            pivot_longer(cols = where(is.numeric), names_to = "count_class", values_to = "count_value") %>%
            factor_species() %>%
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
              linetype = guide_legend(title = "species", order = 1)
            )

          validate(
            need("Beach" %in% names(census.df), "census plot: beach name error"),
            need(n_distinct(census.df$species) == 1, "census plot: beach-species error")
          )
          if (input$summary_level_3 == "by_sp") {
            ggplot.out <- ggplot.out +
              geom_point(aes(color = Beach, size = size.val)) +
              geom_line(aes(group = Beach, color = Beach))
          } else {
            ggplot.out <- ggplot.out +
              geom_point(aes(shape = count_class, color = Beach, size = size.val)) +
              geom_line(aes(group = interaction(Beach, count_class), color = Beach))
          }

        } else {
          ### Color-code lines and points by species
          ggplot.out <- ggplot(census.df, aes(x = !!x.val, y = count_value)) +
            guides(size = FALSE)

          if (input$summary_level_3 == "by_sp") {
            ggplot.out <- ggplot.out +
              geom_point(aes(color = species, size = size.val)) +
              geom_line(aes(group = species, color = species))
          } else {
            ggplot.out <- ggplot.out +
              geom_point(aes(shape = count_class, color = species, size = size.val)) +
              geom_line(aes(group = interaction(species, count_class), color = species))
          }
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
    }
  )
}
