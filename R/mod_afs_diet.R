#' Shiny module for Tag Resights tab
#'
#' Shiny module for Tag Resights tab
#'
#' @name mod_afs_diet
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @export
mod_afs_diet_ui <- function(id) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    fluidRow(
      column(
        width = 6,
        mod_output_ui(ns("output"))
      ),
      column(
        width = 6,
        fluidRow(
          box(
            title = "Top level...", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
            fluidRow(
              column(4, radioButtons(ns("type"), label = tags$h5("Data type"),
                                     choices = list("Scat samples" = "scat",
                                                    "Carapaces" = "carapace",
                                                    "Non-krill (fish and squid)" = "nonkrill"),
                                     selected = "scat")),
              column(4, radioButtons(ns("summary_level_season"), label = tags$h5("Summary level"),
                                     choices = list("Multiple seasons - total" = "fs_multiple_total",
                                                    "Multiple seasons - weekly" = "fs_multiple_week",
                                                    "Single season" = "fs_single",
                                                    "Raw data" = "raw"),
                                     selected = "fs_multiple_total")),
              column(
                width = 4,
                conditionalPanel(
                  condition = "input.summary_level_season != 'raw'", ns = ns,
                  conditionalPanel(
                    condition = "input.type == 'carapace'", ns = ns,
                    radioButtons(ns("carapace_toplot"), tags$h5("Krill data to plot"),
                                 choices = list("Measurements" = "measurements",
                                                "Sexes" = "sexes"),
                                 selected = "measurements")
                  ),
                  conditionalPanel(
                    condition = "(input.type == 'carapace' && input.carapace_toplot == 'sexes') || input.type == 'scat'", ns = ns,
                    radioButtons(ns("plot_y_unit"), tags$h5("Y-axis unit"),
                                 choices = list("Count" = "count", "Percentage" = "percentage"),
                                 selected = "count")
                  ),
                  conditionalPanel(
                    condition = "(input.type == 'carapace' && input.carapace_toplot == 'measurements')", ns = ns,
                    checkboxGroupInput(ns("plot_measurements"), tags$h5("Measurements to plot"),
                                       choices = list("Carapace length" = "carapace_length",
                                                      "Carapace width" = "carapace_width",
                                                      "Krill length" = "krill_length"),
                                       selected = c("carapace_length", "carapace_width", "krill_length"))
                  )
                )
              )
            ),
            tags$h5("For AFS Diet data, 'weekly' refers to the diet/scat week for that particular season.",
                    "See the 'Database and season info' tab for the AFS Diet study start date for each season with data")
          ),
          box(
            title = "Filters", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
            fluidRow(
              mod_season_filter_ui(ns("season_filter"), col.width = 4)
            ),
            uiOutput(ns("raw_columns_uiOut_selectize"))
          )
        )
      )
    )
  )
}



#' @name mod_afs_diet
#'
#' @param pool reactive; a DBI database connection pool. Intended to be the output of \code{\link{mod_database_server}}
#' @param season.df reactive; the season info data frame.
#'   Intended to be the first element (\code{season.df}) of the (list) output of \code{\link{mod_season_filter_server}}
#' @param season.id.list reactive; the (named)list of the season info ID values.
#'   Intended to be the second element (\code{season.id.list}) of the (list) output of \code{\link{mod_season_filter_server}}
#' @param plot.height numeric, height of plot in pixels
#'
#' @export
mod_afs_diet_server <- function(id, pool, season.df, season.id.list, plot.height) {
  stopifnot(
    is.reactive(pool),
    is.reactive(season.df),
    is.reactive(season.id.list)
  )

  moduleServer(
    id,
    function(input, output, session) {
      #########################################################################
      ### RenderUIs
      raw_columns_uiOut_selectize <- renderUI({
        req(input$summary_level_season == "raw")
        cols.list <- as.list(sort(unique(names(diet_df_collect()))))

        selectInput(
          session$ns("raw_columns"), tags$h5("Columns to include in table"),
          choices = cols.list, selected = unlist(cols.list),
          multiple = TRUE, selectize = TRUE
        )
      })

      #########################################################################
      ### Get filter values
      diet_filter <- reactive({
        tbl.sql <- tbl(req(pool()), "vDiet_Scats_Season") %>%
          select(season_info_id, diet_scat_date, date_column = collection_date)

        mod_season_filter_server(
          "season_filter",  reactive(input$summary_level_season), season.df, season.id.list,
          reactive(tbl.sql), week.type = "diet"
        )
      })


      ### Data frame input for both plot and table
      diet_df_collect <- reactive({
        tbl.name <- switch(
          input$type,
          "scat" = "vDiet_Scats_Season",
          "carapace" = "vDiet_Scats_Season_Carapaces",
          "nonkrill" = "vDiet_Scats_Season_NonKrills"
        )

        z <- diet_filter()
        validate(
          need(input$type != "nonkrill", "Not ready to process nonkrill data yet")
        )

        ds.sql.pre <- tbl(req(pool()), tbl.name)

        ds.sql <- if (input$summary_level_season %in% c("fs_multiple_total", "fs_multiple_week")) {
          ds.sql.pre %>%
            filter(between(season_info_id, !!req(z$season_min()), !!req(z$season_max())))

        } else if (input$summary_level_season == "fs_single") {
          ds.sql.pre %>%
            filter(season_info_id == !!req(z$season_select()),
                   between(collection_date, !!req(z$date_range())[1], !!req(z$date_range())[2]))

        } else if (input$summary_level_season == "raw") {
          ds.sql.pre
        } else {
          validate("invalid input$summary_level_season value")
        }

        ds.sql %>%
          collect() %>%
          select(-season_info_id, -season_open_date, -season_close_date, -contains("created_dt")) %>%
          mutate(species = "AFS", site = "CS") %>%
          amlrPinnipeds::afs_diet_week(diet_scat_date, collection_date) %>%
          select(season_name, site, species, week_num, week_start_date, everything()) %>%
          arrange(as.Date(collection_date), diet_scat_id)
      })


      ### Filter by week number
      diet_df_collect_wk <- reactive({
        if (input$summary_level_season == "fs_multiple_week") {
          diet_df_collect() %>% filter(week_num == !!req(diet_filter()$week_num()))
        } else {
          diet_df_collect()
        }
      })


      #########################################################################
      ### Select/rename columns depending on the data type.?
      diet_df_select <- reactive({
        # diet_df_collect_wk() %>%
        #   select(season_name, site, species, diet_scat_id, week_num, collection_date, collector,
        #          process_date, processor,
        #          carapace_length = length, carapace_width = width, carapace_comments)



        # select(season_name, site, species, diet_scat_id, carapace_id, collection_date,
        #        carapace_length = length, carapace_width = width, carapace_comments) %>%
      })



      #########################################################################
      ### Process filtered data
      diet_df_proc <- reactive({
        #------------------------------------------------
        if (input$type == "scat") {
          # Processing for scat data
          if (input$summary_level_season == "raw") {
            diet_df_collect_wk()

          } else {
            diet_scat_summ <- function(x, ...) {
              x %>%
                group_by(...) %>%
                summarise(scat_count = n(),
                          krill_count = sum(krill_type == "Yes"),
                          fish_count = sum(fish_type == "Yes"),
                          squid_count = sum(squid_type == "Yes"))
            }

            if (input$summary_level_season == "fs_multiple_total") {
              diet_scat_summ(diet_df_collect_wk(), season_name)
            } else if (input$summary_level_season %in% c("fs_multiple_week", "fs_single")) {
              diet_scat_summ(diet_df_collect_wk(), season_name, week_num, week_start_date)
            }

            #----------------------------------------------
          }
        } else if (input$type == "carapace") {
          # Processing for carapace data
          carapace.df <- diet_df_collect_wk() %>%
            select(season_name, site, species, week_num, week_start_date, diet_scat_id, carapace_id,
                   collection_date, collector, process_date, processor, tag, carapace_save,
                   diet_scat_comments, carapace_num,
                   carapace_length = length, carapace_width = width, carapace_comments) %>%
            krill_length_regression(carapace_length, carapace_width) %>%
            arrange(as.Date(collection_date), diet_scat_id, carapace_id)

          if (input$summary_level_season == "raw") {
            carapace.df

          } else {
            diet_carapace_summ <- function(x, ...) {
              x %>%
                group_by(...) %>%
                summarise(carapace_count = n(),
                          carapace_length = round(mean(carapace_length, na.rm = TRUE), 2),
                          carapace_width = round(mean(carapace_width, na.rm = TRUE), 2),
                          krill_length = round(mean(krill_length, na.rm = TRUE), 2),
                          female_count = sum(sex == "F", na.rm = TRUE),
                          male_count = sum(sex == "M", na.rm = TRUE),
                          juv_count = sum(is.na(sex)))
            }
            if (input$summary_level_season == "fs_multiple_total") {
              diet_carapace_summ(carapace.df, season_name)
            } else if (input$summary_level_season %in% c("fs_multiple_week", "fs_single")) {
              diet_carapace_summ(carapace.df, season_name, week_num, week_start_date)
            }
          }

          #----------------------------------------------
        } else if (input$type == "nonkrill") {
          # Processing for non-krill data
          browser()

          #----------------------------------------------
        } else {
          validate("invalid input$type argument")
        }
      })


      #########################################################################
      # Reactives that generate plots for specific input$type values

      #----------------------------------------------------
      ### Scat
      plot_scat <- reactive({
        # Generate plot-specific summary data frame
        validate(
          need(nrow(diet_df_proc()) > 0, "No data to plot"),
          need(input$summary_level_season != "raw", "Cannot plot raw scat data")
        )

        df.toplot <- diet_df_proc() %>%
          rename(krill = krill_count, fish = fish_count, squid = squid_count) %>%
          pivot_longer(cols = krill:squid, names_to = "type", values_to = "type_present") %>%
          mutate(type_present_perc = (type_present / scat_count) * 100)


        # Set labels, etc, based on user inputs
        x.lab <- if (input$summary_level_season == "fs_multiple_week") {
          paste("Season, for week number", req(diet_filter()$week_num()))
        } else {
          "Season"
        }

        if (input$summary_level_season == "fs_single") {
          x.var <- as.name("week_start_date")
          x.lab <- "Week start date"
          plot.title <- paste("Scat types, for season", unique(df.toplot$season_name))
          x.axis <- scale_x_date(breaks = sort(unique(df.toplot$week_start_date)))

        } else {
          x.var <- as.name("season_name")
          plot.title <- "Scat types"
          x.axis <- NULL
        }

        if (input$plot_y_unit == "percentage") {
          y.var <- as.name("type_present_perc")
          y.lab <- "Percentage (%)"
          y.axis.vals <- seq(0, 100, by = 10)

        } else {
          y.var <- as.name("type_present")
          y.lab <- "Count"

          # The extra bits are to handle both single and multiple seasons
          by.val <- ifelse(max(df.toplot$type_present) > 15, 10, 1)
          y.max <- ceiling(max(10, df.toplot$type_present) / 10) * 10
          # y.axis.vals <- seq(0, max(10,c df.toplot$type_present), by = by.val)
          y.axis.vals <- seq(0, y.max, by = by.val)
        }
        y.axis <- scale_y_continuous(breaks = y.axis.vals)

        # Generate plot
        ggplot(df.toplot, aes(!!x.var, !!y.var, color = type)) +
          geom_point() +
          geom_path(aes(group = type)) +
          expand_limits(y = 0) +
          xlab(x.lab) +
          ylab(y.lab) +
          ggtitle(plot.title) +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
          y.axis +
          x.axis
      })


      #----------------------------------------------------
      ### Carapaces
      plot_carapace <- reactive({
        # df.toplot.orig <- diet_df_proc()
        validate(
          need(nrow(diet_df_proc()) > 0, "No data to plot")
        )

        if (input$summary_level_season == "raw") {
          # Plot raw carapace data
          df.toplot <- diet_df_proc() %>%
            pivot_longer(cols = carapace_length:carapace_width, names_to = "measurement", values_to = "mm")

          ggplot(df.toplot) +
            geom_histogram(aes(x = mm, color = measurement, fill = measurement),
                           position = "dodge", binwidth = 1)
        } else {
          # Plot not-raw carapace data
          # browser()
          if (input$carapace_toplot == "measurements") {
            validate(
              need(input$plot_measurements, "Please select at least one measurement to plot")
            )
            df.toplot <- diet_df_proc() %>%
              pivot_longer(cols = carapace_length:krill_length, names_to = "measurement", values_to = "mm") %>%
              filter(measurement %in% input$plot_measurements)

            y.var <- as.name("mm")
            grp.var <- as.name("measurement")
            y.lab <- "Millimeters (mm)"
            plot.title.pre <- "Mean carapace measurements"

          } else if (input$carapace_toplot == "sexes") {
            df.toplot <- diet_df_proc() %>%
              rename(fem = female_count, male = male_count, juv = juv_count) %>%
              pivot_longer(cols = fem:juv, names_to = "sex", values_to = "count") %>%
              mutate(perc = count / carapace_count)

            if (input$plot_y_unit == "percentage") {
              y.var <- as.name("perc")
              y.lab <- "Percetnage (%)"
            } else {
              y.var <- as.name("count")
              y.lab <- "Count"
            }

            grp.var <- as.name("sex")
            plot.title.pre <- "Krill sex"
          }

          x.lab <- if (input$summary_level_season == "fs_multiple_week") {
            paste("Season, for week number", req(diet_filter()$week_num()))
          } else {
            "Season"
          }

          if (input$summary_level_season == "fs_single") {
            x.var <- as.name("week_start_date")
            x.lab <- "Week start date"
            plot.title <- paste0(plot.title.pre, ", for season ", unique(df.toplot$season_name))
            x.axis <- scale_x_date(breaks = sort(unique(df.toplot$week_start_date)))

          } else {
            x.var <- as.name("season_name")
            plot.title <- plot.title.pre
            x.axis <- NULL
          }

          # if (input$plot_y_unit == "percentage") {
          #   y.var <- as.name("type_present_perc")
          #   y.lab <- "Percentage (%)"
          #   y.axis.vals <- seq(0, 100, by = 10)
          #
          # } else {
          #   y.var <- as.name("type_present")
          #   y.lab <- "Count"
          #
          #   # The extra bits are to handle both single and multiple seasons
          #   by.val <- ifelse(max(df.toplot$type_present) > 15, 10, 1)
          #   y.max <- ceiling(max(10, df.toplot$type_present) / 10) * 10
          #   # y.axis.vals <- seq(0, max(10,c df.toplot$type_present), by = by.val)
          #   y.axis.vals <- seq(0, y.max, by = by.val)
          # }

          ggplot(df.toplot, aes(!!x.var, color = !!grp.var)) +
            geom_point(aes(y = !!y.var)) +
            geom_path(aes(y = !!y.var, group = !!grp.var)) +
            # expand_limits(y = 0) +
            xlab(x.lab) +
            ylab(y.lab) +
            ggtitle(plot.title) +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
            x.axis
        }
      })


      #########################################################################
      ### Output plot
      plot_output <- reactive({
        if (input$type == "carapace")  {
          plot_carapace()
        } else if (input$type == "scat") {
          plot_scat()
        } else {
          validate("Not ready yet")
        }
      })


      ### Output table
      tbl_output <- reactive({
        # if (input$)
        diet_df_proc()
        # diet_df()
      })


      ### Send to output module
      observe(mod_output_server("output", id, tbl_output, plot_output, plot.height))
    }
  )
}
