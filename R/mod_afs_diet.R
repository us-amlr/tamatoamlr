#' @name shiny_modules
#' @export
mod_afs_diet_ui <- function(id) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    fluidRow(
      box(
        title = "Filters", status = "warning", solidHeader = FALSE, width = 5, collapsible = TRUE,
        fluidRow(
          column(12, mod_filter_season_ui(ns("filter_season"))),
          column(4, uiOutput(ns("week_num_uiOut_select")))
        )
      ),
      box(
        title = "Summary options", status = "warning", solidHeader = FALSE, width = 7, collapsible = TRUE,
        fluidRow(
          column(4, radioButtons(ns("type"), label = tags$h5("Data type"),
                                 choices = list("Scat samples" = "scat",
                                                "Carapaces" = "carapace",
                                                "Non-krill (fish and squid)" = "nonkrill"),
                                 selected = "scat")),
          # column(4, .summaryTimingUI(ns, c("fs_total", "fs_week", "fs_single", "fs_raw"))),
          column(4, .summaryTimingUI(ns, c("fs_total", "fs_week", "fs_single", "fs_raw"), "fs_total")),
          # column(4, radioButtons(ns("summary_timing"), label = tags$h5("Summary level"),
          #                        choices = list("Multiple seasons - total" = "fs_multiple_total",
          #                                       "Multiple seasons - weekly" = "fs_multiple_week",
          #                                       "Single season" = "fs_single",
          #                                       "Raw data" = "raw"),
          #                        selected = "fs_multiple_total")),
          column(
            width = 4,
            conditionalPanel(
              condition = "input.summary_timing != 'fs_raw'", ns = ns,
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
                             selected = "percentage")
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
                "See the 'Database and season info' tab for the AFS Diet study start date for each season")
      )
    ),
    mod_output_ui(ns("output"))
  )
}



#' @name shiny_modules
#' @export
mod_afs_diet_server <- function(id, pool, season.df, season.id.list) {
  stopifnot(
    is.reactive(pool),
    is.reactive(season.df),
    is.reactive(season.id.list)
  )

  moduleServer(
    id,
    function(input, output, session) {
      #########################################################################
      ### Week number dropdown
      output$week_num_uiOut_select <- renderUI({
        req(input$summary_timing == "fs_week")

        wk.list <- as.list(sort(unique(diet_filter_season()$week_num)))

        selectInput(
          session$ns("week_num"), tags$h5("Week number"),
          choices = wk.list, selected = min(unlist(wk.list))
        )
      })


      #########################################################################
      ### Data frame input for both plot and table
      diet_collect <- reactive({
        # tbl.name <- switch(
        #   input$type,
        #   "scat" = "vDiet_Scats_Season",
        #   "carapace" = "vDiet_Scats_Season_Carapaces",
        #   "nonkrill" = "vDiet_Scats_Season_NonKrills"
        # )

        cols.all <- c(
          "season_info_id", "season_name", "diet_scat_date", "diet_scat_id",
          "scat_num", "Location",
          "collection_date", "collector", "process_date", "processor",
          "krill_type", "fish_type", "squid_type", "tag", "carapace_save"
        )


        ds.sql <- if (input$type == "scat") {
          tbl(req(pool()), "vDiet_Scats_Season") %>%
            select(!!cols.all, diet_scat_notes = notes)

        } else if (input$type == "carapace") {
          tbl(req(pool()), "vDiet_Scats_Season_Carapaces") %>%
            select(!!cols.all, diet_scat_notes,
                   carapace_id, carapace_num, length, width, carapace_notes)

        } else if (input$type == "nonkrill") {
          tbl(req(pool()), "vDiet_Scats_Season_NonKrills") %>%
            select(!!cols.all, diet_scat_notes,
                   non_krill_id, species, side, erosion, amount,
                   unidentified_sub, slides_start, slides_end, non_krills_notes)

        } else (
          validate("Invalid type input value")
        )


        ds.sql %>%
          collect() %>%
          mutate(species = "AFS", site = "CS",
                 diet_scat_date = lubridate::ymd(diet_scat_date),
                 collection_date = lubridate::ymd(collection_date),
                 process_date = lubridate::ymd(process_date)) %>%
          amlrPinnipeds::afs_diet_week(diet_scat_date, collection_date) %>%
          select(season_name, site, species, week_num, week_start_date, everything()) %>%
          arrange(collection_date, scat_num)
      })


      ##########################################################################
      ### Get filter values
      filter_season <- reactive({
        mod_filter_season_server(
          "filter_season",  reactive(input$summary_timing),
          season.df, season.id.list
        )
      })


      ### Filter by season/date
      diet_filter_season <- reactive({
        validate(
          need(input$type != "nonkrill", "Not ready to process nonkrill data yet")
        )

        ds.df <- diet_collect()
        z <- filter_season()

        if (input$summary_timing %in% c("fs_total", "fs_week", "fs_raw")) {
          ds.df %>%
            filter(season_info_id %in% !!req(z$season_select()))

        } else if (input$summary_timing == "fs_single") {
          ds.df %>%
            filter(season_info_id == !!req(z$season_select()),
                   between(collection_date, !!req(z$date_range())[1], !!req(z$date_range())[2]))
        } else {
          validate("invalid input$summary_timing value")
        }
      })


      ### Filter by week number
      diet_filter <- reactive({
        if (input$summary_timing == "fs_week") {
          diet_filter_season() %>% filter(week_num == !!req(input$week_num))
        } else {
          diet_filter_season()
        }
      })


      #########################################################################
      ### Process filtered data
      diet_proc <- reactive({
        #------------------------------------------------
        if (input$type == "scat") {
          # Processing for scat data
          if (input$summary_timing == "fs_raw") {
            diet_filter()

          } else {
            diet_scat_summ <- function(x, ...) {
              x %>%
                group_by(...) %>%
                summarise(scat_count = n(),
                          krill_count = sum(krill_type == "Yes"),
                          fish_count = sum(fish_type == "Yes"),
                          squid_count = sum(squid_type == "Yes"))
            }

            if (input$summary_timing == "fs_total") {
              diet_scat_summ(diet_filter(), season_name)
            } else if (input$summary_timing %in% c("fs_week", "fs_single")) {
              diet_scat_summ(diet_filter(), season_name, week_num, week_start_date)
            }

            #----------------------------------------------
          }
        } else if (input$type == "carapace") {
          # Processing for carapace data
          carapace.df <- diet_filter() %>%
            select(season_name, site, species, week_num, week_start_date, diet_scat_id, carapace_id,
                   collection_date, collector, process_date, processor, tag, carapace_save,
                   diet_scat_notes, carapace_num,
                   carapace_length = length, carapace_width = width, carapace_notes) %>%
            krill_length_regression(carapace_length, carapace_width) %>%
            arrange(as.Date(collection_date), diet_scat_id, carapace_id)

          if (input$summary_timing == "fs_raw") {
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
            if (input$summary_timing == "fs_total") {
              diet_carapace_summ(carapace.df, season_name)
            } else if (input$summary_timing %in% c("fs_week", "fs_single")) {
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
          need(nrow(diet_proc()) > 0, "No data to plot"),
          need(input$summary_timing != "fs_raw", "Cannot plot raw scat data")
        )

        df.toplot <- diet_proc() %>%
          rename(krill = krill_count, fish = fish_count, squid = squid_count) %>%
          pivot_longer(cols = krill:squid, names_to = "type", values_to = "type_present") %>%
          mutate(type_present_perc = (type_present / scat_count) * 100)


        # Set labels, etc, based on user inputs
        x.lab <- if (input$summary_timing == "fs_week") {
          paste("Season, for week number", input$week_num)
        } else {
          "Season"
        }

        if (input$summary_timing == "fs_single") {
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
        # df.toplot.orig <- diet_proc()
        validate(
          need(nrow(diet_proc()) > 0, "No data to plot")
        )

        if (input$summary_timing == "fs_raw") {
          # Plot raw carapace data
          df.toplot <- diet_proc() %>%
            pivot_longer(cols = carapace_length:carapace_width, names_to = "measurement", values_to = "mm")

          ggplot(df.toplot) +
            geom_histogram(aes(x = mm, color = measurement, fill = measurement),
                           position = "dodge", binwidth = 1)
        } else {
          # Plot not-raw carapace data
          if (input$carapace_toplot == "measurements") {
            validate(
              need(input$plot_measurements, "Please select at least one measurement to plot")
            )
            df.toplot <- diet_proc() %>%
              pivot_longer(cols = carapace_length:krill_length, names_to = "measurement", values_to = "mm") %>%
              filter(measurement %in% input$plot_measurements)

            y.var <- as.name("mm")
            grp.var <- as.name("measurement")
            y.lab <- "Millimeters (mm)"
            plot.title.pre <- "Mean carapace measurements"

          } else if (input$carapace_toplot == "sexes") {
            df.toplot <- diet_proc() %>%
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

          x.lab <- if (input$summary_timing == "fs_week") {
            paste("Season, for week number", input$week_num)
          } else {
            "Season"
          }

          if (input$summary_timing == "fs_single") {
            x.var <- as.name("week_start_date")
            x.lab <- "Week start date"
            plot.title <- paste0(plot.title.pre, ", for season ", unique(df.toplot$season_name))
            x.axis <- scale_x_date(breaks = sort(unique(df.toplot$week_start_date)))

          } else {
            x.var <- as.name("season_name")
            plot.title <- plot.title.pre
            x.axis <- NULL
          }

          ggplot(df.toplot, aes(!!x.var, color = !!grp.var)) +
            geom_point(aes(y = !!y.var)) +
            geom_path(aes(y = !!y.var, group = !!grp.var)) +
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
        diet_proc()
      })


      ### Send to output module
      observe(mod_output_server("output", id, tbl_output, plot_output))
    }
  )
}
