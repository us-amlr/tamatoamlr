#' @name shiny_modules
#' @export
mod_afs_pinniped_season_ui <- function(id) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    fluidRow(
      box(
        title = "Filters", status = "warning", solidHeader = FALSE, width = 5, collapsible = TRUE,
        fluidRow(
          mod_filter_season_ui(ns("filter_season"), col.width = 6)
        ),
        conditionalPanel(
          condition = "input.type == 'natality' && input.plot_x_axis == 'age'", ns = ns,
          sliderInput(ns("filter_age"), tags$h5("Ages to plot"), min = 1, max = 25, value = c(4, 20))
        )
      ),
      box(
        title = "User selections", status = "warning", solidHeader = FALSE, width = 7, collapsible = TRUE,
        fluidRow(
          column(4, radioButtons(ns("type"), label = tags$h5("Data type"),
                                 choices = list("Overview - raw data" = "overview_raw",
                                                "Return rate" = "return",
                                                "Natality rate" = "natality",
                                                # "Proportional loss type" = "prop_loss",
                                                "Pup mortality" = "pup_mortality"),
                                 selected = "overview_raw")),
          # column(4, radioButtons(ns("summ_season"), label = tags$h5("Summary level"),
          #                        choices = list("Multiple seasons - total" = "fs_multiple_total",
          #                                       # "Multiple seasons - weekly" = "fs_multiple_week",
          #                                       "Single season" = "fs_single",
          #                                       "Raw data" = "raw"),
          #                        selected = "fs_multiple_total")),
          column(
            width = 4,
            conditionalPanel(
              condition = "input.type == 'pup_mortality'", ns = ns,
              radioButtons(ns("mortality_type"), tags$h5("Plot by:"),
                           choices = list("Mortality rate" = "rate",
                                          "Proportional loss" = "prop_loss"),
                           selected = "rate")
            ),
            conditionalPanel(
              condition = "input.type == 'natality'", ns = ns,
              radioButtons(ns("plot_x_axis"), tags$h5("Plot by:"),
                           choices = list("Season" = "season", "Age" = "age"),
                           selected = "season"),
              conditionalPanel(
                condition = "input.plot_x_axis == 'season'", ns = ns,
                checkboxInput(ns("prime"), "Plot prime age", value = TRUE)
              )
            )
          )
        ),
        tags$h5("'Adult female' refers to females that have pupped in at least one previous season."),
        textOutput(ns("type_description"))
      )
    ),
    mod_output_ui(ns("output"))
  )
}



#' @name shiny_modules
#' @export
mod_afs_pinniped_season_server <- function(id, pool, season.df, season.id.list) {
  stopifnot(
    is.reactive(pool),
    is.reactive(season.df),
    is.reactive(season.id.list)
  )

  moduleServer(
    id,
    function(input, output, session) {
      # Intended to be used in pmap_lgl, e.g. pmap_lgl(list(x, y), x_in_y)
      x_in_y <- function(i, j) {i %in% j}

      #########################################################################
      filter_season <- reactive({
        mod_filter_season_server(
          "filter_season",  reactive("fs_multiple_total"), season.df, season.id.list,
          NULL
        )
      })

      si_df <- reactive({
        season.df() %>% select(season_info_id = ID, season_name, season_open_date)
      })


      #########################################################################
      ### Descriptive text depending on the user-selected data type
      output$type_description <- renderText({
        txt.overview <- paste(
          "The raw data consists of all of the records from the pinniped_season table,",
          "joined with season/pinniped/tag resight",
          "data to get season names, pinniped cohort/age, and parturition beach (todo)."
        )
        txt.return <- "Return rate - by season, the percentage of adult females, resighted the previous year, that were resighted."
        txt.natality <- paste(
          "Natality rate - by season, the percentage of adult females, resighted the previous year,",
          "that pupped in each season (todo: on study beaches)."
        )
        txt.mortality.rate <- "The plot shows, out of all the pups born, the percentage that died from each mortality type."
        txt.mortality.prop <- paste(
          "The plot shows, of the potential pups that did not survive to adulthood,",
          "whether the loss was a) because an adult female did not pup or b) because of a type of mortality."
        )


        switch(
          input$type,
          "overview_raw" = txt.overview,
          "return" = txt.return,
          "natality" = txt.natality,
          "pup_mortality" = ifelse(input$mortality_type == "rate", txt.mortality.rate, txt.mortality.prop)
        )
      })


      #########################################################################
      ### Get 'base' pinniped_season info
      ps_collect <- reactive({
        ps.sql.pre <- tbl(req(pool()), "pinniped_season")  #TODO: use some view
        z <- filter_season()

        ps.sql <- if (input$type == "overview_raw") {
          ps.sql.pre
        } else {
          ps.sql.pre %>%
            filter(between(season_info_id, !!req(z$season_min()), !!req(z$season_max())))
        }

        # ps.sql <- if (input$summ_season %in% c("fs_multiple_total", "fs_multiple_week")) {
        #   ps.sql.pre %>%
        #     filter(between(season_info_id, !!req(z$season_min()), !!req(z$season_max())))
        #
        # } else if (input$summ_season == "fs_single") {
        #   ps.sql.pre %>%
        #     filter(season_info_id == !!req(z$season_select()))
        #   # between(collection_date, !!req(z$date_range())[1], !!req(z$date_range())[2]))
        #
        # } else if (input$summ_season == "raw") {
        #   ps.sql.pre
        # } else {
        #   validate("invalid input$summ_season value")
        # }

        # Join with season info and pinniped info
        ps.sql %>%
          collect() %>%
          left_join(si_df(), by = "season_info_id") %>%
          select(-created_dt) %>%
          select(pinniped_season_id, season_info_id, season_name, season_open_date,
                 everything())
      })


      ### Get pinniped info, roughly filtered for useful rows
      pinnipeds_ps_df <- reactive({
        tbl(req(pool()), "vPinnipeds_Primary_Tag") %>%
          filter(tolower(species) == "fur seal",
                 pinniped_id %in% !!req(ps_collect()$pinniped_id)) %>%
          select(pinniped_id, cohort, tag_primary = tag_display, tag_type) %>%
          collect()
      })

      ### Join in pinniped info to pinniped_season data
      ps_collect_pinniped <- reactive({
        ps_collect() %>%
          left_join(pinnipeds_ps_df(), by = "pinniped_id") %>%
          mutate(age = pinniped_age(season_open_date, cohort),
                 prime = between(age, 7, 17)) %>%
          # TODO: make the tag value be by date? Or add a separate column for this?
          select(pinniped_season_id, season_info_id, season_name, season_open_date,
                 pinniped_id, cohort, age, prime, tag_primary, tag_type, everything())
      })


      #########################################################################
      ### Get the first season in which each female pupped, to use to ID 'adult' females
      ps_adult_female_date <- reactive({
        tbl(req(pool()), "vPinniped_Season") %>%
          filter(parturition == 1) %>%
          group_by(pinniped_id) %>%
          summarise(season_open_date_first_pup = min(season_open_date, na.rm = TRUE)) %>%
          arrange(pinniped_id) %>%
          collect() %>%
          mutate(season_open_date_first_pup = as.Date(season_open_date_first_pup))
      })


      ### Get a data frame of the adult females, NOT grouped by season
      ps_adult_female_by_season <- reactive({
        # TODO: Call from ps_collect_pinniped()?
        afs.ad.fem.date <- ps_adult_female_date()
        ps.df <- ps_collect()

        ps.df %>%
          left_join(afs.ad.fem.date, by = "pinniped_id") %>%
          filter(season_open_date > season_open_date_first_pup) #%>%
        # group_by(season_name) %>%
        # summarise(count = n(),
        #           pinniped_id_list = list(sort(pinniped_id)))
      })


      #########################################################################
      ### Return rate
      return_rate <- reactive({
        # Get the pinniped IDs of all AFS that were 'adult females', at any point in time
        ps.ad.fem <- ps_adult_female_by_season()
        pinniped.id.ad.fem <- unique(ps.ad.fem$pinniped_id)

        # Get tag resights, grouped by season_name and pinniped_id
        tr.df <- tbl(req(pool()), "vTag_Resights") %>%
          group_by(season_name, pinniped_id) %>%
          summarise(resight_count = n(), .groups = "drop") %>%
          # Initial filter, for efficiency
          filter(pinniped_id %in% !!pinniped.id.ad.fem) %>%
          collect() %>%
          arrange(season_name, pinniped_id)

        # Create summary data frame of pinniped_ids seen each season
        tr.summ <- tr.df %>%
          group_by(season_name) %>%
          summarise(pinniped_id_list = list(sort(pinniped_id)))

        # Create a data frame with 'seals resighted in the previous season'
        tr.summ.prev <- tr.summ  %>%
          # TODO: Need a more robust way to determine the 'previous' season. Add column to season_info table?
          mutate(pinniped_id_list_prev_season = c(NA, head(pinniped_id_list, -1))) %>%
          filter(!is.na(pinniped_id_list_prev_season)) %>%
          select(season_name, pinniped_id_list_prev_season)


        # Filter by 1) this season is after the seal 'became an adult female' (gave birth),
        #   and 2) the seal was resighted the previous year
        x <- tr.df %>%
          left_join(si_df(), by = "season_name") %>%
          left_join(ps_adult_female_date(), by = "pinniped_id") %>%
          left_join(tr.summ.prev, by = "season_name") %>%
          filter(!is.na(pinniped_id_list_prev_season),
                 season_open_date > season_open_date_first_pup) %>%
          mutate(in_prev_season = pmap_lgl(list(pinniped_id, pinniped_id_list_prev_season), x_in_y)) %>%
          filter(in_prev_season)


        # Finally, group by season name and calculate the return rate
        x %>%
          group_by(season_name) %>%
          summarise(returner_count = n(),
                    denominator = unique(lengths(pinniped_id_list_prev_season)),
                    return_rate = returner_count / denominator)
      })


      #########################################################################
      ### Natality rate
      natality_rate <- reactive({
        req(ps_adult_female_by_season())
        # pinnipeds <- tbl(req(pool()), "pinnipeds") %>%
        #   filter(tolower(species) == "fur seal",
        #          ID %in% !!ps_adult_female_by_season()$pinniped_id) %>%
        #   select(pinniped_id = ID, cohort) %>%
        #   collect()

        # browser()

        # TODO: filter for only study beach animals


        ps.ad <- req(ps_adult_female_by_season()) %>%
          left_join(select(pinnipeds_ps_df(), pinniped_id, cohort), by = "pinniped_id") %>%
          mutate(age = pinniped_age(season_open_date, cohort),
                 prime = between(age, 7, 17)) %>%
          replace_na(list(prime = FALSE)) %>%
          # TODO: remove this after fixing cohorts
          filter(age > 0 | is.na(age))

        natal.summ <- function(x, ...) {
          x %>%
            group_by(...) %>%
            summarise(total_count = n(),
                      parturition_count = sum(parturition),
                      natality_rate = round(parturition_count / total_count, 2))
        }

        if (input$plot_x_axis == "season") {
          if (input$prime) {
            bind_rows(
              ps.ad %>% natal.summ(season_name) %>% mutate(type = "All"),
              ps.ad %>%
                filter(prime) %>%
                natal.summ(season_name) %>%
                mutate(type = "Prime")
            )
          } else {
            ps.ad %>%
              natal.summ(season_name) %>%
              mutate(type = "All")
          }

        } else if (input$plot_x_axis == "age") {
          ps.ad %>%
            filter(!is.na(age)) %>%
            natal.summ(age)
        }
      })


      #########################################################################
      ### Predation, top level
      mortality <- reactive({
        # table(ps_collect()$pup_mortality, useNA = "always")
        ps_collect() %>%
          # filter(parturition) %>%
          group_by(season_name) %>%
          summarise(total_count = sum(parturition),
                    natality_loss_count = sum(!parturition),
                    mortality_count = sum(!is.na(pup_mortality)),
                    mortality_leop_count = sum(pup_mortality == "leop", na.rm = TRUE),
                    mortality_leop_likely_count = sum(pup_mortality == "likely leop", na.rm = TRUE),
                    mortality_leop_total_count = mortality_leop_count + mortality_leop_likely_count,
                    mortality_neonate_count = sum(pup_mortality == "nn", na.rm = TRUE),
                    mortality_other_count = sum(pup_mortality == "other", na.rm = TRUE))
      })


      ### Predation - pup mortality percentages from total counts
      mortality_rate <- reactive({
        mortality() %>%
          mutate(mortality_predation_perc = round((mortality_leop_total_count / total_count), 4),
                 mortality_neonate_perc = round((mortality_neonate_count / total_count), 4),
                 mortality_other_perc = round((mortality_other_count / total_count), 4))
      })


      ### Predation - proportional loss type
      mortality_prop_loss <- reactive({
        mortality() %>%
          mutate(total_prop_loss_count = natality_loss_count + mortality_count,
                 natality_loss_perc = round(natality_loss_count / total_prop_loss_count, 4),
                 mortality_predation_perc = round((mortality_leop_total_count / total_prop_loss_count), 4),
                 mortality_neonate_perc = round((mortality_neonate_count / total_prop_loss_count), 4),
                 mortality_other_perc = round((mortality_other_count / total_prop_loss_count), 4)) %>%
          select(season_name, total_count, total_prop_loss_count, natality_loss_count, mortality_count,
                 everything())
      })


      #########################################################################
      ### Output plot
      plot_output <- reactive({
        validate(
          need(nrow(ps_collect()) > 0, "The database contains no pinniped_season data")
        )


        #--------------------------------------------------
        if (input$type == "overview_raw") {
          df.toplot <- ps_collect_pinniped() %>%
            group_by(season_name) %>%
            summarise(total = n(),
                      fem_with_pup = sum(parturition),
                      fem_no_pup = sum(!parturition)) %>%
            pivot_longer(cols = total:fem_no_pup, names_to = "type", values_to = "count") %>%
            mutate(type = factor(type, levels = c("total", "fem_with_pup", "fem_no_pup")))

          ggplot(df.toplot, aes(season_name, count, color = type, group = type)) +
            geom_line() +
            geom_path() +
            xlab("Season") +
            ylab("Count") +
            ggtitle("All females tracked") +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


          #------------------------------------------------
        } else if (input$type == "return") {
          ggplot(return_rate(), aes(season_name, return_rate)) +
            geom_point() +
            geom_path(group = 1) +
            xlab("Season") +
            ylab("Return rate") +
            ylim(c(0, 1)) +
            ggtitle(NULL) +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

          #------------------------------------------------
        } else if (input$type == "natality") {
          if (input$plot_x_axis == "season") {
            ggplot(natality_rate(),
                   aes(season_name, natality_rate, color = type, group = type)) +
              geom_point() +
              geom_path() +
              xlab("Season") +
              ylab("Natality rate") +
              # ylim(c(0, 1)) +
              ggtitle(NULL) +
              theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

          } else if (input$plot_x_axis == "age") {
            natality_rate() %>%
              filter(between(age, input$filter_age[1], input$filter_age[2])) %>%
              ggplot(aes(age, natality_rate)) +
              geom_point() +
              geom_path(aes(group = 1)) +
              xlab("Age") +
              ylab("Natality rate") +
              # ylim(c(0, 1)) +
              ggtitle(NULL)

          } else {
            validate("invalid input$plot_x_axis value")
          }

          #------------------------------------------------
        } else if (input$type == "pup_mortality") {
          ### Plot two different types of pup mortality
          mortality_long <- function(x) {
            x %>%
              select(season_name,
                     predation = mortality_predation_perc,
                     natality = natality_loss_perc,
                     neonate = mortality_neonate_perc,
                     other = mortality_other_perc) %>%
              pivot_longer(cols = predation:other, names_to = "type", values_to = "percentage") %>%
              mutate(type = factor(type, levels = c("predation", "natality", "neonate", "other")))
          }

          if (input$mortality_type == "rate") {
            # Prep for plotting mortality rates
            df.toplot <- mortality_rate() %>%
              mutate(natality_loss_perc = NA) %>%
              mortality_long() %>%
              filter(type != "natality")

            y.lab <- "Mortality rate"
            plot.title <- "Pup mortality - percentage of pups that died, by mortality type"
            y.cols <- scales::hue_pal()(4)[c(1, 3, 4)] #to make types the same color

          } else if (input$mortality_type == "prop_loss") {
            # Prep for plotting proportional loss
            df.toplot <- mortality_long(mortality_prop_loss())

            y.lab <- "Proportion of pup loss"
            plot.title <- "Proportional loss"
            y.cols <- scales::hue_pal()(4)

          } else {
            validate("invalid input$mortality_type value")
          }

          # Plot
          ggplot(df.toplot, aes(season_name, percentage, color = type, group = type)) +
            geom_point() +
            geom_path() +
            scale_color_manual(values = y.cols) +
            xlab("Season") +
            ylab(y.lab) +
            ylim(c(0, 1)) +
            ggtitle(plot.title) +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

          #------------------------------------------------
        } else {
          validate("Nope nope nope")
        }
      })


      ### Output table
      tbl_output <- reactive({
        req(nrow(ps_collect()) > 0)

        if (input$type == "overview_raw") {
          ps_collect_pinniped()
        } else if (input$type == "return") {
          return_rate()
        } else if (input$type == "natality") {
          natality_rate()
        } else if (input$type == "pup_mortality") {
          if (input$mortality_type == "rate") {
            mortality_rate()
          } else if (input$mortality_type == "prop_loss") {
            mortality_prop_loss()
          } else {
            validate("invalid input$mortality_type value")
          }
        } else {
          validate("Nope nada nein")
        }
      })


      ### Send to output module
      observe(mod_output_server("output", id, tbl_output, plot_output))
    }
  )
}
