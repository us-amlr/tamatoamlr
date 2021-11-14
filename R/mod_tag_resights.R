#' @name shiny_modules
#' @export
mod_tag_resights_ui <- function(id) {
  ns <- NS(id)

  pinniped.sp.list.tr <- amlrPinnipeds::pinniped.sp.list[
    c("Fur seal", "Elephant seal", "Leopard seal", "Weddell seal")
  ]

  # assemble UI elements
  tagList(
    fluidRow(
      box(
        title = "Filters", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
        fluidRow(
          mod_filter_season_ui(ns("filter_season"), col.width = 4),
          column(4, checkboxGroupInput(ns("species"), label = tags$h5("Species"),
                                       choices = pinniped.sp.list.tr,
                                       selected = "fur seal")),

        )
      ),
      box(
        title = "Plot info", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
        fluidRow(
          column(4, radioButtons(ns("summary_level_1"), label = tags$h5("Summary level 1"),
                                 choices = list("Multiple seasons - total" = "fs_multiple_total",
                                                # "Multiple seasons - weekly" = "fs_multiple_week",
                                                "Single season" = "fs_single"),
                                 selected = "fs_multiple_total")),
          column(4, uiOutput(ns("type_uiOut_radio"))),
          column(4, uiOutput(ns("fill_zero_uiOut_check")))
        )
      )
    ),
    mod_output_ui(ns("tr_out"), tags$br(), uiOutput(ns("warning_na_records")))
  )
}



#' @name shiny_modules
#' @export
mod_tag_resights_server <- function(id, pool, season.df, season.id.list) {
  stopifnot(
    is.reactive(pool),
    is.reactive(season.df),
    is.reactive(season.id.list)
  )

  moduleServer(
    id,
    function(input, output, session) {
      #########################################################################
      val.warning.na.records <- reactiveVal()

      ### Message about records filtered out
      output$warning_na_records <- renderUI({
        span(req(val.warning.na.records()), style = "color:red;")
      })


      ### Generate plot type widget
      output$type_uiOut_radio <- renderUI({
        choices.list <- if (input$summary_level_1 == "fs_multiple_total") {
          list("Total individuals by year" = "tot_ind_by_year",
               "Total resights by year" = "tot_by_year",
               "Individual resights by year" = "ind_by_year",
               "Raw data" = "raw")

        } else if (input$summary_level_1 == "fs_single") {
          list("Individual resights by year" = "ind_by_year",
               "Raw data" = "raw")

        } else {
          validate("invalid input$summary_level_1")
        }

        radioButtons(session$ns("type"), label = tags$h5("Data to plot"),
                     choices = choices.list)
      })

      ### Generate plot type widget
      output$fill_zero_uiOut_check <- renderUI({
        # req(input$type)
        req(input$type == "ind_by_year")
        checkboxInput(session$ns("fill_zero"), "Fill blanks with zeros", value = FALSE)
      })

      #########################################################################
      filter_season <- reactive({
        mod_filter_season_server(
          "filter_season",  reactive(input$summary_level_1), season.df, season.id.list,
          tbl.df = NULL #b/c we aren't doing any week number stuff
        )
      })


      #########################################################################
      ### Generate base sql query
      tr_sql <- reactive({
        validate(
          need(input$species, "Please select at least one species"),
          #TODO: incorporate leops
          need(!("leopard seal" %in% input$species), "Have not incorporated leop tag resights")
        )

        z <- filter_season()


        # Start putting together tag_resights query
        tr.sql.pre <- tbl(req(pool()), "vTag_Resights")

        # Filter by season
        tr.sql <- if (input$summary_level_1 == "fs_multiple_total") {
          tr.sql.pre %>%
            filter(between(season_info_id, !!req(z$season_min()), !!req(z$season_max())))

        } else if (input$summary_level_1 == "fs_single") {
          tr.sql.pre %>%
            filter(season_info_id == !!req(z$season_select()),
                   between(resight_date, !!req(z$date_range())[1], !!req(z$date_range())[2]))

        } else {
          validate("invalid input$summary_level_1")
        }

        # Generate message about NULL rows, if necessary
        tr.na <- tr.sql %>%
          filter(is.na(season_info_id) |is.na(pinniped_id)) %>%
          collect()

        if (nrow(tr.na) == 0) {
          val.warning.na.records(NULL)
        } else {
          val.warning.na.records(
            glue::glue(
              "There were {nrow(tr.na)} rows removed because of ",
              "a NULL 'season_info_id' and/or 'pinniped_id' column"
            )
          )
        }

        # # Get pinniped info, to use to filter by species
        # pinniped.tbl <- tbl(req(pool()), "pinnipeds") %>%
        #   select(pinniped_id = ID, species, cohort, pinniped_sex = sex) %>%
        #   mutate(species = tolower(species))

        # Filters, join in pinnipeds table
        tr.sql %>%
          # left_join(pinniped.tbl, by = "pinniped_id") %>%
          filter(!is.na(season_info_id), !is.na(pinniped_id),
                 tolower(species) %in% !!input$species)
      })


      #########################################################################
      #------------------------------------------------------------------------
      ### Reactive with season info to use to complete data frame
      season_info_tojoin <- reactive({
        z <- filter_season()

        season.info.tojoin <- season.df() %>%
          arrange(season_open_date) %>%
          select(season_info_id = ID, season_name)

        if (input$summary_level_1 == "fs_multiple_total") {
          season.info.tojoin %>%
            filter(between(season_info_id, !!req(z$season_min()), !!req(z$season_max())))

        } else if (input$summary_level_1 == "fs_single") {
          season.info.tojoin %>%
            filter(season_info_id == !!req(z$season_select()))

        } else {
          validate("invalid input$summary_level_1")
        }
      })

      ### Reactive with tags info to join to tag resights output
      tags_tojoin <- reactive({
        tags.tbl <- tbl(req(pool()), "tags") %>%
          mutate(tag_info = tag_type + ISNULL(' | ' + color_f + '/' + color_m, ''))

        tags.df.primary <- tags.tbl %>%
          filter(Primary_Tag == 1) %>%
          select(pinniped_id, primary_tag = tag, primary_tag_info = tag_info) %>%
          collect()

        tags.df.other <- tags.tbl %>%
          select(tag_id = ID, resight_tag = tag, resight_tag_info = tag_info) %>%
          collect()

        list(primary_df = tags.df.primary, other_df = tags.df.other)
      })


      #------------------------------------------------------------------------
      ### Collect and process 'total' summaries
      tr_collect_tot <- reactive({
        tr_sql() %>%
          group_by(species, season_name) %>%
          summarise(count = n(),
                    count_distinct_pinnipeds = n_distinct(pinniped_id),
                    .groups = "drop") %>%
          collect() %>%
          mutate(season_name = factor(season_name, levels = season_info_tojoin()$season_name),
                 species = str_to_sentence(species)) %>%
          mutate_factor_species(levels = str_to_sentence(input$species)) %>%
          complete(species, season_name, fill = list(count = 0, count_distinct_pinnipeds = 0)) %>%
          arrange(season_name)
      })


      ### Collect and process summary of individual resights by year
      tr_collect_ind_by_year <- reactive({
        # For an unclear (to Sam) reason, this code runs much faster with this collect() order
        tr_sql() %>%
          collect() %>%
          left_join(tags_tojoin()$primary_df, by = "pinniped_id") %>%
          group_by(species, pinniped_id, primary_tag, primary_tag_info, season_name) %>%
          summarise(count = n(), .groups = "drop") %>%
          # collect() %>%
          mutate(season_name = factor(season_name, levels = season_info_tojoin()$season_name),
                 species = str_to_sentence(species)) %>%
          complete(season_name, fill = list(count = 0)) %>%
          arrange_season(season.df(), .desc = FALSE) %>%
          pivot_wider(id_cols = species:primary_tag_info, names_from = season_name,
                      values_from = count) %>%
          filter(!is.na(species)) %>%
          #^ to get rid of NA from complete()
          arrange(species, primary_tag)
      })


      ### Collect and process raw data
      tr_collect_raw <- reactive({
        # For an unclear (to Sam) reason, this code runs much faster with this collect() order
        tr_sql() %>%
          collect() %>%
          left_join(tags_tojoin()$primary_df, by = "pinniped_id") %>%
          left_join(tags_tojoin()$other_df, by = "tag_id") %>%
          # collect() %>%
          select(season_name, species, pinniped_id, cohort, pinniped_sex, primary_tag, primary_tag_info,
                 tag_id, resight_tag, resight_tag_info, everything())
      })


      #########################################################################
      ### Collect grouped data frame, depending on input type
      tr_df <- reactive({
        req(input$type)

        # Summarize and collect, depending on user selections
        if (req(input$type) %in% c("tot_ind_by_year", "tot_by_year")) {
          tr_collect_tot()
        } else if (input$type == "ind_by_year") {
          tr_collect_ind_by_year()
        } else if (input$type == "raw") {
          tr_collect_raw()
        } else {
          validate("invalid input$type")
        }
      })


      ### Replace NAs with 0s, if specified by the user. Done here for efficiency
      tr_df_fill_zero <- reactive({
        if (!is.null(input$fill_zero)) {
          if (input$fill_zero) {
            tr_df() %>% mutate(across(where(is.numeric), ~replace_na(.x, 0)))
          } else {
            tr_df()
          }
        } else {
          tr_df()
        }
      })


      #########################################################################
      ### Output plot
      plot_output <- reactive({
        if (input$type == "tot_ind_by_year") {
          y.val <- "count_distinct_pinnipeds"
          y.lab <- "Resight count - distinct pinnipeds"

        } else if (input$type == "tot_by_year") {
          y.val <- "count"
          y.lab <- "Resight count - total"

        } else {
          validate("tag resights - no plot")
        }

        pd <- position_dodge2(width = 0.15)

        ggplot(mutate_factor_species(tr_df()),
               aes(x = season_name, y = !!as.name(y.val), color = species, group = species)) +
          geom_point(position = pd) +
          geom_line(position = pd) +
          scale_color_manual(values = amlrPinnipeds::pinniped.sp.colors) +
          expand_limits(y = 0) +
          xlab("Season") +
          ylab(y.lab) +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      })


      ### Output table
      tbl_output <- reactive({
        if (req(input$type) %in% c("tot_ind_by_year", "tot_by_year")) {
          tr_df() %>%
            select(Species = species, Season = season_name,
                   `Resight count - total` = count,
                   `Resight count - distinct pinnipeds` = count_distinct_pinnipeds)

        } else {
          tr_df_fill_zero()
        }
      })


      ### Send to output module
      observe(mod_output_server("tr_out", id, tbl_output, plot_output))
    }
  )
}
