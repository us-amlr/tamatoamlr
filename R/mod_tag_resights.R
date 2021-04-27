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
          mod_season_filter_ui(ns("season_filter"), col.width = 4),
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
          column(4, uiOutput(ns("type_uiOut_radio")))
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

      output$warning_na_records <- renderUI({
        span(req(val.warning.na.records()), style = "color:red;")
      })


      # RenderUI For plot type
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

      #########################################################################
      # tbl_pinnipeds_species <- reactive({
      #   tbl(pool(), "pinnipeds_species") %>% collect()
      # })

      # Get dates
      tr_df_collect_pre <- reactive({
        # Validate checks - req() here and validate() in census_df_collect() so UIs don't get validate
        req(input$species)

        # Generate base sql query, passed to future reactives and season_filter module
        tbl(req(pool()), "vTag_Resights_Season") %>%
          filter(species %in% input$species)
      })

      tr_filter <- reactive({
        tbl.sql <- tr_df_collect_pre() %>%
          select(season_info_id, date_column = resight_date)

        mod_season_filter_server(
          "season_filter",  reactive(input$summary_level_1), season.df, season.id.list,
          reactive(tbl.sql)
        )
      })

      #########################################################################
      #------------------------------------------------------------------------
      ### Generate base sql query
      tr_sql <- reactive({
        validate(
          need(input$species, "Please select at least one species"),
          #TODO: incorporate leops
          need(!("leopard seal" %in% input$species), "Have not incorporated leop tag resights")
        )

        z <- tr_filter()


        # Start putting together tag_resights query
        tr.sql.pre <- tbl(req(pool()), "vTag_Resights_Season")

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
            paste("There were", nrow(tr.na),
                  "rows removed because of a NULL 'season_info_id' and/or 'pinniped_id' column")
          )
        }


        # Generate queries for other tables that will be joined in
        pinniped.tbl <- tbl(req(pool()), "pinnipeds") %>%
          select(pinniped_id = ID, species, cohort, pinniped_sex = sex) %>%
          mutate(species = tolower(species))

        # Filters, join in pinnipeds table
        tr.sql %>%
          left_join(pinniped.tbl, by = "pinniped_id") %>%
          filter(!is.na(season_info_id), !is.na(pinniped_id),
                 tolower(species) %in% !!input$species)
      })


      #------------------------------------------------------------------------
      ### Reactive with season info to use to complete data frame
      season_info_tojoin <- reactive({
        z <- tr_filter()

        season.info.tojoin <- season.df() %>%
          arrange(season_open_date) %>%
          select(season_info_id = ID, season_name) %>%
          filter(between(season_info_id, !!req(z$season_min()), !!req(z$season_max())))
      })


      ### Collect grouped data frame, depending on input type
      tr_collect <- reactive({
        req(input$type)
        # season.info.tojoin <- season_info_tojoin()

        # Generate queries for other tables that will be joined in
        tags.tbl <- tbl(req(pool()), "tags") %>%
          mutate(tag_info = tag_type + ISNULL(' | ' + color_f + '/' + color_m, ''))

        tags.df.primary <- tags.tbl %>%
          filter(Primary_Tag == 1) %>%
          select(pinniped_id = Pinniped_ID, primary_tag = tag, primary_tag_info = tag_info) %>%
          collect()

        tags.df.other <- tags.tbl %>%
          select(Tag_ID = ID, resight_tag = tag, resight_tag_info = tag_info) %>%
          collect()

        # Summarize and collect, depending on user selections
        tr.sql <- tr_sql()

        # browser()
        if (input$type %in% c("tot_ind_by_year", "tot_by_year")) {
          # Number of resights, summarized by season
          tr.sql %>%
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


        } else if (input$type == "ind_by_year") {
          # For an unclear (to Sam) reason, this code runs much faster with this collect() order
          tr.sql %>%
            collect() %>%
            left_join(tags.df.primary, by = "pinniped_id") %>%
            group_by(species, pinniped_id, primary_tag, primary_tag_info, season_name) %>%
            summarise(count = n(), .groups = "drop") %>%
            # collect() %>%
            mutate(season_name = factor(season_name, levels = season_info_tojoin()$season_name),
                   species = str_to_sentence(species)) %>%
            complete(season_name, fill = list(count = 0)) %>%
            arrange_season(season.df(), .desc = FALSE) %>%
            pivot_wider(id_cols = species:primary_tag_info, names_from = season_name,
                        values_from = count, values_fill = 0) %>%
            filter(!is.na(species)) %>% #get rid of NA from complete()
            arrange(species, primary_tag)


        } else if (input$type == "raw") {
          # For an unclear (to Sam) reason, this code runs much faster with this collect() order
          tr.sql %>%
            collect() %>%
            left_join(tags.df.primary, by = "pinniped_id") %>%
            left_join(tags.df.other, by = "Tag_ID") %>%
            # collect() %>%
            select(-season_open_date, season_close_date) %>%
            select(season_name, species, pinniped_id, cohort, pinniped_sex, primary_tag, primary_tag_info,
                   Tag_ID, resight_tag, resight_tag_info, everything())

        } else {
          validate("invalid input$type")
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

        ggplot(mutate_factor_species(tr_collect()),
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
          tr_collect() %>%
            select(Species = species, Season = season_name,
                   `Resight count - total` = count,
                   `Resight count - distinct pinnipeds` = count_distinct_pinnipeds)

        } else {
          tr_collect()
        }
      })


      ### Send to output module
      observe(mod_output_server("tr_out", id, tbl_output, plot_output))
    }
  )
}
