# Server code for tag resights module

tagresights_mod_server <- function(id, vals.db.reac, vals.si.reac, ) {
  moduleServer(
    id,
    function(input, output, session) {
      ### Server code for tag resights tab
      pool <- reactiveVal()
      season.info <- reactiveVal()
      print("tr_mod_server")

      observe({
        print("obs pool")
        pool(vals.db.reac()$pool)
      })
      observe(season.info(vals.si.reac()$df))


      tbl_pinnipeds_species <- reactive({
        tbl(pool(), "pinnipeds_species") %>% collect()
      })


      tr_si <- seasoninfo_mod_server("season_info", vals.si.reac)


      ### Data frame input for both plot and table
      tr_tbl_group_df <- reactive({
        season.info <- season.info()
        season.id.min <- tr_si$minvar()
        season.id.max <- tr_si$maxvar()
        validate(
          need(!is.na(season.id.min) & !is.na(season.id.max), "Invalid season ID values")
        )

        validate(
          need(input$species, "Please select at least one species"),
          #TODO: incorporate leops
          need(!("leopard seal" %in% input$species), "Have not incorporated leop tag resights")
        )
        tr.species.str <- tolower(input$species)
        tr.species.df <- data.frame(species = tr.species.str, stringsAsFactors = FALSE)

        vtrs.summ <- tbl(pool(), "vTag_Resights_Season") %>%
          filter(between(season_info_id, season.id.min, season.id.max)) %>%
          left_join(tbl(pool(), "pinnipeds"), by = c("pinniped_id" = "ID")) %>%
          mutate(species = tolower(species)) %>%
          filter(species %in% tr.species.str) %>%
          group_by(species, season_info_id) %>%
          summarise(count = n(),
                    count_distinct_pinnipeds = n_distinct(pinniped_id),
                    .groups = "drop") %>%
          collect()

        season.info.tojoin <- season.info %>%
          select(season_info_id = ID) %>%
          filter(between(season_info_id, season.id.min, season.id.max))

        vtrs.summ %>%
          full_join(season.info.tojoin, by = "season_info_id") %>%
          full_join(tr.species.df, by = c("species" = "species")) %>%
          complete(species, season_info_id, fill = list(count = 0, count_distinct_pinnipeds = 0)) %>%
          filter(!is.na(species), !is.na(season_info_id)) %>%
          left_join(select(season.info, season_info_id = ID, season_name), by = "season_info_id") %>%
          mutate(species = str_to_sentence(species)) %>%
          arrange_season_info(season.info, species)
      })


      ### Output plot
      output$plot <- renderPlot({
        print("plot")
        if (input$type == "ind_by_year") {
          y.val <- "count_distinct_pinnipeds"
          y.lab <- "Resight count - distinct pinnipeds"

        } else if (input$type == "tot_by_year") {
          y.val <- "count"
          y.lab <- "Resight count - total"

        } else {
          validate("tag resights - Sam has not made this plot yet")
        }

        ggplot(as_factor_species(tr_tbl_group_df()),
               aes(x = season_name, y = !!as.name(y.val), color = species, group = species)) +
          geom_point() +
          geom_line() +
          scale_color_manual(values = pinniped.sp.colors) +
          expand_limits(y = 0) +
          xlab("Season") +
          ylab(y.lab) +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      })


      ### Output table
      output$tbl <- renderDT({
        print("table")
        tr_tbl_group_df() %>%
          select(Species = species, Season = season_name,
                 `Resight count - total` = count,
                 `Resight count - distinct pinnipeds` = count_distinct_pinnipeds)
      })

    }
  )
}
