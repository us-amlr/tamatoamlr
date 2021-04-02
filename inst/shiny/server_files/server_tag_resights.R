### Server code for tag resights tab

tbl_pinnipeds_species <- reactive({
  tbl(pool, "pinnipeds_species") %>% collect()
})

### Data frame input for both plot and table
tr_tbl_group_df <- reactive({
  season.id.min <- as.integer(input$tr_season_min)
  season.id.max <- as.integer(input$tr_season_max)

  validate(
    need(input$tr_species, "Please select at least one species"),
    #TODO: incorporate leops
    need(!("leopard seal" %in% input$tr_species), "Have not incorporated leop tag resights")
  )
  tr.species.str <- tolower(input$tr_species)
  tr.species.df <- data.frame(species = tr.species.str, stringsAsFactors = FALSE)

  vtrs.summ <- tbl(pool, "vTag_Resights_Season") %>%
    filter(between(season_info_id, season.id.min, season.id.max)) %>%
    left_join(tbl(pool, "pinnipeds"), by = c("pinniped_id" = "ID")) %>%
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
    arrange_season_info(species)
})


### Output plot
output$tr_plot <- renderPlot({
  if (input$tr_type == "ind_by_year") {
    y.val <- "count_distinct_pinnipeds"
    y.lab <- "Resight count - distinct pinnipeds"

  } else if (input$tr_type == "tot_by_year") {
    y.val <- "count"
    y.lab <- "Resight count - total"

  } else {
    validate("tag resights - tell Sam to do the plot")
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
output$tr_tbl <- renderDT({
  tr_tbl_group_df() %>%
    select(Species = species, Season = season_name,
           `Resight count - total` = count,
           `Resight count - distinct pinnipeds` = count_distinct_pinnipeds)
})
