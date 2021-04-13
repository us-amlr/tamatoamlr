# Code for...



output$afs_natal_plot <- renderPlot({
  # afs_natal_plot_df()


  # browser()

  season.id.min <- as.integer(input$afs_natal_season_min)
  season.id.max <- as.integer(input$afs_natal_season_max)

  # Get pinniped_season info
  vps <- tbl(vals.db$pool, "vPinniped_Season") %>%
    filter(between(season_info_id, season.id.min, season.id.max)) %>%
    collect()


  if (input$afs_natal_type == "overview") {
    # Summary pinniped_season things
    vps.summ.season <- vps %>%
      group_by(season_info_id, season_name) %>%
      summarise(count = n(),
                .groups = "drop")

    vps.summ.fate <- vps %>%
      group_by(season_info_id, season_name, pup_fate) %>%
      summarise(count = n(),
                .groups = "drop")

    # Get number of tag resights per field season
    vtrs.summ <- tbl(vals.db$pool, "vTag_Resights_Season") %>%
      left_join(tbl(vals.db$pool, "pinnipeds"), by = c("pinniped_id" = "ID")) %>%
      filter(tolower(species) == "fur seal",
             between(season_info_id, season.id.min, season.id.max)) %>%
      group_by(season_info_id, season_name) %>%
      summarise(afs_resights_unique = n_distinct(pinniped_id),
                afs_resights_total = n(),
                .groups = "drop") %>%
      arrange(season_name) %>%
      collect()


  } else if (input$afs_natal_type == "natal_rate") {
    # Natality rate - for each season, what was the rate of females that came back that gave birth?
    vps.summ.natal <- vps %>%
      group_by(season_info_id, season_name) %>%
      summarise(parturition_true = sum(parturition),
                parturition_false = sum(!parturition),
                count = n(),
                natality_rate = parturition_true / count,
                .groups = "drop")

    ggplot(vps.summ.natal, aes(x = season_name, y = natality_rate, group = "black")) +
      geom_point() +
      geom_line() +
      ylim(0, 1) +
      xlab("Season") +
      ylab("Natality rate")

  } else if (input$afs_natal_type == "prop_loss") {
    validate("Not ready yet")

  } else {
    validate("Sam forgot to make this plot - please bug him")
  }
})



