### Server code for census tab


###############################################################################
# Generate SQL query, and collect data from census table
census_df_collect <- reactive({
  #----------------------------------------------
  # Set values
  census.type <- switch(
    input$census_type,
    afs_pup = "Capewide",
    afs_study_beach = "AFS Study Beach",
    phocid = "Phocid"
  )

  season.id.min <- as.integer(input$census_season_min)
  season.id.max <- as.integer(input$census_season_max)
  season.id.select <- as.integer(input$census_season_select)

  #----------------------------------------------
  # Generate base sql query
  vcs.sql <- tbl(pool, "vCensus_Season") %>%
    filter(census_type == census.type)

  # Add on season/date filters
  if (input$census_summary_level_1 == "fs_multiple") {
    vcs.sql <- vcs.sql %>%
      filter(between(season_info_id, season.id.min, season.id.max))

  } else if (input$census_summary_level_1 == "fs_single") {
    vcs.sql <- vcs.sql %>%
      filter(season_info_id == season.id.select,
             between(census_date, !!input$census_date_range[1], !!input$census_date_range[2]))

  } else {
    validate("invalid input$census_summary_level_1 value")
  }



  # Add on species filters (if applicable)
  if (input$census_type == "phocid") {
    vcs.sql <- vcs.sql %>%
      filter(tolower(species) %in% !!input$census_species)
  }

  # Collect query
  vcs <-  vcs.sql %>%
    collect() %>%
    mutate(species = str_to_sentence(species))

  #----------------------------------------------
  # For AFS Capewide pup census data, average across observer
  #   This feels like it should be in census_df(), but is here b/c of input$census_type reference
  if (input$census_type == "afs_pup") {
    vcs %>%
      group_by(season_name, species, Beach) %>%
      summarise(across(ad_female_count:unk_unk_count, ~round(mean(.x, na.rm = TRUE), 0), na.rm = TRUE),
                .groups = "drop")
    # TODO: only do this for pup counts, have census_df()'s summarise() use where(is.numeric)

  } else {
    vcs
  }
})


###############################################################################
### Process collected census data
census_df <- reactive({
  vcs <- census_df_collect()
  stopifnot(
    c("season_name", "species", "Beach") %in% names(vcs)
  )


  #----------------------------------------------
  # Summarize as specified
  vcs_summ_func <- function(y, ...) {
    y %>%
      group_by(...) %>%
      summarise(across(ad_female_count:unk_unk_count, sum, na.rm = TRUE),
                .groups = "drop") %>%
      complete(...) %>%
      mutate(across(ad_female_count:unk_unk_count, ~replace_na(.x, 0))) %>%
      arrange_season_info(species)
  }

  if (input$census_summary_level_2 == "by_capewide") {
    vcs %>% vcs_summ_func(season_name, species)
  } else if (input$census_summary_level_2 == "by_beach") {
    vcs %>% vcs_summ_func(season_name, species, Beach)
  } else {
    validate("invalid input$census_summary_level_2 value")
  }


  # else if (input$census_summary_level == "by_beach") {
  #   stopifnot(n_distinct(vcs$census_date) == 1)
  #   vcs.summ <- vcs %>%
  #     group_by(season_name, species, Beach) %>%
  #     summarise(across(ad_female_count:unk_unk_count, sum, na.rm = TRUE),
  #               .groups = "drop") %>%
  #     complete(season_name, species, Beach) %>%
  #     mutate(across(ad_female_count:unk_unk_count, ~replace_na(.x, 0)))
  #
  # }
})


###############################################################################
### Output table
output$census_tbl <- renderDT({
  census_df()
}, options = list(scrollX = TRUE))

### Output plot
output$census_plot <- renderPlot({
  if (input$census_summary_level_1 == "fs_multiple" && input$census_summary_level_2 == "by_capewide") {
    ggplot(census_df(),
           aes(x = season_name, y = ad_female_count, color = species, group = species)) +
      geom_point() +
      geom_line() +
      expand_limits(y = 0) +
      xlab("Season") +
      ylab("Adult female") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  } else if (input$census_summary_level_1 == "fs_single") {

  } else {
    validate("not ready yet")
  }
})
