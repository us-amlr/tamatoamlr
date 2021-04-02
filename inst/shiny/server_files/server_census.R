### Server code for census tab


###############################################################################
# Census-specific common values

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

### Store the selected beaches and column names
observe(vals$census.beaches <- input$census_beach)
observe(vals$census.cols <- input$census_age_sex)

observe({
  input$tabs
  input$census_type

  isolate({
    vals$census.beaches <- NULL
    vals$census.cols <- NULL
  })
})



### Update week num dropdown depending on the currently filtered for data
observe({
  week.list <- if (input$census_summary_level_1 == "fs_multiple_week") {
    as.list(sort(unique(census_df_collect()$week_num)))
  } else {
    NULL
  }

  updateSelectInput(session, "census_week_num", choices = week.list)
})


### Update the date range limits based on the selected field season
observe({
  if (input$census_summary_level_1 == "fs_single") {
    season.curr <- season.info %>% filter(ID == as.numeric(input$census_season_select))
    stopifnot(nrow(season.curr) == 1)
    start <- min <- season.curr[["season_open_date"]]
    end <- max <- season.curr[["season_close_date"]]

  } else {
    start <- min <- end <- max <- NULL
  }

  updateDateRangeInput(session, "census_date_range",
                       start = start, end = end, min = min, max = max)
})


###############################################################################
# RenderUIs

### Warning messages
output$census_warning_na_records <- renderUI({
  span(req(vals$census.warning.na.records), style = "color:red")
})

### Beaches dropdown
output$census_beach_uiOut_selectize <- renderUI({
  req(input$census_summary_level_2 == "by_beach")
  beaches.list <- as.list(sort(unique(census_df_collect()$Beach)))

  selectInput(
    "census_beach", tags$h5("Beach(es)"),
    choices = beaches.list, selected = vals$census.beaches,
    multiple = TRUE, selectize = TRUE
  )
})

### Columns dropdown
output$census_age_sex_uiOut_selectize <- renderUI({
  req(input$census_summary_level_3 == "by_sp_age_sex")

  tmp <- tbl(pool, "vCensus_Season") %>%
    head(1) %>%
    collect() %>%
    select(ad_female_count:unk_unk_count)

  choices.list <- switch(
    input$census_type,
    afs_pup = census.cols.afs.pup,
    afs_study_beach = census.cols.afs.study.beach,
    phocid = census.cols.phocid
  )
  selected.vals <- switch(
    input$census_type,
    afs_pup = census.cols.afs.pup,
    census.cols.afs.study.beach[[1]]
  )

  validate(need(choices.list, "invalid input$census_summary_level_1 value"))
  validate(
    need(all(unlist(choices.list) %in% names(tmp)),
         "Invalid column names, please report to Sam")
  )

  selectInput(
    "census_age_sex", tags$h5("Columns to plot"),
    choices = choices.list, selected = selected.vals,
    multiple = TRUE, selectize = TRUE
  )
})


###############################################################################
# Generate SQL query, and collect data from census table
census_df_collect <- reactive({
  vals$census.warning.na.records <- NULL

  # Validate checks
  validate(
    need(!(input$census_type != "phocid" && input$census_summary_level_3 == "by_sp"),
         "You can only summarize phocid census data 'by species'"),
    need(!(input$census_type != "phocid" && input$census_summary_level_1 == "fs_multiple_week"),
         "You can only summarize phocid census data by 'multiple seasons - weekly' (by weeks across multiple seasons)")
  )

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

  # Add on season/date filters. Week num filtering done below to not requery whole thing (??)
  if (input$census_summary_level_1 %in% census.summ1.mult) {
    vcs.sql <- vcs.sql %>%
      filter(between(season_info_id, season.id.min, season.id.max))

  } else if (input$census_summary_level_1 == "fs_single") {
    vcs.sql <- vcs.sql %>%
      filter(season_info_id == season.id.select,
             between(census_date, !!input$census_date_range[1], !!input$census_date_range[2]))

  } else {
    validate("invalid input$census_summary_level_1 value")
  }


  # Add on species filters, if applicable
  if (input$census_type == "phocid") {
    vcs.sql <- vcs.sql %>%
      filter(tolower(species) %in% !!input$census_species)
  }

  # Collect query
  vcs <-  vcs.sql %>%
    collect() %>%
    mutate(species = str_to_sentence(species),
           week_num = lubridate::week(census_date))

  #----------------------------------------------
  # For AFS Capewide pup census data, average across observer
  #   This feels like it should be in census_df(), but is here b/c of input$census_type reference
  if (input$census_type == "afs_pup") {
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
  vals$census.warning.na.records <- if (vcs.nrow.diff != 0) {
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
  if (input$census_summary_level_1 == "fs_multiple_week")
    vcs <- vcs %>% filter(week_num == input$census_week_num)

  # Filter for Beach
  if (input$census_summary_level_2 == "by_beach") {
    validate(
      need(input$census_beach, "Please select at least one beach name")
    )
    vcs <- vcs %>% filter(Beach %in% input$census_beach)
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
      arrange_season_info(species)
  }

  if (input$census_summary_level_2 == "by_capewide" && input$census_summary_level_1 == "fs_single") {
    vcs %>% vcs_summ_func(season_name, species, census_date)

  } else if (input$census_summary_level_2 == "by_capewide" && input$census_summary_level_1 %in% census.summ1.mult) {
    vcs %>% vcs_summ_func(season_name, species)

  } else if (input$census_summary_level_2 == "by_beach" && input$census_summary_level_1 == "fs_single") {
    vcs %>% vcs_summ_func(season_name, species, Beach, census_date)

  } else if (input$census_summary_level_2 == "by_beach" && input$census_summary_level_1 %in% census.summ1.mult) {
    vcs %>% vcs_summ_func(season_name, species, Beach)

  } else {
    validate("invalid input$census_summary_level_1 + input$census_summary_level_2 combo")
  }
})


#------------------------------------------------------------------------------
### Process collected census data, part 2 (summary level 3)
census_df <- reactive({
  validate(
    need(between(length(input$census_age_sex), 1, 6),
         "Please select between one and six columns to plot")
  )

  vcs.summ <- census_df_summ() %>%
    select(where(is.character), where(is.Date), !!!as.list(input$census_age_sex))

  if (input$census_summary_level_3 == "by_sp") {
    grp.names.all <- c("season_name", "species", "Beach", "census_date")
    grp.syms <- syms(dplyr::intersect(grp.names.all, names(vcs.summ)))

    vcs.summ <- vcs.summ %>%
      pivot_longer(cols = where(is.numeric), names_to = "count_class", values_to = "count_value") %>%
      group_by(!!!grp.syms) %>%
      summarise(count_value = sum(count_value),
                .groups = "drop") %>%
      arrange_season_info(!!!syms(dplyr::intersect(grp.names.all[-1], names(vcs.summ))))
  }

  if (input$census_summary_level_1 == "fs_single" && input$census_cumsum) {
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
output$census_tbl <- renderDT({
  census_df() %>%
    nest(data = where(is.numeric)) %>%
    mutate(flag0 = pmap_lgl(list(data), function(i) all(i == 0))) %>%
    filter(!flag0) %>%
    unnest(cols = c(data)) %>%
    select(-flag0)
}, options = list(scrollX = TRUE))


#------------------------------------------------------------------------------
### Output plot
output$census_plot <- renderPlot({
  #--------------------------------------------------------
  if (input$census_type == "phocid") {
    validate(
      need(input$census_summary_level_2 != "by_beach" || length(input$census_species) == 1,
           "When plotting phocid census data by beach, please select exactly one species")
    )
  }

  #--------------------------------------------------------
  # Set plot variable depending on user selections
  if (input$census_summary_level_1 == "fs_single") {
    x.val <- as.name("census_date")
    x.lab <- "Date"
  } else if (input$census_summary_level_1 %in% census.summ1.mult) {
    x.val <- as.name("season_name")
    x.lab <- "Season"
  } else {
    validate("census plot - invalid input$census_summary_level_1 value")
  }

  if (input$census_summary_level_1 == "fs_multiple_week") {
    x.lab <- paste("Season, for week number", input$census_week_num)
  }

  y.lab <- if (input$census_cumsum) "Count (cumulative sum)" else "Count"

  gg.title <- switch(
    input$census_type,
    afs_pup = "AFS Capewide Pup Census",
    afs_study_beach = "AFS Study Beach Census",
    phocid = "Weekly Phocid Census"
  )

  size.val <- 1.2


  #--------------------------------------------------------
  # This processing is done here so that output$census_tbl is wide
  grp.names.all <- c("species", "Beach", "census_date")
  grp.syms <- syms(dplyr::intersect(grp.names.all, names(census_df())))

  census.df <- if (input$census_summary_level_3 == "by_sp") {
    census_df() %>%
      as_factor_species() %>%
      arrange_season_info(!!!grp.syms)
  } else {
    census_df() %>%
      pivot_longer(cols = where(is.numeric), names_to = "count_class", values_to = "count_value") %>%
      as_factor_species() %>%
      arrange_season_info(!!!grp.syms, count_class)
  }

  validate(need(nrow(census.df) > 0, "No data to plot"))

  #--------------------------------------------------------
  # Plotting

  # Generate initial plot pieces that depend on user selection
  if (input$census_summary_level_2 == "by_beach") {
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
    if (input$census_summary_level_3 == "by_sp") {
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

    if (input$census_summary_level_3 == "by_sp") {
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
