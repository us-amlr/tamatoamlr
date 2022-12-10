# amlrPinnipeds internal functions


#-------------------------------------------------------------------------------
### Summary function used by census tabs
.vcs_summ_func <- function(y, ..., season.df, beach.chr = FALSE) {
  df.out <-  y %>%
    group_by(...) %>%
    summarise(across(where(is.numeric), ~if_else(all(is.na(.x)), NA_integer_,
                                                 sum(.x, na.rm = TRUE))),
              Beaches = paste(sort(unique(Beach)), collapse = ", "),
              .groups = "drop") %>%
    arrange_season(season.df, species)

  if (!beach.chr) select(df.out, -Beaches) else df.out
}



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

.summaryTimingChoicesList <- list(
  "Multiple seasons - total" = "fs_total",
  "Multiple seasons - by week" = "fs_week",
  "Multiple seasons - date series" = "fs_date_series",
  # "Multiple seasons - by date" = "fs_date_single",
  "Single season" = "fs_single",
  "Raw data" = "fs_raw"
)
.summaryTimingChoices <- unlist(unname(.summaryTimingChoicesList))

.summaryTimingMultiple <- c("fs_total", "fs_week", "fs_date_series")
.summaryTimingSingle <- c("fs_single")


### Returns the radioButton widget for selecting the timing summary level
### choices argument is the choices that should be included
.summaryTimingUI <- function(
    ns, choices = .summaryTimingChoices, choices.selected = "fs_single"
) {
  choices.args <- match.arg(choices, several.ok = TRUE)

  choices.list.all <- .summaryTimingChoicesList
  #   list(
  #   "Multiple seasons - total" = "fs_total",
  #   "Multiple seasons - by week" = "fs_week",
  #   "Multiple seasons - date series" = "fs_date_series",
  #   # "Multiple seasons - by date" = "fs_date_single",
  #   "Single season" = "fs_single",
  #   "Raw data" = "fs_raw"
  # )

  if (!all(choices.args %in% choices.list.all))
    stop("Need to update internal function - please contact the database manager")

  choices.list <- choices.list.all[choices.list.all %in% choices.args]

  if (!(choices.selected %in% choices.list))
    stop("choices.selected must be one of the choices")

  radioButtons(ns("summary_timing"), label = tags$h5("Summary type"),
               choices = choices.list, selected = choices.selected)
}



#-------------------------------------------------------------------------------
### Returns the radioButton widget for selecting the location summary level
.summaryLocationUI <- function(
    ns,
    choices = c("by_capewide", "by_amlr", "by_beach"),
    choices.selected = "by_capewide")
{
  choices.args <- match.arg(choices, several.ok = TRUE)

  choices.list.all <- list(
    "Cape-wide" = "by_capewide",
    "AMLR study beaches" = "by_amlr",
    "By beach" = "by_beach"
  )

  if (!all(choices.args %in% choices.list.all))
    stop("Need to update internal function - please contact the database manager")

  choices.list <- choices.list.all[choices.list.all %in% choices.args]

  if (!(choices.selected %in% choices.list))
    stop("choices.selected must be one of the choices")

  list(
    radioButtons(ns("summary_location"), label = tags$h5("Location"),
                 choices = choices.list, selected = choices.selected),
    conditionalPanel(
      condition = "input.summary_location == 'by_beach'", ns = ns,
      checkboxInput(ns("location_aggregate"), "Group beaches", value = TRUE)
    )
  )
}




#-------------------------------------------------------------------------------
### Returns the radioButton widget for selecting the sp/age/sex summary level
.summarySpAgeSexUI <- function(
    ns,
    choices = c("by_sp", "by_sp_age_sex"),
    choices.selected = "by_sp")
{
  choices.args <- match.arg(choices, several.ok = TRUE)

  choices.list.all <- list(
    "By species" = "by_sp",
    # "By species and age class" = "by_sp_age",
    "By species, sex, and age class" = "by_sp_age_sex"
  )

  if (!all(choices.args %in% choices.list.all))
    stop("Need to update internal function - please contact the database manager")

  choices.list <- choices.list.all[choices.list.all %in% choices.args]

  if (!(choices.selected %in% choices.list))
    stop("choices.selected must be one of the choices")

  radioButtons(ns("summary_sas"), label = tags$h5("Species/age class/sex"),
               choices = choices.list, selected = choices.selected)
}

#-------------------------------------------------------------------------------
###Returns a list of all the species/color pairs present in a table
.colorsPresent <- function(table) {
  colors.all <- amlrPinnipeds::pinniped.sp.colors
  color.values <- colors.all[names(colors.all) %in% table$species]
  return(color.values)
}


