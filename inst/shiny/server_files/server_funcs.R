### Functions specific to the amlrPinnipeds Shiny app


#------------------------------------------------------------------------------
### Sort x by corresponding season_open_date (descending), then other user-provided columns
arrange_season_info <- function(x, ...) {
  stopifnot(
    inherits(x, "data.frame"),
    "season_name" %in% names(x),
    exists("season.info")
  )

  season.info.sel <- season.info %>% select(season_name, season_open_date)
  x.out <- left_join(x, season.info.sel, by = "season_name") %>%
    arrange(desc(season_open_date), ...) %>%  # TODO also by ...
    select(-season_open_date)

  if (nrow(x) != nrow(x.out)) stop("Error in arrange_season_info() - additional rows")

  x.out
}


#------------------------------------------------------------------------------
### Make the 'species' column a factor with the levels of pinniped.sp.levels
as_factor_species <- function(x) {
  stopifnot(
    inherits(x, "data.frame"),
    "species" %in% names(x),
    exists("pinniped.sp.levels")
  )

  x %>% mutate(species = factor(species, levels = pinniped.sp.levels))
}

#------------------------------------------------------------------------------
