#' @name shiny_modules
#' @export
mod_dcc_raw_ui <- function(id) {
  ns <- NS(id)

  # assemble UI elements
  tagList(

  )
}



#' @name shiny_modules
#' @export
mod_dcc_raw_server <- function(id, pool, season.df) {
  stopifnot(
    is.reactive(pool),
    is.reactive(season.df)
  )

  moduleServer(
    id,
    function(input, output, session) {
      ##########################################################################
      # General
    }
  )
}
