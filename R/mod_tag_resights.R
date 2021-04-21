#' Shiny module for Tag Resights tab
#'
#' Shiny module for Tag Resights tab
#'
#' @name mod_tag_resights
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @export
mod_tag_resights_ui <- function(id) {
  ns <- NS(id)

  pinniped.sp.list.tr <- amlrPinnipeds::pinniped.sp.list[
    c("Fur seal", "Elephant seal", "Leopard seal", "Weddell seal")
  ]

  # assemble UI elements
  tagList(
    fluidRow(
      column(6, mod_output_ui(ns("tr_out"))),
      column(
        width = 6,
        fluidRow(
          box(
            title = "Plot info", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
            fluidRow(
              column(4, radioButtons(ns("type"), label = tags$h5("Data to plot"),
                                     choices = list("Individuals by year" = "ind_by_year",
                                                    "Total resights by year" = "tot_by_year"),
                                     selected = NULL))
            )
          ),
          box(
            title = "Filters", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
            fluidRow(
              mod_season_range_ui(ns("season_info"), 4),
              column(4, checkboxGroupInput(ns("species"), label = tags$h5("Species"),
                                           choices = pinniped.sp.list.tr,
                                           selected = "fur seal")),

            )
          )
        )
      )
    )
  )
}



#' @name mod_tag_resights
#'
#' @param pool reactive; a DBI database connection pool. Intended to be the output of \code{\link{mod_database_server}}
#' @param season.df reactive; the season info data frame.
#'   Intended to be the first element (\code{season.df}) of the (list) output of \code{\link{mod_season_filter_server}}
#' @param season.id.list reactive; the (named)list of the season info ID values.
#'   Intended to be the second element (\code{season.id.list}) of the (list) output of \code{\link{mod_season_filter_server}}
#' @param plot.height numeric, height of plot in pixels
#'
#' @export
mod_tag_resights_server <- function(id, pool, season.df, season.id.list, plot.height) {
  stopifnot(
    is.reactive(pool),
    is.reactive(season.df),
    is.reactive(season.id.list)
  )

  moduleServer(
    id,
    function(input, output, session) {
      #########################################################################
      tbl_pinnipeds_species <- reactive({
        tbl(pool(), "pinnipeds_species") %>% collect()
      })

      tr_si <- mod_season_range_server("season_info", season.id.list)


      ### Data frame input for both plot and table
      tr_tbl_group_df <- reactive({
        season.df.val <- season.df()
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

        season.info.tojoin <- season.df.val %>%
          select(season_info_id = ID) %>%
          filter(between(season_info_id, season.id.min, season.id.max))

        vtrs.summ %>%
          full_join(season.info.tojoin, by = "season_info_id") %>%
          full_join(tr.species.df, by = c("species" = "species")) %>%
          complete(species, season_info_id, fill = list(count = 0, count_distinct_pinnipeds = 0)) %>%
          filter(!is.na(species), !is.na(season_info_id)) %>%
          left_join(select(season.df.val, season_info_id = ID, season_name), by = "season_info_id") %>%
          mutate(species = str_to_sentence(species)) %>%
          arrange_season(season.df.val, species)
      })


      #########################################################################
      ### Output plot
      plot_output <- reactive({
        if (input$type == "ind_by_year") {
          y.val <- "count_distinct_pinnipeds"
          y.lab <- "Resight count - distinct pinnipeds"

        } else if (input$type == "tot_by_year") {
          y.val <- "count"
          y.lab <- "Resight count - total"

        } else {
          validate("tag resights - Sam has not made this plot yet")
        }

        ggplot(factor_species(tr_tbl_group_df()),
               aes(x = season_name, y = !!as.name(y.val), color = species, group = species)) +
          geom_point() +
          geom_line() +
          scale_color_manual(values = amlrPinnipeds::pinniped.sp.colors) +
          expand_limits(y = 0) +
          xlab("Season") +
          ylab(y.lab) +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      })


      ### Output table
      tbl_output <- reactive({
        tr_tbl_group_df() %>%
          select(Species = species, Season = season_name,
                 `Resight count - total` = count,
                 `Resight count - distinct pinnipeds` = count_distinct_pinnipeds)
      })


      ### Send to output module
      observe(mod_output_server("tr_out", id, tbl_output, plot_output, plot.height))
    }
  )
}
