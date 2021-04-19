#' Shiny module for Tag Resights tab
#'
#' Shiny module for Tag Resights tab
#'
#' @name mod_afs_diet
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @export
mod_afs_diet_ui <- function(id) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    fluidRow(
      column(
        width = 6,
        mod_output_ui(ns("output"))
      ),
      column(
        width = 6,
        fluidRow(
          box(
            title = "Top level...", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
            fluidRow(
              column(4, radioButtons(ns("type"), label = tags$h5("Data type"),
                                     choices = list("Scat samples" = "scat",
                                                    "Carapaces" = "carapace",
                                                    "Non-krill (fish and squid)" = "nonkrill"),
                                     selected = "scat")),
              column(4, radioButtons(ns("summary_level_season"), label = tags$h5("Summary level"),
                                     choices = list("Multiple seasons - total" = "fs_multiple_total",
                                                    "Multiple seasons - weekly" = "fs_multiple_week",
                                                    "Single season" = "fs_single",
                                                    "Raw data" = "raw"),
                                     selected = "raw")),
              column(
                width = 4,
                conditionalPanel(
                  condition = "input.type == 'scat'", ns = ns,
                  checkboxInput(ns("percentage"), "Plot percentages")
                )
              )
            ),
            tags$h5("todo? descriptive text")
          ),
          box(
            title = "Filters", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
            fluidRow(
              # mod_season_range_ui(ns("season"), 4)
              mod_season_filter_ui(ns("season_filter"), col.width = 4)
            )
          )
        )
      )
    )
  )
}



#' @name mod_afs_diet
#'
#' @param pool reactive; a DBI database connection pool. Intended to be the output of \code{\link{mod_database_server}}
#' @param season.df reactive; the season info data frame.
#'   Intended to be the first element (\code{season.df}) of the (list) output of \code{\link{mod_season_filter_server}}
#' @param season.id.list reactive; the (named)list of the season info ID values.
#'   Intended to be the second element (\code{season.id.list}) of the (list) output of \code{\link{mod_season_filter_server}}
#'
#' @export
mod_afs_diet_server <- function(id, pool, season.df, season.id.list) {
  stopifnot(
    is.reactive(pool),
    is.reactive(season.df),
    is.reactive(season.id.list)
  )

  moduleServer(
    id,
    function(input, output, session) {
      #########################################################################
      diet_season_filter_val <- reactiveVal()

      week_list <- reactive({
        NULL
      })


      observe({
        diet_season_filter_val(mod_season_filter_server(
          "season_filter",  reactive(input$summary_level_season), season.df, season.id.list,
          reactive("vDiet_Scats_Season")
        ))
      })


      ### Data frame input for both plot and table
      diet_df <- reactive({
        # browser()
        # Can't have req() here or else it stops the whole function; they need to be whenever that widget becomes relevant
        season.filter.list <- diet_season_filter_val()
        season.id.min <- req(season.filter.list$season_min())
        season.id.max <- req(season.filter.list$season_max())
        season.id.sel <- req(season.filter.list$season_select())
        wk.val <- req(season.filter.list$week_num())
        date.range <- season.filter.list$date_range() #no req trial

        tbl.name <- switch(
          input$type,
          "scat" = "vDiet_Scats_Season",
          "carapace" = "vDiet_Scats_Season_Carapaces",
          "nonkrill" = "vDiet_Scats_Season_NonKrills"
        )

        validate(
          need(input$type == "carapace" && input$summary_level_season == "raw",
               "Can only currently do carapace + raw")
        )

        ds.tbl <- tbl(req(pool()), tbl.name) %>%
          filter(between(season_info_id, season.id.min, season.id.max),
                 !is.na(carapace_num)) %>%
          mutate(species = "AFS", site = "CS") %>%
          select(season_name, site, species, diet_scat_id, carapace_id, collection_date,
                 carapace_length = length, carapace_width = width, carapace_comments) %>%
          arrange(as.Date(collection_date), diet_scat_id, carapace_id)



        ds.tbl %>% collect()
      })

      diet_df_proc <- reactive({
        diet_df() %>%
          krill_length_regression(carapace_length, carapace_width)
      })


      #########################################################################
      ### Output plot
      plot_output <- reactive({
        df.toplot <- diet_df_proc() %>%
          # df.toplot <- diet_df() %>%
          pivot_longer(cols = carapace_length:carapace_width, names_to = "measurement", values_to = "mm")

        ggplot(df.toplot) +
          geom_histogram(aes(x = mm, color = measurement, fill = measurement),
                         position = "dodge",
                         # position = position_dodge(preserve = "single"),
                         binwidth = 1)
      })


      ### Output table
      tbl_output <- reactive({
        diet_df_proc()
        # diet_df()
      })


      ### Send to output module
      observe(mod_output_server("output", id, tbl_output, plot_output))
    }
  )
}
