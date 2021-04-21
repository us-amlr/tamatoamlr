#' Shiny module for AFS Natality and Pup Mortality tab
#'
#' Shiny module for AFS Natality and Pup Mortality tab
#'
#' @name mod_afs_pinniped_season
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @export
mod_afs_pinniped_season_ui <- function(id) {
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
              column(4, radioButtons(ns("type"), label = tags$h5("Summary type"),
                                     choices = list("Overview" = "overview",
                                                    "Return rate" = "return",
                                                    "Natality rate" = "natality",
                                                    "Pup mortality" = "pup_mortality",
                                                    "Proportional loss type" = "prop_loss"),
                                     selected = "overview")),
              column(4, radioButtons(ns("summ_season"), label = tags$h5("Summary level"),
                                     choices = list("Multiple seasons - total" = "fs_multiple_total",
                                                    # "Multiple seasons - weekly" = "fs_multiple_week",
                                                    "Single season" = "fs_single",
                                                    "Raw data" = "raw"),
                                     selected = "fs_multiple_total")),
              column(
                width = 4,
                conditionalPanel(
                  condition = "input.type == 'natality'", ns = ns,
                  radioButtons(ns("plot_x_axis"), tags$h5("Plot by:"),
                               choices = list("Season" = "season",
                                              "Age" = "age")),
                  conditionalPanel(
                    condition = "input.plot_x_axis == 'season'", ns = ns,
                    checkboxInput(ns("prime"), "Plot prime age", value = TRUE)
                  )
                )
              )
            ),
            tags$h5("todo")
          ),
          box(
            title = "Filters", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
            fluidRow(
              mod_season_filter_ui(ns("season_filter"), col.width = 4)
            ),
            conditionalPanel(
              condition = "input.type == 'natality' && input.plot_x_axis == 'age'", ns = ns,
              sliderInput(ns("filter_age"), tags$h5("Ages to plot"), min = 1, max = 25, value = c(4, 20))
            )
          )
        )
      )
    )
  )
}



#' @name mod_afs_pinniped_season
#'
#' @param pool reactive; a DBI database connection pool. Intended to be the output of \code{\link{mod_database_server}}
#' @param season.df reactive; the season info data frame.
#'   Intended to be the first element (\code{season.df}) of the (list) output of \code{\link{mod_season_filter_server}}
#' @param season.id.list reactive; the (named)list of the season info ID values.
#'   Intended to be the second element (\code{season.id.list}) of the (list) output of \code{\link{mod_season_filter_server}}
#' @param plot.height numeric, height of plot in pixels
#'
#' @export
mod_afs_pinniped_season_server <- function(id, pool, season.df, season.id.list, plot.height) {
  stopifnot(
    is.reactive(pool),
    is.reactive(season.df),
    is.reactive(season.id.list)
  )

  moduleServer(
    id,
    function(input, output, session) {
      # Intended to be used in pmap_lgl, e.g. pmap_lgl(list(x, y), x_in_y)
      x_in_y <- function(i, j) {i %in% j}

      #########################################################################
      season_filter <- reactive({
        mod_season_filter_server(
          "season_filter",  reactive(input$summ_season), season.df, season.id.list,
          NULL
        )
      })

      si_df <- reactive({
        season.df() %>% select(season_info_id = ID, season_name, season_open_date)
      })

      pinnipeds_afs_df <- reactive({
        tbl(req(pool()), "pinnipeds") %>%
          filter(tolower(species) == "fur seal") %>%
          collect()
      })

      #########################################################################
      ### Get 'base' pinniped_season info
      ps_collect <- reactive({
        ps.sql.pre <- tbl(req(pool()), "pinniped_season")  #TODO: use some view
        z <- season_filter()

        ps.sql <- if (input$summ_season %in% c("fs_multiple_total", "fs_multiple_week")) {
          ps.sql.pre %>%
            filter(between(season_info_id, !!req(z$season_min()), !!req(z$season_max())))

        } else if (input$summ_season == "fs_single") {
          ps.sql.pre %>%
            filter(season_info_id == !!req(z$season_select()))
          # between(collection_date, !!req(z$date_range())[1], !!req(z$date_range())[2]))

        } else if (input$summ_season == "raw") {
          ps.sql.pre
        } else {
          validate("invalid input$summ_season value")
        }

        # Join with season info
        ps.df <- ps.sql %>%
          collect() %>%
          left_join(si_df(), by = "season_info_id")
      })

      ### Get the first season in which each female pupped, to use to filter for 'adult' females
      ps_adult_female_date <- reactive({
        tbl(req(pool()), "vPinniped_Season_Season") %>%
          filter(parturition == 1) %>%
          group_by(pinniped_id) %>%
          summarise(season_open_date_first_pup = min(season_open_date, na.rm = TRUE)) %>%
          # summarise(season_name_min = season_name[season_open_date == min(season_open_date)])
          arrange(pinniped_id) %>%
          collect() %>%
          mutate(season_open_date_first_pup = as.Date(season_open_date_first_pup))
      })


      ### Get a data frame of the adult females, (NOT) grouped by season
      ps_adult_female_by_season <- reactive({
        afs.ad.fem.date <- ps_adult_female_date()
        ps.df <- ps_collect()

        ps.df %>%
          left_join(afs.ad.fem.date, by = "pinniped_id") %>%
          filter(season_open_date > season_open_date_first_pup) #%>%
        # group_by(season_name) %>%
        # summarise(count = n(),
        #           pinniped_id_list = list(sort(pinniped_id)))
      })


      #########################################################################
      ### Return rate
      return_rate <- reactive({
        validate(
          need(input$summ_season == "fs_multiple_total",
               "Return rate can only be summarized for each season")
        )

        # Get the pinniped IDs of all AFS that were 'adult females', at any point in time
        ps.ad.fem <- ps_adult_female_by_season()
        pinniped.id.ad.fem <- unique(ps.ad.fem$pinniped_id)

        # Get tag resights, grouped by season_name and pinniped_id
        tr.df <- tbl(req(pool()), "vTag_Resights_Season_Summary") %>%
          # Initial filter, for efficiency
          filter(pinniped_id %in% !!pinniped.id.ad.fem) %>%
          collect() %>%
          arrange(season_name, pinniped_id)

        # Create summary data frame of pinniped_ids seen each season
        tr.summ <- tr.df %>%
          group_by(season_name) %>%
          summarise(pinniped_id_list = list(sort(pinniped_id)))

        # Create a data frame with 'seals resighted in the previous season'
        tr.summ.prev <- tr.summ  %>%
          # TODO: Need a more robust way to determine the 'previous' season
          mutate(pinniped_id_list_prev_season = c(NA, head(pinniped_id_list, -1))) %>%
          filter(!is.na(pinniped_id_list_prev_season)) %>%
          select(season_name, pinniped_id_list_prev_season)


        # Filter by 1) this season is after the seal 'became an adult female' (gave birth),
        #   and 2) the seal was resighted the previous year
        x <- tr.df %>%
          left_join(si_df(), by = "season_name") %>%
          left_join(ps_adult_female_date(), by = "pinniped_id") %>%
          left_join(tr.summ.prev, by = "season_name") %>%
          filter(!is.na(pinniped_id_list_prev_season),
                 season_open_date > season_open_date_first_pup) %>%
          mutate(in_prev_season = pmap_lgl(list(pinniped_id, pinniped_id_list_prev_season), x_in_y)) %>%
          filter(in_prev_season)


        # Finally, group by season name and calculate the return rate
        x %>%
          group_by(season_name) %>%
          summarise(returner_count = n(),
                    denominator = unique(lengths(pinniped_id_list_prev_season)),
                    return_rate = returner_count / denominator)
      })


      #########################################################################
      ### Natality rate
      natality_rate <- reactive({
        req(ps_adult_female_by_season())
        pinnipeds <- tbl(req(pool()), "pinnipeds") %>%
          filter(tolower(species) == "fur seal",
                 ID %in% !!ps_adult_female_by_season()$pinniped_id) %>%
          select(pinniped_id = ID, cohort) %>%
          collect()

        # browser()

        # TODO: filter for only study beach animals


        ps.ad <- ps_adult_female_by_season() %>%
          left_join(pinnipeds, by = "pinniped_id") %>%
          mutate(age = pinniped_age(season_open_date, cohort),
                 prime = between(age, 7, 17)) %>%
          replace_na(list(prime = FALSE)) %>%
          # TODO: remove this after fixing cohorts
          filter(age > 0 | is.na(age))

        natal.summ <- function(x, ...) {
          x %>%
            group_by(...) %>%
            summarise(total_count = n(),
                      parturition_count = sum(parturition),
                      natality_rate = parturition_count / total_count)
        }

        if (input$plot_x_axis == "season") {
          if (input$prime) {
            bind_rows(
              ps.ad %>% natal.summ(season_name) %>% mutate(type = "All"),
              ps.ad %>%
                filter(prime) %>%
                natal.summ(season_name) %>%
                mutate(type = "Prime")
            )
          } else {
            ps.ad %>%
              natal.summ(season_name) %>%
              mutate(type = "All")
          }

        } else if (input$plot_x_axis == "age") {
          ps.ad %>%
            filter(!is.na(age)) %>%
            natal.summ(age)
        }
      })


      #########################################################################
      ### Output plot
      plot_output <- reactive({
        #--------------------------------------------------
        if (input$type == "return") {
          ggplot(return_rate(), aes(season_name, return_rate)) +
            geom_point() +
            geom_path(group = 1) +
            xlab("Season") +
            ylab("Return rate") +
            ylim(c(0, 1)) +
            ggtitle(NULL) +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

          #------------------------------------------------
        } else if (input$type == "natality") {
          if (input$plot_x_axis == "season") {
            ggplot(natality_rate(), aes(season_name, natality_rate, color = type)) +
              geom_point() +
              geom_path(aes(group = type)) +
              xlab("Season") +
              ylab("Natality rate") +
              # ylim(c(0, 1)) +
              ggtitle(NULL) +
              theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

          } else if (input$plot_x_axis == "age") {
            natality_rate() %>%
              filter(between(age, input$filter_age[1], input$filter_age[2])) %>%
              ggplot(aes(age, natality_rate)) +
              geom_point() +
              geom_path(aes(group = 1)) +
              xlab("Age") +
              ylab("Natality rate") +
              # ylim(c(0, 1)) +
              ggtitle(NULL)
          }

          #------------------------------------------------
        } else {
          validate("Nope nope nope")
        }
      })


      ### Output table
      tbl_output <- reactive({
        if (input$type == "return") {
          return_rate()
        } else if (input$type == "natality") {
          natality_rate()
        } else {
          validate("Nope nada nein")
          ps_collect()
        }
      })


      ### Send to output module
      observe(mod_output_server("output", id, tbl_output, plot_output, plot.height))
    }
  )
}
