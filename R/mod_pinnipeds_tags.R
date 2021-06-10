#' @name shiny_modules
#' @export
mod_pinnipeds_tags_ui <- function(id) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    fluidRow(
      box(
        title = "Filters", status = "warning", solidHeader = FALSE, width = 5, collapsible = TRUE,
        fluidRow(
          column(4, checkboxGroupInput(ns("species"), label = tags$h5("Species"),
                                       choices = pinniped.sp.list,
                                       selected = unname(unlist(pinniped.sp.list)))),
          column(
            width = 4,
            radioButtons(ns("cohort"), tags$h5("Filter for:"),
                         choices = list("All pinnipeds" = "all",
                                        "Pinnipeds with cohorts" = "w_cohort",
                                        "Pinnipeds without cohorts" = "wout_cohort"),
                         selected = "all"),
            conditionalPanel(
              condition = "input.cohort == 'w_cohort'", ns = ns,
              uiOutput(ns("cohort_vals_uiOut_selectize"))
            )
          )
        )
      ),
      box(
        title = "User selections", status = "warning", solidHeader = FALSE, width = 7, collapsible = TRUE,
        fluidRow(
          column(4, radioButtons(ns("plot_type"), tags$h5("Plot type"),
                                 choices = list("Number of tagged pinnipeds" = "pinnipeds",
                                                "Number of tags deployed" = "tags"),
                                 selected = "pinnipeds"))
        )
      )
    ),
    mod_output_ui(ns("output"))
  )
}



#' @name shiny_modules
#' @export
mod_pinnipeds_tags_server <- function(id, pool) {
  stopifnot(
    is.reactive(pool)
  )

  moduleServer(
    id,
    function(input, output, session) {
      #########################################################################
      output$cohort_vals_uiOut_selectize <- renderUI({
        cohort.list <- req(cohort_list())

        selectInput(
          session$ns("cohort_vals"), tags$h5("Cohorts to include"),
          choices = cohort.list, selected = unlist(cohort.list),
          selectize = TRUE, multiple = TRUE
        )
      })

      #########################################################################
      ### Generate base SQL query
      tags_pinnipeds_tbl <- reactive({
        req(pool())
        tbl.tags <- tbl(pool(), "vTags") %>%
          select(pinniped_id, primary_tag, tag_id = ID,
                 tag, tag_type, tag_species, color, tag_notes = notes, tag_sort)

        tbl.pinnipeds <- tbl(pool(), "pinnipeds") %>%
          select(pinniped_id = ID, species, sex, cohort, mothers_id, pinniped_notes = notes)

        validate(
          need(input$species, "Please select at least one species")
        )

        tbl.sql <- full_join(tbl.tags, tbl.pinnipeds, by = "pinniped_id") %>%  #TODO: use view
          filter(tolower(species) %in% !!input$species) %>%
          select(pinniped_id, tag_id, species, sex, cohort,
                 primary_tag, tag, tag_type, color, pinniped_notes, tag_notes, tag_sort)
      })


      ### Get list of cohort values
      cohort_list <- reactive({
        df.out <- tags_pinnipeds_tbl() %>%
          distinct(cohort) %>%
          arrange(cohort) %>%
          collect()

        as.list(na.omit(df.out$cohort))
      })


      ### Get sorted pinnipeds+tags records, filtered by cohort if necessary
      tags_pinnipeds <- reactive({
        tbl.sql <- if (input$cohort == "w_cohort") {
          validate(
            need(input$cohort_vals, "Please select at least one cohort value")
          )

          tags_pinnipeds_tbl() %>% filter(!is.na(cohort), cohort %in% !!input$cohort_vals)
        } else if (input$cohort == "wout_cohort") {
          tags_pinnipeds_tbl() %>% filter(is.na(cohort))
        } else {
          tags_pinnipeds_tbl()
        }

        tbl.sql %>%
          collect() %>%
          arrange(species, pinniped_id, primary_tag, tag_type, tag_sort) %>%
          select(-tag_sort)
      })


      #########################################################################
      ### Output plot
      plot_output <- reactive({


        if (input$plot_type == "pinnipeds") {
          df.summ <- tags_pinnipeds() %>%
            distinct(pinniped_id, .keep_all = TRUE) %>%
            mutate(known_age = !is.na(cohort)) %>%
            group_by(species) %>%
            summarise(total = n(),
                      anonymous = sum(!known_age),
                      `known age` = sum(known_age)) %>%
            mutate_factor_species() %>%
            pivot_longer(cols = anonymous:`known age`, names_to = "type", values_to = "count")

        } else {
          df.summ <- tags_pinnipeds() %>%
            # mutate(known_age = !is.na(cohort)) %>%
            group_by(species) %>%
            summarise(pinniped_count = n_distinct(pinniped_id),
                      tag_count = n_distinct(tag_id)) %>%
            mutate_factor_species()
        }

        if (input$plot_type == "pinnipeds") {
          y.name <- sym("count")
          fill.name <- sym("type")
          plot.title <- "Number of tagged pinnipeds"

        } else if (input$plot_type == "tags") {
          y.name <- sym("tag_count")
          fill.name <- sym("species")
          plot.title <- "Number of deployed tags"

        } else {
          validate("invalid input$plot_type value")
        }


        ggplot(df.summ, aes(species, !!y.name, fill = !!fill.name)) +
          geom_bar(stat = "identity", position = "stack") +
          xlab("Species") +
          ylab("Count") +
          ggtitle(plot.title)
      })


      ### Output table
      tbl_output <- reactive({
        tags_pinnipeds()
      })


      ### Send to output module
      observe(mod_output_server("output", id, tbl_output, plot_output))
    }
  )
}
