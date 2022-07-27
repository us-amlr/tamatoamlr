

summary <- c("Summary 1: Captures in all seasons", "Summary 2: Captures in one season", "Summary 3: Captures by Individual", "raw data")
data_type <- c("Masses(kg)","Body Conditions (Mass/Length)", "Capture to Release Times", "Times on Gas (Fur Seals only)",
                   "Capture to Reunion Times (Fur Seals only)", "Recovery in Box Times (Fur Seals only)", "Number of Captures")

season_from_date <- function(date) {
  formatted_date <- lubridate::as_date(date)
  year <- lubridate::year(formatted_date)
  month <- lubridate::month(formatted_date)
  return(ifelse(month < 7, paste0(year - 1, "/", substring(as.character(year), 3,4)), paste0(year, "/", substring(as.character(year + 1), 3,4))))
}

captures_ui <- function(id) {
  ns = NS(id)
  tagList(
    box(title = "Filters", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
        #Use or statements, javascript is not amenable to in.
        conditionalPanel(condition = "input.summary == 'Summary 2: Captures in one season' |
                                                        input.summary == 'raw data' |
                                                        input.summary == 'Summary 3: Captures by Individual'",
                         mod_filter_season_ui(ns("season_filter")), ns = ns),
        conditionalPanel(condition = "input.data_type == 'Masses(kg)' |
                                                               input.data_type == 'Body Conditions (Mass/Length)' |
                                                               input.data_type == 'Capture to Release Times' |
                                                               input.data_type == 'Number of Captures'",
                         checkboxGroupInput(ns("species_input"), "Select Species", choices = c("Fur seal", "Elephant seal", "Leopard seal", "Weddell seal")),
                         ns = ns),
        conditionalPanel(condition = "!(input.data_type == 'Masses(kg)' |
                           input.data_type == 'Body Conditions (Mass/Length)' |
                           input.data_type == 'Capture to Release Times' |
                           input.data_type == 'Number of Captures')",
                         textOutput(ns("fur_seals_only")),
                         ns = ns)
        #column(5, uiOutput(ns("species_list")))
    ),
    box(title = "Summary Options", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
      fluidRow(
        column(5,radioButtons(ns("summary"), "Choose Summary Option", summary)),
        column(5,conditionalPanel(condition = "input.summary != 'Summary 3: Captures by Individual' && input.summary != 'raw data'",
          radioButtons(ns("data_type"), "Choose Data Type Option", data_type, selected = "Number of Captures"),
          ns = ns))
        )
      ),
    mod_output_ui(ns("table_and_plot"), plotOutput(ns("plot"), width = "700px"), DTOutput(ns("summarydatatbl")))
    # box(title = "Graph", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
    #   conditionalPanel(condition = "input.summary != 'raw data'",
    #                    plotOutput(ns("plot"), width = "700px"), ns = ns)),
    # DTOutput(ns("summarydatatbl")),
    # downloadButton(ns("downloadData"), "Download")
  )
}

captures_server <- function(id, con) {
  moduleServer(
    id,
    function(input, output, session) {
      captures <- reactive({
        collect(tbl(req(con()), "vCaptures")) #vCaptures
      })
      season_info <- reactive({
        tbl(req(con()), "season_info") %>%
          select(-ts) %>%
          collect()
      })
      pinnipeds <- reactive({
        tbl(req(con()), "pinnipeds") %>%
          select(-ts) %>%
          collect()
      })
      beaches <- reactive({
        collect(tbl(req(con()), "beaches"))
      })
      tags <- reactive({
        collect(tbl(req(con()), "tags"))
      })
      output$datatbl <- renderDT({
        season_info()
      }, options = list(lengthChange = FALSE)
      )

      #Uses mod_filter_season to implement the season and date filters
      summ_level <- reactive(
        "fs_multiple_total"
      )
      season_filter = reactive(mod_filter_season_server("season_filter", summ_level , season_info))


      #joins the captures and pinnipeds tables together
      # join_captures_pinnipeds <- reactive({
      #   req()
      #   captures <- mutate(captures(), capture_season = season_from_date(capture_date))
      #   pinnipeds <- pinnipeds() %>%
      #     rename(pinniped_id = ID)
      #   pinnipeds_simple <- pinnipeds %>%
      #     select(pinniped_id, species, sex)
      #   captures <- captures %>%
      #     left_join(pinnipeds_simple)
      #   captures
      # })

      #summary table 1
      DTsummary1 <- reactive({
        req()
        captures <- captures()
        if(input$data_type %in% c("Masses(kg)", "Body Conditions (Mass/Length)", "Number of Captures", "Capture to Release Times")) {
          captures <- captures %>%
            filter(species %in% input$species_input)
        }
        if(input$data_type == "Number of Captures") {
          captures_by_season <- captures %>%
            group_by(season_name, species) %>%
            summarize(number_of_captures = n())
        }
        if(input$data_type == "Masses(kg)") {
          captures_by_season <- captures %>%
            group_by(species) %>%
            mutate(Mass = mass_total_kg - tare_kg)
        }
        if(input$data_type == "Capture to Release Times") {
          captures_by_season <- captures %>%
            group_by(species) %>%
            mutate(capt_to_release = as.numeric(difftime(hms::as_hms(release_time), hms::as_hms(capture_time), units = "mins")))
        }
        if(input$data_type == "Body Conditions (Mass/Length)") {
          #browser()
          captures_by_season <- captures %>%
            group_by(species) %>%
            mutate(body_condition = (mass_total_kg - tare_kg)/std_length_cm)
        }
        if(input$data_type == "Capture to Reunion Times (Fur Seals only)") {
          captures_by_season <- captures %>%
            filter(species == "Fur seal") %>%
            mutate(capt_to_reunion = as.numeric(difftime(hms::as_hms(reunion_time), hms::as_hms(capture_time), units = "mins")))
        }
        if(input$data_type == "Times on Gas (Fur Seals only)") {
          captures_by_season <- captures %>%
            filter(species == "Fur seal") %>%
            mutate(gas_time = as.numeric(difftime(hms::as_hms(gas_off), hms::as_hms(gas_on), units = "mins")))
        }
        if(input$data_type == "Recovery in Box Times (Fur Seals only)") {
          captures_by_season <- captures %>%
            filter(species == "Fur seal") %>%
            mutate(box_time = as.numeric(difftime(hms::as_hms(release_time), hms::as_hms(in_box), units = "mins")))
        }
        return(captures_by_season)
      })

      #reactive for creating summary 2 table
      DTsummary2 <- reactive({
        req(season_filter()$season_select(), season_filter()$date_range())
        #browser()
        captures <- captures()
        if(input$data_type %in% c("Mass(kg)", "Number of Captures", "Capture to Release Times")) {
          captures <- captures %>%
            filter(species %in% input$species_input)
        }
        browser()
        one_season <- captures %>%
          filter(season_name %in% season_filter()$season_select()) %>%
          filter(capture_date > season_filter()$date_range()[[1]] & capture_date < season_filter()$date_range()[[2]])
        if(input$data_type == "Number of Captures") {
          one_season <- one_season %>%
            group_by(capture_date, species) %>%
            summarize(number_of_captures = n())
        }
        if(input$data_type == "Masses(kg)") {
          one_season <- one_season %>%
            group_by(species) %>%
            mutate(Mass = mass_total_kg - tare_kg)
        }
        if(input$data_type == "Capture to Release Times") {
          one_season <- one_season %>%
            group_by(species) %>%
            mutate(capt_to_release = as.numeric(difftime(hms::as_hms(release_time), hms::as_hms(capture_time), units = "mins")))
        }
        if(input$data_type == "Body Conditions (Mass/Length)") {
          one_season <- one_season %>%
            group_by(species) %>%
            mutate(body_condition = (mass_total_kg - tare_kg)/std_length_cm)
        }
        if(input$data_type == "Capture to Reunion Times (Fur Seals only)") {
          one_season <- one_season %>%
            filter(species == "Fur seal") %>%
            mutate(capt_to_reunion = as.numeric(difftime(hms::as_hms(reunion_time), hms::as_hms(capture_time), units = "mins")))
        }
        if(input$data_type == "Times on Gas (Fur Seals only)") {
          one_season <- one_season %>%
            filter(species == "Fur seal") %>%
            mutate(gas_time = as.numeric(difftime(hms::as_hms(gas_off), hms::as_hms(gas_on), units = "mins")))
        }
        if(input$data_type == "Recovery in Box Times (Fur Seals only)") {
          one_season <- one_season %>%
            filter(species == "Fur seal") %>%
            mutate(box_time = as.numeric(difftime(hms::as_hms(release_time), hms::as_hms(in_box), units = "mins")))
        }
        return(one_season)
      })

      DTsummary3 <- reactive({
        captures <- captures()
        captures <- captures %>%
          group_by(pinniped_id, season_name) %>%
          summarize(number_of_captures = n())
        return(captures)
      })

      #creates raw data table
      DTrawdata <- reactive({
        req()
        # pinnipeds <- pinnipeds() %>%
        #   rename(pinniped_id = ID)
        # pinnipeds_simple <- pinnipeds %>%
        #   select(pinniped_id, species, sex)
        # tags_simple <- tags() %>%
        #   filter(primary_tag == "TRUE") %>%
        #   select(tag, tag_type, pinniped_id, tagging_date)
        # captures <- captures() %>%
        #   left_join(pinnipeds_simple) %>%
        #   left_join(tags_simple)
        captures <- captures()
        if(input$data_type %in% c("Masses(kg)", "Body Conditions (Mass/Length)", "Number of Captures", "Capture to Release Times")) {
          captures <- captures %>%
            filter(species %in% input$species_input)
        } else {
          captures <- captures %>%
            filter(species == "Fur seal")
        }
        if(input$summary == 'Summary 2: Captures in one season' || input$summary == 'raw data'){
          captures <- captures %>%
            filter(season_name %in% season_filter()$season_select(),
                   capture_date > season_filter()$date_range()[[1]] & capture_date < season_filter()$date_range()[[2]])
        }
        return(captures)
      })


      output$fur_seals_only <- renderText({
        "The selected summary is only available for fur seals"})

      #DT tables above render with the appropriate data
      summary_table_reactive <- reactive({
        if(input$summary == "Summary 3: Captures by Individual") {
          browser()
          return(DTsummary3() %>%
                   pivot_wider(names_from = season_name, values_from = number_of_captures) %>%
                   left_join(pinnipeds()) %>%
                   left_join(
                     select(filter(
                       tags(), primary_tag == "TRUE"),
                       tag_type, pinniped_id, tagging_date)
                   )
          )
        }else if(input$data_type == "Number of Captures") {
          if(input$summary == "Summary 1: Captures in all seasons") {
            return(DTsummary1())
          } else if(input$summary == "Summary 2: Captures in one season") {
            return(DTsummary2())
          } else {
            return(DTrawdata())
          }
        } else {
          return(DTrawdata())
        }
      })

      # output$summarydatatbl <- renderDT({
      #   summary_table_reactive()}, options = list(lengthChange = FALSE)
      # )

      #Creates the plots of captures by season and date
      summary_plot_reactive <- reactive({
        #browser()
        if(input$summary == "Summary 1: Captures in all seasons") {
          table_for_plot <- DTsummary1()
          x_var <- table_for_plot$season_name
        } else if(input$summary == "Summary 2: Captures in one season") {
          table_for_plot <- DTsummary2()
          x_var <- table_for_plot$capture_date
        } else if(input$summary == "Summary 3: Captures by Individual") {
          #browser()
          return(ggplot(DTsummary3(), aes(x = number_of_captures)) +
                          geom_histogram() +
                          xlab("Number of Captures by Individual"))
        } else {
          validate(need(input$summary != "raw data", "no plot for raw data"))
        }
        if(input$data_type == "Number of Captures") {
          return(ggplot(table_for_plot, aes(x = x_var, y = number_of_captures, color = species, group = species)) +
                   geom_point(position = "identity", stat = "identity") +
                   geom_line(position = "identity", stat = "identity") +
                   theme(axis.text.x = element_text(angle = 90)) +
                   scale_color_manual(values = .colorsPresent(table_for_plot)) +
                   labs(x = ifelse(input$summary == "Summary 1: Captures in all seasons", "Season", "Date"), y = "Number of Captures"))
        }
        if(input$data_type == "Masses(kg)") {
          return(ggplot(table_for_plot, aes(x = Mass, fill = species, group = species)) +
                   geom_histogram() +
                   scale_fill_manual(values = .colorsPresent(table_for_plot)) +
                   xlab("Mass(kg)"))
        }
        if(input$data_type == "Body Conditions (Mass/Length)") {
          return(ggplot(table_for_plot, aes(x = body_condition, fill = species, group = species)) +
                   geom_histogram() +
                   scale_fill_manual(values = .colorsPresent(table_for_plot)) +
                   xlab("Body Condition(Mass/length)"))
        }
        if(input$data_type == "Capture to Release Times") {
          return(ggplot(table_for_plot, aes(x = capt_to_release, fill = species, group = species)) +
                   geom_histogram() +
                   scale_fill_manual(values = .colorsPresent(table_for_plot)) +
                   xlab("Capture to Release Time (minutes)"))
        }
        if(input$data_type == "Capture to Reunion Times (Fur Seals only)") {
          return(ggplot(table_for_plot, aes(x = capt_to_reunion, fill = species, group = species)) +
                   geom_histogram() +
                   scale_fill_manual(values = .colorsPresent(table_for_plot)) +
                   xlab("Capture to Reunion Time (minutes)"))
        }
        if(input$data_type == "Times on Gas (Fur Seals only)") {
          return(ggplot(table_for_plot, aes(x = gas_time, fill = species, group = species)) +
                   geom_histogram() +
                   scale_fill_manual(values = .colorsPresent(table_for_plot)) +
                   xlab("Time on Gas"))
        }
        if(input$data_type == "Recovery in Box Times (Fur Seals only)") {
          return(ggplot(table_for_plot, aes(x = box_time, fill = species, group = species)) +
                   geom_histogram() +
                   scale_fill_manual(values = .colorsPresent(table_for_plot)) +
                   xlab("Time Recovering in Box (minutes)"))
        }
      })

      # output$plot <- renderPlot({
      #   validate(need(input$summary != "raw data", "no plot for raw data"))
      #   summary_plot_reactive()
      # }, res = 96)

      #controls widget to download the tables
      # output$downloadData <- downloadHandler(
      #   filename = function() {
      #     paste(input$summarydatatbl, ".csv", sep = "")
      #   },
      #   content = function(file) {
      #     write.csv(summary_table_reactive(), file, row.names = FALSE)
      #   }
      # )

      observe({
        # if (!inherits(summary_table_reactive(), "data.frame"))
        mod_output_server("table_and_plot", session, summary_table_reactive, summary_plot_reactive)
        })
    }
  )
}
