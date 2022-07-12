

summary <- c("Summary 1: Captures in all seasons", "Summary 2: Captures in one season", "raw data")
morphometrics <- c("Masses(kg)", "Capture to Release Times", "Times on Gas (Fur Seals only)",
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
    box(title = "Summary Options", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
      fluidRow(
        column(5,radioButtons(ns("summary"), "Choose Summary Option", summary)),
        # Figure out how too gray out the fur seal specific stuff unless only fur seals are selected.
        column(5,radioButtons(ns("morphometrics"), "Choose Morphometric Option", morphometrics, selected = "Number of Captures"))
        )
      ),
    box(title = "Filters", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
        conditionalPanel(condition = "input.summary == 'Summary 2: Captures in one season'",
          mod_filter_season_ui(ns("season_filter")), ns = ns),
        #column(3,checkboxGroupInput(ns("species_input"), "Select Species", choices = c(pinniped.sp, "All Species"), selected = "All Species"))
        column(3, uiOutput(ns("species_list")))
        ),

    box(title = "Graph", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
      conditionalPanel(condition = "input.summary != 'raw data'",
                       plotOutput(ns("plot"), width = "700px"), ns = ns)),
    DTOutput(ns("summarydatatbl")),
    downloadButton(ns("downloadData"), "Download")
  )
}

captures_server <- function(id, con) {
  moduleServer(
    id,
    function(input, output, session) {
      captures <- reactive({
        collect(tbl(con(), "captures"))
      })
      season_info <- reactive({
        tbl(con(), "season_info") %>%
          select(-ts) %>%
          collect()
      })
      pinnipeds <- reactive({
        tbl(con(), "pinnipeds") %>%
          select(-ts) %>%
          collect()
      })
      beaches <- reactive({
        collect(tbl(con(), "beaches"))
      })
      tags <- reactive({
        collect(tbl(con(), "tags"))
      })
      output$datatbl <- renderDT({
        season_info()
      }, options = list(lengthChange = FALSE)
      )

      #Uses mod_filter_season to implement the season and date filters
      summ_level <- reactive(
        "fs_single"
      )
      season_filter = reactive(mod_filter_season_server("season_filter", summ_level , season_info))


      #joins the captures and pinnipeds tables together
      join_captures_pinnipeds <- reactive({
        req()
        captures <- mutate(captures(), capture_season = season_from_date(capture_date))
        pinnipeds <- pinnipeds() %>%
          rename(pinniped_id = ID)
        pinnipeds_simple <- pinnipeds %>%
          select(pinniped_id, species)
        captures <- captures %>%
          left_join(pinnipeds_simple)
        captures
      })

      #summary table 1
      DTsummary1 <- reactive({
        req()
        captures <- join_captures_pinnipeds()
        if(!("All Species" %in% input$species_input)) {
          captures <- captures %>%
            filter(species %in% input$species_input)
        }
        if(input$morphometrics == "Number of Captures") {
          captures_by_season <- captures %>%
            group_by(capture_season, species) %>%
            summarize(number_of_captures = n())
        }
        else if(input$morphometrics == "Masses(kg)") {
          captures_by_season <- captures %>%
            group_by(species) %>%
            summarize(Mass = mass_total_kg)
        }
        else if(input$morphometrics == "Capture to Release Times") {
          captures_by_season <- captures %>%
            group_by(species) %>%
            summarize(capt_to_release = release_time-capture_time)
        }
        else {
          captures_by_season = captures
        }
        return(captures_by_season)
      })

      #reactive for creating summary 2 table
      DTsummary2 <- reactive({
        req(season_filter)
        captures <- join_captures_pinnipeds()
        if(!("All Species" %in% input$species_input)) {
          captures <- captures %>%
            filter(species %in% input$species_input)
        }
        one_season <- captures %>%
          filter(capture_season == season_filter()$season_select()) %>%
          filter(capture_date > season_filter()$date_range()[[1]] && capture_date < season_filter()$date_range()[[2]])
        if(input$morphometrics == "Number of Captures") {
          one_season <- one_season %>%
            group_by(capture_date, species) %>%
            summarize(number_of_captures = n())
        }
        if(input$morphometrics == "Masses(kg)") {
          one_season <- one_season %>%
            group_by(species) %>%
            summarize(Mass = mass_total_kg)
        }
        if(input$morphometrics == "Capture to Release Times") {
          one_season <- one_season %>%
            group_by(species) %>%
            summarize(capt_to_release = release_time-capture_time)
        }
        return(one_season)
      })

      #creates raw data table
      DTrawdata <- reactive({
        req()
        pinnipeds <- pinnipeds() %>%
          rename(pinniped_id = ID)
        pinnipeds_simple <- pinnipeds %>%
          select(pinniped_id, species, sex)
        tags_simple <- tags() %>%
          filter(primary_tag == "TRUE") %>%
          select(tag, tag_type, pinniped_id, tagging_date)
        captures <- captures() %>%
          left_join(pinnipeds_simple) %>%
          left_join(tags_simple)
        if(!("All Species" %in% input$species_input)) {
          captures <- captures %>%
            filter(species %in% input$species_input)
        }
        return(captures)
      })

      output$species_list <- renderUI({
        #browser()
        req(input$summary, input$morphometrics)
        if (input$morphometrics == "Masses(kg)" || input$morphometrics == "Capture to Release Times" ||
            input$morphometrics == "Number of Captures") {
          checkboxGroupInput(session$ns("species_input"), "Select Species", choices = c(pinniped.sp, "All Species"), selected = "All Species")
        }
        else {
          textOutput(session$ns("fur_seals_only"))
        }
      })

      output$fur_seals_only <- renderText({
        "The selected summary is only available for fur seals"})

      #DT tables above render with the appropriate data
      summary_table_reactive <- reactive({
        if(input$summary == "Summary 1: Captures in all seasons") {
          return(DTsummary1())
        }
        if(input$summary == "Summary 2: Captures in one season") {
          return(DTsummary2())
        }
        if (input$summary == "raw data") {
          return(DTrawdata())
        }
      })

      output$summarydatatbl <- renderDT({
        summary_table_reactive()}, options = list(lengthChange = FALSE)
      )

      #Creates the plots of captures by season and date
      summary_plot_reactive <- reactive({
        if(input$summary == "Summary 1: Captures in all seasons") {
          table_for_plot <- DTsummary1()
          x_var <- table_for_plot$capture_season
        }
        else if(input$summary == "Summary 2: Captures in one season") {
          table_for_plot <- DTsummary2()
          x_var <- table_for_plot$capture_date
        }
        else {
          pass
        }
        if(input$morphometrics == "Number of Captures") {
          return(ggplot(table_for_plot, aes(x = x_var, y = number_of_captures, color = species, group = species)) +
                   geom_point(position = "identity", stat = "identity") +
                   geom_line(position = "identity", stat = "identity") +
                   theme(axis.text.x = element_text(angle = 90))) +
                   scale_color_manual(values = pinniped.sp.colors)
        }
        if(input$morphometrics == "Masses(kg)") {
          return(ggplot(table_for_plot, aes(x = Mass, fill = species, group = species)) +
                   geom_histogram() +
                   scale_fill_manual(values = pinniped.sp.colors))
        }
        if(input$morphometrics == "Capture to Release Times") {
          return(ggplot(table_for_plot, aes(x = capt_to_release, fill = species, group = species)) +
                   geom_histogram() +
                   scale_fill_manual(values = pinniped.sp.colors))
        }
      })

      output$plot <- renderPlot({
        validate(need(input$summary != "raw data", "no plot for raw data"))
        summary_plot_reactive()
      }, res = 96)

      #controls widget to download the tables
      output$downloadData <- downloadHandler(
        filename = function() {
          paste(input$summarydatatbl, ".csv", sep = "")
        },
        content = function(file) {
          write.csv(summary_table_reactive(), file, row.names = FALSE)
        }
      )
    }
  )
}
