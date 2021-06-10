#' Shiny module for output plot and table for AMLR shiny apps
#'
#' Shiny module for output plot and table for AMLR shiny apps
#'
#' @name mod_output
#'
#' @param id character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#' @param ... UI elements to be displayed below the plot and above the 'download plot'save plot' button
#'
#' @export
mod_output_ui <- function(id, ...) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    fluidRow(
      box(
        status = "primary", width = 12,
        fluidRow(
          column(
            width = 10,
            plotOutput(ns("plot"), height = "auto", width = "auto"),
            ...
          ),
          column(
            width = 2,
            numericInput(ns("plot_height"), tags$h5("Plot height (pixels)"), value = 450, min = 0, step = 50, width = "200px"),
            numericInput(ns("plot_width"), tags$h5("Plot width (pixels)"), value = 900, min = 0, step = 50, width = "200px"),
            tags$br(),
            downloadButton(ns("plot_download"), "Save plot as PNG")
          )
        )
      ),
      box(
        status = "primary", width = 12,
        # tags$h5("Note that all rows with only counts of zero for the selected columns (may) have been filtered out.",
        #         "See below the table to specify the column(s) to include in the output table/CSV file."),
        # tags$br(),
        DTOutput(ns("tbl")),
        tags$br(),
        uiOutput(ns("tbl_cols_uiOut_selectize")),
        actionButton(ns("tbl_cols_reset"), "Re-select all columns in original order"),
        downloadButton(ns("tbl_download"), "Download table as CSV")
      )
    )
  )
}

#' @name mod_output
#'
#' @param id.parent parent module ID; used to generate default filename and get plot window dimensions
#' @param tbl.reac reactive; data frame to be displayed in the table
#' @param plot.reac reactive; \code{\link[ggplot2]{ggplot}} object to be plotted
#' @param plot.res numeric; plot resolution, passed to \code{\link[shiny]{renderPlot}} and
#'   used to determine output file dimensions
#'
#' @returns Nothing
#'
#' @export
mod_output_server <- function(id, id.parent, tbl.reac, plot.reac, plot.res = 96) {
  stopifnot(
    is.reactive(tbl.reac),
    is.reactive(plot.reac),
    inherits(tbl.reac(), "data.frame"),
    inherits(plot.reac(), "ggplot")
  )

  moduleServer(
    id,
    function(input, output, session) {
      #------------------------------------------------------------------------
      # Columns to display in table
      output$tbl_cols_uiOut_selectize <- renderUI({
        tbl.names <- names(req(tbl.reac()))

        selectInput(
          session$ns("tbl_cols"), "Columns to display in table and output CSV",
          choices = as.list(tbl.names), selected = tbl.names,
          multiple = TRUE, selectize = TRUE
        )
      })

      observeEvent(input$tbl_cols_reset, {
        updateSelectInput(session, "tbl_cols", selected = names(req(tbl.reac())))
      })

      # Output table
      output$tbl <- renderDT({
        validate(
          need(input$tbl_cols, "Please select at least one column to display")
        )
        req(all(input$tbl_cols %in% names(tbl.reac())))
        tbl.reac() %>% select(input$tbl_cols)
      }, options = list(scrollX = TRUE))

      # Download table
      output$tbl_download <- downloadHandler(
        filename = function() {
          paste0(id.parent, "_table.csv")
        },
        content = function(file) {
          tbl.out <- tbl.reac() %>% select(req(input$tbl_cols))
          write.csv(tbl.out, file = file, row.names = FALSE, na = "")
        }
      )


      #------------------------------------------------------------------------
      plot_height <- reactive({
        validate(
          need(input$plot_height > 100, "The plot height must be at least 100")
        )
        input$plot_height
      })

      plot_width <- reactive({
        validate(
          need(input$plot_width > 100, "The plot width must be at least 100")
        )
        input$plot_width
      })

      # Output plot
      output$plot <- renderPlot({
        plot.reac()
      }, height = plot_height, width = plot_width, units = "px", res = plot.res)

      # Download plot
      output$plot_download <- downloadHandler(
        filename = function() {
          paste0(id.parent, "_plot.png")
        },
        content = function(file) {
          x <- req(session$clientData[[paste0("output_", id.parent, "-", id, "-plot_width")]]) / plot.res
          y <- req(session$clientData[[paste0("output_", id.parent, "-", id, "-plot_height")]]) / plot.res

          # NOTE: if the user needs control over the resolution,
          #   must use png() device directly per https://github.com/tidyverse/ggplot2/issues/2276
          # ggsave docs have an example of this (https://ggplot2.tidyverse.org/reference/ggsave.html),
          #   basically just make sure to print the ggplot object

          ggsave(file, plot = plot.reac(), device = "png", height = y, width = x, units = "in")
        }
      )
    }
  )
}
