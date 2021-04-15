#' Shiny module for output plot and table
#'
#' Shiny module for output plot and table
#'
#' @name mod_output
#'
#' @param id character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @export
mod_output_ui <- function(id, ...) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    fluidRow(
      box(
        status = "primary", width = 12,
        plotOutput(ns("plot")),
        ...,
        tags$br(),
        downloadButton(ns("plot_download"), "Save plot as PNG")
      ),
      box(
        status = "primary", width = 12,
        downloadButton(ns("tbl_download"), "Download table as CSV"),
        tags$h5("TODO: change"),
        tags$h5("This table shows the data displayed in the plot above.",
                "Note that all rows with only counts of zero for the selected columns have been filtered out."),
        DTOutput(ns("tbl"))
      )
    )
  )
}

#' @name mod_output
#'
#' @param tbl.reac reactive
#' @param plot.reac reactive
#' @param plot.res resolution
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
      #----------------------------------------------------
      # Output table
      output$tbl <- renderDT(tbl.reac(), options = list(scrollX = TRUE))

      # Download plot
      output$tbl_download <- downloadHandler(
        filename = function() {
          "table.csv"
        },
        content = function(file) {
          write.csv(tbl.reac(), file = file, row.names = FALSE)
        }
      )

      #----------------------------------------------------
      # Output plot
      output$plot <- renderPlot(plot.reac(), res = plot.res)

      # Download plot
      output$plot_download <- downloadHandler(
        filename = function() {
          "plot.png"
        },
        content = function(file) {
          # x <- req(session$clientData[[paste0("output_", id.parent, "-", id, "-plot_width")]]) / plot.res
          # y <- req(session$clientData[[paste0("output_", id.parent, "-", id, "-plot_height")]]) / plot.res

          # file.res <- 300
          # plot.format <- "PNG"

          ggsave(file, plot = plot.reac(), device = "png") #, height = y, width = x, units = "in")
        }
      )
    }
  )
}
