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
        # div(class = "pull-right", downloadButton(ns("plot_download"), "Save plot as PNG")),
        downloadButton(ns("plot_download"), "Save plot as PNG")
      ),
      box(
        status = "primary", width = 12,
        # div(class = "pull-right", downloadButton(ns("tbl_download"), "Download table as CSV")),
        downloadButton(ns("tbl_download"), "Download table as CSV"),
        tags$br(),
        tags$br(),
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

      # Download table
      output$tbl_download <- downloadHandler(
        filename = function() {
          paste0(id.parent, "_table.csv")
        },
        content = function(file) {
          write.csv(tbl.reac(), file = file, row.names = FALSE, na = "")
        }
      )

      #----------------------------------------------------
      # Output plot
      output$plot <- renderPlot(plot.reac(), res = plot.res)

      # Download plot
      output$plot_download <- downloadHandler(
        filename = function() {
          paste0(id.parent, "_plot.png")
        },
        content = function(file) {
          x <- req(session$clientData[[paste0("output_", id.parent, "-", id, "-plot_width")]]) / plot.res
          y <- req(session$clientData[[paste0("output_", id.parent, "-", id, "-plot_height")]]) / plot.res

          # file.res <- 300 #
          # plot.format <- "PNG"

          # NOTE: if the user needs control over the resolution, must use png() device directly per https://github.com/tidyverse/ggplot2/issues/2276
          # ggsave docs have an example of this (https://ggplot2.tidyverse.org/reference/ggsave.html),
          #   basically just make sure to print the ggplot object

          # ggsave by default uses the device dimensions, huzzah
          ggsave(file, plot = plot.reac(), device = "png", height = y, width = x, units = "in")
        }
      )
    }
  )
}
