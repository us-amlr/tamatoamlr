#' Open amlrPinnipeds Shiny app
#'
#' Open the amlrPinnipeds R Shiny application
#'
#' @param launch.browser Logical with default of \code{TRUE};
#'   passed to \code{launch.browser} argument of \code{\link[shiny]{runApp}}
#'
#' @examples
#' if (interactive()) amlrPinnipeds_gui(launch.browser = TRUE)
#'
#' @seealso \url{https://www.fisheries.noaa.gov/about/antarctic-ecosystem-research-division-southwest-fisheries-science-center}
#'
#' @export
amlrPinnipeds_gui <- function(launch.browser = TRUE) {
  appDir <- system.file("shiny", package = "amlrPinnipeds")
  if (appDir == "") {
    stop("There was an error opening the amlrPinnipeds Shiny app; try re-installing 'amlrPinnipeds'",
         call. = FALSE)
  }
  shiny::runApp(appDir, launch.browser = launch.browser, display.mode = "normal")
}
