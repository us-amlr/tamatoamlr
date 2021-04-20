#' Convert species column to a factor
#'
#' Convert species column to a factor with specific levels
#'
#' @param x data frame that at minimum contains a 'species' column
#'
#' @return \code{x}, with the column 'species' converted to a \link[base]{factor} that
#'   has levels: \code{\link[base]{names}(\link{pinniped.sp.list})}
#'
#' @export
factor_species <- function(x) {
  stopifnot(
    inherits(x, "data.frame"),
    "species" %in% names(x)
  )

  x %>% mutate(species = factor(species, levels = names(amlrPinnipeds::pinniped.sp.list)))
}
