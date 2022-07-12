#' Convert species column to a factor
#'
#' A 'wrapper' around mutate that converts the species column to a factor with the specified levels
#'
#' @param x data frame that at minimum contains a 'species' column
#' @param levels character; passed to 'levels' argument of \link[base]{factor}.
#'   Default is \code{names(\link{pinniped.sp})}
#'
#' @details A wrapper around \code{\link[dplyr]{mutate}} that converts the species column to a factor with the specified levels.
#'   This allows this function to be used directly in dplyr pipelines.
#'
#' @return \code{x}, with the column 'species' converted to a \link[base]{factor} that
#'   has levels: \code{\link[base]{names}(\link{pinniped.sp})}, i.e.
#'   \code{c("Fur seal", "Crabeater seal", etc.)}
#'
#' @examples
#' x <- data.frame(species = c("Fur seal", "Weddell seal"), count = c(2, 1))
#' mutate_factor_species(x)
#' mutate_factor_species(x, levels = c("Fur seal", "Weddell seal", "Eseal"))
#'
#' @export
mutate_factor_species <- function(x, levels = names(amlrPinnipeds::pinniped.sp)) {
  stopifnot(
    inherits(x, "data.frame"),
    "species" %in% names(x),
    inherits(levels, "character")
  )

  x %>% mutate(species = factor(tolower(species), levels = levels))
}
