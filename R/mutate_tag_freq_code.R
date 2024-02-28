#' Create tag/freq/code ID column
#'
#' A 'wrapper' around mutate that creates a new tag/freq/code ID column
#'
#' @param x data frame; must contain 'tag', 'freq', and 'code' columns
#'
#' @details A wrapper around \code{\link[dplyr]{mutate}} that pastes the tag,
#'   freq, and code values together to create a single identifying string.
#'   Typically used in [mod_dcc_pinniped_server()] or other Pinniped DCC data
#'   processing scripts
#'
#' @return \code{x}, with the new column 'tag_freq_code' calculated as
#'   `paste(tag, freq, code, sep = " | ")`
#'
#' @examples
#' x <- data.frame(tag = "A21", freq = 164.105, code = 12)
#' mutate_tag_freq_code(x)
#'
#' @export
mutate_tag_freq_code <- function(x) {
  stopifnot(
    inherits(x, "data.frame"),
    c("tag", "freq", "code") %in% names(x)
  )

  x %>% mutate(tag_freq_code = paste(tag, freq, code, sep = " | "))
}
