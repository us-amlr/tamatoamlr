#' Vector of relevant pinniped species
#'
#' The vector contents are lowercase, while the vector names are 'sentence'
#' case (via \code{\link[stringr]{str_to_sentence}}).
#' The species included in this list are: Fur seal (Antarctic fur seal),
#' Crabeater seal, Elephant seal (Southern elephant seal), Leopard seal, and Weddell seal
#'
#' @format A (named) character vector
"pinniped.sp"


#' Vector of relevant phocid species
#'
#' The same as \code{\link{pinniped.sp}}, except only phocids (i.e., not Fur seals)
#'
#' @format A (named) character vector
"pinniped.phocid.sp"


#' Colors used in plots by pinniped species
#'
#' The vector contents are colors, specifically, \code{\link[scales]{hue_pal}()(5)},
#' while the vector names are the same as \code{\link{pinniped.sp}}
#'
#' @format A (named) character vector
"pinniped.sp.colors"
