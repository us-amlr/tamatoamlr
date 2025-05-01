#' Vector of relevant pinniped species
#'
#' The vector contents and vector names are 'sentence' case (via
#' [stringr::str_to_sentence()]). The species included in this list are: Fur
#' seal (Antarctic fur seal), Crabeater seal, Elephant seal (Southern elephant
#' seal), Leopard seal, and Weddell seal
#'
#' @format A (named) character vector
"pinniped.sp"


#' Colors used in plots by pinniped species
#'
#' The vector contents are colors, specifically,
#' \code{\link[scales]{hue_pal}()(5)}, while the vector names are the same as
#' \code{\link{pinniped.sp}}
#'
#' @format A (named) character vector
"pinniped.sp.colors"


#' Vector of relevant phocid species
#'
#' The same as \code{\link{pinniped.sp}}, except only phocids (i.e., not Fur
#' seals)
#'
#' @format A (named) character vector
"pinniped.phocid.sp"

#' Vector of study pinniped species
#'
#' The same as \code{\link{pinniped.sp}}, except only US AMLR study species:
#' Antarctic fur seals, southern elephant seals, leopard seals, and Weddell seals
#'
#' @format A (named) character vector
"pinniped.sp.study"


#' CS-PHOC core census locations
#'
#' Names of Cape Shirreff Phocid Census (CS-PHOC) core census locations. Note
#' that while the majority of these correspond directly to a row in the
#' beaches table, officially these are all location group values.
#' This means they have been designed to correspond to the location_group column
#' in the vCensus_Phocid view.
#'
#' @format A character vector
"csphoc.core.location.groups"


#' AFS Study Beach census columns
#'
#' Names of the AFS study beach census columns, in order, to select for and
#' display
#'
#' @format A character vector
"afs.study.beach.counts"


#' Sample DCC data
#'
#' Sample DCC data for both standard (from R4500 units) and coded (from R4500C)
#' units. These data were adapted from raw U.S. AMLR Pinniped DCC data files.
#' Note that sample raw DCC download files are available in inst/extdata, e.g
#' using `system.file("extdata", "dcc-coded.csv")`
#'
#' @format ##`dcc` A data frame with 52 rows (i.e., 'pings') and either 7
#'   columns for coded data, or 6 columns for standard data:
#' \describe{
#'   \item{Yr, Day, Hr, Mn}{Year, Julian day, hour (24Hr), and minute as
#'   integers, respectively}
#'   \item{Fr}{Transmitter frequency Will either be six digits (full frequency
#'   omitting the decimal, e.g. 164305) or four digits (omitting the decimal and
#'   the '16', e.g. 4305)}
#'   \item{Sig}{Signal strength of the recorded ping; an integer greater than 0.
#'   For U.S. AMLR Pinniped data,
#'   over 80 is typically considered good, while over 90 is ideal}
#'   \item{Code}{Transmitter code. Only present for coded data}
#'   \item{station}{Column with station info; note not part of the CSV files}
#'   \item{Ant,NumDet,NumMort,DataInd,Data}{Columns in dcc-coded.csv file,
#'   that are ignored and not included in dcc.coded}
#'   \item{NumPls,PR,NumMatch}{Columns in dcc-standard.csv file,
#'   that are ignored and not included in dcc.standard}
#' }
#'
#' @name dcc-data
"dcc.coded"

#' @name dcc-data
"dcc.standard"
