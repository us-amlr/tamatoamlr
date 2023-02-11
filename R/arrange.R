#' Sort pinniped data
#'
#' Sort pinniped data
#'
#' @param x data frame. This data frame must contain specific columns; see Details
#' @param season.info season.info data frame; ideally the season.info
#'   data frame from the output of \code{\link{mod_season_info}}
#' @param sort_columns character vector of column(s) by which to first sort
#'   \code{x} when using \code{arrange_tag}. Ignored if \code{NULL};
#'   default is \code{c("species", "non_amlr_tag")}
#' @param ... additional tidyselect arguments passed to
#'   \code{\link[dplyr]{arrange}()}, after the primary (i.e., season or tag) sort
#' @param .desc logical; for \code{arrange_season} only.
#'   Indicates if the data should be sorted by the descending season open date.
#'   Default is \code{TRUE}
#' @param .remove logical; should the additional columns created for sorting
#'   be removed before returning the data frame?
#'   Default is \code{TRUE}
#'
#' @details
#' \code{arrange_season} sorts \code{x} by season,
#' specifically by the corresponding season_open_date (descending, by default),
#' and then by other columns provided via \code{...}.
#'
#' \code{arrange_tag} sorts \code{x} by the columns specified as
#' character strings in 'sort_columns', if provided,
#' then by the tag number, and finally by other columns provided via \code{...}.
#' For the tag number, tags with leading letters (e.g., 'A' in 'A100')
#' are sorted after tag numbers without letters.
#' Tag numbers with no numbers are sorted last.
#'
#' @return \code{x}, sorted as described in Details
#'
#' @examples
#' tags.df <- data.frame(
#'   species = c("Leopard seal", rep("Fur seal", 7)),
#'   non_amlr_tag = TRUE,
#'   tag = c("10", "j89", "090", "A89", "C004", "A10", "A100", "CTOrange")
#' )
#' arrange_tag(tags.df)
#' arrange_tag(tags.df, sort_columns = NULL)
#' arrange_tag(tags.df, .remove = FALSE)
#'
#' @name arrange
#'
#' @export
arrange_season <- function(x, season.info, ..., .desc = TRUE, .remove = TRUE) {
  stopifnot(
    inherits(x, "data.frame"),
    "season_name" %in% names(x),
    inherits(season.info, "data.frame"),
    all(c("season_name", "season_open_date") %in% names(season.info)),
    inherits(.desc, "logical"),
    inherits(.remove, "logical")
  )

  season.info.sel <- season.info %>% select(season_name, season_open_date)
  x.out <- left_join(x, season.info.sel, by = "season_name") %>%
    arrange(if (.desc) desc(season_open_date) else season_open_date, ...)

  if (nrow(x) != nrow(x.out)) stop("Error in arrange_season_info() - additional rows")

  if (.remove) x.out %>% select(-season_open_date) else x.out
}


#' @name arrange
#' @export
arrange_tag <- function(x, ..., sort_columns = c("species", "non_amlr_tag"),
                        .remove = TRUE) {
  stopifnot(
    inherits(x, "data.frame"),
    "tag" %in% names(x),
    inherits(.remove, "logical")
  )
  if (!is.null(sort_columns)) stopifnot(all(sort_columns %in% names(x)))

  x.out <- x %>%
    mutate(tag_letter = str_replace_na(str_match(tag, "[A-Za-z]"), "0"),
           tag_numeric = as.numeric(str_remove_all(tag, "[A-Za-z]")),
           tag_letter = if_else(is.na(tag_numeric), NA_character_, tag_letter)) %>%
    arrange(!!!syms(sort_columns), tag_letter, tag_numeric)

  if (.remove) x.out %>% select(-c(tag_letter, tag_numeric)) else x.out
}
