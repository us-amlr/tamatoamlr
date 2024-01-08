#' Generate column to sort Pinniped tags
#'
#' Generate column to sort Pinniped tags
#'
#' @param x data frame, with at least one of 'tag' or 'tag_primary' columns
#' @param tag.sort logical, indicates if a tag__sort column be
#'   generated using the tag column? Default is \code{TRUE}
#' @param tag.sort.primary logical, indicates if a tag_primary_sort column be
#'   generated using the tag_primary column. Default is \code{TRUE}
#'
#' @details
#' Pinniped tags may contain a mix of letters and numbers.
#' Specifically, tags may have a capital letter as the first symbol,
#' followed by some number of 0s.
#' Because of this, tags must be treated as characters,
#' and thus will not properly sort if left as-is
#' (e.g., A100 will sort in front of A21).
#' This function generates column(s) with the tag values padded with 0s
#' in the relevant places to provide intuitive sorting.
#'
#' If one of tag.sort and tag.sort.primary are \code{TRUE}, then the output
#' data frame will be sorted by the relevant sort column.
#' If both of tag.sort and tag.sort.primary are \code{TRUE}, then the output
#' will be sorted by tag_sort_primary.
#'
#' @examples
#' df <- data.frame(
#'   tag_primary = c("10", "A100", "A20", "5"),
#'   tag = c("C99", "A100", "A20", "5")
#' )
#' tag_sort(df, tag.sort = TRUE, tag.sort.primary = TRUE)
#'
#' @export
tag_sort <- function(x, tag.sort = TRUE, tag.sort.primary = TRUE) {
  stopifnot(
    is_bool(tag.sort),
    is_bool(tag.sort.primary),
    inherits(x, "data.frame")
  )

  if (!tag.sort && !tag.sort.primary)
    warning("Both 'tag.sort' and 'tag.sort.primary' are false, ",
            "and thus no columns will be added to x")

  if (tag.sort & !("tag" %in% names(x)))
    stop("If 'tag.sort' is TRUE, then x must contain the column 'tag'")

  if (tag.sort.primary & !("tag_primary" %in% names(x)))
    stop("If 'tag.sort.primary' is TRUE, ",
         "then x must contain the column 'tag_primary'")

  LETTERS.regex <- paste0("[", paste0(LETTERS, collapse = ""), "]")


  if (tag.sort) {
    x <- x %>%
      mutate(tag_sort = case_when(
        str_detect(tag, LETTERS.regex) ~ #& !non_amlr_tag_primary ~
          paste0(
            str_sub(tag, 1, 1),
            str_pad(
              str_split_i(tag, LETTERS.regex, 2),
              width = 9, side = "left", pad = "0")
          ),
        .default = str_pad(tag, width = 10, side = "left", pad = "0")
      )) %>%
      arrange(tag_sort)
  }

  if (tag.sort.primary) {
    x <- x %>%
      mutate(tag_sort_primary = case_when(
        str_detect(tag_primary, LETTERS.regex) ~ #& !non_amlr_tag_primary ~
          paste0(
            str_sub(tag_primary, 1, 1),
            str_pad(
              str_split_i(tag_primary, LETTERS.regex, 2),
              width = 9, side = "left", pad = "0")
          ),
        .default = str_pad(tag_primary, width = 10, side = "left", pad = "0")
      )) %>%
      arrange(tag_sort_primary)
  }

  x
}
