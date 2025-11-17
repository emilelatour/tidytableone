#' @title
#' Get info about variables in a data frame
#'
#' @description
#' Mostly a helper function for other tidy_table_one functions. The function
#' takes a data frame or tibble and returns a descriptive tibble with the
#' variable names, class, binary type (categorical or continuous), and the
#' factor levels.
#'
#' @param data A data.frame or tbl_df
#' @param .vars A character string of variable/column names. If empty, all
#'   columns are used
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @importFrom dplyr select_if
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @importFrom tibble enframe
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#'
#' @return
#' A tbl_df with colums for
#' \describe{
#'   \item{var}{name of the variable/column}
#'   \item{level}{factor level if applicable, character levels too}
#'   \item{class}{variable class}
#'   \item{var_type}{variable type: categorical or continuous}
#' }
#' @export
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' lapply(diamonds, class)
#' get_var_info(data = diamonds,
#'              .vars = c("carat",
#'                        "cut",
#'                        "color",
#'                        "clarity",
#'                        "depth",
#'                        "table",
#'                        "price")) %>%
#'   print(n = Inf)
#'
#'
#' get_var_info(data = pbc_mayo)
#'
#' get_var_info(data = pbc_mayo,
#'              .vars = c("status", "sex", "stage", "age"))


get_var_info <- function(data, .vars = NULL) {
  
  if (is.null(.vars)) .vars <- names(data)

  out <- lapply(.vars, function(v) {
    x <- data[[v]]
    cls <- class(x)[1]

    if (is.factor(x)) {
      tibble::tibble(
        var = v,
        level = as.character(levels(x)),
        class = cls,
        var_type = "categorical"
      )
    } else if (is.character(x)) {
      lv <- sort(unique(x))
      tibble::tibble(
        var = v,
        level = lv,
        class = cls,
        var_type = "categorical"
      )
    } else {
      tibble::tibble(
        var = v,
        level = NA_character_,
        class = cls,
        var_type = "continuous"
      )
    }
  })

  dplyr::bind_rows(out)
}


