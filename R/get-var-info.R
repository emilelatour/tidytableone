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


get_var_info <- function(data,
                         .vars = NULL) {


  if (is.null(.vars)) {
    .vars = names(data)
  }


  #### Get variable names, class, and levels --------------------------------

  ## Factor variables ----------------

  names_fct <- data %>%
    dplyr::select_if(function(col) is.factor(col)) %>%
    purrr::map(.x = .,
               .f = ~ levels(.x)) %>%
    tibble::enframe(.,
                    name = "var",
                    value = "level") %>%
    tidyr::unnest(cols = c(level)) %>%
    mutate_all(as.character)


  ## character variables ----------------

  names_chr <- data %>%
    dplyr::select_if(function(col) is.character(col)) %>%
    mutate_all(.tbl = .,
               .funs = list(~ as.factor(.))) %>%
    purrr::map(.x = .,
               .f = ~ levels(.x)) %>%
    tibble::enframe(.,
                    name = "var",
                    value = "level") %>%
    tidyr::unnest(cols = c(level)) %>%
    mutate_all(as.character)


  ## non-factor and non-character variables ----------------

  names_num <- data %>%
    dplyr::select_if(function(col) {!(is.factor(col) | is.character(col))}) %>%
    names(.) %>%
    tibble::tibble(var = .,
                   level = NA_character_) %>%
    mutate_all(as.character)


  ## Get the variable classes ----------------

  var_class <- data %>%
    purrr::map(.x = .,
               .f = ~ class(.x))


  ## Combine them ----------------

  var_info <-  var_class %>%
    purrr::map_chr(.x = .,
                   .f = ~ .x[[1]]) %>%
    tibble::enframe(name = "var",
                    value = "class") %>%
    dplyr::left_join(dplyr::bind_rows(names_fct,
                                      names_chr,
                                      names_num),
                     .,
                     by = "var") %>%
    dplyr::filter(var %in% .vars) %>%
    mutate(var = factor(var,
                        levels = .vars)) %>%
    dplyr::arrange(var) %>%
    mutate(var_type = dplyr::case_when(
      class %in% c("factor", "character", "ordered") ~ "categorical",
      TRUE ~ "continuous"),
      var = as.character(var))


  #### End of function --------------------------------

  return(var_info)

}




