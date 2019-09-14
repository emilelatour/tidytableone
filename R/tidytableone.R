#' tidytableone: Tidy construction of "Table 1"
#'
#' A tidy approach to creating the "Table 1" of study characteristics common in
#' bimoedical research.
#'
#' @examples
#' # Example usage:
#' library(tidytableone)
#'
#' @docType package
#' @name tidytableone
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
## From Jenny Bryan's googlesheets package
## From infer package
## https://github.com/tidymodels/infer/blob/master/R/infer.R
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(".",
      "level",
      "var",
      "bartlett_test",
      "check_categorical_test",
      "chisq_test",
      "fisher_test",
      "form",
      "kruskal_test",
      "n_level",
      "oneway_test",
      "tab",
      "value",
      "var_type",
      "x",
      "y"
    )
  )
}

