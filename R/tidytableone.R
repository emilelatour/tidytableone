#' tidytableone: Tidy construction of "Table 1"
#'
#' A tidy approach to creating the "Table 1" of study characteristics common in
#' biomedical research.
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
      "fisher_test_simulated",
      "form",
      "kruskal_test",
      "levene_test",
      "n_level",
      "n_strata",
      "oneway_test",
      "oneway_test_unequal_var",
      "oneway_test_equal_var",
      "shapiro_test",
      "ad_test",
      "ks_test",
      "tab",
      "value",
      "var_type",
      "var_level",
      "x",
      "y"
    )
  )
}

