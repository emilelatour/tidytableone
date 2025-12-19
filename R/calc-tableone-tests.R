#' @title
#' Return variables from table one to look into
#'
#' @description
#' Given the results of the `create_tidy_table_one` this will give you a list
#' with the variable names for those that were, for example, non-normal by
#' Shapiro-Wilkes test or that had unequal variance by Barlett's test.
#'
#' @param table_one A tibble result from `create_tidy_table_one`
#'
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom stringr str_detect
#'
#' @return
#' A list
#'
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' tab1 <- create_tidytableone(data = pbc_mayo,
#'                               strata = "trt",
#'                               vars = c("time",
#'                                         "status",
#'                                         "trt",
#'                                         "age",
#'                                         "sex",
#'                                         "ascites",
#'                                         "hepato",
#'                                         "spiders",
#'                                         "edema",
#'                                         "bili",
#'                                         "chol",
#'                                         "albumin",
#'                                         "copper",
#'                                         "alk_phos",
#'                                         "ast",
#'                                         "trig",
#'                                         "platelet",
#'                                         "protime",
#'                                         "stage"))
#'
#' dplyr::glimpse(tab1)
#'
#' calc_tableone_tests(tab1)
calc_tableone_tests <- function(table_one) {

#### shapiro_test --------------------------------

# if the p value is less than the chosen alpha level, then the null hypothesis
# is rejected and there is evidence that the data tested are not normally
# distributed

non_normal_shapiro <- table_one %>%
  dplyr::filter(shapiro_test < 0.05) %>%
  dplyr::distinct(var) %>%
  dplyr::pull(var) %>%
  as.character()

#### Kolmogorov-Smirnov Test --------------------------------

non_normal_ks <- table_one %>%
  dplyr::filter(ks_test < 0.05) %>%
  dplyr::distinct(var) %>%
  dplyr::pull(var) %>%
  as.character()

#### Andersen-Darling Test --------------------------------

non_normal_ad <- table_one %>%
  dplyr::filter(ad_test < 0.05) %>%
  dplyr::distinct(var) %>%
  dplyr::pull(var) %>%
  as.character()


#### bartlett_test  --------------------------------

# the null hypothesis, H0 that all k population variances are equal against the
# alternative that at least two are different.

unequal_variance_bartlett <- table_one %>%
  dplyr::filter(bartlett_test < 0.05) %>%
  dplyr::distinct(var) %>%
  dplyr::pull(var) %>%
  as.character()

#### levene_test  --------------------------------

# the null hypothesis, H0 that all k population variances are equal against the
# alternative that at least two are different.

unequal_variance_levene <- table_one %>%
  dplyr::filter(levene_test < 0.05) %>%
  dplyr::distinct(var) %>%
  dplyr::pull(var) %>%
  as.character()


#### Chi-square is suspect --------------------------------

doubtful_chisq_test <- table_one %>%
  # dplyr::filter(check_categorical_test != "ok" |
  #                 check_categorical_test == "warning" |
  #                 !is.na(check_categorical_test)) %>%
  dplyr::filter(check_categorical_test == "warning") %>%
  dplyr::filter(stringr::str_detect(var_type,
                                    pattern = paste(c("Categorical",
                                                      "categorical"),
                                                    collapse = "|"))) %>%
  dplyr::distinct(var) %>%
  dplyr::pull(var) %>%
  as.character()


list_of_tests <- list(non_normal_shapiro = non_normal_shapiro,
     non_normal_ks = non_normal_ks,
     non_normal_ad = non_normal_ad,
     unequal_variance_bartlett = unequal_variance_bartlett,
     unequal_variance_levene = unequal_variance_levene,
     doubtful_chisq_test = doubtful_chisq_test)

list_of_tests[lengths(list_of_tests) == 0] <- NA_character_

return(list_of_tests)

}
