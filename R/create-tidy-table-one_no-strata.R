
#' @title
#' Tidy table one with no strata
#'
#' @description
#' Creates a tidy data frame of the results that can go into a "Table 1" of
#' summary descriptive statistics of a study sample. Inspiration for this is owed
#' to the `tableone` package by Kazuki Yoshida.
#'
#' @param data A data frame or tibble containing the variables to be summarized.
#' @param strata NULL
#' @param vars Character vector of the variable names to be summarized. If
#'   empty, then all variables in the given data frame are used.
#' @param na_level Character string of the text to replace `NA` in the strata
#'   variable, if any exist.
#' @param b_replicates an integer specifying the number of replicates used in
#'   the Monte Carlo test for Fisher's Exact test and Chi-square test.
#' @param ... Additional arguments. Not used.
#'
#' @importFrom car leveneTest
#' @importFrom dplyr bind_rows
#' @importFrom dplyr count
#' @importFrom dplyr count
#' @importFrom dplyr distinct
#' @importFrom dplyr everything
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_if
#' @importFrom dplyr n
#' @importFrom dplyr n_distinct
#' @importFrom dplyr one_of
#' @importFrom dplyr pull
#' @importFrom dplyr relocate
#' @importFrom dplyr rename
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom dplyr transmute
#' @importFrom dplyr ungroup
#' @importFrom forcats fct_na_value_to_level
#' @importFrom glue glue
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @importFrom purrr map_dbl
#' @importFrom purrr map2
#' @importFrom nortest ad.test
#' @importFrom purrr pluck
#' @importFrom rlang ensym
#' @importFrom stats as.formula
#' @importFrom stats bartlett.test
#' @importFrom stats chisq.test
#' @importFrom stats fisher.test
#' @importFrom stats kruskal.test
#' @importFrom stats ks.test
#' @importFrom stats oneway.test
#' @importFrom stats quantile
#' @importFrom stats sd
#' @importFrom stats shapiro.test
#' @importFrom tableone CreateTableOne
#' @importFrom tableone ExtractSmd
#' @importFrom testit has_warning
#' @importFrom tibble as_tibble
#' @importFrom tibble tibble
#' @importFrom tidyr complete
#' @importFrom tidyr gather
#'
#' @return
#' A tibble with the following results
#' \describe{
#'   \item{strata}{Level of the stratifying variable}
#'   \item{var}{Variable/column name}
#'   \item{n}{Number of records}
#'   \item{n_distinct}{Numer of distinct values}
#'   \item{complete}{Number of non-missing observations}
#'   \item{missing}{Number of missing observations}
#'   \item{mean}{Mean}
#'   \item{sd}{Standard deviation}
#'   \item{p0}{Minimum}
#'   \item{p25}{25th percentile}
#'   \item{p50}{Median}
#'   \item{p75}{75th percentile}
#'   \item{p100}{Maximum}
#'   \item{cv}{Coefficient of variation}
#'   \item{shapiro_test}{Shapiro-Wilkes test: p-value}
#'   \item{ks_test}{Kolmogorov-Smirnov test: p-value}
#'   \item{ad_test}{Anderson-Darling test for normality: p-value}
#'   \item{level}{Level of the variable}
#'   \item{n_level}{Total number in the variable's group}
#'   \item{n_strata}{Total number in the variable group and strata}
#' }
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' tab1 <- create_tidy_table_one_no_strata(data = pbc_mayo,
#'                                         strata = NULL,
#'                                         vars = c("time",
#'                                                  "status",
#'                                                  "trt",
#'                                                  "age",
#'                                                  "sex",
#'                                                  "ascites",
#'                                                  "hepato",
#'                                                  "spiders",
#'                                                  "edema",
#'                                                  "bili",
#'                                                  "chol",
#'                                                  "albumin",
#'                                                  "copper",
#'                                                  "alk_phos",
#'                                                  "ast",
#'                                                  "trig",
#'                                                  "platelet",
#'                                                  "protime",
#'                                                  "stage"))
#'
#' dplyr::glimpse(tab1)
#'
#'
#' library(ggplot2)  # diamonds data set
#'
#' tab2 <- create_tidy_table_one_no_strata(data = diamonds,
#'                                         vars = c("carat",
#'                                                  "cut",
#'                                                  "color",
#'                                                  "clarity",
#'                                                  "depth",
#'                                                  "table",
#'                                                  "price"))
#'
#' dplyr::glimpse(tab2)


create_tidy_table_one_no_strata <- function(data,
                                            strata = NULL,
                                            vars,
                                            na_level = "(Missing)",
                                            b_replicates = 2000, ...) {

  # Silence no visible binding for global variable
  dat <- res <- n_level_valid <- n_strata_valid <- label <- sort1 <- sort2 <- NULL

  # Ensure strata is NULL since this function does not handle stratified data
  if (!is.null(strata)) {
    stop("Strata must be NULL for create_tidy_table_one_no_stata.")
  }

  # Convert ordered factors to regular factors
  data <- data |>
    dplyr::mutate_if(.predicate = ~ ("ordered" %in% class(.)),
                     .funs = ~ factor(., ordered = FALSE))


  # If no variables are specified, use all variables in the dataset
  if (missing(vars)) {
    vars <- names(data)
  }


  #### Get variable info --------------------------------

  # Extract variable labels for later use
  var_lbls <- tibble::tibble(var = names(data)) |>
    mutate(label = purrr::map_chr(.x = data[, var],
                                  .f = ~ get_var_labels(x = .x)))

  # Get variable types and other meta-information
  var_info <- get_var_info(data = data,
                           .vars = vars)

  # Create sorting variables to maintain order in the output
  var_info <- var_info |>
    mutate(sort1 = cumsum(var != dplyr::lag(var, default = dplyr::first(var))),
           sort1 = sort1 + 1) |>
    group_by(var) |>
    mutate(sort2 = dplyr::row_number()) |>
    ungroup()

  # Identify categorical and continuous variables
  cat_vars <- var_info |>
    dplyr::filter(var_type == "categorical") |>
    dplyr::pull(var) |>
    unique()

  con_vars <- var_info |>
    dplyr::filter(var_type == "continuous") |>
    dplyr::pull(var) |>
    unique()


  #### Get tables stats --------------------------------

  res_stats <- list()

  # Process categorical variables, if any
  if (length(cat_vars) > 0) {

    suppressWarnings(

      cat_stats <- tibble::tibble(var = cat_vars) |>
        mutate(dat = purrr::map(.x = var,
                                .f = ~ dplyr::select(data,
                                                     dplyr::all_of(.x))),
               res = purrr::map(.x = dat,
                                .f = ~ do_one_cat(.x))) |>
        dplyr::select(var, res) |>
        tidyr::unnest(res) |>
        mutate(pct = n_level / n_strata,
               pct_valid = n_level_valid / n_strata_valid)

    )

    res_stats <- dplyr::bind_rows(res_stats,
                                  cat_stats)

  }


  # Process continuous variables, if any
  if (length(con_vars) > 0) {

    con_stats <- data |>
      dplyr::select(dplyr::one_of(con_vars)) |>
      tidyr::pivot_longer(cols = dplyr::all_of(con_vars),
                          names_to = "var",
                          values_to = "value") |>
      group_by(var) |>
      summarise(n = dplyr::n(),
                n_distinct = dplyr::n_distinct(value),
                complete = sum(!is.na(value)),
                missing = sum(is.na(value)),
                mean = mean(value, na.rm = TRUE),
                sd = sd(value, na.rm = TRUE),
                p0 = custom_min(value, na.rm = TRUE),
                p25 = quantile(value, probs = 0.25, na.rm = TRUE),
                p50 = quantile(value, probs = 0.50, na.rm = TRUE),
                p75 = quantile(value, probs = 0.75, na.rm = TRUE),
                p100 = custom_max(value, na.rm = TRUE),
                cv = sd / mean,
                shapiro_test = calc_shapiro_test(var = value),
                ks_test = calc_ks_test(var = value),
                ad_test = calc_ad_test(var = value)) |>
      ungroup()

    res_stats <- dplyr::bind_rows(res_stats,
                                  con_stats)

  }


  #### Clean up and arrange --------------------------------

  # Add variable info (class, type) and labels to the results
  class_and_type <- var_info |>
    dplyr::select(-level,
                  -sort1,
                  -sort2) |>
    dplyr::distinct()

  res_stats <- res_stats |>
    dplyr::left_join(class_and_type,
                     by = "var") |>
    dplyr::left_join(var_lbls,
                     by = "var")


  # Arrange results by variable and level, maintaining the original order
  if (length(cat_vars) > 0) {

    sort_vars <- var_info |>
      dplyr::select(var, level, sort1, sort2)

    res_stats <- res_stats |>
      dplyr::left_join(sort_vars,
                       by = c("var", "level")) |>
      dplyr::arrange(sort1, sort2) |>
      mutate(var = forcats::fct_inorder(var),
             level = forcats::fct_inorder(level)) |>
      dplyr::select(-sort1, -sort2) |>
      dplyr::relocate(class,
                      var_type,
                      label,
                      .after = dplyr::everything())
  } else {

    sort_vars <- var_info |>
      dplyr::select(var, sort1, sort2)

    res_stats <- res_stats |>
      dplyr::left_join(sort_vars,
                       by = c("var")) |>
      dplyr::arrange(sort1, sort2) |>
      mutate(var = forcats::fct_inorder(var)) |>
      dplyr::select(-sort1, -sort2) |>
      dplyr::relocate(class,
                      var_type,
                      label,
                      .after = dplyr::everything())

  }


  #### Return results --------------------------------

  return(res_stats)

}




#### Helper function --------------------------------

do_one_cat <- function(x) {

  # Silence no visible binding for global variable
  n_level_valid <- NULL

  # Summarize categorical variable, filling in missing levels if necessary
  x |>
    dplyr::rename("level" = 1) |>
    dplyr::count(level,
                 name = "n_level",
                 .drop = FALSE) |>
    tidyr::complete(level,
                    fill = list(n_level = 0)) |>
    mutate(n_strata = sum(n_level, na.rm = TRUE)) |>
    mutate(n_level_valid = dplyr::if_else(is.na(level), NA_integer_, n_level),
           n_strata_valid = sum(n_level_valid, na.rm = TRUE))
}
