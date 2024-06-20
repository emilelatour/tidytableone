
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


  if (!is.null(strata)) {
    stop("Strata must be NULL for create_tidy_table_one_no_stata.")
  }

  data <- data |>
    dplyr::mutate_if(.predicate = ~ ("ordered" %in% class(.)),
                     .funs = ~ factor(., ordered = FALSE))


  if (missing(vars)) {
    vars <- names(data)
  }


  #### Get variable info --------------------------------

  var_lbls <- tibble::tibble(var = names(data)) |>
    mutate(label = purrr::map_chr(.x = data[, var],
                                  .f = ~ get_var_labels(x = .x)))

  var_info <- get_var_info(data = data,
                           .vars = vars)

  cat_vars <- var_info |>
    dplyr::filter(var_type == "categorical") |>
    dplyr::pull(var) |>
    unique()

  con_vars <- var_info |>
    dplyr::filter(var_type == "continuous") |>
    dplyr::pull(var) |>
    unique()


  if (length(cat_vars) > 0 & length(con_vars) > 0) {

    #### Categorical stats --------------------------------

    suppressWarnings(
      cat_stats <- data |>
        dplyr::select(dplyr::one_of(cat_vars)) |>
        tidyr::pivot_longer(cols = dplyr::one_of(cat_vars),
                            names_to = "var",
                            values_to = "level") |>
        dplyr::count(var, level,
                     name = "n_level",
                     .drop = FALSE) |>
        group_by(var) |>
        tidyr::complete(level,
                        fill = list(n_level = 0)) |>
        mutate(n_strata = sum(n_level, na.rm = TRUE)) |>
        ungroup()
    )

    # Calc percentage
    cat_stats <- cat_stats |>
      mutate(pct = n_level / n_strata)

    #### Continuous stats --------------------------------

    con_stats <- data |>
      dplyr::select(dplyr::one_of(con_vars)) |>
      tidyr::pivot_longer(cols = dplyr::one_of(con_vars),
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


    #### Calc SMD --------------------------------

    # Not calculated

    #### Hypothesis tests --------------------------------

    # No hypothesis tests performed when only one group

    #### Combine results --------------------------------

    res_stats <- dplyr::bind_rows(con_stats,
                                  cat_stats)


    #### Arrange results --------------------------------

    var_lvls <- unique(var_info$var)

    level_lvls <- var_info |>
      dplyr::transmute(var_level = glue::glue("{var}_{level}")) |>
      dplyr::pull()

    res_stats <- res_stats |>
      mutate(var = factor(var,
                          levels = var_lvls),
             var_level = glue::glue("{var}_{level}"),
             var_level = factor(var_level,
                                levels = level_lvls)) |>
      dplyr::arrange(var, var_level) |>
      mutate(level = factor(level)) |>
      dplyr::select(-var_level)


    #### Add on var_info --------------------------------

    res_stats <- var_info |>
      dplyr::select(-level) |>
      dplyr::distinct() |>
      mutate(var = factor(var,
                          levels = levels(res_stats$var))) |>
      dplyr::left_join(res_stats,
                       .,
                       by = "var")

    res_stats <- res_stats |>
      dplyr::left_join(var_lbls,
                       by = "var")

    #### Arrange  --------------------------------

    res_stats <- res_stats |>
      mutate(var = factor(var, levels = vars)) |>
      dplyr::arrange(var, level)


    #### Return results --------------------------------

    res_stats <- res_stats |>
      dplyr::relocate(class,
                      var_type,
                      label,
                      .after = dplyr::everything())

    return(res_stats)


  } else if (length(cat_vars) > 0) {

    #### Categorical stats --------------------------------

    suppressWarnings(
      cat_stats <- data |>
        dplyr::select(dplyr::one_of(cat_vars)) |>
        tidyr::pivot_longer(cols = dplyr::one_of(cat_vars),
                            names_to = "var",
                            values_to = "level") |>
        dplyr::count(var, level,
                     name = "n_level",
                     .drop = FALSE) |>
        group_by(var) |>
        tidyr::complete(level,
                        fill = list(n_level = 0)) |>
        mutate(n_strata = sum(n_level, na.rm = TRUE)) |>
        ungroup()
    )

    # Calc percentage
    cat_stats <- cat_stats |>
      mutate(pct = n_level / n_strata)


    #### Calc SMD --------------------------------

    # Not calculated

    #### Hypothesis tests --------------------------------

    # No hypothesis tests performed when only one group

    #### Combine results --------------------------------

    res_stats <- cat_stats


    #### Arrange results --------------------------------

    var_lvls <- unique(var_info$var)

    level_lvls <- var_info |>
      dplyr::transmute(var_level = glue::glue("{var}_{level}")) |>
      dplyr::pull()

    res_stats <- res_stats |>
      mutate(var = factor(var,
                          levels = var_lvls),
             var_level = glue::glue("{var}_{level}"),
             var_level = factor(var_level,
                                levels = level_lvls)) |>
      dplyr::arrange(var, var_level) |>
      mutate(level = factor(level)) |>
      dplyr::select(-var_level)


    #### Add on var_info --------------------------------

    res_stats <- var_info |>
      dplyr::select(-level) |>
      dplyr::distinct() |>
      mutate(var = factor(var,
                          levels = levels(res_stats$var))) |>
      dplyr::left_join(res_stats,
                       .,
                       by = "var")

    res_stats <- res_stats |>
      dplyr::left_join(var_lbls,
                       by = "var")


    #### Arrange  --------------------------------

    res_stats <- res_stats |>
      mutate(var = factor(var, levels = vars)) |>
      dplyr::arrange(var, level)


    #### Return results --------------------------------

    res_stats <- res_stats |>
      dplyr::relocate(class,
                      var_type,
                      label,
                      .after = dplyr::everything())

    return(res_stats)

  } else if (length(con_vars) > 0) {

    #### Continuous stats --------------------------------

    con_stats <- data |>
      dplyr::select(dplyr::one_of(con_vars)) |>
      tidyr::pivot_longer(cols = dplyr::one_of(con_vars),
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


    #### Calc SMD --------------------------------

    # Not calculated

    #### Hypothesis tests --------------------------------

    # No hypothesis tests performed when only one group

    #### Combine results --------------------------------

    res_stats <- con_stats


    #### Arrange results --------------------------------

    var_lvls <- unique(var_info$var)

    res_stats <- res_stats |>
      mutate(var = factor(var,
                          levels = var_lvls)) |>
      dplyr::arrange(var)

    #### Add on var_info --------------------------------

    res_stats <- var_info |>
      dplyr::select(-level) |>
      dplyr::distinct() |>
      mutate(var = factor(var,
                          levels = levels(res_stats$var))) |>
      dplyr::left_join(res_stats,
                       .,
                       by = "var")

    res_stats <- res_stats |>
      dplyr::left_join(var_lbls,
                       by = "var")


    #### Arrange  --------------------------------

    res_stats <- res_stats |>
      mutate(var = factor(var, levels = vars)) |>
      dplyr::arrange(var, level)


    #### Return results --------------------------------

    res_stats <- res_stats |>
      dplyr::relocate(class,
                      var_type,
                      label,
                      .after = dplyr::everything())

    return(res_stats)

  }

}
