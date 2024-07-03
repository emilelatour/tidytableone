
# TODO
# Add a row to output for N
# incloude missing strata?
# name for missing level?

#' @title
#' Tidy table one
#'
#' @description
#' Creates a tidy data frame of the results that can go into a "Table 1" of
#' summary descriptive statistics of a study sample. Inspiration for this is owed
#' to the `tableone` package by Kazuki Yoshida.
#'
#' @param data A data frame or tibble containing the variables to be summarized.
#' @param strata Character vector of the stratifying (grouping) variable.
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
#'   \item{chisq_test}{Chi square test: p-value, with continuity correction}
#'   \item{chisq_test_no_correction}{Chi square test: p-value, without continuity correction}
#'   \item{chisq_test_simulated}{Chi square test: p-value: simulated p-value}
#'   \item{fisher_test}{Fisher's exact test: p-value}
#'   \item{fisher_test_simulated}{Fisher's exact test: simulated p-value}
#'   \item{check_categorical_test}{Is Chi square OK? Consider Fisher}
#'   \item{oneway_test_unequal_var}{Oneway anova test: p-value, equivalent to t-test when only 2 groups, unequal variances}
#'   \item{oneway_test_equal_var}{Oneway anova test: p-value, equivalent to t-test when only 2 groups, equal variances}
#'   \item{kruskal_test}{Kruskal-Wallis Rank Sum Test: p-value, equivalent to Mann-Whitney U test when only 2 groups}
#'   \item{bartlett_test}{Bartlett's test for homogeneity of variances: p-value}
#'   \item{levene_test}{Levene's test for homogeneity of variances: p-value}
#'   \item{smd}{Standarized mean difference for all pairwise comparisons}
#' }
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' tab1 <- create_tidy_table_one(data = pbc_mayo,
#'                               strata = "trt",
#'                               vars = c("time",
#'                                        "status",
#'                                        "trt",
#'                                        "age",
#'                                        "sex",
#'                                        "ascites",
#'                                        "hepato",
#'                                        "spiders",
#'                                        "edema",
#'                                        "bili",
#'                                        "chol",
#'                                        "albumin",
#'                                        "copper",
#'                                        "alk_phos",
#'                                        "ast",
#'                                        "trig",
#'                                        "platelet",
#'                                        "protime",
#'                                        "stage"))
#'
#' dplyr::glimpse(tab1)
#'
#'
#' library(ggplot2)  # diamonds data set
#'
#' tab2 <- create_tidy_table_one(data = diamonds,
#'                               strata = "cut",
#'                               vars = c("carat",
#'                                        # Don't have to include the strata variable
#'                                        # "cut",
#'                                        "color",
#'                                        "clarity",
#'                                        "depth",
#'                                        "table",
#'                                        "price"))
#'
#' dplyr::glimpse(tab2)

create_tidy_table_one <- function(data,
                                  strata = NULL,
                                  vars,
                                  na_level = "(Missing)",
                                  b_replicates = 2000, ...) {


  if (is.null(strata)) {
    # stop("Currently, the function only works when a strata is given.")
    res_stats <- create_tidy_table_one_no_strata(data = data,
                                                 strata = NULL,
                                                 vars = vars,
                                                 na_level = na_level,
                                                 b_replicates = b_replicates, ...)

    #### Return results --------------------------------

    return(res_stats)

  }

  data <- data %>%
    dplyr::mutate_if(.tbl = .,
                     .predicate = ~ ("ordered" %in% class(.)),
                     .funs = ~ factor(., ordered = FALSE))


  strata_sym <- rlang::ensym(strata)

  if (any(is.na(purrr::pluck(data, strata)))) {

    df_omit_na_strata <- data %>%
      dplyr::filter(!is.na(!! strata_sym))

    data <- data %>%
      mutate(!! strata_sym := forcats::fct_na_value_to_level(!! strata_sym,
                                                             level = na_level))
  } else {

    df_omit_na_strata <- data

  }

  if (missing(vars)) {
    vars <- names(data)
  }



  #### Get variable info --------------------------------

  var_lbls <- tibble::tibble(var = names(data)) |>
    mutate(label = purrr::map_chr(.x = data[, var],
                                  .f = ~ get_var_labels(x = .x)))

  var_info <- get_var_info(data = data,
                           .vars = vars)

  cat_vars <- var_info %>%
    dplyr::filter(var_type == "categorical") %>%
    dplyr::pull(var) %>%
    unique()

  con_vars <- var_info %>%
    dplyr::filter(var_type == "continuous") %>%
    dplyr::pull(var) %>%
    unique()

  cat_vars <- cat_vars[!cat_vars %in% strata]
  con_vars <- con_vars[!con_vars %in% strata]


  if (length(cat_vars) > 0 & length(con_vars) > 0) {

    #### Categorical stats --------------------------------

    suppressWarnings(
      cat_strata <- tibble::tibble(var = cat_vars) |>
        mutate(dat = purrr::map(.x = var,
                                .f = ~ dplyr::select(data,
                                                     !! strata_sym,
                                                     dplyr::one_of(.x))),
               res = purrr::map(.x = dat,
                                .f = ~ do_one_cat_strata(x = .x,
                                                         strata_sym = strata_sym))) |>
        dplyr::select(res) |>
        tidyr::unnest(res)
      )

    cat_overall <- cat_strata %>%
      dplyr::group_by(var, level) %>%
      mutate(n_level = sum(n_level, na.rm = TRUE),
             n_level_valid = dplyr::if_else(is.na(level), NA_integer_, n_level)) %>%
      group_by(!! strata_sym, var) %>%
      mutate(n_strata = sum(n_level, na.rm = TRUE),
             n_strata_valid = sum(n_level_valid, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(!! strata_sym := "Overall") %>%
      dplyr::distinct()

    cat_stats <- dplyr::bind_rows(cat_overall,
                                  cat_strata)

    # Calc percentage
    cat_stats <- cat_stats |>
      mutate(pct = n_level / n_strata,
             pct_valid = n_level_valid / n_strata_valid)

    #### Continuous stats --------------------------------

    con_strata <- data %>%
      dplyr::select(!! strata_sym,
                    dplyr::one_of(con_vars)) %>%
      tidyr::pivot_longer(data = .,
                          cols = - !! strata_sym,
                          names_to = "var",
                          values_to = "value") %>%
      group_by(!! strata_sym, var) %>%
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
                ad_test = calc_ad_test(var = value),
                .groups = "drop") %>%
      ungroup() %>%
      mutate(!! strata_sym := as.character(!! strata_sym))

    con_overall <- data %>%
      dplyr::select(!! strata_sym, dplyr::one_of(con_vars)) %>%
      tidyr::pivot_longer(data = .,
                          cols = - !! strata_sym,
                          names_to = "var",
                          values_to = "value") %>%
      group_by(var) %>%
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
                ad_test = calc_ad_test(var = value),
                .groups = "drop") %>%
      ungroup() %>%
      mutate(!! strata_sym := "Overall") %>%
      dplyr::select(!! strata_sym, dplyr::everything())


    con_stats <- dplyr::bind_rows(con_overall,
                                  con_strata)


    #### Calc SMD --------------------------------

    smd_res <- get_smd(data = df_omit_na_strata,
                       strata = strata,
                       vars = vars)

    # 10: In StdDiff(variable = var, group = strataVar) :
    # Variable has only NA's in at least one stratum. na.rm turned off.

    #### Hypothesis tests --------------------------------

    if(!is.null(strata)) {

      htest_res <- dplyr::bind_rows(
        calc_cat_htest(data = df_omit_na_strata,
                       strata = strata,
                       vars = cat_vars,
                       b_replicates = b_replicates),

        calc_con_htest(data = df_omit_na_strata,
                       strata = strata,
                       vars = con_vars)
      )

    }


    #### Combine results --------------------------------

    res_stats <- dplyr::bind_rows(con_stats,
                                  cat_stats) %>%
      dplyr::left_join(.,
                       htest_res,
                       by = "var") %>%
      dplyr::rename("strata" = !! strata_sym) %>%
      dplyr::left_join(.,
                       smd_res,
                       by = "var")

    #### Arrange results --------------------------------

    var_lvls <- unique(var_info$var)

    level_lvls <- var_info %>%
      dplyr::transmute(var_level = glue::glue("{var}_{level}")) %>%
      dplyr::pull()

    if (is.factor(purrr::pluck(data, strata))) {
      strata_lvls <- c("Overall", levels(purrr::pluck(data, strata)))
    } else {
      strata_lvls <- c("Overall", unique(purrr::pluck(data, strata)))
    }

    res_stats <- res_stats %>%
      mutate(var = factor(var,
                          levels = var_lvls),
             strata = factor(strata,
                             levels = strata_lvls),
             var_level = glue::glue("{var}_{level}"),
             var_level = factor(var_level,
                                levels = level_lvls)) %>%
      dplyr::arrange(var, strata, var_level) %>%
      mutate(level = factor(level)) %>%
      dplyr::select(-var_level)


    #### Add on var_info --------------------------------

    res_stats <- var_info %>%
      dplyr::select(-level) %>%
      dplyr::distinct() %>%
      mutate(var = factor(var,
                          levels = levels(res_stats$var))) %>%
      dplyr::left_join(res_stats,
                       .,
                       by = "var")

    res_stats <- res_stats |>
      dplyr::left_join(var_lbls,
                       by = "var")


    #### Arrange  --------------------------------

    res_stats <- res_stats |>
      mutate(var = factor(var, levels = vars)) |>
      dplyr::arrange(var, strata, level)


    #### Return results --------------------------------

    return(res_stats)

  } else if (length(cat_vars) > 0) {

    #### Categorical stats --------------------------------

    suppressWarnings(
      cat_strata <- tibble::tibble(var = cat_vars) |>
        mutate(dat = purrr::map(.x = var,
                                .f = ~ dplyr::select(data,
                                                     !! strata_sym,
                                                     dplyr::one_of(.x))),
               res = purrr::map(.x = dat,
                                .f = ~ do_one_cat_strata(x = .x,
                                                         strata_sym = strata_sym))) |>
        dplyr::select(res) |>
        tidyr::unnest(res)
    )

    cat_overall <- cat_strata %>%
      dplyr::group_by(var, level) %>%
      mutate(n_level = sum(n_level, na.rm = TRUE),
             n_level_valid = dplyr::if_else(is.na(level), NA_integer_, n_level)) %>%
      group_by(!! strata_sym, var) %>%
      mutate(n_strata = sum(n_level, na.rm = TRUE),
             n_strata_valid = sum(n_level_valid, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(!! strata_sym := "Overall") %>%
      dplyr::distinct()

    cat_stats <- dplyr::bind_rows(cat_overall,
                                  cat_strata)

    # Calc percentage
    cat_stats <- cat_stats |>
      mutate(pct = n_level / n_strata,
             pct_valid = n_level_valid / n_strata_valid)


    #### Calc SMD --------------------------------

    smd_res <- get_smd(data = df_omit_na_strata,
                       strata = strata,
                       vars = vars)

    # 10: In StdDiff(variable = var, group = strataVar) :
    # Variable has only NA's in at least one stratum. na.rm turned off.

    #### Hypothesis tests --------------------------------

    if(!is.null(strata)) {

      htest_res <- dplyr::bind_rows(
        calc_cat_htest(data = df_omit_na_strata,
                       strata = strata,
                       vars = cat_vars)
      )

    }


    #### Combine results --------------------------------

    res_stats <- cat_stats %>%
      dplyr::left_join(.,
                       htest_res,
                       by = "var") %>%
      dplyr::rename("strata" = !! strata_sym) %>%
      dplyr::left_join(.,
                       smd_res,
                       by = "var")

    #### Arrange results --------------------------------

    var_lvls <- unique(var_info$var)

    level_lvls <- var_info %>%
      dplyr::transmute(var_level = glue::glue("{var}_{level}")) %>%
      dplyr::pull()

    if (is.factor(purrr::pluck(data, strata))) {
      strata_lvls <- c("Overall", levels(purrr::pluck(data, strata)))
    } else {
      strata_lvls <- c("Overall", unique(purrr::pluck(data, strata)))
    }

    res_stats <- res_stats %>%
      mutate(var = factor(var,
                          levels = var_lvls),
             strata = factor(strata,
                             levels = strata_lvls),
             var_level = glue::glue("{var}_{level}"),
             var_level = factor(var_level,
                                levels = level_lvls)) %>%
      dplyr::arrange(var, strata, var_level) %>%
      mutate(level = factor(level)) %>%
      dplyr::select(-var_level)


    #### Add on var_info --------------------------------

    res_stats <- var_info %>%
      dplyr::select(-level) %>%
      dplyr::distinct() %>%
      mutate(var = factor(var,
                          levels = levels(res_stats$var))) %>%
      dplyr::left_join(res_stats,
                       .,
                       by = "var")

    res_stats <- res_stats |>
      dplyr::left_join(var_lbls,
                       by = "var")


    #### Arrange  --------------------------------

    res_stats <- res_stats |>
      mutate(var = factor(var, levels = vars)) |>
      dplyr::arrange(var, strata, level)


    #### Return results --------------------------------

    return(res_stats)

  } else if (length(con_vars) > 0) {

    #### Continuous stats --------------------------------

    con_strata <- data %>%
      dplyr::select(!! strata_sym,
                    dplyr::one_of(con_vars)) %>%
      tidyr::pivot_longer(data = .,
                          cols = - !! strata_sym,
                          names_to = "var",
                          values_to = "value") %>%
      group_by(!! strata_sym, var) %>%
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
                ad_test = calc_ad_test(var = value),
                .groups = "drop") %>%
      ungroup() %>%
      mutate(!! strata_sym := as.character(!! strata_sym))

    con_overall <- data %>%
      dplyr::select(!! strata_sym, dplyr::one_of(con_vars)) %>%
      tidyr::pivot_longer(data = .,
                          cols = - !! strata_sym,
                          names_to = "var",
                          values_to = "value") %>%
      group_by(var) %>%
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
                ad_test = calc_ad_test(var = value),
                .groups = "drop") %>%
      ungroup() %>%
      mutate(!! strata_sym := "Overall") %>%
      dplyr::select(!! strata_sym, dplyr::everything())


    con_stats <- dplyr::bind_rows(con_overall,
                                  con_strata)


    #### Calc SMD --------------------------------

    smd_res <- get_smd(data = df_omit_na_strata,
                       strata = strata,
                       vars = vars)

    # 10: In StdDiff(variable = var, group = strataVar) :
    # Variable has only NA's in at least one stratum. na.rm turned off.

    #### Hypothesis tests --------------------------------

    if (!is.null(strata)) {

      htest_res <- dplyr::bind_rows(
        calc_con_htest(data = df_omit_na_strata,
                       strata = strata,
                       vars = con_vars)
      )

    }


    #### Combine results --------------------------------

    res_stats <- con_stats %>%
      dplyr::left_join(.,
                       htest_res,
                       by = "var") %>%
      dplyr::rename("strata" = !! strata_sym) %>%
      dplyr::left_join(.,
                       smd_res,
                       by = "var")

    #### Arrange results --------------------------------

    var_lvls <- unique(var_info$var)

    level_lvls <- var_info %>%
      dplyr::transmute(var_level = glue::glue("{var}_{level}")) %>%
      dplyr::pull()

    if (is.factor(purrr::pluck(data, strata))) {
      strata_lvls <- c("Overall", levels(purrr::pluck(data, strata)))
    } else {
      strata_lvls <- c("Overall", unique(purrr::pluck(data, strata)))
    }

    res_stats <- res_stats %>%
      mutate(var = factor(var,
                          levels = var_lvls),
             level = NA_character_,
             strata = factor(strata,
                             levels = strata_lvls),
             var_level = glue::glue("{var}_{level}"),
             var_level = factor(var_level,
                                levels = level_lvls)) %>%
      dplyr::arrange(var, strata, var_level) %>%
      mutate(level = factor(level)) %>%
      dplyr::select(-var_level,
                    -level)


    #### Add on var_info --------------------------------

    res_stats <- var_info %>%
      dplyr::select(-level) %>%
      dplyr::distinct() %>%
      mutate(var = factor(var,
                          levels = levels(res_stats$var))) %>%
      dplyr::left_join(res_stats,
                       .,
                       by = "var")

    res_stats <- res_stats |>
      dplyr::left_join(var_lbls,
                       by = "var")


    #### Arrange  --------------------------------

    if (any(res_stats$var_type == "continuous") & any(res_stats$var_type == "categorical")) {

      res_stats <- res_stats |>
        mutate(var = factor(var,
                            levels = vars)) |>
        dplyr::arrange(var, strata, level)

    } else if (any(res_stats$var_type == "continuous")) {

      res_stats <- res_stats |>
        mutate(var = factor(var,
                            levels = vars)) |>
        dplyr::arrange(var, strata)


    } else if (any(res_stats$var_type == "categorical")) {

      res_stats <- res_stats |>
        mutate(var = factor(var,
                            levels = vars)) |>
        dplyr::arrange(var, strata, level)


    }


    #### Return results --------------------------------

    return(res_stats)

  }




}



#### Helper functions --------------------------------

## get_smd ----------------
# a wrapper around tablone package funcions

get_smd <- function(data,
                    strata = NULL,
                    vars) {

  if (is.null(strata)) {

    tibble::tibble(var = vars,
                   smm = NA_real_)

  } else {

    tableone::CreateTableOne(vars = vars,
                             strata = strata,
                             data = data) %>%
      tableone::ExtractSmd(.) %>%
      tibble::as_tibble(.,
                        rownames = "var") %>%
      dplyr::select(1:2) %>%
      dplyr::rename(smd = 2)

  }

}

## shapiro test ----------------

calc_shapiro_test <- function(var) {
  tryCatch(shapiro.test(var)[["p.value"]],
           error = function(err) NA)
}

## Kolmogorov-Smirnov Tests ----------------

calc_ks_test <- function(var) {
  tryCatch(ks.test(unique(var), "pnorm")[["p.value"]],
           error = function(err) NA)
}

## anderson-darling test ----------------

# The K-S test is not suitable when estimating the parameters from the data. You
# can use the following code, which relies on the Anderson-Darling test for
# normality, and does not require you to supply the mean and the stddev. This
# test is stronger in accuracy than the Lilliefors test.

calc_ad_test <- function(var) {
  tryCatch(nortest::ad.test(var)[["p.value"]],
           error = function(err) NA)
}


## oneway test (unequal variances) ----------------

calc_oneway_test_unequal <- function(data, form, var.equal = FALSE) {

  tryCatch(oneway.test(formula = as.formula(form),
                       data = data,
                       var.equal = var.equal) %>%
             purrr::pluck(., "p.value"),
           error = function(err) NA)



}


## oneway test (equal variances) ----------------

calc_oneway_test_equal <- function(data, form, var.equal = TRUE) {

  tryCatch(oneway.test(formula = as.formula(form),
                       data = data,
                       var.equal = var.equal) %>%
             purrr::pluck(., "p.value"),
           error = function(err) NA)



}


## kruskal test ----------------

calc_kruskal_test <- function(data, form) {

  tryCatch(kruskal.test(formula = as.formula(form), data = data) %>%
             purrr::pluck(., "p.value"),
           error = function(err) NA)

}


## bartlett test ----------------

calc_bartlett_test <- function(data, form) {

  tryCatch(bartlett.test(formula = as.formula(form), data = data) %>%
             purrr::pluck(., "p.value"),
           error = function(err) NA)

}

## levene test ----------------

calc_levene_test <- function(data, form) {

  tryCatch(car::leveneTest(as.formula(form), data = data) %>%
             purrr::pluck(., "Pr(>F)", 1),
           error = function(err) NA)

}


## cat_check ----------------

cat_check <- function(tab) {

  dplyr::if_else(testit::has_warning(chisq.test(tab)),
                 "warning",
                 "ok")
}


## calc_fisher_test ----------------

calc_fisher_test <- function(tab,
                             simulate.p.value = FALSE,
                             B = 2000) {
  tryCatch(fisher.test(tab,
                       alternative = "two.sided",
                       conf.int = FALSE,
                       simulate.p.value = simulate.p.value,
                       B = B) %>%
             purrr::pluck(., "p.value"),
           error = function(err) NA)
}


## calc_fisher_test, simulate p-value ----------------

calc_fisher_test_sim_p <- function(tab,
                                   simulate.p.value = TRUE,
                                   B = 2000) {
  tryCatch(fisher.test(tab,
                       alternative = "two.sided",
                       conf.int = FALSE,
                       simulate.p.value = simulate.p.value,
                       B = B) %>%
             purrr::pluck(., "p.value"),
           error = function(err) NA)
}


## calc_chisq_test ----------------

calc_chisq_test <- function(tab,
                            correct = TRUE,
                            simulate.p.value = FALSE,
                            B = 2000) {

  tryCatch(chisq.test(tab,
                      correct = correct,
                      simulate.p.value = simulate.p.value,
                      B = B)  %>%
             purrr::pluck(., "p.value"),
           error = function(err) NA)

}

## calc_chisq_test (no correction) ----------------

calc_chisq_test_no_correct <- function(tab,
                                       correct = FALSE,
                                       simulate.p.value = FALSE,
                                       B = 2000) {

  tryCatch(chisq.test(tab,
                      correct = correct,
                      simulate.p.value = simulate.p.value,
                      B = B)  %>%
             purrr::pluck(., "p.value"),
           error = function(err) NA)

}


## calc_chisq_test (simluated p) ----------------

calc_chisq_test_sim_p <- function(tab,
                                  correct = TRUE,
                                  simulate.p.value = TRUE,
                                  B = 2000) {

  tryCatch(chisq.test(tab,
                      correct = correct,
                      simulate.p.value = simulate.p.value,
                      B = B)  %>%
             purrr::pluck(., "p.value"),
           error = function(err) NA)

}


## calc_cat_htest ----------------

calc_cat_htest <- function(data, strata, vars, b_replicates) {

  tibble::tibble(strata = strata,
                 var = vars) %>%
    mutate(x = purrr::map(.x = strata,
                          .f = ~ purrr::pluck(data, .x)),
           y = purrr::map(.x = var,
                          .f = ~ purrr::pluck(data, .x))) %>%
    dplyr::filter(strata != var) %>%
    mutate(tab = purrr::map2(.x = x,
                             .y = y,
                             .f = ~ table(.x, .y)),
           chisq_test = purrr::map_dbl(.x = tab,
                                       .f = ~ calc_chisq_test(.x)),
           chisq_test_no_correction = purrr::map_dbl(.x = tab,
                                                     .f = ~ calc_chisq_test_no_correct(.x)),
           chisq_test_simulated = purrr::map_dbl(.x = tab,
                                                 .f = ~ calc_chisq_test_sim_p(.x,
                                                                              B = b_replicates)),

           fisher_test = purrr::map_dbl(.x = tab,
                                        .f = ~ calc_fisher_test(.x)),
           fisher_test_simulated = purrr::map_dbl(.x = tab,
                                                  .f = ~ calc_fisher_test_sim_p(.x,
                                                                                B = b_replicates)),
           check_categorical_test = purrr::map_chr(.x = tab,
                                                   .f = ~ cat_check(.x))) %>%
    dplyr::select(var,
                  chisq_test,
                  chisq_test_no_correction,
                  chisq_test_simulated,
                  fisher_test,
                  fisher_test_simulated,
                  check_categorical_test)
}


## calc_con_htest ----------------

calc_con_htest <- function(data, strata, vars) {

  tibble::tibble(strata = strata,
                 var = vars) %>%
    mutate(form = glue::glue("{var} ~ {strata}")) %>%
    mutate(oneway_test_unequal_var =
             purrr::map_dbl(.x = form,
                            .f = ~ calc_oneway_test_unequal(data = data,
                                                            form = .x)),
           oneway_test_equal_var =
             purrr::map_dbl(.x = form,
                            .f = ~ calc_oneway_test_equal(data = data,
                                                          form = .x)),
           kruskal_test =
             purrr::map_dbl(.x = form,
                            .f = ~ calc_kruskal_test(data = data,
                                                     form = .x)),
           bartlett_test =
             purrr::map_dbl(.x = form,
                            .f = ~ calc_bartlett_test(data = data,
                                                      form = .x)),
           levene_test =
             purrr::map_dbl(.x = form,
                            .f = ~ calc_levene_test(data = data,
                                                    form = .x))) %>%
    dplyr::select(var,
                  oneway_test_unequal_var,
                  oneway_test_equal_var,
                  kruskal_test,
                  bartlett_test,
                  levene_test)
}


## custom_min custom_max----------------

custom_min <- function(x, na.rm = TRUE) {

  if (is.integer(x)) {

    dflt_miss <- NA_integer_

  } else if (is.numeric(x)) {

    dflt_miss <- NA_real_

  } else {

    dflt_miss <- Inf

  }

  if (all(is.na(x))) {
    dflt_miss
  } else {
    min(x, na.rm = na.rm)
  }
}

custom_max <- function(x, na.rm = TRUE) {

  if (is.integer(x)) {

    dflt_miss <- NA_integer_

  } else if (is.numeric(x)) {

    dflt_miss <- NA_real_

  } else {

    dflt_miss <- Inf

  }

  if (all(is.na(x))) {
    dflt_miss
  } else {
    max(x, na.rm = na.rm)
  }
}


get_var_labels <- function(x) {

  x_lbl <- attr(x,
                which = "label",
                exact = TRUE)

  if (is.null(x_lbl)) {
    NA_character_
  } else {
    x_lbl
  }

}

do_one_cat_strata <- function(x, strata_sym) {
  x |>
    tidyr::pivot_longer(cols = - !! strata_sym,
                        names_to = "var",
                        values_to = "level") |>
    dplyr::count(!! strata_sym, var, level,
                 name = "n_level",
                 .drop = FALSE)  |>
    group_by(var) |>
    tidyr::complete(!! strata_sym, level,
                    fill = list(n_level = 0)) |>
    group_by(!! strata_sym, var) |>
    mutate(n_strata = sum(n_level, na.rm = TRUE),
           n_level_valid = dplyr::if_else(is.na(level), NA_integer_, n_level),
           n_strata_valid = sum(n_level_valid, na.rm = TRUE)) |>
    ungroup() |>
    dplyr::select(!! strata_sym, dplyr::everything()) |>
    mutate(!! strata_sym := as.character(!! strata_sym))
}
