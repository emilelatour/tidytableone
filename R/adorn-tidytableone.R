

#' @title Format tidytableone for display and reporting
#'
#' @description
#' Take the raw summary statistics from `create_tidy_table_one` and format them
#' for inclusion in a presentable table.
#'
#' @param tidy_t1 Results in a tibble from `create_tidy_table_one`
#' @param default_continuous A glue statement that provides the formatting for
#'   continuous variables, Default is "{mean} ({sd})"
#' @param default_categorical A glue statement that provides the formatting for
#'   categorical variables, Default is "{n} ({p})"
#' @param fmt_vars A list of variable names and glue statements to override the
#'   defaults for specific variables.
#' @param con_accuracy A number to round to for continuous variables. Use (e.g.)
#'   0.01 to show 2 decimal places of precision. Default is 0.1.
#' @param cat_accuracy A number to round to for categorical variables. Use
#'   (e.g.) 0.01 to show 2 decimal places of precision. Default is 0.1.
#' @param p_accuracy  A number to round to for p-values and smd (when shown).
#'   Use (e.g.) 0.01 to show 2 decimal places of precision. Default is 0.001.
#' @param prefix Additional text to display before the number. The suffix is
#'   applied to absolute value before style_positive and style_negative are
#'   processed so that prefix = "$" will yield (e.g.) ⁠-$1⁠ and ⁠($1)⁠.
#' @param suffix Additional text to display after the number.
#' @param big_mark Character used between every 3 digits to separate thousands.
#' @param decimal_mark The character to be used to indicate the numeric decimal point.
#' @param style_positive A string that determines the style of positive numbers.
#'   See `?scales:number` for more information.
#' @param style_negative  A string that determines the style of negative
#'   numbers. See `?scales:number` for more information.
#' @param scale_cut Named numeric vector that allows you to rescale large (or
#'   small) numbers and add a prefix. See `?scales:number` for more information.
#' @param con_trim Logical, if FALSE, values are right-justified to a common width (see `base::format()`).
#' @param cat_trim Logical, if FALSE, values are right-justified to a common width (see `base::format()`).
#' @param show_pct Logical, if FALSE, "%" is omitted from percentages.
#' @param exact String vector of variable names to use exact tests for p-values.
#' @param nonnormal String vector of variable names to use non-parametric tests for p-values.
#' @param equal_variance String vector of variable names to assume equal variance.
#' @param no_cont_correction String vector of variable names to assume continuity correction.
#' @param monte_carlo_p String vector of variable names to simulate p-values.
#' @param show_test Logical, if FALSE, the names of the test are omitted from the table.
#' @param show_smd Logical, if FALSE, Standardized Mean Differences (SMD) are not inlcuded
#' @param ... Additional arguments. Not used.
#'
#' @importFrom dplyr across
#' @importFrom dplyr add_row
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr distinct
#' @importFrom dplyr everything
#' @importFrom dplyr filter
#' @importFrom dplyr if_else
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr rows_update
#' @importFrom dplyr rowwise
#' @importFrom dplyr select
#' @importFrom glue glue
#' @importFrom purrr map_chr
#' @importFrom purrr map_df
#' @importFrom scales number
#' @importFrom scales pvalue
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr separate_longer_delim
#'
#' @return A tibble
#'
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' dplyr::glimpse(pbc_mayo)
#'
#' tab1 <- create_tidy_table_one(data = pbc_mayo,
#'                               strata = "trt",
#'                               vars = c("time",
#'                                        "status",
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
#' # Check to see if assumptions may be violated
#' check_tests <- calc_table_one_tests(tab1)
#'
#'
#' dplyr::distinct(tab1, var)
#'
#'
#' adorn_tidytableone(tidy_t1 = tab1)
#'
#' adorn_tidytableone(tidy_t1 = tab1,
#'                    show_test = TRUE)
#'
#'
#' adorn_tidytableone(tidy_t1 = tab1,
#'                    show_test = TRUE,
#'                    exact = "status",
#'                    nonnormal = check_tests$non_normal_shapiro)


adorn_tidytableone <- function(tidy_t1,
                               default_continuous = "{mean} ({sd})",
                               default_categorical = "{n} ({p})",
                               fmt_vars = NULL,
                               con_accuracy = 0.1,
                               cat_accuracy = 0.1,
                               p_accuracy = 0.001,
                               prefix = "",
                               suffix = "",
                               big_mark = "",
                               decimal_mark = ".",
                               style_positive = c("none", "plus"),
                               style_negative = c("hyphen", "minus", "parens"),
                               scale_cut = NULL,
                               con_trim = TRUE,
                               cat_trim = FALSE,
                               show_pct = TRUE,
                               exact = NULL,
                               nonnormal = NULL,
                               equal_variance = NULL,
                               no_cont_correction = NULL,
                               monte_carlo_p = NULL,
                               show_test = FALSE,
                               show_smd = FALSE, ...) {

  # Silence no visible binding for global variable
  p_value <- test <- smd <- NULL

  #### Get the stats --------------------------------

  tab_stats <- make_t1_pretty(t1 = tidy_t1,
                              default_continuous = default_continuous,
                              default_categorical = default_categorical,
                              fmt_vars = fmt_vars,
                              con_accuracy = con_accuracy,
                              cat_accuracy = cat_accuracy,
                              prefix = prefix,
                              suffix = suffix,
                              big_mark = big_mark,
                              decimal_mark = decimal_mark,
                              style_positive = style_positive[[1]],
                              style_negative = style_negative[[1]],
                              scale_cut = scale_cut,
                              con_trim = con_trim,
                              cat_trim = cat_trim,
                              show_pct = show_pct)

  #### Get the p-values --------------------------------

  if (any(tidy_t1$var_type == "continuous") & any(tidy_t1$var_type == "categorical")) {

    tab_pvals <- tidy_t1 |>
      dplyr::distinct(var,
                      var_type,
                      chisq_test,
                      chisq_test_no_correction,
                      chisq_test_simulated,
                      fisher_test,
                      fisher_test_simulated,
                      oneway_test_unequal_var,
                      oneway_test_equal_var,
                      kruskal_test) |>
      mutate(p_value = dplyr::case_when(
        var_type == "continuous" & var %in% nonnormal ~ kruskal_test,
        var_type == "continuous" & var %in% equal_variance ~ oneway_test_equal_var,
        var_type == "continuous"  ~ oneway_test_unequal_var,
        var_type == "categorical" & var %in% exact & var %in% monte_carlo_p ~ fisher_test_simulated,
        var_type == "categorical" & var %in% exact ~ fisher_test,
        var_type == "categorical" & var %in% no_cont_correction ~ chisq_test_no_correction,
        var_type == "categorical" & var %in% monte_carlo_p ~ chisq_test_simulated,
        var_type == "categorical" ~ chisq_test,
        TRUE ~ NA_real_),
        test = dplyr::case_when(
          var_type == "continuous" & var %in% nonnormal ~ "Kruskal-Wallis Rank Sum Test",
          var_type == "continuous" & var %in% equal_variance ~ "Oneway test, equal variance",
          var_type == "continuous"  ~ "Oneway test, unequal variance",
          var_type == "categorical" & var %in% exact & var %in% monte_carlo_p ~ "Fisher's Exact Test, simuation",
          var_type == "categorical" & var %in% exact ~ "Fisher's Exact Test",
          var_type == "categorical" & var %in% no_cont_correction ~ "Chi-squared Test, no continuity correction",
          var_type == "categorical" & var %in% monte_carlo_p ~ "Chi-squared Test, simulation",
          var_type == "categorical" ~ "Chi-squared Test, with continuty correction",
          TRUE ~ NA_character_),
        p_value = scales::pvalue(p_value,
                                 accuracy = p_accuracy,
                                 decimal.mark = ".",
                                 prefix = NULL,
                                 add_p = FALSE)) |>
      dplyr::select(var,
                    p_value,
                    test)

  } else if (any(tidy_t1$var_type == "continuous")) {

    tab_pvals <- tidy_t1 |>
      dplyr::distinct(var,
                      var_type,
                      oneway_test_unequal_var,
                      oneway_test_equal_var,
                      kruskal_test) |>
      mutate(p_value = dplyr::case_when(
        var_type == "continuous" & var %in% nonnormal ~ kruskal_test,
        var_type == "continuous" & var %in% equal_variance ~ oneway_test_equal_var,
        var_type == "continuous"  ~ oneway_test_unequal_var,
        TRUE ~ NA_real_),
        test = dplyr::case_when(
          var_type == "continuous" & var %in% nonnormal ~ "Kruskal-Wallis Rank Sum Test",
          var_type == "continuous" & var %in% equal_variance ~ "Oneway test, equal variance",
          var_type == "continuous"  ~ "Oneway test, unequal variance",
          TRUE ~ NA_character_),
        p_value = scales::pvalue(p_value,
                                 accuracy = p_accuracy,
                                 decimal.mark = ".",
                                 prefix = NULL,
                                 add_p = FALSE)) |>
      dplyr::select(var,
                    p_value,
                    test)

  } else if (any(tidy_t1$var_type == "categorical")) {

    tab_pvals <- tidy_t1 |>
      dplyr::distinct(var,
                      var_type,
                      chisq_test,
                      chisq_test_no_correction,
                      chisq_test_simulated,
                      fisher_test,
                      fisher_test_simulated) |>
      mutate(p_value = dplyr::case_when(
        var_type == "categorical" & var %in% exact & var %in% monte_carlo_p ~ fisher_test_simulated,
        var_type == "categorical" & var %in% exact ~ fisher_test,
        var_type == "categorical" & var %in% no_cont_correction ~ chisq_test_no_correction,
        var_type == "categorical" & var %in% monte_carlo_p ~ chisq_test_simulated,
        var_type == "categorical" ~ chisq_test,
        TRUE ~ NA_real_),
        test = dplyr::case_when(
          var_type == "categorical" & var %in% exact & var %in% monte_carlo_p ~ "Fisher's Exact Test, simuation",
          var_type == "categorical" & var %in% exact ~ "Fisher's Exact Test",
          var_type == "categorical" & var %in% no_cont_correction ~ "Chi-squared Test, no continuity correction",
          var_type == "categorical" & var %in% monte_carlo_p ~ "Chi-squared Test, simulation",
          var_type == "categorical" ~ "Chi-squared Test, with continuty correction",
          TRUE ~ NA_character_),
        p_value = scales::pvalue(p_value,
                                 accuracy = p_accuracy,
                                 decimal.mark = ".",
                                 prefix = NULL,
                                 add_p = FALSE)) |>
      dplyr::select(var,
                    p_value,
                    test)
  }

  #### Get the SMD --------------------------------

  tab_smd <- tidy_t1 |>
    dplyr::distinct(var, smd) |>
    mutate(smd = scales::number(smd,
                                accuracy = p_accuracy,
                                decimal.mark = "."))



  #### Make the table --------------------------------

  tab_vars <- dplyr::distinct(tidy_t1, var) |>
    dplyr::pull() |>
    as.character()


  adorned_tidy_t1 <- purrr::map_df(.x = tab_vars,
                                   .f = ~ build_tab1(tab_var = .x,
                                                     tab_pvals = tab_pvals,
                                                     tab_stats = tab_stats,
                                                     show_test = show_test,
                                                     show_smd = show_smd,
                                                     tab_smd = tab_smd)) |>
    dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
                                .fns = ~ dplyr::if_else(is.na(.), "", .)))


  #### Return table --------------------------------

  return(adorned_tidy_t1)





}


#### Build tab1 --------------------------------

build_tab1 <- function(tab_var,
                       tab_pvals,
                       tab_stats,
                       show_test = FALSE,
                       show_smd = FALSE,
                       tab_smd) {

  # Silence no visible binding for global variable
  p_value <- test <- smd <- NULL

  p_i <- tab_pvals |>
    dplyr::filter(var == tab_var) |>
    dplyr::pull(p_value)

  s_i <- tab_stats |>
    dplyr::filter(var == tab_var) |>
    dplyr::select(-var,
                  -var_type) |>
    mutate(var = NA_character_,
           p_value = NA_character_)

  t_i <- tab_pvals |>
    dplyr::filter(var == tab_var) |>
    dplyr::pull(test)

  smd_i <- tab_smd |>
    dplyr::filter(var == tab_var) |>
    dplyr::pull(smd)

  res <- tibble::tibble(var = tab_var,
                        p_value = p_i,
                        test = t_i,
                        smd = smd_i) |>
    dplyr::bind_rows(s_i) |>
    dplyr::select(var,
                  dplyr::everything(),
                  -p_value,
                  -test,
                  -smd,
                  p_value,
                  test,
                  smd) |>
    dplyr::add_row()


  if (!show_test) {

    res <- res |>
      dplyr::select(-test)

  }

  if (!show_smd) {

    res <- res |>
      dplyr::select(-smd)

  }


  # if (show_test) {
  #
  #   t_i <- tab_pvals |>
  #     dplyr::filter(var == tab_var) |>
  #     dplyr::pull(test)
  #
  #   res <- tibble(var = tab_var,
  #                 p_value = p_i,
  #                 test = t_i)
  #
  #   res <- res |>
  #     dplyr::bind_rows(s_i) |>
  #     dplyr::select(var,
  #                   dplyr::everything(),
  #                   -p_value,
  #                   -test,
  #                   p_value,
  #                   test) |>
  #     dplyr::add_row()
  #
  #
  # } else {
  #   res <- tibble(var = tab_var,
  #                 p_value = p_i)
  #
  #
  #   res <- res |>
  #     dplyr::bind_rows(s_i) |>
  #     dplyr::select(var,
  #                   dplyr::everything(),
  #                   -p_value,
  #                   p_value) |>
  #     dplyr::add_row()
  #
  # }

  return(res)

}


#### Make t1 pretty --------------------------------

make_t1_pretty <- function(t1,
                           default_continuous = "{mean} ({sd})",
                           default_categorical = "{n} ({p})",
                           fmt_vars = NULL,
                           con_accuracy = 0.1,
                           cat_accuracy = 0.1,
                           prefix = "",
                           suffix = "",
                           big_mark = "",
                           decimal_mark = ".",
                           style_positive = "none",
                           style_negative = "hyphen",
                           scale_cut = NULL,
                           con_trim = TRUE,
                           cat_trim = FALSE,
                           show_pct = TRUE, ...) {

  # Silence no visible binding for global variable
  glue_formula <- pct <- cv <- strata <- glue_formula2 <- NULL

  # Percentage suffix
  if (show_pct) {
    pct_suffix = "%"
  } else {
    pct_suffix = ""
  }


  #### Glue formulae --------------------------------

  if (is.null(fmt_vars)) {

    formula_for_table <- t1 |>
      dplyr::distinct(var,
                      var_type) |>
      mutate(glue_formula = dplyr::case_when(
        var_type == "continuous" ~ default_continuous,
        var_type == "categorical" ~ default_categorical,
        .default = NA_character_))

  } else {

    override_formulae <- tibble::tibble(var = names(fmt_vars),
                                        glue_formula = purrr::map_chr(.x = var,
                                                                      .f = ~ fmt_vars[[.x]]))
    formula_for_table <- t1 |>
      dplyr::distinct(var,
                      var_type) |>
      mutate(glue_formula = dplyr::case_when(
        var_type == "continuous" ~ default_continuous,
        var_type == "categorical" ~ default_categorical,
        .default = NA_character_)) |>
      dplyr::rows_update(override_formulae,
                         by = "var")

  }

  ## Fix formula ----------------


  formula_for_table <- formula_for_table |>
    mutate(glue_formula = stringr::str_replace_all(string = glue_formula,
                                                   pattern = "median|Median|med|med",
                                                   replacement = "p50")) |>
    mutate(glue_formula = stringr::str_replace_all(string = glue_formula,
                                                   pattern = "min|Min|minimum|Minimum",
                                                   replacement = "p0")) |>
    mutate(glue_formula = stringr::str_replace_all(string = glue_formula,
                                                   pattern = "max|Max|maximum|Maximum",
                                                   replacement = "p100")) |>
    mutate(glue_formula = stringr::str_replace_all(string = glue_formula,
                                                   pattern = "max|Max|maximum|Maximum",
                                                   replacement = "p100")) |>
    mutate(glue_formula = stringr::str_replace_all(string = glue_formula,
                                                   pattern = "\\{p\\}",
                                                   replacement = "{pct}")) |>
    mutate(glue_formula = stringr::str_replace_all(string = glue_formula,
                                                   pattern = "\\{n\\}",
                                                   replacement = "{n_level}")) |>
    mutate(glue_formula = stringr::str_replace_all(string = glue_formula,
                                                   pattern = "\\{N\\}",
                                                   replacement = "{n_strata}"))



  #### Make the pretty t1 --------------------------------


  if (any(t1$var_type == "categorical")) {

    t1 <- t1 |>
      # dplyr::filter(var == "status") |>
      mutate(pct = n_level / n_strata) |>
      # Format counts for strata
      mutate(dplyr::across(.cols = c(n_level,
                                     n_strata),
                           .fns = ~ scales::number(x = .,
                                                   accuracy = 1.0,
                                                   scale = 1,
                                                   prefix = "",
                                                   suffix = "",
                                                   big.mark = "",
                                                   decimal.mark = ".",
                                                   style_positive = "none",
                                                   style_negative = "hyphen",
                                                   scale_cut = NULL,
                                                   trim = FALSE))) |>
      # Format categorical Percentages
      mutate(pct = scales::percent(x = pct,
                                   accuracy = cat_accuracy,
                                   scale = 100,
                                   prefix = prefix,
                                   suffix = pct_suffix,
                                   big.mark = big_mark,
                                   decimal.mark = decimal_mark,
                                   style_positive = style_positive,
                                   style_negative = style_negative,
                                   scale_cut = scale_cut,
                                   trim = cat_trim))
  }

  if (any(t1$var_type == "continuous")) {

    # Format continuous stats: mean, sd, median, etc.
    t1 <- t1 |>
      mutate(dplyr::across(.cols = c(mean:cv),
                           .fns = ~ scales::number(x = .,
                                                   accuracy = con_accuracy,
                                                   scale = 1,
                                                   prefix = prefix,
                                                   suffix = suffix,
                                                   big.mark = big_mark,
                                                   decimal.mark = decimal_mark,
                                                   style_positive = style_positive,
                                                   style_negative = style_negative,
                                                   scale_cut = scale_cut,
                                                   trim = con_trim)))

  }


  if (!any(names(t1) == "level")) {

    t1 <- t1 |>
      mutate(level = "")
  }



  t1 |>
    dplyr::left_join(formula_for_table,
                     by = c("var",
                            "var_type")) |>
    dplyr::rowwise() |>
    mutate(glue_formula2 = glue::glue(glue_formula)) |>
    dplyr::select(strata,
                  var,
                  level,
                  var_type,
                  glue_formula,
                  glue_formula2) |>
    tidyr::pivot_wider(names_from = strata,
                       values_from = glue_formula2) |>
    tidyr::separate_longer_delim(cols = c(-var, -var_type),
                                 delim = "\n") |>
    # Replace labels for stats
    mutate(glue_formula = stringr::str_replace(glue_formula,
                                               pattern = "\\{mean\\}",
                                               replacement = "Mean"),
           glue_formula = stringr::str_replace(glue_formula,
                                               pattern = "\\{sd\\}",
                                               replacement = "SD"),
           glue_formula = stringr::str_replace(glue_formula,
                                               pattern = "\\{p50\\}",
                                               replacement = "Median"),
           glue_formula = stringr::str_replace(glue_formula,
                                               pattern = "\\{p0\\}",
                                               replacement = "Min."),
           glue_formula = stringr::str_replace(glue_formula,
                                               pattern = "\\{p100\\}",
                                               replacement = "Max."),
           glue_formula = stringr::str_replace(glue_formula,
                                               pattern = "\\{n_level\\}",
                                               replacement = "n"),
           glue_formula = stringr::str_replace(glue_formula,
                                               pattern = "\\{n_strata\\}",
                                               replacement = "N"),
           glue_formula = stringr::str_replace(glue_formula,
                                               pattern = "\\{pct\\}",
                                               replacement = "%"))

}
