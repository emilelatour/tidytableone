

#' @title Format tidytableone for display and reporting
#'
#' @description
#' Take the raw summary statistics from `create_tidy_table_one` and format them
#' for inclusion in a presentable table.
#'
#' @param tidy_t1 Results in a tibble from `create_tidy_table_one`
#' @param default_continuous A glue statement that provides the formatting for
#'   continuous variables, Default is `"{mean} ({sd})"`
#' @param default_categorical A glue statement that provides the formatting for
#'   categorical variables, Default is `"{n} ({p})"`
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
#' @param show_smd Logical, if FALSE, Standardized Mean Differences (SMD) are not included.
#' @param use_labels Logical, if TRUE, labels are used instead of variable names.
#' @param combine_level_col Combines the `var` and `level` columns into one instead
#'   of two. HTML won't recognize the extra spaces when printing, but you can
#'   used `flextable::padding` to indent the right rows in that column later.
#' @param missing Indicates whether to include counts of NA values in the table.
#'   Allowed values are "no" (shows a column of number not missing), "ifany" (only display if
#'   any NA values), and "always" (includes NA count row for all variables).
#'   Default is "no".
#' @param missing_text Character string to use in place of `NA` when missing is
#'   "ifany" or "always". Default is "(Missing)".
#' @param default_miss A glue statement that provides the formatting for
#'   missing, Default is `"{n}"`
#' @param checkbox_p Logical; show per‑level checkbox p‑values column (default FALSE).
#' @param checkbox_p_adjust Character; p.adjust method for checkbox p‑values (default "none").
#' @param checkbox_block_p Logical; show one overall p‑value per checkbox block (default FALSE).
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
                               default_continuous = "{mean} ({sd})\n{median} [{range}]",
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
                               show_smd = FALSE,
                               use_labels = TRUE,
                               combine_level_col = TRUE,
                               missing = "no",
                               missing_text = "(Missing)",
                               default_miss = "{n}", 
                               checkbox_p = c("none","per_level"),
                               checkbox_p_adjust = "none",
                               checkbox_block_p = c("any","min"),
                               ...) {
  
  # Silence no visible binding for global variable
  p_value <- test <- smd <- label <- glue_formula <- NULL
  strata <- num_not_miss <- NULL
  
  checkbox_p <- match.arg(checkbox_p)
  checkbox_block_p <- match.arg(checkbox_block_p)
  
  # Checkbox p‑adjust method for both header and per‑level display
  cb_padj_method <- checkbox_p_adjust
  
  if (.is_no_strata(tidy_t1)) {
    res_stats <- adorn_tidytableone_no_strata(tidy_t1 = tidy_t1,
                                              default_continuous = default_continuous,
                                              default_categorical = default_categorical,
                                              fmt_vars = fmt_vars,
                                              con_accuracy = con_accuracy,
                                              cat_accuracy = cat_accuracy,
                                              p_accuracy = p_accuracy,
                                              prefix = prefix,
                                              suffix = suffix,
                                              big_mark = big_mark,
                                              decimal_mark = decimal_mark,
                                              style_positive = style_positive,
                                              style_negative = style_negative,
                                              scale_cut = scale_cut,
                                              con_trim = con_trim,
                                              cat_trim = cat_trim,
                                              show_pct = show_pct,
                                              exact = exact,
                                              nonnormal = nonnormal,
                                              equal_variance = equal_variance,
                                              no_cont_correction = no_cont_correction,
                                              monte_carlo_p = monte_carlo_p,
                                              show_test = show_test,
                                              show_smd = show_smd,
                                              use_labels = use_labels,
                                              combine_level_col = combine_level_col,
                                              missing = missing,
                                              missing_text = missing_text,
                                              default_miss = default_miss, ...)
    
    #### Return results --------------------------------
    
    return(res_stats)
    
  }
  
  
  #### get variable labels --------------------------------
  
  var_lbls <- tidy_t1 |>
    dplyr::select(var, var_type, label) |>
    dplyr::distinct() |>
    mutate(label = dplyr::if_else(is.na(label), var, label))
  
  #### keep class per var (for checkbox de-dup) -------------------------
  var_class <- tidy_t1 |>
    dplyr::distinct(var, class = dplyr::coalesce(class, NA_character_))
  
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
                              show_pct = show_pct,
                              missing = missing)
  
  tab_stats <- tab_stats |> 
    dplyr::left_join(var_class, 
                     by = dplyr::join_by(var))
  
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
          var_type == "categorical" & var %in% exact & var %in% monte_carlo_p ~ "Fisher's Exact Test, simulation",
          var_type == "categorical" & var %in% exact ~ "Fisher's Exact Test",
          var_type == "categorical" & var %in% no_cont_correction ~ "Chi-squared Test, no continuity correction",
          var_type == "categorical" & var %in% monte_carlo_p ~ "Chi-squared Test, simulation",
          var_type == "categorical" ~ "Chi-squared Test, with continuity correction",
          TRUE ~ NA_character_),
        p_value = scales::pvalue(p_value,
                                 accuracy = p_accuracy,
                                 decimal.mark = ".",
                                 prefix = NULL,
                                 add_p = FALSE)) |>
      dplyr::select(var,
                    p_value,
                    test)
    
    tab_pvals <- tab_pvals %>% dplyr::distinct(var, .keep_all = TRUE)
    
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
    
    tab_pvals <- tab_pvals %>% dplyr::distinct(var, .keep_all = TRUE)
    
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
          var_type == "categorical" & var %in% exact & var %in% monte_carlo_p ~ "Fisher's Exact Test, simulation",
          var_type == "categorical" & var %in% exact ~ "Fisher's Exact Test",
          var_type == "categorical" & var %in% no_cont_correction ~ "Chi-squared Test, no continuity correction",
          var_type == "categorical" & var %in% monte_carlo_p ~ "Chi-squared Test, simulation",
          var_type == "categorical" ~ "Chi-squared Test, with continuity correction",
          TRUE ~ NA_character_),
        p_value = scales::pvalue(p_value,
                                 accuracy = p_accuracy,
                                 decimal.mark = ".",
                                 prefix = NULL,
                                 add_p = FALSE)) |>
      dplyr::select(var,
                    p_value,
                    test)
    
    tab_pvals <- tab_pvals %>% dplyr::distinct(var, .keep_all = TRUE)
  }
  
  
  #### Get the SMD --------------------------------
  
  tab_smd <- tidy_t1 |>
    dplyr::distinct(var, smd) |>
    mutate(smd = scales::number(smd,
                                accuracy = p_accuracy,
                                decimal.mark = "."))
  
  
  #### Get the missing --------------------------------
  
  tab_miss <- tidy_t1 |>
    get_miss(missing = missing,
             missing_text = missing_text,
             default_miss = default_miss,
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
  
  
  #### Make the table --------------------------------
  
  # Detect real checkbox mode from the data (not just labels)
  is_checkbox_mode <- any(tidy_t1$class %in% "checkbox", na.rm = TRUE)
  
  # Vars present in the tidy table
  tidy_t1_vars <- dplyr::distinct(tidy_t1, var) |>
    dplyr::pull() |>
    as.character()
  
  # Vars that are truly checkbox rows
  cb_vars <- tidy_t1 |>
    dplyr::filter(class == "checkbox") |>
    dplyr::distinct(var) |>
    dplyr::pull() |>
    as.character()
  
  ## Lock in the variable order ---------------- 
  
  # # Group variables by their “base” label for display (e.g., race___1..98 → one group)
  # grouped_tidy_t1_vars <- group_similar_vars(tidy_t1_vars)
  # 
  # # Build a list: names = group label; values = member vars in that group
  # groups <- split(grouped_tidy_t1_vars$var, grouped_tidy_t1_vars$group_label_first)
  
  groups <- .order_groups_like_vars(tidy_t1, group_similar_vars)
  
  
  ## Proceed making the table ---------------- 
  
  # For each display group:
  adorned_tidy_t1 <- purrr::imap(groups, function(members, grp_label) {
    grp_cb_vars <- intersect(members, cb_vars)
    
    if (length(grp_cb_vars) > 0) {
      # This group is a checkbox block: build a single block using only the group’s checkbox vars
      build_tab1_cb(
        tab_var   = grp_label,
        cb_vars   = grp_cb_vars,            # <- pass ONLY this group’s checkbox members
        tab_pvals = tab_pvals,
        tab_stats = tab_stats,
        tab_miss  = tab_miss,
        missing   = missing,
        show_test = show_test,
        show_smd  = show_smd,
        tab_smd   = tab_smd
      )
    } else {
      # Non-checkbox: build rows for ALL members in this group, then bind them
      purrr::map_dfr(members, ~ build_tab1_noncb(
        tab_var   = .x,
        tab_pvals = tab_pvals,
        tab_stats = tab_stats,
        tab_miss  = tab_miss,
        missing   = missing,
        show_test = show_test,
        show_smd  = show_smd,
        tab_smd   = tab_smd
      ))
    }
  }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(dplyr::across(dplyr::everything(),
                                ~ dplyr::if_else(is.na(.), "", .)))
  
  
  #### Apply labels --------------------------------
  
  if (use_labels) {
    
    adorned_tidy_t1 <- adorned_tidy_t1 |>
      dplyr::left_join(var_lbls,
                       by = "var") |>
      dplyr::mutate(label = dplyr::if_else(is.na(label), "", label),
                    var = label)
    
  } else {
    
    adorned_tidy_t1 <- adorned_tidy_t1 |>
      dplyr::left_join(var_lbls,
                       by = "var")
    
  }
  
  
  adorned_tidy_t1 <- adorned_tidy_t1 |>
    tidyr::fill(var_type,
                .direction = "down") |>
    mutate(level = dplyr::if_else(level == "" & var_type == "continuous",
                                  glue_formula,
                                  level),
           glue_formula = dplyr::if_else(glue_formula == "", NA_character_, glue_formula)) |>
    tidyr::fill(glue_formula,
                .direction = "up") |>
    dplyr::mutate(var = dplyr::if_else(var_type == "categorical" & var != "",
                                       glue::glue("{var}, {glue_formula}"),
                                       var)) |>
    dplyr::select(-glue_formula,
                  -var_type,
                  -label)
  
  
  #### Combine var and level columns --------------------------------
  
  if (combine_level_col) {
    
    adorned_tidy_t1 <- adorned_tidy_t1 |>
      mutate(var = glue::glue("{var}  {level}")) |>
      dplyr::select(-level)
    
  }
  
  
  
  #### Top row (n) --------------------------------
  
  if (any(tidy_t1$var_type == "continuous") & any(tidy_t1$var_type == "categorical")) {
    
    top_row <- tidy_t1 |>
      dplyr::distinct(strata,
                      n) |>
      dplyr::filter(!is.na(n)) |>
      mutate(n = scales::number(x = n,
                                accuracy = 1.0,
                                scale = 1,
                                prefix = "",
                                suffix = "",
                                big.mark = "",
                                decimal.mark = ".",
                                style_positive = "none",
                                style_negative = "hyphen",
                                scale_cut = NULL,
                                trim = FALSE)) |>
      tidyr::pivot_wider(names_from = strata,
                         values_from = n) |>
      mutate(var = "n",
             p_value = "") |>
      dplyr::select(var,
                    dplyr::everything())
    
  } else if (any(tidy_t1$var_type == "continuous")) {
    
    top_row <- tidy_t1 |>
      dplyr::distinct(strata,
                      n) |>
      dplyr::filter(!is.na(n)) |>
      mutate(n = scales::number(x = n,
                                accuracy = 1.0,
                                scale = 1,
                                prefix = "",
                                suffix = "",
                                big.mark = "",
                                decimal.mark = ".",
                                style_positive = "none",
                                style_negative = "hyphen",
                                scale_cut = NULL,
                                trim = FALSE)) |>
      tidyr::pivot_wider(names_from = strata,
                         values_from = n) |>
      mutate(var = "n",
             p_value = "") |>
      dplyr::select(var,
                    dplyr::everything())
    
    
  } else if (any(tidy_t1$var_type == "categorical")) {
    
    top_row <- tidy_t1 |>
      dplyr::distinct(strata,
                      n_strata) |>
      dplyr::rename(n = n_strata) |>
      dplyr::filter(!is.na(n)) |>
      mutate(n = scales::number(x = n,
                                accuracy = 1.0,
                                scale = 1,
                                prefix = "",
                                suffix = "",
                                big.mark = "",
                                decimal.mark = ".",
                                style_positive = "none",
                                style_negative = "hyphen",
                                scale_cut = NULL,
                                trim = FALSE)) |>
      tidyr::pivot_wider(names_from = strata,
                         values_from = n) |>
      mutate(var = "n",
             p_value = "") |>
      dplyr::select(var,
                    dplyr::everything())
  }
  
  
  empty_row <- tibble::as_tibble(lapply(top_row, function(x) ""))
  
  adorned_tidy_t1 <- top_row |>
    dplyr::bind_rows(empty_row) |>
    dplyr::bind_rows(adorned_tidy_t1)
  
  
  #### Not combine var and level columns --------------------------------
  
  if (!combine_level_col) {
    
    adorned_tidy_t1 <- adorned_tidy_t1 |>
      dplyr::relocate(level,
                      .after = var)
    
  }
  
  #### Final clean-up --------------------------------
  
  if (missing == "no") {
    
    adorned_tidy_t1 <- adorned_tidy_t1 |>
      dplyr::relocate(num_not_miss,
                      .before = "Overall")|>
      mutate(num_not_miss = as.character(num_not_miss),
             num_not_miss = tidyr::replace_na(num_not_miss, ""))
    
  }
  
  
  if (show_test == TRUE) {
    
    adorned_tidy_t1 <- adorned_tidy_t1 |>
      mutate(test = tidyr::replace_na(test, ""))
    
  }
  
  if (show_smd == FALSE) {
    
    adorned_tidy_t1 <- adorned_tidy_t1 |>
      mutate(smd = tidyr::replace_na(smd, ""))
    
  }
  
  
  adorned_tidy_t1 <- adorned_tidy_t1 |>
    mutate(dplyr::across(.cols = dplyr::everything(),
                         .fns = ~ tidyr::replace_na(., ""))) |>
    mutate(dplyr::across(.cols = dplyr::everything(),
                         .fns = ~ as.character(.)))
  
  if ("class" %in% names(adorned_tidy_t1)) {
    
    adorned_tidy_t1 <- adorned_tidy_t1 |> 
      dplyr::select(-class)
  }
  
  
  #### Return table --------------------------------
  
  return(adorned_tidy_t1)
  
  
}


#### Build tab1 --------------------------------

build_tab1 <- function(tab_var,
                       cb_vars,
                       tab_pvals,
                       tab_stats,
                       tab_miss,
                       missing = "no",
                       show_test = FALSE,
                       show_smd = FALSE,
                       tab_smd) {
  
  if (tab_var %in% cb_vars) {
    
    res <- build_tab1_cb(tab_var = tab_var,
                         cb_vars = cb_vars,
                         tab_pvals = tab_pvals,
                         tab_stats = tab_stats,
                         tab_miss = tab_miss,
                         missing = missing,
                         show_test = show_test,
                         show_smd = show_smd,
                         tab_smd = tab_smd)
    
  } else {
    
    res <- build_tab1_noncb(tab_var = tab_var,
                            tab_pvals = tab_pvals,
                            tab_stats = tab_stats,
                            tab_miss = tab_miss,
                            missing = missing,
                            show_test = show_test,
                            show_smd = show_smd,
                            tab_smd = tab_smd)
    
    
  }
  
  
  
  return(res)
  
}





# For non-checkboxes
build_tab1_noncb <- function(tab_var,
                             tab_pvals,
                             tab_stats,
                             tab_miss,
                             missing = "no",
                             show_test = FALSE,
                             show_smd = FALSE,
                             tab_smd) {
  
  # Silence no visible binding for global variable
  p_value <- test <- smd <- num_not_miss <- NULL
  
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
  
  if (missing == "no") {
    
    m_i <- tab_miss |>
      dplyr::filter(var == tab_var) |>
      dplyr::pull(num_not_miss)
    
    res <- tibble::tibble(var = tab_var,
                          num_not_miss = m_i,
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
    
  } else {
    
    m_i <- tab_miss |>
      dplyr::filter(var == tab_var) |>
      dplyr::select(-var)
    
    res <- tibble::tibble(var = tab_var,
                          p_value = p_i,
                          test = t_i,
                          smd = smd_i) |>
      dplyr::bind_rows(s_i) |>
      dplyr::bind_rows(m_i) |>
      dplyr::select(var,
                    dplyr::everything(),
                    -p_value,
                    -test,
                    -smd,
                    p_value,
                    test,
                    smd) |>
      dplyr::add_row()
    
  }
  
  
  
  if (!show_test) {
    
    res <- res |>
      dplyr::select(-test)
    
  }
  
  if (!show_smd) {
    
    res <- res |>
      dplyr::select(-smd)
    
  }
  
  
  
  return(res)
  
}



# For checkboxes
build_tab1_cb <- function(tab_var,
                          cb_vars,
                          tab_pvals,
                          tab_stats,
                          tab_miss,
                          missing = "no",
                          show_test = FALSE,
                          show_smd = FALSE,
                          tab_smd) {
  
  # Silence no visible binding for global variable
  p_value <- test <- smd <- num_not_miss <- NULL
  
  p_i <- tab_pvals |>
    dplyr::filter(var %in% cb_vars)
  
  s_i <- tab_stats |>
    dplyr::filter(var %in% cb_vars)
  
  t_i <- tab_pvals |>
    dplyr::filter(var %in% cb_vars) |>
    dplyr::pull(test)
  
  smd_i <- tab_smd |>
    dplyr::filter(var %in% cb_vars)
  
  s_i <- s_i |>
    dplyr::left_join(p_i,
                     by = dplyr::join_by(var)) |>
    dplyr::left_join(smd_i,
                     by = dplyr::join_by(var)) |>
    dplyr::select(-var,
                  -var_type,
                  -class)
  
  if (missing == "no") {
    
    m_i <- tab_miss |>
      dplyr::filter(var %in% cb_vars) |>
      dplyr::pull(num_not_miss)
    
    res <- tibble::tibble(var = cb_vars[[1]],
                          num_not_miss = m_i[1],   # NEeds to be fixed later
                          p_value = NA_character_,  # NA for now. Maybe an overall p-value if that ever gets implemented
                          test = t_i[1],            # Maybe will get revised later
                          smd = NA_character_) |>
      dplyr::bind_rows(s_i) |>
      dplyr::select(var,
                    dplyr::everything(),
                    -p_value,
                    -test,
                    -smd,
                    p_value,
                    test,
                    smd) |>
      dplyr::add_row() |>
      tidyr::fill(glue_formula,
                  .direction = "up") |>
      mutate(glue_formula = dplyr::if_else(dplyr::row_number() == 1, glue_formula, NA_character_))
    
  } else {
    
    m_i <- tab_miss |>
      dplyr::filter(var %in% cb_vars) |>
      dplyr::select(-var) |>
      dplyr::distinct()
    
    res <- tibble::tibble(var = cb_vars[[1]],
                          p_value = NA_character_,  # NA for now. Maybe an overall p-value if that ever gets implemented
                          test = t_i[1],            # Maybe will get revised later
                          smd = NA_character_) |>
      dplyr::bind_rows(s_i) |>
      dplyr::bind_rows(m_i) |>
      dplyr::select(var,
                    dplyr::everything(),
                    -p_value,
                    -test,
                    -smd,
                    p_value,
                    test,
                    smd) |>
      dplyr::add_row() |>
      tidyr::fill(glue_formula,
                  .direction = "up") |>
      mutate(glue_formula = dplyr::if_else(dplyr::row_number() == 1, glue_formula, NA_character_))
    
  }
  
  
  
  if (!show_test) {
    
    res <- res |>
      dplyr::select(-test)
    
  }
  
  if (!show_smd) {
    
    res <- res |>
      dplyr::select(-smd)
    
  }
  
  
  
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
                           show_pct = TRUE,
                           missing = "ifany", ...) {
  
  # Silence no visible binding for global variable
  glue_formula <- pct_fmt <- cv <- strata <- glue_formula2 <- NULL
  n_level_valid <- n_strata_valid <- n_fmt <- N_fmt <- NULL
  class <- var_type <- level <- var <- NULL
  
  # Percentage suffix
  pct_suffix <- if (show_pct) "%" else ""
  
  #### Glue formulae --------------------------------
  if (is.null(fmt_vars)) {
    formula_for_table <- t1 |>
      dplyr::distinct(var, var_type) |>
      dplyr::mutate(glue_formula = dplyr::case_when(
        var_type == "continuous"  ~ default_continuous,
        var_type == "categorical" ~ default_categorical,
        .default = NA_character_
      ))
  } else {
    override_formulae <- tibble::tibble(
      var = names(fmt_vars),
      glue_formula = purrr::map_chr(.x = var, .f = ~ fmt_vars[[.x]])
    )
    formula_for_table <- t1 |>
      dplyr::distinct(var, var_type) |>
      dplyr::mutate(glue_formula = dplyr::case_when(
        var_type == "continuous"  ~ default_continuous,
        var_type == "categorical" ~ default_categorical,
        .default = NA_character_
      )) |>
      dplyr::rows_update(override_formulae, by = "var")
  }
  
  ## Normalize tokens used in glue ----------------
  formula_for_table <- formula_for_table |>
    dplyr::mutate(
      glue_formula = stringr::str_replace_all(glue_formula, "median|Median|med|Med", "p50"),
      glue_formula = stringr::str_replace_all(glue_formula, "min|Min|minimum|Minimum", "p0"),
      glue_formula = stringr::str_replace_all(glue_formula, "max|Max|maximum|Maximum", "p100"),
      glue_formula = stringr::str_replace_all(glue_formula, "\\{iqr\\}|\\{IQR\\}", "{p25} to {p75}"),
      glue_formula = stringr::str_replace_all(glue_formula, "\\{range\\}", "{p0} to {p100}"),
      # IMPORTANT: point placeholders to *formatted* aliases
      glue_formula = stringr::str_replace_all(glue_formula, "\\{p\\}", "{pct_fmt}"),
      glue_formula = stringr::str_replace_all(glue_formula, "\\{n\\}", "{n_fmt}"),
      glue_formula = stringr::str_replace_all(glue_formula, "\\{N\\}", "{N_fmt}")
    )
  
  #### Build formatting aliases for counts & percents ----------------
  t1 <- t1 |>
    dplyr::mutate(
      n_fmt   = dplyr::if_else(!is.na(class) & class == "checkbox", n_level,         n_level_valid),
      N_fmt   = dplyr::if_else(!is.na(class) & class == "checkbox", n_strata,        n_strata_valid),
      pct_fmt = dplyr::if_else(!is.na(class) & class == "checkbox",
                               pct,                                   n_level_valid / n_strata_valid)
    )
  
  #### Make the pretty t1 --------------------------------
  if (any(t1$var_type == "continuous") & any(t1$var_type == "categorical")) {
    
    pretty_t1 <- t1 |>
      # format counts shown in table
      dplyr::mutate(dplyr::across(c(n_fmt, N_fmt),
                                  ~ scales::number(.,
                                                   accuracy = 1.0, scale = 1,
                                                   prefix = "", suffix = "",
                                                   big.mark = "", decimal.mark = ".",
                                                   style_positive = "none",
                                                   style_negative = "hyphen",
                                                   scale_cut = NULL, trim = FALSE))) |>
      # format percentages
      dplyr::mutate(pct_fmt = scales::percent(pct_fmt,
                                              accuracy = cat_accuracy, scale = 100,
                                              prefix = prefix, suffix = pct_suffix,
                                              big.mark = big_mark, decimal.mark = decimal_mark,
                                              style_positive = style_positive,
                                              style_negative = style_negative,
                                              scale_cut = scale_cut, trim = cat_trim)) |>
      # format continuous stats
      dplyr::mutate(dplyr::across(mean:cv, ~ scales::number(.,
                                                            accuracy = con_accuracy, scale = 1,
                                                            prefix = prefix, suffix = suffix,
                                                            big.mark = big_mark, decimal.mark = decimal_mark,
                                                            style_positive = style_positive,
                                                            style_negative = style_negative,
                                                            scale_cut = scale_cut, trim = con_trim))) |>
      dplyr::filter(!(is.na(level) & var_type == "categorical"))
    
  } else if (any(t1$var_type == "categorical")) {
    
    pretty_t1 <- t1 |>
      dplyr::mutate(dplyr::across(c(n_fmt, N_fmt),
                                  ~ scales::number(.,
                                                   accuracy = 1.0, scale = 1,
                                                   prefix = "", suffix = "",
                                                   big.mark = "", decimal.mark = ".",
                                                   style_positive = "none",
                                                   style_negative = "hyphen",
                                                   scale_cut = NULL, trim = FALSE))) |>
      dplyr::mutate(pct_fmt = scales::percent(pct_fmt,
                                              accuracy = cat_accuracy, scale = 100,
                                              prefix = prefix, suffix = pct_suffix,
                                              big.mark = big_mark, decimal.mark = decimal_mark,
                                              style_positive = style_positive,
                                              style_negative = style_negative,
                                              scale_cut = scale_cut, trim = cat_trim)) |>
      dplyr::filter(!is.na(level))
    
  } else { # continuous only
    
    pretty_t1 <- t1 |>
      dplyr::mutate(dplyr::across(mean:cv, ~ scales::number(.,
                                                            accuracy = con_accuracy, scale = 1,
                                                            prefix = prefix, suffix = suffix,
                                                            big.mark = big_mark, decimal.mark = decimal_mark,
                                                            style_positive = style_positive,
                                                            style_negative = style_negative,
                                                            scale_cut = scale_cut, trim = con_trim)))
  }
  
  if (!"level" %in% names(pretty_t1)) {
    pretty_t1 <- pretty_t1 |> dplyr::mutate(level = "")
  }
  
  pretty_t1 |>
    dplyr::left_join(formula_for_table, by = c("var", "var_type")) |>
    dplyr::rowwise() |>
    dplyr::mutate(glue_formula2 = glue::glue(glue_formula)) |>
    dplyr::select(strata, var, level, var_type, glue_formula, glue_formula2) |>
    tidyr::pivot_wider(names_from = strata, values_from = glue_formula2) |>
    tidyr::separate_longer_delim(cols = c(-var, -var_type), delim = "\n") |>
    # relabel statistic names
    dplyr::mutate(
      glue_formula = stringr::str_replace(glue_formula, "\\{mean\\}", "Mean"),
      glue_formula = stringr::str_replace(glue_formula, "\\{sd\\}", "SD"),
      glue_formula = stringr::str_replace(glue_formula, "\\{p50\\}", "Median"),
      glue_formula = stringr::str_replace(glue_formula, "\\{p0\\}", "Min."),
      glue_formula = stringr::str_replace(glue_formula, "\\{p100\\}", "Max."),
      glue_formula = stringr::str_replace(glue_formula, "\\{n_fmt\\}", "n"),
      glue_formula = stringr::str_replace(glue_formula, "\\{N_fmt\\}", "N"),
      glue_formula = stringr::str_replace(glue_formula, "\\{pct_fmt\\}", "%"),
      glue_formula = stringr::str_replace(glue_formula, "\\{p25\\} to \\{p75\\}", "IQR")
    )
}


#### Handle NAs --------------------------------

get_miss <- function(t1,
                     missing = "no",
                     missing_text = "(Missing)",
                     default_miss = "{n}",
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
  n_strata_valid <- n_available <- p_available <- missing_p <- num_not_miss <- n_miss <- NULL
  
  
  # Percentage suffix
  if (show_pct) {
    pct_suffix = "%"
  } else {
    pct_suffix = ""
  }
  
  
  if (any(t1$var_type == "continuous") & any(t1$var_type == "categorical")) {
    
    t1 <- t1
    
  } else if (any(t1$var_type == "continuous")) {
    
    t1 <- t1 |>
      mutate(n_strata = NA_integer_,
             n_strata_valid = NA_integer_,
             level = NA_character_)
    
  } else if (any(t1$var_type == "categorical")) {
    
    t1 <- t1 |>
      mutate(n = NA_integer_,
             complete = NA_integer_,
             missing = NA_integer_)
    
  }
  
  
  if (missing == "no") {
    
    miss_tab <- t1 |>
      dplyr::filter(strata == "Overall") |>
      dplyr::distinct(var,
                      n,
                      complete,
                      n_strata,
                      n_strata_valid,
                      var_type) |>
      mutate(n = dplyr::coalesce(n, n_strata),
             n_available = dplyr::coalesce(complete, n_strata_valid),
             p_available = n_available / n) |>
      dplyr::select(var, n, n_available, p_available)
    
    
    miss_tab <- miss_tab |>
      # Format counts for strata
      mutate(dplyr::across(.cols = c(n,
                                     n_available),
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
      mutate(p_available = scales::percent(x = p_available,
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
    
  } else if (missing == "ifany") {
    
    any_miss <- t1 |>
      dplyr::filter(strata == "Overall") |>
      mutate(n = dplyr::coalesce(n, n_strata),
             missing = dplyr::if_else(var_type == "continuous",
                                      missing,
                                      n_strata - n_strata_valid)) |>
      dplyr::filter(missing > 0) |>
      dplyr::pull(var)
    
    miss_tab <- t1 |>
      dplyr::filter(var %in% any_miss) |>
      dplyr::select(strata,
                    var,
                    n,
                    missing,
                    level,
                    n_strata,
                    n_strata_valid,
                    var_type) |>
      mutate(n = dplyr::coalesce(n, n_strata),
             missing = dplyr::if_else(var_type == "continuous",
                                      missing,
                                      n_strata - n_strata_valid),
             missing_p = missing / n) |>
      dplyr::distinct(strata,
                      var,
                      n,
                      missing,
                      missing_p,
                      var_type)
    
    
    miss_tab <- miss_tab |>
      # Format counts for strata
      mutate(dplyr::across(.cols = c(n,
                                     missing),
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
      mutate(missing_p = scales::percent(x = missing_p,
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
    
    
    
  } else if (missing == "always") {
    
    miss_tab <- t1 |>
      dplyr::select(strata,
                    var,
                    n,
                    missing,
                    level,
                    n_strata,
                    n_strata_valid,
                    var_type) |>
      mutate(n = dplyr::coalesce(n, n_strata),
             missing = dplyr::if_else(var_type == "continuous",
                                      missing,
                                      n_strata - n_strata_valid),
             missing_p = missing / n) |>
      dplyr::distinct(strata,
                      var,
                      n,
                      missing,
                      missing_p,
                      var_type)
    
    
    miss_tab <- miss_tab |>
      # Format counts for strata
      mutate(dplyr::across(.cols = c(n,
                                     missing),
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
      mutate(missing_p = scales::percent(x = missing_p,
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
  
  
  
  # Format results
  
  if (missing == "no") {
    
    miss_tab <- miss_tab |>
      mutate(glue_formula = default_miss,
             glue_formula = stringr::str_replace_all(string = glue_formula,
                                                     pattern = "\\{n\\}",
                                                     replacement = "{n_available}"),
             glue_formula = stringr::str_replace_all(string = glue_formula,
                                                     pattern = "\\{p\\}",
                                                     replacement = "{p_available}")) |>
      dplyr::rowwise() |>
      mutate(num_not_miss = glue::glue(glue_formula)) |>
      dplyr::select(var, num_not_miss)
    
  } else {
    
    miss_tab <- miss_tab |>
      mutate(glue_formula = default_miss,
             glue_formula = stringr::str_replace_all(string = glue_formula,
                                                     pattern = "\\{n\\}",
                                                     replacement = "{missing}"),
             glue_formula = stringr::str_replace_all(string = glue_formula,
                                                     pattern = "\\{p\\}",
                                                     replacement = "{missing_p}")) |>
      dplyr::rowwise() |>
      mutate(n_miss = glue::glue(glue_formula)) |>
      mutate(level = missing_text) |>
      dplyr::select(strata, var, level, n_miss) |>
      tidyr::pivot_wider(names_from = strata,
                         values_from = n_miss)
    
  }
  
  
  
  
  return(miss_tab)
  
  
}


#### Order groups like vars -------------------------------- 

.order_groups_like_vars <- function(tidy_t1, group_similar_vars) {
  ord <- tidy_t1 |>
    dplyr::distinct(var) |>
    dplyr::pull(var) |>
    as.character()

  grouped <- group_similar_vars(ord) |>
    dplyr::filter(var %in% ord) |>
    dplyr::mutate(var_pos = match(var, ord))

  group_order <- grouped |>
    dplyr::group_by(group_label_first) |>
    dplyr::summarise(first_pos = min(var_pos), .groups = "drop") |>
    dplyr::arrange(first_pos) |>
    dplyr::pull(group_label_first)

  grouped <- grouped |>
    dplyr::mutate(group_label_first = factor(group_label_first, levels = group_order))

  groups <- split(grouped$var, grouped$group_label_first)
  lapply(groups, function(v) v[order(match(v, ord))])
}



# helper (optional; you can inline the logic if you prefer)
.is_no_strata <- function(t1) {
  has_strata     <- "strata" %in% names(t1)
  has_strata_var <- "strata_var" %in% names(t1)

  only_overall <- has_strata && all(is.na(t1$strata) | t1$strata == "Overall")
  svar_all_na  <- has_strata_var && all(is.na(t1$strata_var))

  !has_strata || only_overall || svar_all_na
}
