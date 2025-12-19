

#' @title Format tidytableone for display and reporting
#'
#' @description
#' Take the raw summary statistics from `create_tidytableone` and format them
#' for inclusion in a presentable table.
#'
#' @param tidy_t1 Results in a tibble from `create_tidytableone`
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
#'   processed so that prefix = "$" will yield (e.g.) -$1⁠ and ⁠($1)⁠.
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
#' @param range_sep Character string used to separate the lower and upper bounds
#'   of ranges in continuous variable summaries (e.g., Min\\u2013Max, IQR).  
#'   Defaults to an en dash surrounded by spaces (`" \\u2013 "`).  
#'   You may supply any separator, such as `"\\u2013"` (no spaces) or `" to "`.  
#'   `range_sep = "\\u2013"` yields `"41.0\\u20134556.0"`, while `range_sep = " \\u2013 "`
#'   yields `"41.0 \\u2013 4556.0"`.
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
#' tab1 <- create_tidytableone(data = pbc_mayo,
#'                             strata = "trt",
#'                             vars = c("time",
#'                                      "status",
#'                                      "age",
#'                                      "sex",
#'                                      "ascites",
#'                                      "hepato",
#'                                      "spiders",
#'                                      "edema",
#'                                      "bili",
#'                                      "chol",
#'                                      "albumin",
#'                                      "copper",
#'                                      "alk_phos",
#'                                      "ast",
#'                                      "trig",
#'                                      "platelet",
#'                                      "protime",
#'                                      "stage"))
#'
#' dplyr::glimpse(tab1)
#'
#' # Check to see if assumptions may be violated
#' check_tests <- calc_tableone_tests(tab1)
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


adorn_tidytableone_no_strata <- function(tidy_t1,
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
                                         range_sep = " \u2013 ",
                                         ...) {
  
  # Silence no visible binding for global variable
  label <- glue_formula <- glue_formula2 <- Overall <- num_not_miss <- NULL
  
  #### get variable labels --------------------------------
  
  var_lbls <- tidy_t1 |>
    dplyr::select(var, var_type, label) |>
    dplyr::distinct() |>
    mutate(label = dplyr::if_else(is.na(label), var, label))
  
  #### Get the stats --------------------------------
  
  tab_stats <- make_t1_pretty_no_strata(t1 = tidy_t1,
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
  
  
  #### Get the missing --------------------------------
  
  tab_miss <- tidy_t1 |>
    get_miss_no_strata(missing = missing,
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
  
  stopifnot("class" %in% names(tidy_t1))  # optional but helpful
  
  # Checkbox vars are identified via `class == "checkbox"` and grouped by `label`
  # (eg, label = "Race" shared across race___1, race___2, race___any_selected).
  cb_groups <- tidy_t1 |>
    dplyr::filter(class == "checkbox") |>
    dplyr::distinct(var, label) |>
    dplyr::mutate(label = dplyr::if_else(is.na(label), var, label))

  # cb_labels <- cb_groups |>
  #   dplyr::distinct(label) |>
  #   dplyr::pull(label) |>
  #   as.character()
  # 
  # non_cb_vars <- tidy_t1 |>
  #   dplyr::distinct(var, class) |>
  #   dplyr::filter(class != "checkbox") |>
  #   dplyr::pull(var) |>
  #   as.character()
  # 
  # # Render non-checkbox vars as usual
  # out_non_cb <- purrr::map_df(
  #   .x = non_cb_vars,
  #   .f = ~ build_tab1_no_strata(
  #     tab_var = .x,
  #     tab_stats = tab_stats,
  #     tab_miss  = tab_miss,
  #     missing   = missing
  #   )
  # )
  # 
  # # Render checkbox blocks: one block per checkbox label
  # out_cb <- purrr::map_df(
  #   .x = cb_labels,
  #   .f = ~ build_tab1_checkbox_no_strata(
  #     cb_label = .x,
  #     cb_groups = cb_groups,
  #     tab_stats = tab_stats,
  #     tab_miss  = tab_miss,
  #     missing   = missing
  #   )
  # )
  # 
  # adorned_tidy_t1 <- dplyr::bind_rows(out_non_cb, out_cb) |>
  #   dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
  #                               .fns = ~ dplyr::if_else(is.na(.), "", .)))
  
  # Desired order comes from tidy_t1$var levels (if factor), else first appearance
  var_levels <- if (is.factor(tidy_t1$var)) levels(tidy_t1$var) else unique(as.character(tidy_t1$var))
  var_levels <- as.character(var_levels)
  
  # Map checkbox member var -> checkbox label (block)
  cb_map <- cb_groups |>
    dplyr::distinct(var, label) |>
    dplyr::mutate(var = as.character(var),
                  label = as.character(label))
  
  # Build a single render plan in order:
  # - normal vars appear as themselves
  # - checkbox member vars are collapsed to a single label (block) at the position
  #   of the first member encountered in var_levels
  seen_blocks <- character(0)
  render_plan <- character(0)
  
  for (v in var_levels) {
    idx <- match(v, cb_map$var)
    if (!is.na(idx)) {
      lab <- cb_map$label[[idx]]
      if (!lab %in% seen_blocks) {
        render_plan <- c(render_plan, lab)
        seen_blocks <- c(seen_blocks, lab)
      }
    } else {
      render_plan <- c(render_plan, v)
    }
  }
  
  # Render in the planned order
  adorned_tidy_t1 <- purrr::map_df(
    .x = render_plan,
    .f = function(item) {
      if (item %in% cb_map$label) {
        build_tab1_checkbox_no_strata(
          cb_label  = item,
          cb_groups = cb_groups,
          tab_stats = tab_stats,
          tab_miss  = tab_miss,
          missing   = missing
        )
      } else {
        build_tab1_no_strata(
          tab_var   = item,
          tab_stats = tab_stats,
          tab_miss  = tab_miss,
          missing   = missing
        )
      }
    }
  ) |>
    dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
                                .fns = ~ dplyr::if_else(is.na(.), "", .)))
  
  
  #### Apply labels --------------------------------
  
  if (use_labels) {
    
    adorned_tidy_t1 <- adorned_tidy_t1 |>
      dplyr::left_join(var_lbls, by = "var") |>
      dplyr::mutate(label = dplyr::if_else(is.na(label), "", label),
                    var = dplyr::if_else(label == "", var, label))
    
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
  
  
  #### Rename column --------------------------------
  
  adorned_tidy_t1 <- adorned_tidy_t1 |>
    dplyr::rename(Overall = glue_formula2)
  
  
  #### Top row (n) --------------------------------
  
  if (any(tidy_t1$var_type == "continuous") & any(tidy_t1$var_type == "categorical")) {
    
    top_row <- tidy_t1 |>
      dplyr::distinct(n) |>
      dplyr::filter(!is.na(n)) |>
      dplyr::rename(Overall = n) |>
      mutate(Overall = scales::number(x = Overall,
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
      mutate(var = "n") |>
      dplyr::select(var,
                    dplyr::everything())
    
  } else if (any(tidy_t1$var_type == "continuous")) {
    
    top_row <- tidy_t1 |>
      dplyr::distinct(n) |>
      dplyr::filter(!is.na(n)) |>
      dplyr::rename(Overall = n) |>
      mutate(Overall = scales::number(x = Overall,
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
      mutate(var = "n") |>
      dplyr::select(var,
                    dplyr::everything())
    
    
  } else if (any(tidy_t1$var_type == "categorical")) {
    
    top_row <- tidy_t1 |>
      dplyr::distinct(n_strata) |>
      dplyr::rename(n = n_strata) |>
      dplyr::filter(!is.na(n)) |>
      dplyr::rename(Overall = n) |>
      mutate(Overall = scales::number(x = Overall,
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
      mutate(var = "n") |>
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
  
  
  adorned_tidy_t1 <- adorned_tidy_t1 |>
    mutate(dplyr::across(.cols = dplyr::everything(),
                         .fns = ~ tidyr::replace_na(., ""))) |>
    mutate(dplyr::across(.cols = dplyr::everything(),
                         .fns = ~ as.character(.)))
  
  
  #### Return table --------------------------------
  
  return(adorned_tidy_t1)
  
  
}


#### Build tab1 --------------------------------

build_tab1_no_strata <- function(tab_var,
                                 tab_stats,
                                 tab_miss,
                                 missing = "no") {
  
  num_not_miss <- NULL
  
  s_i <- tab_stats |>
    dplyr::filter(var == tab_var) |>
    dplyr::select(-var,
                  -var_type) |>
    mutate(var = NA_character_)
  
  
  
  if (missing == "no") {
    
    m_i <- tab_miss |>
      dplyr::filter(var == tab_var) |>
      dplyr::pull(num_not_miss)
    
    res <- tibble::tibble(var = tab_var,
                          num_not_miss = m_i) |>
      dplyr::bind_rows(s_i) |>
      dplyr::select(var,
                    dplyr::everything()) |>
      dplyr::add_row()
    
  } else {
    
    m_i <- tab_miss |>
      dplyr::filter(var == tab_var) |>
      dplyr::select(-var)
    
    res <- tibble::tibble(var = tab_var) |>
      dplyr::bind_rows(s_i) |>
      dplyr::bind_rows(m_i) |>
      dplyr::select(var,
                    dplyr::everything()) |>
      dplyr::add_row()
    
  }
  
  
  
  
  return(res)
  
}


build_tab1_checkbox_no_strata <- function(cb_label,
                                          cb_groups,
                                          tab_stats,
                                          tab_miss,
                                          missing = "no") {
  
  # all vars that belong to this checkbox group label (e.g., Race)
  cb_vars <- cb_groups |>
    dplyr::filter(label == cb_label) |>
    dplyr::pull(var) |>
    as.character()
  
  # Build the stats rows for ALL member vars, but only keep the level rows
  # (the header row is created once below)
  s_all <- tab_stats |>
    dplyr::filter(var %in% cb_vars) |>
    dplyr::select(-var, -var_type) |>
    dplyr::mutate(var = NA_character_)
  
  # Add one header row for the group (var = cb_label)
  # Choose num_not_miss from the "any_selected" var if present, else first member
  if (missing == "no") {
    
    miss_var <- cb_vars[1]
    any_var <- cb_vars[grepl("___any_selected$", cb_vars)]
    if (length(any_var) > 0) miss_var <- any_var[1]
    
    m_i <- tab_miss |>
      dplyr::filter(var == miss_var) |>
      dplyr::pull(num_not_miss)
    
    res <- tibble::tibble(var = cb_label,
                          num_not_miss = m_i) |>
      dplyr::bind_rows(s_all) |>
      dplyr::select(var, dplyr::everything()) |>
      dplyr::add_row()
    
  } else {
    
    # if you show missing rows, you probably want missing per member var;
    # simplest is: do not append missing rows for checkbox blocks (or implement if needed)
    res <- tibble::tibble(var = cb_label) |>
      dplyr::bind_rows(s_all) |>
      dplyr::select(var, dplyr::everything()) |>
      dplyr::add_row()
  }
  
  res
}


#### Make t1 pretty --------------------------------

make_t1_pretty_no_strata <- function(t1,
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
                                     range_sep = " \u2013 ",
                                     ...) {
  
  # Silence no visible binding for global variable
  glue_formula <- pct <- cv <- strata <- glue_formula2 <- NULL
  n_level_valid <- n_strata_valid <- NULL
  
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
                                                   pattern = "\\{iqr\\}",
                                                   replacement = "{p25}{range_sep}{p75}")) |>
    mutate(glue_formula = stringr::str_replace_all(string = glue_formula,
                                                   pattern = "\\{IQR\\}",
                                                   replacement = "{p25}{range_sep}{p75}")) |>
    mutate(glue_formula = stringr::str_replace_all(string = glue_formula,
                                                   pattern = "\\{range\\}",
                                                   replacement = "{p0}{range_sep}{p100}")) |>
    mutate(glue_formula = stringr::str_replace_all(string = glue_formula,
                                                   pattern = "\\{p\\}",
                                                   replacement = "{pct}")) |>
    mutate(glue_formula = stringr::str_replace_all(string = glue_formula,
                                                   pattern = "\\{n\\}",
                                                   replacement = "{n_level_valid}")) |>
    mutate(glue_formula = stringr::str_replace_all(string = glue_formula,
                                                   pattern = "\\{N\\}",
                                                   replacement = "{n_strata_valid}"))
  
  
  
  
  #### Make the pretty t1 --------------------------------
  
  if (any(t1$var_type == "continuous") & any(t1$var_type == "categorical")) {
    
    pretty_t1 <- t1 |>
      mutate(pct = n_level_valid / n_strata_valid) |>
      # Format counts for strata
      mutate(dplyr::across(.cols = c(n_level_valid,
                                     n_strata_valid),
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
                                   trim = cat_trim)) |>
      # Format for continuous data
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
                                                   trim = con_trim))) |>
      dplyr::filter(!(is.na(level) & var_type == "categorical"))
    
    
    
  } else if (any(t1$var_type == "categorical")) {
    
    pretty_t1 <- t1 |>
      mutate(pct = n_level_valid / n_strata_valid) |>
      # Format counts for strata
      mutate(dplyr::across(.cols = c(n_level_valid,
                                     n_strata_valid),
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
                                   trim = cat_trim)) |>
      dplyr::filter(!is.na(level))
    
  } else if (any(t1$var_type == "continuous")) {
    
    # Format continuous stats: mean, sd, median, etc.
    pretty_t1 <- t1 |>
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
  
  
  if (!any(names(pretty_t1) == "level")) {
    
    pretty_t1 <- pretty_t1 |>
      mutate(level = "")
  }
  
  
  pretty_t1 <- pretty_t1 |>
    dplyr::left_join(formula_for_table,
                     by = c("var",
                            "var_type")) |>
    dplyr::rowwise() |>
    mutate(glue_formula2 = glue::glue(glue_formula)) |>
    dplyr::select(var,
                  level,
                  var_type,
                  glue_formula,
                  glue_formula2) |>
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
                                               pattern = "\\{n_level_valid\\}",
                                               replacement = "n"),
           glue_formula = stringr::str_replace(glue_formula,
                                               pattern = "\\{n_strata_valid\\}",
                                               replacement = "N"),
           glue_formula = stringr::str_replace(glue_formula,
                                               pattern = "\\{pct\\}",
                                               replacement = "%"),
           glue_formula = stringr::str_replace(glue_formula,
                                               pattern = "\\{p25\\}\\{range_sep\\}\\{p75\\}", 
                                               replacement = "IQR"),
           glue_formula = stringr::str_replace(glue_formula,
                                               pattern = "\\{range_sep\\}", 
                                               replacement = range_sep))
  
  
  return(pretty_t1)
  
  
}




#### Handle NAs --------------------------------

get_miss_no_strata <- function(t1,
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
      mutate(n = dplyr::coalesce(n, n_strata),
             missing = dplyr::if_else(var_type == "continuous",
                                      missing,
                                      n_strata - n_strata_valid)) |>
      dplyr::filter(missing > 0) |>
      dplyr::pull(var)
    
    miss_tab <- t1 |>
      dplyr::filter(var %in% any_miss) |>
      dplyr::select(var,
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
      dplyr::distinct(var,
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
      dplyr::select(var,
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
      dplyr::distinct(var,
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
      mutate(glue_formula2 = glue::glue(glue_formula)) |>
      mutate(level = missing_text) |>
      dplyr::select(var, level, glue_formula2)
    
  }
  
  return(miss_tab)
  
  
}
