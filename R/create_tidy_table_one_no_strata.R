#' Create a tidy “Table 1” without strata
#'
#' Produce the same schema as [create_tidy_table_one()] but with a single
#' overall group (every row has `strata = "Overall"`). No between‑group tests
#' are computed in this variant. This *legacy* function does **not** process
#' checkbox (multi‑response) blocks — see
#' [create_tidy_table_one_no_strata_checkbox()] for that.
#'
#' @param data A data frame with the variables to summarise.
#' @param vars Character vector of column names to include. If omitted,
#'   all columns of `data` are considered.
#' @param na_level Character label for missing values in categorical
#'   summaries. Default is `"(Missing)"`.
#' @param b_replicates Integer; number of bootstrap replicates for select
#'   diagnostics (reserved for future use). Default `2000`.
#' @param ... Reserved for future arguments; currently ignored.
#' @param checkbox Must be `NULL` for this legacy variant (checkbox blocks are
#'   not handled here).
#' @param checkbox_opts Ignored (present for API symmetry).
#'
#' @return A tibble with the same columns as [create_tidy_table_one()], e.g.:
#'   `strata_var`, `strata`, `var`, `level`, wide continuous summaries
#'   (`n`, `mean`, `sd`, `p25`, `p50`, `p75`, …), tall categorical summaries
#'   (`n_level`, `pct`, `pct_valid`, …), testing columns present but `NA`
#'   (since there are no groups), and metadata (`class`, `var_type`, `label`).
#'
#' @seealso [create_tidy_table_one()], [create_tidy_table_one_no_strata_checkbox()],
#'   [adorn_tidytableone()]
#'
#' @examples
#' # Minimal example with one continuous and one categorical variable
#' set.seed(2)
#' df <- data.frame(
#'   age  = round(rnorm(100, mean = 50, sd = 10), 1),
#'   sex  = sample(c("Female", "Male"), size = 100, replace = TRUE),
#'   misc = rnorm(100)  # ignored below
#' )
#'
#' tab <- create_tidy_table_one_no_strata(
#'   data = df,
#'   vars = c("age", "sex")
#' )
#' head(tab)
#'
#' # Continuous only
#' tab_num <- create_tidy_table_one_no_strata(
#'   data = df,
#'   vars = "age"
#' )
#' head(tab_num)
#'
#' # Categorical only
#' tab_cat <- create_tidy_table_one_no_strata(
#'   data = df,
#'   vars = "sex"
#' )
#' head(tab_cat)
#'
#' # (If available) present like a traditional Table 1
#' # adorn_tidytableone(tab)
#'
#' @name create_tidy_table_one_no_strata
#' @export
create_tidy_table_one_no_strata <- function(data,
                                            vars,
                                            na_level = "(Missing)",
                                            b_replicates = 2000,
                                            ...,
                                            checkbox = NULL,
                                            checkbox_opts = NULL) {
  
  stopifnot(is.null(checkbox))  # legacy: no checkbox handling here
  
  # Use all variables if vars is not provided
  if (missing(vars)) vars <- names(data)
  
  # labels & var info
  var_lbls <- tibble::tibble(var = names(data)) |>
    dplyr::mutate(label = purrr::map_chr(data[, var], ~ get_var_labels(.x)))
  
  var_info <- get_var_info(data = data, .vars = vars) |>
    dplyr::mutate(
      sort1 = cumsum(var != dplyr::lag(var, default = dplyr::first(var))) + 1L
    ) |>
    dplyr::group_by(var) |>
    dplyr::mutate(sort2 = dplyr::row_number()) |>
    dplyr::ungroup()
  
  cat_vars <- var_info |>
    dplyr::filter(.data$var_type == "categorical") |>
    dplyr::pull(var) |>
    unique()
  
  con_vars <- var_info |>
    dplyr::filter(.data$var_type == "continuous") |>
    dplyr::pull(var) |>
    unique()
  
  res_stats <- list()
  
  # continuous overall
  if (length(con_vars) > 0) {
    con_overall <- process_continuous_nostrata(data, con_vars) |>
      dplyr::mutate(strata = "Overall")
    res_stats <- dplyr::bind_rows(res_stats, con_overall)
  }
  
  # categorical overall
  if (length(cat_vars) > 0) {
    cat_overall <- process_categorical_nostrata(data, cat_vars) |>
      dplyr::mutate(strata = "Overall")
    res_stats <- dplyr::bind_rows(res_stats, cat_overall)
  }
  
  # add NA test columns (no between‑group tests when no strata)
  res_stats <- tibble::as_tibble(res_stats) |>
    dplyr::mutate(
      chisq_test = NA_real_,
      chisq_test_no_correction = NA_real_,
      chisq_test_simulated = NA_real_,
      fisher_test = NA_real_,
      fisher_test_simulated = NA_real_,
      check_categorical_test = NA_character_,
      oneway_test_unequal_var = NA_real_,
      oneway_test_equal_var = NA_real_,
      kruskal_test = NA_real_,
      bartlett_test = NA_real_,
      levene_test = NA_real_,
      smd = NA_real_
    )
  
  # join class/type & labels, order rows
  class_and_type <- var_info |>
    dplyr::select(-level, -sort1, -sort2) |>
    dplyr::distinct()
  
  res_stats <- res_stats |>
    dplyr::left_join(class_and_type, by = "var") |>
    dplyr::left_join(var_lbls,       by = "var")
  
  # Ensure columns for downstream uniformity
  if (!"level" %in% names(res_stats)) res_stats$level <- NA_character_
  
  # ordering
  if ("level" %in% names(var_info)) {
    sort_vars <- var_info |>
      dplyr::select(var, level, sort1, sort2)
    res_stats <- res_stats |>
      dplyr::left_join(sort_vars, by = c("var","level")) |>
      dplyr::arrange(.data$sort1, .data$sort2) |>
      dplyr::select(-sort1, -sort2)
  } else {
    sort_vars <- var_info |>
      dplyr::select(var, sort1, sort2)
    res_stats <- res_stats |>
      dplyr::left_join(sort_vars, by = "var") |>
      dplyr::arrange(.data$sort1, .data$sort2) |>
      dplyr::select(-sort1, -sort2)
  }
  
  res_stats <- .ensure_cols(
    res_stats,
    cols_types = list(
      strata      = "Overall",
      strata_var  = NA_character_,
      level       = NA_character_,
      # wide stats (continuous)
      n = NA_integer_, n_distinct = NA_integer_, complete = NA_integer_, missing = NA_integer_,
      mean = NA_real_, sd = NA_real_, p0 = NA_real_, p25 = NA_real_, p50 = NA_real_,
      p75 = NA_real_, p100 = NA_real_, cv = NA_real_,
      # categorical tall stats
      n_level = NA_integer_, n_strata = NA_integer_, n_level_valid = NA_integer_, n_strata_valid = NA_integer_,
      pct = NA_real_, pct_valid = NA_real_,
      # tests (kept for schema symmetry)
      chisq_test = NA_real_, chisq_test_no_correction = NA_real_, chisq_test_simulated = NA_real_,
      fisher_test = NA_real_, fisher_test_simulated = NA_real_, check_categorical_test = NA_character_,
      oneway_test_unequal_var = NA_real_, oneway_test_equal_var = NA_real_,
      kruskal_test = NA_real_, bartlett_test = NA_real_, levene_test = NA_real_,
      smd = NA_real_,
      # meta
      class = NA_character_, var_type = NA_character_, label = NA_character_
    )
  )
  
  # uniform column order and strata_var = NA (no strata used)
  res_stats |>
    dplyr::mutate(strata = factor(strata, levels = "Overall"),
                  strata_var = NA_character_) |>
    dplyr::relocate(
      strata_var, strata, var, level,
      n, n_distinct, complete, missing,
      n_level, n_strata, n_level_valid, n_strata_valid,
      mean, sd, p0, p25, p50, p75, p100, cv,
      pct, pct_valid,
      chisq_test, chisq_test_no_correction, chisq_test_simulated,
      fisher_test, fisher_test_simulated, check_categorical_test,
      oneway_test_unequal_var, oneway_test_equal_var,
      kruskal_test, bartlett_test, levene_test, smd,
      .before = dplyr::everything()
    ) |>
    dplyr::relocate(class, var_type, label, .after = dplyr::last_col())
}

#' Create a tidy “Table 1” without strata, with checkbox (multi‑response) blocks
#'
#' Summarise variables with a single overall group (`strata = "Overall"`) and
#' add multi‑response "checkbox" blocks (e.g., multiple race selections).
#' Between‑group tests are not computed in this variant (testing columns are
#' included but set to `NA` for schema compatibility).
#'
#' @param data A data frame with the variables to summarise.
#' @param vars Character vector of column names to include. If omitted,
#'   all columns of `data` are considered.
#' @param na_level Character label for missing values in categorical
#'   summaries. Default is `"(Missing)"`.
#' @param b_replicates Integer; number of bootstrap replicates for select
#'   diagnostics (reserved for future use). Default `2000`.
#' @param checkbox A data frame (or tibble) specifying checkbox blocks with
#'   **four columns**:
#'   - `var`: the *shared* conceptual name of the block (e.g., `"race"`).
#'   - `overall_lbl`: label to display for the block in the `var` column
#'      (e.g., `"Race"`).
#'   - `checkbox_lbl`: the label for each checkbox level (e.g., `"White"`).
#'   - `checkbox_txt`: the value in the data that indicates the box is checked
#'      (often `"Checked"`).
#'
#'   Each row corresponds to one checkbox column in `data` (e.g., `race___1`,
#'   `race___2`, …) and must appear in `vars`.
#'
#' @param checkbox_opts A named list of options for checkbox blocks:
#'   - `denom`: character, one of `"group"`, `"nonmissing"`, `"responders"`.
#'      Controls the denominator used for percentages. In no‑strata mode,
#'      `"group"` and `"nonmissing"` both use `nrow(data)`; `"responders"`
#'      uses the number with any selection in the block.
#'   - `show_any`: logical; include an `"Any selected"` row. Default `TRUE`.
#'   - `pvals`, `test`, `p_adjust`, `note`: kept for API symmetry; p‑values are
#'      not computed in no‑strata mode.
#' @param ... Reserved for future arguments; currently ignored.
#'
#' @return A tibble with the same schema as [create_tidy_table_one()], containing
#'   overall summaries for continuous and categorical variables and appended rows
#'   for each checkbox block (marked with `class == "checkbox"` and
#'   `var_type == "categorical"`). For checkbox rows, denominators and
#'   percentages obey `checkbox_opts$denom`.
#'
#' @seealso [create_tidy_table_one_no_strata()], [create_tidy_table_one()],
#'   [adorn_tidytableone()]
#'
#' @examples
#' # Small no‑strata checkbox example
#' set.seed(2)
#' dfc <- data.frame(
#'   age = round(rnorm(40, 50, 12), 1),
#'   gender = sample(c("Female", "Male"), 40, TRUE),
#'   race___1 = sample(c("Checked", "Unchecked"), 40, TRUE, prob = c(0.6, 0.4)), # White
#'   race___2 = sample(c("Checked", "Unchecked"), 40, TRUE, prob = c(0.2, 0.8)), # Black
#'   race___3 = sample(c("Checked", "Unchecked"), 40, TRUE, prob = c(0.1, 0.9)), # AI/AN
#'   race___98 = sample(c("Checked", "Unchecked"), 40, TRUE, prob = c(0.05, 0.95)) # PNA
#' )
#'
#' cb <- data.frame(
#'   var          = c("race___1","race___2","race___3","race___98"),
#'   overall_lbl  = "Race",
#'   checkbox_lbl = c("White", "Black or African-American",
#'                    "American Indian or Alaska Native", "Prefer not to answer"),
#'   checkbox_txt = "Checked",
#'   stringsAsFactors = FALSE
#' )
#'
#' tab_cb <- create_tidy_table_one_no_strata_checkbox(
#'   data = dfc,
#'   vars = c("age","gender","race___1","race___2","race___3","race___98"),
#'   checkbox = cb,
#'   checkbox_opts = list(denom = "group", show_any = TRUE)
#' )
#' head(tab_cb)
#'
#' # Change denominator to "responders" for the checkbox block
#' tab_cb_resp <- create_tidy_table_one_no_strata_checkbox(
#'   data = dfc,
#'   vars = c("age","gender","race___1","race___2","race___3","race___98"),
#'   checkbox = cb,
#'   checkbox_opts = list(denom = "responders", show_any = TRUE)
#' )
#' head(tab_cb_resp)
#'
#' # (If available) present like a traditional Table 1
#' # adorn_tidytableone(tab_cb)
#'
#' @name create_tidy_table_one_no_strata_checkbox
#' @export
create_tidy_table_one_no_strata_checkbox <- function(data,
                                                     vars,
                                                     na_level = "(Missing)",
                                                     b_replicates = 2000,
                                                     checkbox = NULL,
                                                     checkbox_opts = list(
                                                       denom   = "group",
                                                       pvals   = "per_level",  # ignored (no groups)
                                                       test    = "auto",       # ignored (no groups)
                                                       p_adjust = "none",
                                                       show_any = TRUE,
                                                       note     = "Participants could select more than one option; percentages may exceed 100%."
                                                     ),
                                                     ...) {
  # Use all variables if vars is not provided
  if (missing(vars)) vars <- names(data)
  
  # labels & var info
  var_lbls <- tibble::tibble(var = names(data)) |>
    dplyr::mutate(label = purrr::map_chr(data[, var], ~ get_var_labels(.x)))
  
  var_info <- get_var_info(data = data, .vars = vars) |>
    dplyr::mutate(
      sort1 = cumsum(var != dplyr::lag(var, default = dplyr::first(var))) + 1L
    ) |>
    dplyr::group_by(var) |>
    dplyr::mutate(sort2 = dplyr::row_number()) |>
    dplyr::ungroup()
  
  # checkbox spec (optional)
  cb_blocks <- list()
  cb_vars   <- character(0)
  if (!is.null(checkbox)) {
    cb_spec   <- validate_checkbox_spec(checkbox, data = data, vars = vars)
    cb_blocks <- prepare_checkbox_blocks(cb_spec)
    cb_vars   <- unique(cb_spec$var)
    # add conceptual checkbox "overall rows" to var_info so ordering works
    cb_vi <- tibble::tibble(
      var = names(cb_blocks),
      level = NA_character_,
      var_type = "categorical",
      class = "checkbox",
      sort1 = match(var, vars),
      sort2 = 0L
    )
    var_info <- dplyr::bind_rows(var_info, cb_vi)
  }
  
  # regular variables (exclude the individual checkbox columns from standard cat path)
  cat_vars <- var_info |>
    dplyr::filter(.data$var_type == "categorical",
                  is.na(.data$class) | .data$class != "checkbox") |>
    dplyr::pull(var) |>
    unique() |>
    setdiff(cb_vars)
  
  con_vars <- var_info |>
    dplyr::filter(.data$var_type == "continuous") |>
    dplyr::pull(var) |>
    unique()
  
  res_stats <- list()
  
  if (length(con_vars) > 0) {
    res_stats <- dplyr::bind_rows(res_stats,
                                  process_continuous_nostrata(data, con_vars) |>
                                    dplyr::mutate(strata = "Overall")
    )
  }
  
  if (length(cat_vars) > 0) {
    res_stats <- dplyr::bind_rows(res_stats,
                                  process_categorical_nostrata(data, cat_vars) |>
                                    dplyr::mutate(strata = "Overall")
    )
  }
  
  # checkbox block rows (overall only)
  if (length(cb_blocks) > 0) {
    res_cb <- process_checkbox_blocks_overall(
      data   = data,
      blocks = cb_blocks,
      opts   = checkbox_opts
    ) |>
      dplyr::mutate(strata = "Overall")  # class/var_type already set by helper
    
    res_stats <- dplyr::bind_rows(res_stats, res_cb)
    
    # keep footnote attrs like the strata’d path
    attr(res_stats, "checkbox_blocks") <- cb_blocks
    attr(res_stats, "checkbox_opts")   <- checkbox_opts
  }
  
  res_stats <- tibble::as_tibble(res_stats)
  
  # test columns are NA (no between‑group tests)
  res_stats <- res_stats |>
    dplyr::mutate(
      chisq_test = NA_real_,
      chisq_test_no_correction = NA_real_,
      chisq_test_simulated = NA_real_,
      fisher_test = NA_real_,
      fisher_test_simulated = NA_real_,
      check_categorical_test = NA_character_,
      oneway_test_unequal_var = NA_real_,
      oneway_test_equal_var = NA_real_,
      kruskal_test = NA_real_,
      bartlett_test = NA_real_,
      levene_test = NA_real_,
      smd = NA_real_
    )
  
  # join class/type & labels
  class_and_type <- var_info |>
    dplyr::select(-level, -sort1, -sort2) |>
    dplyr::distinct()
  
  res_stats <- res_stats |>
    dplyr::left_join(class_and_type, by = "var") |>
    dplyr::left_join(var_lbls,       by = "var")
  
  # coalesce any duplicate columns produced by the joins
  res_stats <- res_stats |>
    safe_merge_cols("label",    c("label.x", "label.y", "label")) |>
    safe_merge_cols("var_type", c("var_type.x", "var_type.y", "var_type")) |>
    safe_merge_cols("class",    c("class.x", "class.y", "class"))
  
  # --- normalize checkbox denominators/pct for no‑strata ---
  if (length(cb_blocks) > 0) {
    denom <- match.arg(checkbox_opts$denom, c("group","nonmissing","responders"))
    
    if (identical(denom, "responders")) {
      # responders: per block, number of rows with any selection in that block
      resp_n <- vapply(names(cb_blocks), function(nm) {
        bl <- cb_blocks[[nm]]
        sel_mat <- as.data.frame(lapply(bl$vars, function(v)
          as.integer(data[[v]] == bl$select_txt[[v]])))
        sum(rowSums(sel_mat, na.rm = TRUE) > 0L, na.rm = TRUE)
      }, integer(1))
      denom_map <- tibble::tibble(var = names(cb_blocks), .denom = as.integer(resp_n))
    } else {
      # group/nonmissing currently both use total N (adjust here if you implement true nonmissing)
      denom_map <- tibble::tibble(var = names(cb_blocks), .denom = nrow(data))
    }
    
    res_stats <- res_stats |>
      dplyr::left_join(denom_map, by = "var") |>
      dplyr::mutate(
        n_strata  = dplyr::if_else(.data$class == "checkbox" & !is.na(.denom), .denom, .data$n_strata),
        pct       = dplyr::if_else(.data$class == "checkbox" & !is.na(.denom) & .data$n_strata > 0,
                                   .data$n_level / .data$n_strata, .data$pct),
        pct_valid = dplyr::if_else(.data$class == "checkbox" & !is.na(.denom) & .data$n_strata > 0,
                                   .data$n_level / .data$n_strata, .data$pct_valid)
      ) |>
      dplyr::select(-dplyr::any_of(".denom"))
  }
  
  # guarantee metadata cols exist for relocate()
  res_stats <- .ensure_cols(
    res_stats,
    cols_types = list(
      class    = NA_character_,
      var_type = NA_character_,
      label    = NA_character_
    )
  )
  
  if (!"level" %in% names(res_stats)) res_stats$level <- NA_character_
  
  # order to match var_info
  if ("level" %in% names(var_info)) {
    sort_vars <- var_info |>
      dplyr::select(var, level, sort1, sort2)
    res_stats <- res_stats |>
      dplyr::left_join(sort_vars, by = c("var","level")) |>
      dplyr::arrange(.data$sort1, .data$sort2) |>
      dplyr::select(-sort1, -sort2)
  } else {
    sort_vars <- var_info |>
      dplyr::select(var, sort1, sort2)
    res_stats <- res_stats |>
      dplyr::left_join(sort_vars, by = "var") |>
      dplyr::arrange(.data$sort1, .data$sort2) |>
      dplyr::select(-sort1, -sort2)
  }
  
  
  res_stats <- .ensure_cols(
    res_stats,
    cols_types = list(
      strata      = "Overall",
      strata_var  = NA_character_,
      level       = NA_character_,
      # wide stats (continuous)
      n = NA_integer_, n_distinct = NA_integer_, complete = NA_integer_, missing = NA_integer_,
      mean = NA_real_, sd = NA_real_, p0 = NA_real_, p25 = NA_real_, p50 = NA_real_,
      p75 = NA_real_, p100 = NA_real_, cv = NA_real_,
      # categorical tall stats
      n_level = NA_integer_, n_strata = NA_integer_, n_level_valid = NA_integer_, n_strata_valid = NA_integer_,
      pct = NA_real_, pct_valid = NA_real_,
      # tests (kept for schema symmetry)
      chisq_test = NA_real_, chisq_test_no_correction = NA_real_, chisq_test_simulated = NA_real_,
      fisher_test = NA_real_, fisher_test_simulated = NA_real_, check_categorical_test = NA_character_,
      oneway_test_unequal_var = NA_real_, oneway_test_equal_var = NA_real_,
      kruskal_test = NA_real_, bartlett_test = NA_real_, levene_test = NA_real_,
      smd = NA_real_,
      # meta
      class = NA_character_, var_type = NA_character_, label = NA_character_
    )
  )
  
  # final column order
  res_stats |>
    dplyr::mutate(strata = factor("Overall", levels = "Overall"),
                  strata_var = NA_character_) |>
    dplyr::relocate(
      strata_var, strata, var, level,
      n, n_distinct, complete, missing,
      n_level, n_strata, n_level_valid, n_strata_valid,
      mean, sd, p0, p25, p50, p75, p100, cv,
      pct, pct_valid,
      chisq_test, chisq_test_no_correction, chisq_test_simulated,
      fisher_test, fisher_test_simulated, check_categorical_test,
      oneway_test_unequal_var, oneway_test_equal_var,
      kruskal_test, bartlett_test, levene_test, smd,
      .before = dplyr::everything()
    ) |>
    dplyr::relocate(class, var_type, label, .after = dplyr::last_col())
}

# ---- no‑strata helpers ------------------------------------------------

process_continuous_nostrata <- function(data, con_vars) {
  data |>
    dplyr::select(dplyr::all_of(con_vars)) |>
    tidyr::pivot_longer(dplyr::everything(),
                        names_to = "var", values_to = "value") |>
    dplyr::group_by(.data$var) |>
    dplyr::summarise(
      n = dplyr::n(),
      n_distinct = dplyr::n_distinct(.data$value),
      complete = sum(!is.na(.data$value)),
      missing = sum(is.na(.data$value)),
      mean = mean(.data$value, na.rm = TRUE),
      sd = stats::sd(.data$value, na.rm = TRUE),
      p0 = custom_min(.data$value, na.rm = TRUE),
      p25 = stats::quantile(.data$value, 0.25, na.rm = TRUE),
      p50 = stats::quantile(.data$value, 0.50, na.rm = TRUE),
      p75 = stats::quantile(.data$value, 0.75, na.rm = TRUE),
      p100 = custom_max(.data$value, na.rm = TRUE),
      cv = sd / mean,
      shapiro_test = calc_shapiro_test(.data$value),
      ks_test = calc_ks_test(.data$value),
      ad_test = calc_ad_test(.data$value),
      .groups = "drop"
    )
}

process_categorical_nostrata <- function(data, cat_vars) {
  data |>
    dplyr::mutate(dplyr::across(dplyr::all_of(cat_vars), ~ as.character(.))) |>
    dplyr::select(dplyr::all_of(cat_vars)) |>
    tidyr::pivot_longer(dplyr::everything(),
                        names_to = "var", values_to = "level") |>
    dplyr::count(.data$var, .data$level, name = "n_level", .drop = FALSE) |>
    dplyr::group_by(.data$var) |>
    dplyr::mutate(
      n_strata = sum(.data$n_level, na.rm = TRUE),
      n_level_valid = dplyr::if_else(is.na(.data$level), NA_integer_, .data$n_level),
      n_strata_valid = sum(.data$n_level_valid, na.rm = TRUE),
      pct = .data$n_level / .data$n_strata,
      pct_valid = .data$n_level_valid / .data$n_strata_valid
    ) |>
    dplyr::ungroup()
}

# overall‑only checkbox rows
process_checkbox_blocks_nostrata <- function(data, blocks, opts) {
  denom <- match.arg(opts$denom, c("group","nonmissing","responders"))
  
  out <- lapply(blocks, function(bl) {
    sel <- stats::setNames(
      lapply(bl$vars, function(v) as.integer(data[[v]] == bl$select_txt[[v]])),
      bl$vars
    )
    sel <- tibble::as_tibble(sel)
    
    any_selected <- as.integer(rowSums(sel, na.rm = TRUE) > 0L)
    
    overall_denom <- switch(
      denom,
      group = nrow(data),
      nonmissing = nrow(data),         # adjust if you implement true NA exclusion
      responders = sum(any_selected, na.rm = TRUE)
    )
    
    # counts per level overall
    long <- sel |>
      tidyr::pivot_longer(dplyr::everything(),
                          names_to = "level_var",
                          values_to = "selected") |>
      dplyr::group_by(.data$level_var) |>
      dplyr::summarise(n_level = sum(.data$selected, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(
        group_n = overall_denom,
        pct     = ifelse(group_n > 0, n_level / group_n, NA_real_),
        var     = bl$overall_lbl,
        level   = bl$labels[level_var]
      )
    
    # Optional "Any selected"
    any_row <- NULL
    if (isTRUE(opts$show_any)) {
      any_row <- tibble::tibble(
        n_level = sum(any_selected, na.rm = TRUE),
        group_n = overall_denom,
        pct     = ifelse(group_n > 0, n_level / group_n, NA_real_),
        var     = bl$overall_lbl,
        level   = "Any selected",
        level_var = NA_character_
      )
    }
    
    dplyr::bind_rows(long, any_row)
  })
  
  out <- dplyr::bind_rows(out)
  
  # conform to standard columns
  out |>
    dplyr::transmute(
      var, level,
      n_strata = n_level,
      n_level = NA_integer_,
      n_level_valid = NA_integer_,
      n_strata_valid = NA_integer_,
      pct, pct_valid = pct,
      label = var
    )
}

process_checkbox_blocks_overall <- function(data, blocks, opts) {
  denom <- match.arg(opts$denom, c("group","nonmissing","responders"))
  
  overall_N <- switch(
    denom,
    group       = nrow(data),
    nonmissing  = nrow(data),                    # refine if you later track true NAs
    responders  = {
      sel_any <- sapply(blocks, function(bl) {
        rowSums(as.data.frame(lapply(bl$vars, function(v) as.integer(data[[v]] == bl$select_txt[[v]]))), na.rm = TRUE) > 0L
      })
      # if multiple blocks, “responders” is per block; we’ll set per-block below anyway
      NULL
    }
  )
  
  out <- lapply(blocks, function(bl) {
    # 0/1 matrix for this block
    sel_mat <- as.data.frame(lapply(bl$vars, function(v) as.integer(data[[v]] == bl$select_txt[[v]])))
    names(sel_mat) <- bl$vars
    
    n_level <- colSums(sel_mat, na.rm = TRUE)
    df <- tibble::tibble(
      strata = "Overall",
      var    = bl$overall_lbl,
      level  = unname(bl$labels[names(n_level)]),
      n_level = as.integer(n_level)
    )
    
    # denominator for this block (responders can differ by block)
    block_N <- if (identical(denom, "responders")) {
      sum(rowSums(sel_mat, na.rm = TRUE) > 0L, na.rm = TRUE)
    } else {
      overall_N
    }
    
    df$pct       <- ifelse(block_N > 0, df$n_level / block_N, NA_real_)
    df$n_strata  <- block_N
    df$pct_valid <- df$pct
    
    # optional “Any selected”
    any_row <- NULL
    if (isTRUE(opts$show_any)) {
      any_count <- sum(rowSums(sel_mat, na.rm = TRUE) > 0L, na.rm = TRUE)
      any_row <- tibble::tibble(
        strata   = "Overall",
        var      = bl$overall_lbl,
        level    = "Any selected",
        n_level  = as.integer(any_count),
        n_strata = block_N,
        pct      = ifelse(block_N > 0, any_count / block_N, NA_real_),
        pct_valid = ifelse(block_N > 0, any_count / block_N, NA_real_)
      )
    }
    
    dplyr::bind_rows(df, any_row)
  })
  
  dplyr::bind_rows(out) |>
    dplyr::mutate(
      var_type = "categorical",
      class    = "checkbox"
    )
}