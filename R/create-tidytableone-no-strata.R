#' Create a tidy "Table 1" without strata
#'
#' Produce the same schema as [create_tidytableone()] but with a single
#' overall group (every row has `strata = "Overall"`). No between‑group tests
#' are computed in this variant. This *legacy* function does **not** process
#' checkbox (multi‑response) blocks.
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
#' @return A tibble with the same columns as [create_tidytableone()], e.g.:
#'   `strata_var`, `strata`, `var`, `level`, wide continuous summaries
#'   (`n`, `mean`, `sd`, `p25`, `p50`, `p75`, …), tall categorical summaries
#'   (`n_level`, `pct`, `pct_valid`, …), testing columns present but `NA`
#'   (since there are no groups), and metadata (`class`, `var_type`, `label`).
#'
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
#' tab <- create_tidytableone(
#'   data = df,
#'   vars = c("age", "sex")
#' )
#' head(tab)
#'
#' # Continuous only
#' tab_num <- create_tidytableone(
#'   data = df,
#'   vars = "age"
#' )
#' head(tab_num)
#'
#' # Categorical only
#' tab_cat <- create_tidytableone(
#'   data = df,
#'   vars = "sex"
#' )
#' head(tab_cat)
#'
#' # (If available) present like a traditional Table 1
#' # adorn_tidytableone(tab)
#'
#' @name create_tidytableone_no_strata
# #' @export
#' @noRd
create_tidytableone_no_strata <- function(data,
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
    dplyr::select(var, class, var_type) |>
    dplyr::distinct()
  
  # Normalize variable types similar to .create_tidytableone_core()
  data[vars] <- lapply(vars, function(v) {
    x <- data[[v]]
    if (is.character(x)) {
      lv <- sort(unique(data[[v]][!is.na(data[[v]])]))
      factor(x, levels = lv)
    } else if (is.logical(x)) {
      factor(x, levels = c(FALSE, TRUE))
    } else if (is.ordered(x)) {
      factor(x, levels = levels(x), ordered = FALSE)
    } else {
      x
    }
  })
  
  cat_vars <- var_info |>
    dplyr::filter(var_type == "categorical") |>
    dplyr::pull(var) |>
    unique()
  
  con_vars <- var_info |>
    dplyr::filter(var_type == "continuous") |>
    dplyr::pull(var) |>
    unique()
  
  res_stats <- list()
  
  # continuous overall
  if (length(con_vars) > 0) {
    con_overall <- process_continuous(data, strata_sym = NULL, con_vars) |>
      dplyr::mutate(strata = "Overall")
    res_stats <- dplyr::bind_rows(res_stats, con_overall)
  }
  
  # categorical overall
  if (length(cat_vars) > 0) {
    cat_overall <- process_categorical(data, strata_sym = NULL, cat_vars) |>
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
  class_and_type <- var_info %>%
    dplyr::select(dplyr::any_of(c("var", "class", "var_type"))) %>%
    dplyr::distinct()
  
  res_stats <- res_stats |>
    dplyr::left_join(class_and_type, by = "var") |>
    dplyr::left_join(var_lbls,       by = "var")
  
  # Ensure columns for downstream uniformity
  if (!"level" %in% names(res_stats)) res_stats$level <- NA_character_
  
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
  
  level_map <- make_level_map_no_strata(
    data = data,
    vars = vars,
    cb_blocks = list(),
    show_any = FALSE
  )
  
  
  # --- Final, no-strata ordering (keep NA level with its own var, without relabeling)
  # res_stats <- order_within_vars_no_strata(res_stats, vars = vars, checkbox = checkbox)
  res_stats <- res_stats |>
    dplyr::mutate(var = factor(var, levels = vars)) |>
    dplyr::arrange(var)
  
  res_stats <- order_within_vars_no_strata(
    res_stats = res_stats,
    vars = vars,
    level_map = level_map
  )
  
  # uniform column order and strata_var = NA (no strata used)
  res_stats <- res_stats |>
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
  
  return(res_stats)
  
}

#' Create a tidy "Table 1" without strata, with checkbox (multi‑response) blocks
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
#'   - `show_any`: logical; include an `"Any selected"` row. Default `FALSE`.
#'   - `pvals`, `test`, `p_adjust`, `note`: kept for API symmetry; p‑values are
#'      not computed in no‑strata mode.
#' @param ... Reserved for future arguments; currently ignored.
#'
#' @return A tibble with the same schema as [create_tidytableone()], containing
#'   overall summaries for continuous and categorical variables and appended rows
#'   for each checkbox block (marked with `class == "checkbox"` and
#'   `var_type == "categorical"`). For checkbox rows, denominators and
#'   percentages obey `checkbox_opts$denom`.
#'
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
#' tab_cb <- create_tidytableone(
#'   data = dfc,
#'   vars = c("age","gender","race___1","race___2","race___3","race___98"),
#'   checkbox = cb,
#'   checkbox_opts = list(denom = "group", show_any = TRUE)
#' )
#' head(tab_cb)
#'
#' # Change denominator to "responders" for the checkbox block
#' tab_cb_resp <- create_tidytableone(
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
#' @name create_tidytableone_no_strata_checkbox
# #' @export
#' @noRd
create_tidytableone_no_strata_checkbox <- function(data,
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
  
  # labels & var info (this already contains the original checkbox columns)
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
    # IMPORTANT: cb_spec$var should be the original checkbox columns (race___1, ...)
    cb_vars   <- unique(cb_spec$var)
  }
  
  # regular variables (exclude the individual checkbox columns from standard cat path)
  cat_vars <- var_info |>
    dplyr::filter(var_type == "categorical") |>
    dplyr::pull(var) |>
    unique() |>
    setdiff(cb_vars)
  
  con_vars <- var_info |>
    dplyr::filter(var_type == "continuous") |>
    dplyr::pull(var) |>
    unique()
  
  res_stats <- list()
  
  if (length(con_vars) > 0) {
    res_stats <- dplyr::bind_rows(
      res_stats,
      process_continuous(data, strata_sym = NULL, con_vars) |>
        dplyr::mutate(strata = "Overall")
    )
  }
  
  if (length(cat_vars) > 0) {
    res_stats <- dplyr::bind_rows(
      res_stats,
      process_categorical(data, strata_sym = NULL, cat_vars) |>
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
  
  # test columns are NA (no between-group tests)
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
  
  # join class/type & labels (checkbox rows already carry class/var_type; coalesce after)
  class_and_type <- var_info %>%
    dplyr::select(dplyr::any_of(c("var", "class", "var_type"))) %>%
    dplyr::distinct()
  
  res_stats <- res_stats |>
    dplyr::left_join(class_and_type, by = "var") |>
    dplyr::left_join(var_lbls,       by = "var") |>
    safe_merge_cols("label",    c("label.x", "label.y", "label")) |>
    safe_merge_cols("var_type", c("var_type.x", "var_type.y", "var_type")) |>
    safe_merge_cols("class",    c("class.x", "class.y", "class"))
  
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
  
  res_stats <- .ensure_cols(
    res_stats,
    cols_types = list(
      strata      = "Overall",
      strata_var  = NA_character_,
      level       = NA_character_,
      # wide stats (continuous)
      n = NA_integer_, n_distinct = NA_integer_, complete = NA_integer_, missing = NA_integer_,
      mean = NA_real_, sd = NA_real_, p0 = NA_real_, p25 = NA_real_, p50 = NA_real_, p75 = NA_real_, p100 = NA_real_, cv = NA_real_,
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
  
  # Force var ordering to match input vars, with ___any_selected inside its block
  var_levels <- .make_var_levels_with_any(
    vars      = vars,
    cb_blocks = cb_blocks,
    show_any  = checkbox_opts$show_any %||% TRUE
  )
  
  level_map <- make_level_map_no_strata(
    data = data,
    vars = var_levels,
    cb_blocks = cb_blocks,
    show_any = checkbox_opts$show_any %||% TRUE
  )
  
  res_stats <- res_stats |>
    dplyr::mutate(var = factor(var, levels = var_levels)) |>
    dplyr::arrange(var)
  
  # Pass var_levels, not vars
  res_stats <- order_within_vars_no_strata(
    res_stats = res_stats,
    vars = var_levels,
    level_map = level_map
  )
  
  # final column order
  res_stats <- res_stats |>
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
  
  return(res_stats)
}

# ---- no‑strata helpers ------------------------------------------------

process_checkbox_blocks_overall <- function(data, blocks, opts) {
  
  opts  <- normalize_checkbox_opts(opts)
  opts  <- validate_checkbox_opts(opts)
  denom <- match.arg(opts$denom, c("group","nonmissing","responders"))
  
  out <- lapply(blocks, function(bl) {
    # raw cols & 0/1 selection matrix for this block
    raw_mat <- as.data.frame(data[bl$vars], stringsAsFactors = FALSE)
    sel_mat <- as.data.frame(lapply(bl$vars, function(v)
      as.integer(data[[v]] == bl$select_txt[[v]])))
    names(sel_mat) <- bl$vars
    
    # Numerators per level (by original column)
    n_level <- colSums(sel_mat, na.rm = TRUE)
    
    # Display denominator for pct
    # responders_N <- sum(rowSums(!is.na(raw_mat)) > 0L)
    any_selected <- rowSums(sel_mat, na.rm = TRUE) > 0L
    responders_N <- as.integer(sum(any_selected, na.rm = TRUE))
    
    display_N <- switch(
      denom,
      group      = nrow(data),
      responders = responders_N,
      nonmissing = nrow(data)  # display denom; "valid" handled per-level below
    )
    
    # Valid % pieces
    n_level_valid     <- n_level
    nonmiss_per_level <- colSums(!is.na(raw_mat), na.rm = TRUE)
    valid_denom <- switch(
      denom,
      group      = rep(display_N, length(n_level)),
      responders = rep(responders_N, length(n_level)),
      nonmissing = as.integer(nonmiss_per_level)
    )
    
    # Base name for synthetic "Any selected" var (e.g., "race")
    base_name <- sub("___.*$", "", bl$vars[1])
    # var_any   <- paste0(tolower(base_name), "___any_selected")
    var_any   <- paste0(base_name, "___any_selected")
    
    df <- tibble::tibble(
      strata   = "Overall",
      var      = names(n_level),                                    # <- original checkbox columns
      level    = unname(bl$labels[names(n_level)]),                 # printed row label (e.g., "White")
      n_level  = as.integer(n_level),
      n_strata = as.integer(display_N),
      pct      = dplyr::if_else(n_strata > 0L, n_level / n_strata, NA_real_),
      
      n_level_valid  = as.integer(n_level_valid),
      n_strata_valid = as.integer(valid_denom),
      pct_valid      = dplyr::if_else(n_strata_valid > 0L, n_level_valid / n_strata_valid, NA_real_),
      
      label    = bl$overall_lbl,                                    # <- block heading in label
      level_var = names(n_level)                                     # <- keep the original var for helpers
    )
    
    # Optional "Any selected"
    # any_count <- sum(rowSums(sel_mat, na.rm = TRUE) > 0L, na.rm = TRUE)
    any_count <- responders_N
    
    if (isTRUE(opts$show_any)) {
      any_nonmissing <- sum(rowSums(!is.na(raw_mat)) > 0L)
      
      any_display_denom <- switch(
        denom,
        group      = nrow(data),
        responders = responders_N,
        nonmissing = nrow(data)
      )
      
      any_valid_denom <- switch(
        denom,
        group      = any_display_denom,
        responders = responders_N,
        nonmissing = any_nonmissing
      )
      
      df <- dplyr::bind_rows(
        df,
        tibble::tibble(
          strata   = "Overall",
          var      = var_any, 
          level    = "Any selected",
          n_level  = as.integer(any_count),
          n_strata = as.integer(any_display_denom),
          pct      = ifelse(any_display_denom > 0, any_count / any_display_denom, NA_real_),
          
          n_level_valid  = as.integer(any_count),
          n_strata_valid = as.integer(any_valid_denom),
          pct_valid      = ifelse(any_valid_denom > 0, any_count / any_valid_denom, NA_real_),
          
          label    = bl$overall_lbl,
          level_var = NA_character_
        )
      )
    }
    
    df
  })
  
  dplyr::bind_rows(out) %>%
    dplyr::mutate(
      var_type = "categorical",
      class    = "checkbox"
    )
}

