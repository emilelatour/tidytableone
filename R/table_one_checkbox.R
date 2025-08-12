
#### Checkbox helpers -------------------------------- 


validate_checkbox_spec <- function(x, data, vars) {
  req_cols <- c("var","overall_lbl","checkbox_lbl","checkbox_txt")
  if (!all(req_cols %in% names(x))) {
    stop("`checkbox` must include columns: ", paste(req_cols, collapse = ", "), call. = FALSE)
  }
  x <- tibble::as_tibble(x)
  
  # All vars must exist in data AND be in the selected vars list
  missing_in_data <- setdiff(unique(x$var), names(data))
  if (length(missing_in_data)) {
    stop("Checkbox vars not found in data: ", paste(missing_in_data, collapse = ", "), call. = FALSE)
  }
  missing_in_vars <- setdiff(unique(x$var), vars)
  if (length(missing_in_vars)) {
    stop("Checkbox vars must also be present in `vars`: ", paste(missing_in_vars, collapse = ", "), call. = FALSE)
  }
  
  # one label per row; allow repeated overall_lbl to form blocks
  x$overall_lbl <- as.character(x$overall_lbl)
  x$checkbox_lbl <- as.character(x$checkbox_lbl)
  x$checkbox_txt <- as.character(x$checkbox_txt)
  
  # de-duplicate rows if user passed dupes
  dplyr::distinct(x, var, overall_lbl, checkbox_lbl, checkbox_txt)
}

prepare_checkbox_blocks <- function(spec) {
  sp <- split(spec, spec$overall_lbl)
  out <- lapply(names(sp), function(nm) {
    df <- sp[[nm]]
    list(
      overall_lbl = nm,
      vars        = df$var,
      labels      = stats::setNames(df$checkbox_lbl, df$var),
      select_txt  = stats::setNames(df$checkbox_txt, df$var)
    )
  })
  rlang::set_names(out, names(sp))
}

# Compute counts and percents per level; optional "Any selected" row
process_checkbox_blocks <- function(data, strata_var, blocks, opts) {
  # denom options: "group","nonmissing","responders"
  denom <- match.arg(opts$denom, c("group","nonmissing","responders"))
  
  out <- lapply(blocks, function(bl) {
    
    # Build 0/1 selection matrix for this block (NAME FIRST, then tibble)
    sel <- stats::setNames(
      lapply(bl$vars, function(v) as.integer(data[[v]] == bl$select_txt[[v]])),
      bl$vars
    )
    sel <- tibble::as_tibble(sel)
    names(sel) <- bl$vars
    
    # strata as character for joins/printing
    df <- tibble::tibble(.strata = as.character(data[[strata_var]])) |>
      dplyr::bind_cols(sel)
    
    # per-group denominators
    grp_n <- df |>
      dplyr::count(.strata, name = "group_n")
    
    any_selected <- as.integer(rowSums(sel, na.rm = TRUE) > 0L)
    
    denom_df <- switch(
      denom,
      group = grp_n,
      nonmissing = {
        # (placeholder: treat NAs as not selected; if you add true NA handling,
        # update this branch to compute a reduced denominator)
        grp_n
      },
      responders = {
        df |>
          dplyr::mutate(.any = any_selected) |>
          dplyr::group_by(.strata) |>
          dplyr::summarise(group_n = sum(.any, na.rm = TRUE), .groups = "drop")
      }
    )
    
    # Per-strata counts per level (selected == 1)
    long <- df |>
      tidyr::pivot_longer(cols = dplyr::all_of(bl$vars),
                          names_to = "level_var",
                          values_to = "selected") |>
      dplyr::group_by(.strata, level_var) |>
      dplyr::summarise(n_level = sum(selected, na.rm = TRUE), .groups = "drop") |>
      dplyr::left_join(denom_df, by = ".strata") |>
      dplyr::mutate(
        pct   = ifelse(group_n > 0, n_level / group_n, NA_real_),
        var   = bl$overall_lbl,
        level = bl$labels[level_var]
      )
    
    # Overall denominator
    overall_denom <- switch(
      denom,
      group = nrow(data),
      nonmissing = nrow(data),             # refine if adding true NA handling
      responders = sum(any_selected, na.rm = TRUE)
    )
    
    # Overall counts per level
    overall <- long |>
      dplyr::group_by(level_var) |>
      dplyr::summarise(n_level = sum(n_level, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(
        .strata = "Overall",
        group_n = overall_denom,
        pct     = ifelse(group_n > 0, n_level / group_n, NA_real_),
        var     = bl$overall_lbl,
        level   = bl$labels[level_var]
      )
    
    # Optional "Any selected"
    any_row <- NULL
    if (isTRUE(opts$show_any)) {
      any_counts <- df |>
        dplyr::mutate(.any = any_selected) |>
        dplyr::group_by(.strata) |>
        dplyr::summarise(n_level = sum(.any, na.rm = TRUE), .groups = "drop") |>
        dplyr::left_join(denom_df, by = ".strata") |>
        dplyr::mutate(
          pct   = ifelse(group_n > 0, n_level / group_n, NA_real_),
          var   = bl$overall_lbl,
          level = "Any selected",
          level_var = NA_character_
        )
      
      any_overall <- tibble::tibble(
        .strata  = "Overall",
        n_level  = sum(any_selected, na.rm = TRUE),
        group_n  = overall_denom,
        pct      = ifelse(group_n > 0, n_level / group_n, NA_real_),
        var      = bl$overall_lbl,
        level    = "Any selected",
        level_var = NA_character_
      )
      
      any_row <- dplyr::bind_rows(any_counts, any_overall)
    }
    
    dplyr::bind_rows(long, overall, any_row)
  })
  
  out <- dplyr::bind_rows(out)
  
  # Shape to match rest of pipeline
  out |>
    dplyr::rename(n_strata = n_level) |>
    dplyr::mutate(
      n_level    = NA_integer_,     # computed later for overall style if needed
      label      = var,
      pct_valid  = pct,             # same as pct for checkbox rows
      chisq_test = NA_real_,
      chisq_test_no_correction = NA_real_,
      chisq_test_simulated = NA_real_,
      fisher_test = NA_real_,
      fisher_test_simulated = NA_real_,
      check_categorical_test = NA_character_
    )
}

# Add per-level Fisher/χ² p-values (2x2 selected vs not by strata)
add_pvalues_checkbox <- function(tab, data, strata_var, blocks, test, p_adjust = "none") {
  
  needs <- tab |>
    dplyr::filter(!is.na(.data$level_var)) |>
    dplyr::select(var, level, level_var) |>
    dplyr::distinct()
  
  p_res <- needs |>
    dplyr::rowwise() |>
    dplyr::mutate(p_raw = {
      vv <- level_var
      x <- tibble::tibble(
        sel   = as.integer(data[[vv]] == blocks[[var]]$select_txt[[vv]]),
        group = as.character(data[[strata_var]])
      )
      tt <- table(x$sel, x$group, useNA = "no")
      
      if (identical(test, "fisher")) {
        calc_fisher_test(tt)
      } else if (identical(test, "chisq")) {
        calc_chisq_test(tt)
      } else {
        ex <- tryCatch(suppressWarnings(stats::chisq.test(tt)$expected), error = function(e) NULL)
        if (!is.null(ex) && any(ex < 5)) calc_fisher_test(tt) else calc_chisq_test(tt)
      }
    }) |>
    dplyr::ungroup()
  
  if (!identical(p_adjust, "none")) {
    p_res <- p_res |>
      dplyr::group_by(var) |>
      dplyr::mutate(p_value = p.adjust(p_raw, method = p_adjust)) |>
      dplyr::ungroup()
  } else {
    p_res <- dplyr::mutate(p_res, p_value = p_raw)
  }
  
  tab |>
    dplyr::left_join(dplyr::select(p_res, var, level, p_value),
                     by = c("var","level")) |>
    dplyr::mutate(p_value_level = p_value)
}
