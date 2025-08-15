
#### Helper functions --------------------------------


# Process categorical variables
process_categorical <- function(data,
                                strata_sym,
                                cat_vars) {
  
  # Silence no visible binding for global variable
  dat <- res <- n_level_valid <- n_strata_valid <- NULL
  
  
  
  suppressWarnings(
    cat_strata <- tibble::tibble(var = cat_vars) |>
      mutate(dat = purrr::map(.x = var,
                              .f = ~ dplyr::select(data,
                                                   !! strata_sym,
                                                   dplyr::all_of(.x))),
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
  
}

# Process continuous variables
process_continuous <- function(data,
                               strata_sym,
                               con_vars) {
  
  con_strata <- data %>%
    dplyr::select(!! strata_sym,
                  dplyr::all_of(con_vars)) %>%
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
    dplyr::select(!! strata_sym, dplyr::all_of(con_vars)) %>%
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
  
  dplyr::bind_rows(con_overall,
                   con_strata)
  
}

# Safely coalesce a set of candidate columns into `target` (only if present),
# then drop the redundant source columns.
safe_merge_cols <- function(df, target, candidates) {
  present <- intersect(candidates, names(df))
  if (length(present) == 0L) return(df)
  
  # Build a list of the present columns as vectors
  vecs <- lapply(present, function(nm) df[[nm]])
  
  # If only one source, just copy it (unless it's already the target)
  if (length(vecs) == 1L) {
    src <- present[[1]]
    if (!identical(src, target)) df[[target]] <- vecs[[1]]
  } else {
    # Coalesce left-to-right
    df[[target]] <- Reduce(dplyr::coalesce, vecs)
  }
  
  # Drop the redundant source columns (keep the target)
  drop_cols <- setdiff(present, target)
  if (length(drop_cols)) df <- dplyr::select(df, -dplyr::all_of(drop_cols))
  
  df
}


# Arrange results
# helper: make sure columns exist with the right types before relocate()
.ensure_cols <- function(df, cols_types) {
  for (nm in names(cols_types)) {
    if (!nm %in% names(df)) {
      df[[nm]] <- cols_types[[nm]]
    }
  }
  df
}

arrange_results <- function(res_stats,
                            htest_res,
                            smd_res,
                            var_info,
                            var_lbls,
                            vars,
                            cat_vars,
                            con_vars,
                            data,
                            strata,
                            strata_sym) {

  # Silence no visible binding notes
  sort1 <- sort2 <- label <- strata_var <- NULL

  # Strata levels (Overall first)
  if (is.factor(purrr::pluck(data, strata))) {
    strata_lvls <- c("Overall", levels(purrr::pluck(data, strata)))
  } else {
    strata_lvls <- c("Overall", unique(purrr::pluck(data, strata)))
  }

  # ---- Join hypothesis tests (once), guarantee *_ht columns, coalesce into main
  res_stats <- res_stats %>%
    dplyr::left_join(htest_res, by = "var", suffix = c("", "_ht")) %>%
    .ensure_cols(cols_types = list(
      chisq_test_ht               = NA_real_,
      chisq_test_no_correction_ht = NA_real_,
      chisq_test_simulated_ht     = NA_real_,
      fisher_test_ht              = NA_real_,
      fisher_test_simulated_ht    = NA_real_
    )) %>%
    # also ensure the target columns exist *before* coalesce
    .ensure_cols(cols_types = list(
      chisq_test               = NA_real_,
      chisq_test_no_correction = NA_real_,
      chisq_test_simulated     = NA_real_,
      fisher_test              = NA_real_,
      fisher_test_simulated    = NA_real_
    )) %>%
    dplyr::mutate(
      chisq_test               = dplyr::coalesce(.data$chisq_test,               .data$chisq_test_ht),
      chisq_test_no_correction = dplyr::coalesce(.data$chisq_test_no_correction, .data$chisq_test_no_correction_ht),
      chisq_test_simulated     = dplyr::coalesce(.data$chisq_test_simulated,     .data$chisq_test_simulated_ht),
      fisher_test              = dplyr::coalesce(.data$fisher_test,              .data$fisher_test_ht),
      fisher_test_simulated    = dplyr::coalesce(.data$fisher_test_simulated,    .data$fisher_test_simulated_ht)
    ) %>%
    dplyr::select(-dplyr::ends_with("_ht")) %>%
    dplyr::rename("strata" = !!strata_sym) %>%
    dplyr::left_join(smd_res, by = "var") %>%
    dplyr::mutate(strata = factor(.data$strata, levels = strata_lvls))

  # ---- Add class/type + labels
  class_and_type <- var_info %>%
    dplyr::select(-level, -sort1, -sort2) %>%
    dplyr::distinct()

  res_stats <- res_stats %>%
    dplyr::left_join(class_and_type, by = "var") %>%
    dplyr::left_join(var_lbls, by = "var")

  for (nm in c("class", "var_type", "label")) {
    if (!nm %in% names(res_stats)) {
      res_stats[[nm]] <- if (nm == "label") NA_character_ else NA_character_
    }
  }

  # ---- Arrange with resilient sort2 fill (works even if join didn't supply sort2)
  if (length(cat_vars) > 0) {
    sort_vars <- var_info %>% dplyr::select(var, level, sort1, sort2)

    res_stats <- res_stats %>%
      dplyr::left_join(sort_vars, by = c("var", "level")) %>%
      dplyr::group_by(.data$var) %>%
      dplyr::mutate(
        # carry forward sort1 from any non‑missing row in the group
        sort1 = dplyr::coalesce(.data$sort1, dplyr::first(stats::na.omit(.data$sort1))),
        # safe base max (never warns)
        .base_max_num = max(c(-Inf, .data$sort2), na.rm = TRUE),
        .base_max_int = ifelse(is.finite(.base_max_num), as.integer(.base_max_num), 0L),
        # fill missing sort2s sequentially after the base
        sort2 = dplyr::if_else(
          is.na(.data$sort2),
          as.integer(.base_max_int + cumsum(is.na(.data$sort2))),
          .data$sort2
        ),
        # tie‑breakers: keep checkbox “any_selected” last; missing level last
        .is_missing_level = is.na(.data$level),
        .is_any_selected  = .data$class == "checkbox" & grepl("___any_selected$", .data$var, perl = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(.data$sort1, .data$sort2, .data$.is_any_selected, .data$.is_missing_level) %>%
      dplyr::mutate(
        var   = forcats::fct_inorder(.data$var),
        level = forcats::fct_inorder(.data$level)
      ) %>%
      dplyr::select(-sort1, -sort2, -.is_missing_level, -.is_any_selected,
                    -.base_max_num, -.base_max_int) %>%
      dplyr::relocate(class, var_type, label, .after = dplyr::everything())
  } else {
    sort_vars <- var_info %>% dplyr::select(var, sort1, sort2)

    res_stats <- res_stats %>%
      dplyr::left_join(sort_vars, by = "var") %>%
      dplyr::group_by(.data$var) %>%
      dplyr::mutate(
        sort1 = dplyr::coalesce(.data$sort1, dplyr::first(stats::na.omit(.data$sort1))),
        .base_max_num = max(c(-Inf, .data$sort2), na.rm = TRUE),
        .base_max_int = ifelse(is.finite(.base_max_num), as.integer(.base_max_num), 0L),
        sort2 = dplyr::if_else(
          is.na(.data$sort2),
          as.integer(.base_max_int + cumsum(is.na(.data$sort2))),
          .data$sort2
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(.data$sort1, .data$sort2) %>%
      dplyr::mutate(var = forcats::fct_inorder(.data$var)) %>%
      dplyr::select(-sort1, -sort2, -.base_max_num, -.base_max_int) %>%
      dplyr::relocate(class, var_type, label, .after = dplyr::last_col())
  }

  # ---- Merge dup columns & drop helpers (keep p_value_level/level_var intact)
  res_stats <- res_stats %>%
    safe_merge_cols("label",    c("label.x", "label.y", "label")) %>%
    safe_merge_cols("var_type", c("var_type.x", "var_type.y", "var_type")) %>%
    safe_merge_cols("class",    c("class.x", "class.y", "class")) %>%
    dplyr::select(-dplyr::any_of(c(".strata", "group_n", "p_value")))

  # ---- Guarantee denom/percent columns BEFORE computing/coercing
  res_stats <- .ensure_cols(
    res_stats,
    cols_types = list(
      n = NA_integer_, n_distinct = NA_integer_, complete = NA_integer_, missing = NA_integer_,
      n_level = NA_integer_, n_strata = NA_integer_,
      n_level_valid = NA_integer_, n_strata_valid = NA_integer_,
      pct = NA_real_, pct_valid = NA_real_,
      mean = NA_real_, sd = NA_real_, p0 = NA_real_, p25 = NA_real_, p50 = NA_real_,
      p75 = NA_real_, p100 = NA_real_, cv = NA_real_
    )
  )

  # ---- Now safe to coerce & compute pct
  res_stats <- res_stats %>%
    dplyr::mutate(
      n_level        = as.integer(.data$n_level),
      n_strata       = as.integer(.data$n_strata),
      n_level_valid  = dplyr::coalesce(.data$n_level_valid,  .data$n_level),
      n_strata_valid = dplyr::coalesce(.data$n_strata_valid, .data$n_strata),
      pct            = dplyr::coalesce(.data$pct,
                                       dplyr::if_else(.data$n_strata > 0L,       .data$n_level       / .data$n_strata,       NA_real_)),
      pct_valid      = dplyr::coalesce(.data$pct_valid,
                                       dplyr::if_else(.data$n_strata_valid > 0L, .data$n_level_valid / .data$n_strata_valid, NA_real_))
    )

  # ---- Ensure ALL columns referenced by relocate() exist
  res_stats <- .ensure_cols(
    res_stats,
    cols_types = list(
      level = NA_character_, level_var = NA_character_, strata_var = NA_character_,
      chisq_test = NA_real_, chisq_test_no_correction = NA_real_, chisq_test_simulated = NA_real_,
      fisher_test = NA_real_, fisher_test_simulated = NA_real_, check_categorical_test = NA_character_,
      oneway_test_unequal_var = NA_real_, oneway_test_equal_var = NA_real_,
      kruskal_test = NA_real_, bartlett_test = NA_real_, levene_test = NA_real_,
      smd = NA_real_
    )
  )

  # ---- Final column order
  res_stats <- res_stats %>%
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
    ) %>%
    dplyr::relocate(class, var_type, label, .after = dplyr::last_col())

  # Fill strata var name
  res_stats %>%
    dplyr::mutate(strata_var = rlang::quo_name(strata_sym)) %>%
    dplyr::relocate(strata_var, .before = dplyr::everything())
}


# Extracts the label attribute from a variable, if it exists.
# If the label is not found, returns NA as a character string.
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


# Processes a categorical variable across strata, creating a count of each level within the strata.
# The function calculates the total counts and valid counts (non-missing) within each stratum,
# ensuring that all levels are included even if some are missing in certain strata.
# The result is a tibble with counts per level and strata, including handling for missing values.
do_one_cat_strata <- function(x, strata_sym) {
  
  # Silence no visible binding for global variable
  n_level_valid <- NULL
  
  if (ncol(x) <= 1L) {
    nm <- rlang::as_name(strata_sym)
    empty <- setNames(list(character()), nm)
    return(tibble::as_tibble(empty) |>
             dplyr::mutate(
               var = factor(),
               level = factor(),
               n_level = integer(),
               n_strata = integer(),
               n_level_valid = integer(),
               n_strata_valid = integer()
             ))
  }
  
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




