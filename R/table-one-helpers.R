
#### Helper functions --------------------------------


# Process categorical variables.
# If `strata_sym` is NULL, returns a tall tibble (one row per var x level) with
# counts and percents based on the whole `data`. If `strata_sym` is supplied,
# returns the same shape but with an additional column (named after
# `strata_sym`) that takes the value of each stratum AND an explicit "Overall"
# aggregation.
process_categorical <- function(data,
                                strata_sym = NULL,
                                cat_vars) {
  
  # No-strata path: direct per-var level tabulation.
  if (is.null(strata_sym)) {
    return(dplyr::bind_rows(
      lapply(cat_vars, function(v) .categorical_summary_overall(data, v))
    ))
  }
  
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

# No-strata categorical summary for a single variable. NAs are excluded from
# the level set (i.e. no separate "Missing" row); the count of non-missing
# rows is exposed via `n_strata_valid`.
.categorical_summary_overall <- function(data, v) {
  x <- data[[v]]
  
  # Respect existing factor levels if present; otherwise build
  # levels from the observed values (sorted).
  if (is.factor(x)) {
    f   <- x
    lvl <- levels(f)
  } else {
    lvl <- sort(unique(x))
    f   <- factor(x, levels = lvl)
  }
  
  # Tabulate over the full level set
  n_level        <- as.integer(tabulate(as.integer(f), nbins = length(lvl)))
  n_strata       <- length(f)
  n_level_valid  <- n_level   # we do not include a separate "Missing" row
  n_strata_valid <- sum(!is.na(f))
  
  tibble::tibble(
    var            = v,
    # Keep level as a factor with the original ordering
    level          = factor(lvl, levels = lvl),
    n_level        = n_level,
    n_strata       = n_strata,
    n_level_valid  = n_level_valid,
    n_strata_valid = n_strata_valid,
    pct            = n_level / n_strata,
    pct_valid      = n_level_valid / n_strata_valid
  )
}

# Process continuous variables.
# If `strata_sym` is NULL, returns one row per var with summary stats over the
# whole `data`. If `strata_sym` is supplied, returns rows for every stratum
# plus an explicit "Overall" aggregation, in a single column named after
# `strata_sym`.
process_continuous <- function(data,
                               strata_sym = NULL,
                               con_vars) {
  
  has_strata <- !is.null(strata_sym)
  
  # Pivot to long form (with or without a strata column)
  long <- if (has_strata) {
    data %>%
      dplyr::select(!! strata_sym, dplyr::all_of(con_vars)) %>%
      tidyr::pivot_longer(cols = - !! strata_sym,
                          names_to = "var",
                          values_to = "value")
  } else {
    data %>%
      dplyr::select(dplyr::all_of(con_vars)) %>%
      tidyr::pivot_longer(dplyr::everything(),
                          names_to = "var",
                          values_to = "value")
  }
  
  # No-strata: one row per var, no Overall vs strata distinction
  if (!has_strata) {
    return(.continuous_summary(dplyr::group_by(long, var)))
  }
  
  # Per-stratum rows
  con_strata <- long |>
    dplyr::group_by(!! strata_sym, var) |>
    .continuous_summary() |>
    dplyr::mutate(!! strata_sym := as.character(!! strata_sym))
  
  # Overall rows (ignore strata; label as "Overall")
  con_overall <- long |>
    dplyr::group_by(var) |>
    .continuous_summary() |>
    dplyr::mutate(!! strata_sym := "Overall") |>
    dplyr::select(!! strata_sym, dplyr::everything())
  
  dplyr::bind_rows(con_overall, con_strata)
}

# Summarise continuous data within whatever groups are currently set on .data.
# Pulled out so process_continuous can use the same summarise block for both
# the per-stratum and overall passes.
.continuous_summary <- function(.data) {
  .data |>
    dplyr::summarise(
      n            = dplyr::n(),
      n_distinct   = dplyr::n_distinct(value),
      complete     = sum(!is.na(value)),
      missing      = sum(is.na(value)),
      mean         = mean(value, na.rm = TRUE),
      sd           = sd(value, na.rm = TRUE),
      p0           = custom_min(value, na.rm = TRUE),
      p25          = quantile(value, probs = 0.25, na.rm = TRUE),
      p50          = quantile(value, probs = 0.50, na.rm = TRUE),
      p75          = quantile(value, probs = 0.75, na.rm = TRUE),
      p100         = custom_max(value, na.rm = TRUE),
      cv           = sd / mean,
      shapiro_test = calc_shapiro_test(var = value),
      ks_test      = calc_ks_test(var = value),
      ad_test      = calc_ad_test(var = value),
      .groups      = "drop"
    )
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
      chisq_test               = dplyr::coalesce(chisq_test,               chisq_test_ht),
      chisq_test_no_correction = dplyr::coalesce(chisq_test_no_correction, chisq_test_no_correction_ht),
      chisq_test_simulated     = dplyr::coalesce(chisq_test_simulated,     chisq_test_simulated_ht),
      fisher_test              = dplyr::coalesce(fisher_test,              fisher_test_ht),
      fisher_test_simulated    = dplyr::coalesce(fisher_test_simulated,    fisher_test_simulated_ht)
    ) %>%
    dplyr::select(-dplyr::ends_with("_ht")) %>%
    dplyr::rename("strata" = !!strata_sym) %>%
    dplyr::left_join(smd_res, by = "var") %>%
    dplyr::mutate(strata = factor(strata, levels = strata_lvls))
  
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
      dplyr::left_join(sort_vars, by = c("var", "level"), relationship = "many-to-many") %>%
      dplyr::group_by(var) %>%
      dplyr::mutate(
        # carry forward sort1 from any non‑missing row in the group
        sort1 = dplyr::coalesce(sort1, dplyr::first(stats::na.omit(sort1))),
        # safe base max (never warns)
        .base_max_num = max(c(-Inf, sort2), na.rm = TRUE),
        .base_max_int = ifelse(is.finite(.base_max_num), as.integer(.base_max_num), 0L),
        # fill missing sort2s sequentially after the base
        sort2 = dplyr::if_else(
          is.na(sort2),
          as.integer(.base_max_int + cumsum(is.na(sort2))),
          sort2
        ),
        # tie‑breakers: keep checkbox "any_selected" last; missing level last
        .is_missing_level = is.na(level),
        .is_any_selected  = class == "checkbox" & grepl("___any_selected$", var, perl = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(sort1, sort2, .is_any_selected, .is_missing_level) %>%
      dplyr::mutate(
        var   = forcats::fct_inorder(var),
        level = forcats::fct_inorder(level)
      ) %>%
      dplyr::select(-sort1, -sort2, -.is_missing_level, -.is_any_selected,
                    -.base_max_num, -.base_max_int) %>%
      dplyr::relocate(class, var_type, label, .after = dplyr::everything())
  } else {
    sort_vars <- var_info %>% dplyr::select(var, sort1, sort2)
    
    res_stats <- res_stats %>%
      dplyr::left_join(sort_vars, by = "var", relationship = "many-to-many") %>%
      dplyr::group_by(var) %>%
      dplyr::mutate(
        sort1 = dplyr::coalesce(sort1, dplyr::first(stats::na.omit(sort1))),
        .base_max_num = max(c(-Inf, sort2), na.rm = TRUE),
        .base_max_int = ifelse(is.finite(.base_max_num), as.integer(.base_max_num), 0L),
        sort2 = dplyr::if_else(
          is.na(sort2),
          as.integer(.base_max_int + cumsum(is.na(sort2))),
          sort2
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(sort1, sort2) %>%
      dplyr::mutate(var = forcats::fct_inorder(var)) %>%
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
      n_level        = as.integer(n_level),
      n_strata       = as.integer(n_strata),
      n_level_valid  = dplyr::coalesce(n_level_valid,  n_level),
      n_strata_valid = dplyr::coalesce(n_strata_valid, n_strata),
      pct            = dplyr::coalesce(pct,
                                       dplyr::if_else(n_strata > 0L,       n_level       / n_strata,       NA_real_)),
      pct_valid      = dplyr::coalesce(pct_valid,
                                       dplyr::if_else(n_strata_valid > 0L, n_level_valid / n_strata_valid, NA_real_))
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
  
  # If only a strata column exists, return empty structure
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
  
  # Pivot into long form
  long <- x |>
    tidyr::pivot_longer(cols = - !! strata_sym,
                        names_to = "var",
                        values_to = "level")
  
  # Enforce full factor levels 
  long <- long %>%
    dplyr::mutate(
      level = if (is.factor(level)) {
        # preserve full factor levels including unused ones
        factor(level, levels = levels(level))
      } else {
        # for characters, treat all observed + NA distinctly
        factor(level)
      }
    )
  
  # Now count with full levels preserved
  long %>%
    dplyr::count(!! strata_sym, var, level, name = "n_level", .drop = FALSE) %>%
    dplyr::group_by(var) %>%
    tidyr::complete(!! strata_sym, level,
                    fill = list(n_level = 0)) %>%
    dplyr::group_by(!! strata_sym, var) %>%
    dplyr::mutate(
      n_strata      = sum(n_level, na.rm = TRUE),
      n_level_valid = ifelse(is.na(level), NA_integer_, n_level),
      n_strata_valid = sum(n_level_valid, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(!! strata_sym, dplyr::everything()) %>%
    dplyr::mutate(!! strata_sym := as.character(!! strata_sym))
}


# Builds the canonical (var, level, level_id) table used by
# order_within_vars_no_strata() to enforce the user-intended ordering of
# levels per variable (including checkbox blocks and their synthetic
# ___any_selected row).
make_level_map_no_strata <- function(data, vars, cb_blocks = list(), show_any = TRUE) {
  out <- list()
  
  cb_vars <- unlist(purrr::map(cb_blocks, "vars"), use.names = FALSE)
  
  regular_vars <- setdiff(vars, cb_vars)
  
  # Regular variables
  for (v in regular_vars) {
    if (!v %in% names(data)) next
    
    x <- data[[v]]
    
    if (is.factor(x)) {
      lvl <- levels(x)
    } else if (is.logical(x)) {
      lvl <- c(FALSE, TRUE)
    } else if (is.character(x)) {
      lvl <- sort(unique(x[!is.na(x)]))
    } else {
      next
    }
    
    out[[length(out) + 1]] <- tibble::tibble(
      var = v,
      level = as.character(lvl),
      level_id = seq_along(lvl)
    )
  }
  
  # Checkbox variables
  if (length(cb_blocks) > 0) {
    for (bl in cb_blocks) {
      out[[length(out) + 1]] <- tibble::tibble(
        var = bl$vars,
        level = unname(bl$labels[bl$vars]),
        level_id = 1L
      )
      
      if (isTRUE(show_any)) {
        base_name <- sub("___.*$", "", bl$vars[1])
        any_var   <- paste0(base_name, "___any_selected")
        
        out[[length(out) + 1]] <- tibble::tibble(
          var = any_var,
          level = "Any selected",
          level_id = 1L
        )
      }
    }
  }
  
  dplyr::bind_rows(out)
}


#' Order within-vars for no-strata output (no relabeling of NA)
#'
#' @param res_stats tibble from the no-strata engine
#' @param vars      original vars vector the user passed (or NULL)
#' @param level_map Optional data frame giving the full intended level order
#'   for each variable. Must contain columns `var`, `level`, and `level_id`.
#' @return res_stats re-ordered
#' @noRd
order_within_vars_no_strata <- function(res_stats, vars, level_map = NULL) {
  stopifnot(is.character(vars), length(vars) > 0)
  
  has_level_map <- !is.null(level_map) &&
    is.data.frame(level_map) &&
    all(c("var", "level", "level_id") %in% names(level_map)) &&
    nrow(level_map) > 0
  
  res_stats <- res_stats |>
    dplyr::mutate(
      var = as.character(var),
      var_order = match(var, vars),
      level_chr = as.character(level)
    )
  
  if (has_level_map) {
    level_map <- level_map |>
      dplyr::mutate(
        var = as.character(.data$var),
        level = as.character(.data$level)
      )
    
    res_stats <- res_stats |>
      dplyr::left_join(
        level_map,
        by = c("var" = "var", "level_chr" = "level")
      )
  }
  
  if (!"level_id" %in% names(res_stats)) {
    res_stats <- res_stats |>
      dplyr::mutate(level_id = NA_integer_)
  }
  
  res_stats <- res_stats |>
    dplyr::group_by(var) |>
    dplyr::mutate(
      .row_in_var = dplyr::row_number(),
      .is_any     = !is.na(class) & class == "checkbox" &
        !is.na(level_chr) & level_chr == "Any selected",
      .is_missing = is.na(level_chr),
      .level_id   = dplyr::coalesce(level_id, .row_in_var)
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(var_order, .is_any, .is_missing, .level_id, .row_in_var)
  
  if (has_level_map) {
    global_level_order <- level_map |>
      dplyr::mutate(var_order = match(.data$var, vars)) |>
      dplyr::arrange(var_order, .data$level_id) |>
      dplyr::pull(.data$level)
    
    res_stats <- res_stats |>
      dplyr::mutate(
        level = factor(level_chr, levels = unique(global_level_order))
      )
  } else {
    res_stats <- res_stats |>
      dplyr::mutate(
        level = factor(level_chr, levels = unique(level_chr))
      )
  }
  
  res_stats |>
    dplyr::mutate(
      var = factor(var, levels = vars)
    ) |>
    dplyr::select(-var_order, -level_chr, -level_id, -.row_in_var, -.is_any, -.is_missing, -.level_id)
}


.insert_after <- function(x, value, after) {
  if (value %in% x) return(x)
  pos <- match(after, x)
  if (is.na(pos)) return(c(x, value))
  append(x, value, after = pos)
}


.make_var_levels_with_any <- function(vars, cb_blocks, show_any = TRUE) {
  lvls <- vars
  
  if (!isTRUE(show_any) || length(cb_blocks) == 0) return(lvls)
  
  for (bl in cb_blocks) {
    base_name <- sub("___.*$", "", bl$vars[1])
    any_var   <- paste0(base_name, "___any_selected")
    
    # insert after the last checkbox var *as it appears in vars*
    idx <- match(bl$vars, vars)
    idx <- idx[!is.na(idx)]
    if (length(idx) == 0) next
    
    last_cb <- vars[max(idx)]
    lvls <- .insert_after(lvls, any_var, after = last_cb)
  }
  
  lvls
}



#' @title
#' Get info about variables in a data frame
#'
#' @description
#' Mostly a helper function for other tidy_table_one functions. The function
#' takes a data frame or tibble and returns a descriptive tibble with the
#' variable names, class, binary type (categorical or continuous), and the
#' factor levels.
#'
#' @param data A data.frame or tbl_df
#' @param .vars A character string of variable/column names. If empty, all
#'   columns are used
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @importFrom dplyr select_if
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @importFrom tibble enframe
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#'
#' @return
#' A tbl_df with colums for
#' \describe{
#'   \item{var}{name of the variable/column}
#'   \item{level}{factor level if applicable, character levels too}
#'   \item{class}{variable class}
#'   \item{var_type}{variable type: categorical or continuous}
#' }
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' lapply(diamonds, class)
#' get_var_info(data = diamonds,
#'              .vars = c("carat",
#'                        "cut",
#'                        "color",
#'                        "clarity",
#'                        "depth",
#'                        "table",
#'                        "price")) %>%
#'   print(n = Inf)
#'
#'
#' get_var_info(data = pbc_mayo)
#'
#' get_var_info(data = pbc_mayo,
#'              .vars = c("status", "sex", "stage", "age"))
#' @noRd
get_var_info <- function(data, .vars = NULL) {
  
  if (is.null(.vars)) .vars <- names(data)
  
  out <- lapply(.vars, function(v) {
    x <- data[[v]]
    cls <- class(x)[1]
    
    if (is.factor(x)) {
      tibble::tibble(
        var = v,
        level = as.character(levels(x)),
        class = cls,
        var_type = "categorical"
      )
    } else if (is.character(x)) {
      lv <- sort(unique(x))
      tibble::tibble(
        var = v,
        level = lv,
        class = cls,
        var_type = "categorical"
      )
    } else {
      tibble::tibble(
        var = v,
        level = NA_character_,
        class = cls,
        var_type = "continuous"
      )
    }
  })
  
  dplyr::bind_rows(out)
}


# Final assembly for the no-strata code path. Takes the raw stats (continuous +
# categorical + optional checkbox rows), adds the NA test columns for schema
# compatibility, joins class/type/label, ensures all expected columns exist,
# orders rows by `vars` (or var_levels when checkbox blocks are present), and
# relocates columns to canonical order.
.assemble_no_strata <- function(res_stats,
                                data,
                                vars,
                                var_info,
                                var_lbls,
                                cb_blocks,
                                checkbox_opts) {
  
  has_checkbox <- length(cb_blocks) > 0
  
  res_stats <- tibble::as_tibble(res_stats)
  
  # Test columns are NA (no between-group tests when no strata)
  res_stats <- res_stats |>
    dplyr::mutate(
      chisq_test               = NA_real_,
      chisq_test_no_correction = NA_real_,
      chisq_test_simulated     = NA_real_,
      fisher_test              = NA_real_,
      fisher_test_simulated    = NA_real_,
      check_categorical_test   = NA_character_,
      oneway_test_unequal_var  = NA_real_,
      oneway_test_equal_var    = NA_real_,
      kruskal_test             = NA_real_,
      bartlett_test            = NA_real_,
      levene_test              = NA_real_,
      smd                      = NA_real_
    )
  
  # Join class/type and labels. When checkbox rows are present they already
  # carry class/var_type/label, so coalesce after the join.
  class_and_type <- var_info |>
    dplyr::select(dplyr::any_of(c("var", "class", "var_type"))) |>
    dplyr::distinct()
  
  res_stats <- res_stats |>
    dplyr::left_join(class_and_type, by = "var") |>
    dplyr::left_join(var_lbls,       by = "var")
  
  if (has_checkbox) {
    res_stats <- res_stats |>
      safe_merge_cols("label",    c("label.x",    "label.y",    "label")) |>
      safe_merge_cols("var_type", c("var_type.x", "var_type.y", "var_type")) |>
      safe_merge_cols("class",    c("class.x",    "class.y",    "class"))
    
    # Guarantee metadata cols exist for relocate()
    res_stats <- .ensure_cols(
      res_stats,
      cols_types = list(
        class    = NA_character_,
        var_type = NA_character_,
        label    = NA_character_
      )
    )
  }
  
  if (!"level" %in% names(res_stats)) res_stats$level <- NA_character_
  
  # Guarantee the full canonical column schema with sensible NA-typed defaults
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
  
  # Build var ordering. With checkbox blocks present, insert each block's
  # synthetic ___any_selected variable at the right spot.
  var_levels <- if (has_checkbox) {
    .make_var_levels_with_any(
      vars      = vars,
      cb_blocks = cb_blocks,
      show_any  = checkbox_opts$show_any %||% TRUE
    )
  } else {
    vars
  }
  
  level_map <- make_level_map_no_strata(
    data      = data,
    vars      = var_levels,
    cb_blocks = cb_blocks,
    show_any  = has_checkbox && (checkbox_opts$show_any %||% TRUE)
  )
  
  res_stats <- res_stats |>
    dplyr::mutate(var = factor(var, levels = var_levels)) |>
    dplyr::arrange(var)
  
  res_stats <- order_within_vars_no_strata(
    res_stats = res_stats,
    vars      = var_levels,
    level_map = level_map
  )
  
  # Final column order
  res_stats |>
    dplyr::mutate(strata     = factor("Overall", levels = "Overall"),
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
