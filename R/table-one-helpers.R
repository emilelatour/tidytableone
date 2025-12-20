
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
        # tie‑breakers: keep checkbox “any_selected” last; missing level last
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



#' Order vars for no-strata output (no relabeling of NA)
#' @param res_stats tibble from the no-strata engine
#' @param vars      original vars vector the user passed (or NULL)
order_rows_no_strata <- function(res_stats, vars) {
  stopifnot(is.character(vars), length(vars) > 0)

  res_stats <- res_stats |>
    dplyr::mutate(
      var = as.character(var),
      var_order = match(var, vars)
    )

  # Put ___any_selected immediately after the last checkbox var in its block
  any_idx <- which(is.na(res_stats$var_order) & grepl("___any_selected$", res_stats$var))
  if (length(any_idx) > 0) {
    for (i in any_idx) {
      base <- sub("___any_selected$", "", res_stats$var[i])          # e.g. "race"
      block_vars <- vars[grepl(paste0("^", base, "___"), vars)]      # e.g. race___1, race___2, ...
      if (length(block_vars) > 0) {
        res_stats$var_order[i] <- max(match(block_vars, vars), na.rm = TRUE) + 0.5
      } else {
        res_stats$var_order[i] <- 1e9
      }
    }
  }

  # Anything else not in vars goes to the end, but stable
  res_stats$var_order[is.na(res_stats$var_order)] <- 1e9

  # Within-var ordering:
  # - if level is a factor, keep factor order
  # - otherwise keep current row order (stable)
  level_is_factor <- is.factor(res_stats$level)

  res_stats <- res_stats |>
    dplyr::group_by(var) |>
    dplyr::mutate(
      .row_in_var = dplyr::row_number(),
      level_order2 = if (level_is_factor) as.numeric(level) else .row_in_var,
      is_any = dplyr::if_else(
        !is.na(class) & class == "checkbox" &
          !is.na(level) & level == "Any selected",
        1L, 0L
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(var_order, is_any, level_order2, .row_in_var) |>
    dplyr::select(-var_order, -dplyr::any_of(c(".row_in_var", "level_order2", "is_any")))

  res_stats
}

#' Order within-vars for no-strata output (no relabeling of NA)
#' @param res_stats tibble from the no-strata engine
#' @param vars      original vars vector the user passed (or NULL)
#' @return          res_stats re-ordered
order_within_vars_no_strata <- function(res_stats, vars) {
  stopifnot(is.character(vars), length(vars) > 0)

  # Keep the incoming var ordering exactly as provided
  res_stats <- res_stats |>
    dplyr::mutate(var = factor(as.character(var), levels = vars))

  # Preserve factor level order for level when present
  if ("level" %in% names(res_stats) && !is.factor(res_stats$level)) {
    res_stats <- res_stats |>
      dplyr::mutate(level = factor(level))
  }

  # Within each var:
  # - push "Any selected" last
  # - push NA levels last
  res_stats |>
    dplyr::group_by(var) |>
    dplyr::mutate(
      .is_any     = !is.na(class) & class == "checkbox" &
                    !is.na(level) & level == "Any selected",
      .is_missing = is.na(level)
    ) |>
    dplyr::arrange(var, .is_any, .is_missing, level, .by_group = TRUE) |>
    dplyr::ungroup() |>
    dplyr::select(-.is_any, -dplyr::all_of(".is_missing"))
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
# #' @export
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

#' Auto‑group similar variable names by text similarity
#'
#' Uses string distances + hierarchical clustering and auto-selects a cut height
#' by maximizing mean silhouette. Returns a tibble with group ids, sizes, and
#' readable group labels. Optionally absorbs singleton items into a same‑stem
#' multi‑member cluster under conservative thresholds.
#'
#' @param x Character vector of variable names.
#' @param method Distance method for \code{stringdist::stringdistmatrix()} (default "jw").
#' @param normalize If TRUE, lowercase and collapse separators for robustness.
#' @param normalize_numbers If TRUE, replace digit runs with "#".
#' @param min_multisize Minimum size for at least one multi‑member cluster (default 2).
#' @param h_grid Optional numeric vector of candidate cut heights for the dendrogram.
#' @param block_prefix_len Compare only within blocks defined by the first N letters (default 3).
#' @param absorb_singletons_by_stem Try absorbing singletons into same‑stem clusters (default TRUE).
#' @param require_triple_underscores_for_absorb If TRUE, only absorb singletons whose original
#'   name contains "___" (default TRUE).
#' @param relax_factor Allow singleton’s mean distance up to this multiple of the candidate
#'   cluster’s internal cohesion (default 1.2).
#' @param abs_thresh Absolute mean‑distance threshold for absorption (default 0.33).
#'
#' @return A tibble with columns: var, group_id, group_label, group_label_first, group_size.
#'
#' @examples
#' vars <- c("gender","age","education","ethnicity","income","marital_status",
#'           "race___1","race___2","race___3","race___4","race___5","race___6","race___98")
#' group_similar_vars(vars)
#'
#' @importFrom stats as.dist hclust cutree quantile
#' @importFrom stringdist stringdistmatrix
#' @importFrom stringr str_to_lower str_replace_all
#' @importFrom dplyr group_by group_modify ungroup arrange first bind_rows
#' @importFrom tibble tibble
#' @importFrom cluster silhouette
# #' @export
#' @noRd
group_similar_vars <- function(
  x,
  method = "jw",
  normalize = TRUE,
  normalize_numbers = TRUE,
  min_multisize = 2,
  h_grid = NULL,
  block_prefix_len = 3,
  absorb_singletons_by_stem = TRUE,
  require_triple_underscores_for_absorb = TRUE,
  relax_factor = 1.2,
  abs_thresh = 0.33
) {
  stopifnot(is.character(x))
  n <- length(x)
  if (n <= 2L) {
    return(dplyr::tibble(
      var = x,
      group_id = seq_along(x),
      group_label = x,
      group_label_first = x,
      group_size = 1L
    ))
  }

  # --- helpers ---
  .norm <- function(s) {
    if (!normalize) return(s)
    s <- stringr::str_to_lower(s)
    if (normalize_numbers) s <- stringr::str_replace_all(s, "\\d+", "#")
    s <- stringr::str_replace_all(s, "[^a-z0-9]+", "_")
    s <- stringr::str_replace_all(s, "^_+|_+$", "")
    s
  }
  .letters_only <- function(s) gsub("[^a-z]", "", s)

  .lcp <- function(ss) {
    if (length(ss) == 1) return(ss)
    split <- strsplit(ss, "", fixed = TRUE)
    maxlen <- min(lengths(split))
    pref <- character(maxlen)
    for (i in seq_len(maxlen)) {
      ch <- vapply(split, `[[`, "", i)
      if (length(unique(ch)) > 1)
        return(if (i == 1) "" else paste0(pref[seq_len(i-1)], collapse = ""))
      pref[i] <- ch[1]
    }
    paste0(pref, collapse = "")
  }
  .medoid_idx <- function(idx, D) {
    if (length(idx) == 1) return(idx)
    subD <- as.matrix(D)[idx, idx, drop = FALSE]
    idx[which.min(rowMeans(subD))]
  }

  # --- normalize for distance (keep originals for output) ---
  x_norm <- .norm(x)

  # --- block by leading letters to avoid cross-talk ---
  block_key <- substr(.letters_only(x_norm), 1, block_prefix_len)
  block_key[block_key == ""] <- substr(x_norm[block_key == ""], 1, block_prefix_len)

  # --- cluster within each block with auto-tuned cut ---
  cluster_block <- function(ix) {
    xn <- x_norm[ix]
    if (length(ix) == 1L) {
      return(dplyr::tibble(var = x[ix], norm = xn, group_id = 1L))
    }
    D <- stats::as.dist(stringdist::stringdistmatrix(xn, xn, method = method))
    hc <- stats::hclust(D, method = "average")

    h_grid_loc <- if (is.null(h_grid)) {
      merges <- hc$height
      lo <- max(min(merges, na.rm = TRUE) - 1e-6, 0)
      hi <- min(max(merges, na.rm = TRUE) + 1e-6, 1)
      seq(lo, hi, length.out = 40L)
    } else h_grid

    score_cut <- function(h) {
      cl <- cutree(hc, h = h)
      k <- length(unique(cl))
      if (k <= 1L || k >= length(ix)) return(c(score = -Inf, k = k, h = h))
      sizes <- table(cl)
      if (!any(sizes >= min_multisize)) return(c(score = -Inf, k = k, h = h))
      s <- try({
        sil <- cluster::silhouette(cl, D)
        mean(sil[, "sil_width"])
      }, silent = TRUE)
      if (inherits(s, "try-error") || !is.finite(s)) s <- -Inf
      prop_single <- mean(sizes == 1L)
      c(score = s - 0.05 * prop_single, k = k, h = h)
    }

    cand <- vapply(h_grid_loc, score_cut, numeric(3L))
    best_idx <- which.max(cand["score", ])
    cl <- if (!is.finite(cand["score", best_idx])) {
      cutree(hc, k = min(3L, length(ix)))
    } else {
      cutree(hc, h = cand["h", best_idx])
    }

    dplyr::tibble(var = x[ix], norm = xn, group_id = as.integer(cl))
  }

  pieces <- split(seq_along(x), block_key, drop = TRUE)
  res_list <- lapply(pieces, cluster_block)

  # make group ids globally unique
  offset <- 0L
  for (i in seq_along(res_list)) {
    g_local <- res_list[[i]]$group_id
    res_list[[i]]$group_id <- g_local + offset
    offset <- offset + max(g_local)
  }
  df <- dplyr::bind_rows(res_list)

  # --- optional: conservative singleton absorb by stem + triple-underscore guard ---
if (absorb_singletons_by_stem) {
  Dfull <- stats::as.dist(stringdist::stringdistmatrix(df$norm, df$norm, method = method))
  Dm <- as.matrix(Dfull)

  df$stem <- sub("_.+$", "", df$norm)
  df$has_triple <- grepl("___", df$var, fixed = TRUE)

  grp_members <- split(seq_len(nrow(df)), df$group_id)
  grp_sizes   <- vapply(grp_members, length, integer(1))
  multi_ids   <- names(grp_members)[grp_sizes >= 2]

  # Cohesion per multi-member group: use a *looser* yardstick (q90 of off-diagonals)
  grp_intra_q90 <- setNames(rep(Inf, length(grp_members)), names(grp_members))
  for (g in multi_ids) {
    idx <- grp_members[[g]]
    off <- Dm[idx, idx, drop = FALSE]
    off <- off[off != 0]
    grp_intra_q90[g] <- if (length(off)) stats::quantile(off, 0.90, names = FALSE) else 0
  }

  # Majority stem per group + whether group contains any triple-underscore member
  grp_major_stem <- vapply(grp_members, function(ii) {
    names(sort(table(df$stem[ii]), decreasing = TRUE))[1]
  }, character(1))
  grp_has_triple <- vapply(grp_members, function(ii) any(df$has_triple[ii]), logical(1))

  # Singletons to consider
  is_singleton_row <- grp_sizes[match(df$group_id, names(grp_sizes))] == 1L

  for (i in which(is_singleton_row)) {
    # optional gate: only absorb if this singleton has triple underscores
    if (require_triple_underscores_for_absorb && !df$has_triple[i]) next

    st <- df$stem[i]
    # candidate groups: multi, same majority stem, and (optionally) have a triple-underscore member
    cand_groups <- names(grp_members)[grp_sizes >= 2 &
                                      grp_major_stem == st &
                                      (!require_triple_underscores_for_absorb | grp_has_triple)]
    if (length(cand_groups) == 0L) next

    # Evaluate fit to each candidate group
    cand_df <- do.call(rbind, lapply(cand_groups, function(g) {
      idx <- grp_members[[g]]
      data.frame(
        g           = as.integer(g),
        mean_to_grp = mean(Dm[i, idx]),
        intra_q90   = grp_intra_q90[[g]]
      )
    }))

    # Best candidate = smallest mean_to_grp
    cand_df <- cand_df[order(cand_df$mean_to_grp, cand_df$g), , drop = FALSE]
    best <- cand_df[1, ]

    # Accept if the singleton is not worse than (relax_factor * q90) OR clears abs_thresh
    # This avoids punishing very tight clusters.
    if (best$mean_to_grp <= max(relax_factor * best$intra_q90, abs_thresh)) {
      old_gid <- df$group_id[i]
      df$group_id[i] <- best$g
      grp_members[[as.character(best$g)]]  <- c(grp_members[[as.character(best$g)]], i)
      grp_members[[as.character(old_gid)]] <- setdiff(grp_members[[as.character(old_gid)]], i)
    }
  }
}

  # --- labels per final group ---
  add_labels <- function(sub) {
    if (nrow(sub) == 1L) {
      return(dplyr::tibble(
        var = sub$var,
        group_label = sub$var,
        group_label_first = dplyr::first(sub$var),
        group_size = 1L
      ))
    }
    D <- stats::as.dist(stringdist::stringdistmatrix(sub$norm, sub$norm, method = method))
    lcp <- .lcp(sub$norm)
    lab <- gsub("^_+|_+$", "", lcp)
    if (nchar(lab) < 3) {
      med <- sub$var[.medoid_idx(seq_len(nrow(sub)), D)]
      group_label <- med
    } else {
      group_label <- lab
    }
    dplyr::tibble(
      var = sub$var,
      group_label = group_label,
      group_label_first = dplyr::first(sub$var),
      group_size = nrow(sub)
    )
  }

  df |>
    dplyr::group_by(group_id) |>
    dplyr::group_modify(~ add_labels(.x)) |>
    dplyr::ungroup() |>
    dplyr::arrange(group_id, var)
}

