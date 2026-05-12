
#### Checkbox helpers -------------------------------- 


# Build a stable identifier for the synthetic "Any selected" row for a
# checkbox block, derived from `overall_lbl`. Lowercase, non-alphanumeric
# runs collapse to a single underscore, leading/trailing underscores
# trimmed. Falls back to "block" if the slugified string would be empty
# (e.g., a label of "??"). The result is concatenated with the package's
# `___any_selected` suffix.
.slugify_label <- function(x) {
  x <- as.character(x)
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x[x == ""] <- "block"
  x
}

.any_selected_var <- function(overall_lbl) {
  paste0(.slugify_label(overall_lbl), .tidytableone_any_selected_suffix)
}


validate_checkbox_spec <- function(x, data, vars, default_checkbox_txt = "Checked") {
  # `checkbox_txt` is optional: if absent entirely, fill with the call-level
  # default; if present, fill any per-row NAs with the default.
  req_cols <- c("var","overall_lbl","checkbox_lbl")
  if (!all(req_cols %in% names(x))) {
    stop("`checkbox` must include columns: ", paste(req_cols, collapse = ", "), call. = FALSE)
  }
  x <- tibble::as_tibble(x)
  
  if (!"checkbox_txt" %in% names(x)) {
    x$checkbox_txt <- default_checkbox_txt
  } else {
    x$checkbox_txt[is.na(x$checkbox_txt) | x$checkbox_txt == ""] <- default_checkbox_txt
  }
  
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
      select_txt  = stats::setNames(df$checkbox_txt, df$var),
      # Synthetic "Any selected" var name, derived from overall_lbl. Built
      # once here so every consumer (process, p-values, SMD, ordering, level
      # map) reads the same string instead of recomputing and risking drift.
      any_var     = .any_selected_var(nm)
    )
  })
  rlang::set_names(out, names(sp))
}

# Compute counts and percents per level; optional "Any selected" row
process_checkbox_blocks_strata <- function(data, blocks, opts, strata_var, strata_levels) {
  
  # Always normalize/validate opts right here (defensive)
  opts <- normalize_checkbox_opts(opts)
  opts <- validate_checkbox_opts(opts)
  
  # Include an Overall stratum first, keep order stable
  strata_levels_all <- unique(c("Overall", strata_levels))
  
  out <- lapply(blocks, function(bl) {
    dplyr::bind_rows(lapply(strata_levels_all, function(g) {
      # Slice data for this stratum (or all for Overall)
      d_g <- if (identical(g, "Overall")) data else data[data[[strata_var]] %in% g, , drop = FALSE]
      
      # Raw values and selected flags
      raw_mat <- as.data.frame(d_g[bl$vars], stringsAsFactors = FALSE)
      sel_mat <- as.data.frame(lapply(bl$vars, function(v) as.integer(d_g[[v]] == bl$select_txt[[v]])))
      names(sel_mat) <- bl$vars
      
      # Per-level counts (selected)
      n_level <- colSums(sel_mat, na.rm = TRUE)
      
      # Group-based denominator: stratum N for every checkbox column
      group_N      <- as.integer(nrow(d_g))
      n_strata_vec <- rep(group_N, length(bl$vars))
      
      # Sanity: names must align so we can index by bl$vars
      stopifnot(identical(names(n_level), bl$vars))
      
      # Per-level rows (one per checkbox column) 
      df <- tibble::tibble(
        strata         = factor(g, levels = strata_levels_all),
        var            = bl$vars,                        # << keep ORIGINAL checkbox column name
        level_var      = bl$vars,                        # << stable id for per-level tests
        level          = unname(bl$labels[bl$vars]),
        n_level        = as.integer(n_level[bl$vars]),
        n_strata       = as.integer(n_strata_vec),
        pct            = ifelse(n_strata_vec > 0L, n_level[bl$vars] / n_strata_vec, NA_real_),
        
        n_level_valid  = as.integer(n_level[bl$vars]),
        n_strata_valid = as.integer(n_strata_vec),
        pct_valid      = ifelse(n_strata_vec > 0L, n_level[bl$vars] / n_strata_vec, NA_real_),
        
        # tags & label
        var_type       = "categorical",
        class          = "checkbox",
        label          = bl$overall_lbl                  # << display label for the whole block
      )
      
      # Optional "Any selected" row (synthetic binary) 
      if (isTRUE(opts$show_any)) {
        any_count <- as.integer(sum(rowSums(sel_mat, na.rm = TRUE) > 0L, na.rm = TRUE))
        
        df <- dplyr::bind_rows(
          df,
          tibble::tibble(
            strata         = factor(g, levels = strata_levels_all),
            var            = bl$any_var,                         # << slug-derived synthetic id
            level_var      = NA_character_,                      # << exclude from per-level p-values
            level          = "Any selected",
            n_level        = any_count,
            n_strata       = group_N,
            pct            = ifelse(group_N > 0L, any_count / group_N, NA_real_),
            
            n_level_valid  = any_count,
            n_strata_valid = group_N,
            pct_valid      = ifelse(group_N > 0L, any_count / group_N, NA_real_),
            
            var_type       = "categorical",
            class          = "checkbox",
            label          = bl$overall_lbl
          )
        )
      }
      
      df
    }))
  })
  
  dplyr::bind_rows(out)
}


# Compute p-values for each checkbox level (and "Any selected") per block,
# writing results directly into the checkbox rows in `tab`.
#
# Checkbox p-values computed once per checkbox variable.
# tab:        output from process_checkbox_blocks_strata() (has class == "checkbox")
# data:       original data
# strata_var: name of strata column in `data` (string)
# blocks:     list from prepare_checkbox_blocks()
# p_adjust:   multiple-testing adjustment method (e.g. "none","BH","holm",...)
#
# Tests are computed group-based: every individual in the stratum is
# tabulated as Selected or Not selected. All four test variants (chisq,
# chisq no-correction, chisq simulated, fisher, fisher simulated) are
# computed; adorn_tidytableone() picks one via `exact` / `monte_carlo_p`.
add_pvalues_checkbox <- function(tab,
                                 data,
                                 strata_var,
                                 blocks,
                                 p_adjust = "none",
                                 B        = 2000) {
  
  if (!("class" %in% names(tab))) return(tab)
  if (!any(tab$class == "checkbox")) return(tab)
  
  per_var <- list()
  
  # strata factor once
  grp_all <- factor(data[[strata_var]])
  
  for (bl in blocks) {
    
    # per checkbox variable (per-level) 
    for (v in bl$vars) {
      
      # Build Selected/Not selected without converting NA -> Not selected
      sel_chr <- rep(NA_character_, nrow(data))
      not_na  <- !is.na(data[[v]])
      sel_chr[not_na] <- ifelse(data[[v]][not_na] == bl$select_txt[[v]],
                                "Selected", "Not selected")
      sel <- factor(sel_chr, levels = c("Not selected","Selected"))
      
      tbl <- table(grp_all, sel, useNA = "no")
      
      per_var[[length(per_var) + 1]] <- tibble::tibble(
        .block                      = bl$overall_lbl,
        var                         = v,
        chisq_test                  = safe_chisq(tbl,  correct = TRUE,  simulate.p.value = FALSE),
        chisq_test_no_correction    = safe_chisq(tbl,  correct = FALSE, simulate.p.value = FALSE),
        chisq_test_simulated        = safe_chisq(tbl,  correct = TRUE,  simulate.p.value = TRUE,  B = B),
        fisher_test                 = safe_fisher(tbl, simulate.p.value = FALSE),
        fisher_test_simulated       = safe_fisher(tbl, simulate.p.value = TRUE, B = B),
        check_categorical_test      = flag_chisq_ok(tbl)
      )
    }
    
    # synthetic "Any selected" variable 
    var_any <- bl$any_var
    
    # any selected across the block (NA treated as 0 in the rowSums via na.rm=TRUE)
    sel_any <- factor(
      ifelse(
        rowSums(as.data.frame(lapply(bl$vars, function(v)
          as.integer(data[[v]] == bl$select_txt[[v]]))), na.rm = TRUE) > 0L,
        "Selected", "Not selected"
      ),
      levels = c("Not selected","Selected")
    )
    
    tbl_any <- table(grp_all, sel_any, useNA = "no")
    
    per_var[[length(per_var) + 1]] <- tibble::tibble(
      .block                      = bl$overall_lbl,
      var                         = var_any,
      chisq_test                  = safe_chisq(tbl_any,  correct = TRUE,  simulate.p.value = FALSE),
      chisq_test_no_correction    = safe_chisq(tbl_any,  correct = FALSE, simulate.p.value = FALSE),
      chisq_test_simulated        = safe_chisq(tbl_any,  correct = TRUE,  simulate.p.value = TRUE,  B = B),
      fisher_test                 = safe_fisher(tbl_any, simulate.p.value = FALSE),
      fisher_test_simulated       = safe_fisher(tbl_any, simulate.p.value = TRUE, B = B),
      check_categorical_test      = flag_chisq_ok(tbl_any)
    )
  }
  
  tests <- dplyr::bind_rows(per_var)
  
  if (!identical(p_adjust, "none")) {
    # Adjust within each block (one block = one overall_lbl), so checkbox
    # blocks with mixed-name member vars are still grouped correctly.
    tests <- tests %>%
      dplyr::group_by(.data$.block) %>%
      dplyr::mutate(
        chisq_test               = stats::p.adjust(.data$chisq_test,               method = p_adjust),
        chisq_test_no_correction = stats::p.adjust(.data$chisq_test_no_correction, method = p_adjust),
        chisq_test_simulated     = stats::p.adjust(.data$chisq_test_simulated,     method = p_adjust),
        fisher_test              = stats::p.adjust(.data$fisher_test,              method = p_adjust),
        fisher_test_simulated    = stats::p.adjust(.data$fisher_test_simulated,    method = p_adjust)
      ) %>%
      dplyr::ungroup()
  }
  
  tests <- dplyr::select(tests, -".block")
  
  tab %>%
    dplyr::left_join(tests, by = "var")
}


.default_checkbox_opts <- function() {
  list(
    show_pvalues = TRUE,
    p_adjust     = "none",
    show_any     = FALSE,
    note         = "More than one response allowed"
  )
}

normalize_checkbox_opts <- function(x) {
  defaults <- list(
    show_pvalues = TRUE,    # boolean; used only when strata is present
    p_adjust     = "none",
    show_any     = FALSE,   # default off; opt in for "Any selected" row
    note         = "More than one response allowed"
  )
  
  # Treat NULL or empty list as "use defaults"
  if (is.null(x) || (is.list(x) && length(x) == 0)) return(defaults)
  
  if (!is.list(x)) {
    stop("`checkbox_opts` must be a list or NULL.", call. = FALSE)
  }
  
  # Error on options that were renamed/removed in 0.1.0, with migration help.
  removed_msgs <- c(
    denom = "Checkbox percentages are always group-based (denominator = stratum N). Drop this field.",
    test  = "Test selection is controlled at adorn time via `exact` / `monte_carlo_p`. Drop this field.",
    pvals = "Renamed to `show_pvalues` (logical: TRUE/FALSE). Replace `pvals = \"per_level\"` with `show_pvalues = TRUE` and `pvals = \"none\"` with `show_pvalues = FALSE`."
  )
  bad <- intersect(names(removed_msgs), names(x))
  if (length(bad) > 0) {
    msg <- "`checkbox_opts` no longer accepts these fields:\n"
    for (nm in bad) {
      msg <- paste0(msg, "  * `", nm, "`: ", removed_msgs[[nm]], "\n")
    }
    msg <- paste0(msg, "See NEWS.md.")
    stop(msg, call. = FALSE)
  }
  
  utils::modifyList(defaults, x)
}

#' @importFrom stats p.adjust.methods
validate_checkbox_opts <- function(opts) {
  # If normalize_checkbox_opts is always called first, opts should always be a list,
  # but this keeps it robust.
  if (is.null(opts) || (is.list(opts) && length(opts) == 0)) {
    opts <- normalize_checkbox_opts(NULL)
  }
  
  if (!is.logical(opts$show_pvalues) || length(opts$show_pvalues) != 1 || is.na(opts$show_pvalues)) {
    stop("`checkbox_opts$show_pvalues` must be a single logical (TRUE or FALSE).", call. = FALSE)
  }
  opts$p_adjust <- match.arg(opts$p_adjust, stats::p.adjust.methods)
  
  opts$show_any <- isTRUE(opts$show_any)
  
  if (is.null(opts$note)) opts$note <- ""
  opts$note <- as.character(opts$note)
  
  opts
}


# Compute overall (no-strata) checkbox block rows. Schema-compatible with
# process_checkbox_blocks_strata: strata column set to "Overall", same set of
# count/percent columns, plus class = "checkbox" and var_type = "categorical".
process_checkbox_blocks_overall <- function(data, blocks, opts) {
  
  opts  <- normalize_checkbox_opts(opts)
  opts  <- validate_checkbox_opts(opts)
  
  out <- lapply(blocks, function(bl) {
    # raw cols & 0/1 selection matrix for this block
    raw_mat <- as.data.frame(data[bl$vars], stringsAsFactors = FALSE)
    sel_mat <- as.data.frame(lapply(bl$vars, function(v)
      as.integer(data[[v]] == bl$select_txt[[v]])))
    names(sel_mat) <- bl$vars
    
    # Numerators per level (by original column)
    n_level <- colSums(sel_mat, na.rm = TRUE)
    
    # Group-based denominator: total nrow(data) for every level
    display_N <- as.integer(nrow(data))
    
    # Synthetic "Any selected" var name (slug-derived; computed once per block
    # in prepare_checkbox_blocks)
    var_any   <- bl$any_var
    
    df <- tibble::tibble(
      strata   = "Overall",
      var      = names(n_level),                                    # <- original checkbox columns
      level    = unname(bl$labels[names(n_level)]),                 # printed row label (e.g., "White")
      n_level  = as.integer(n_level),
      n_strata = display_N,
      pct      = dplyr::if_else(n_strata > 0L, n_level / n_strata, NA_real_),
      
      n_level_valid  = as.integer(n_level),
      n_strata_valid = display_N,
      pct_valid      = dplyr::if_else(n_strata_valid > 0L, n_level_valid / n_strata_valid, NA_real_),
      
      label    = bl$overall_lbl,                                    # <- block heading in label
      level_var = names(n_level)                                     # <- keep the original var for helpers
    )
    
    # Optional "Any selected"
    if (isTRUE(opts$show_any)) {
      any_count <- as.integer(sum(rowSums(sel_mat, na.rm = TRUE) > 0L, na.rm = TRUE))
      
      df <- dplyr::bind_rows(
        df,
        tibble::tibble(
          strata   = "Overall",
          var      = var_any, 
          level    = "Any selected",
          n_level  = any_count,
          n_strata = display_N,
          pct      = ifelse(display_N > 0L, any_count / display_N, NA_real_),
          
          n_level_valid  = any_count,
          n_strata_valid = display_N,
          pct_valid      = ifelse(display_N > 0L, any_count / display_N, NA_real_),
          
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

