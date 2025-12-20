
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
# process_checkbox_blocks_strata <- function(data, blocks, opts, strata_var, strata_levels) {
#   denom <- match.arg(opts$denom, c("group","nonmissing","responders"))
# 
#   # Include an Overall stratum first, keep order stable
#   strata_levels_all <- unique(c("Overall", strata_levels))
# 
#   out <- lapply(blocks, function(bl) {
#     dplyr::bind_rows(lapply(strata_levels_all, function(g) {
#       # Slice data for this stratum (or all for Overall)
#       d_g <- if (identical(g, "Overall")) data else data[data[[strata_var]] %in% g, , drop = FALSE]
# 
#       # Raw values and selected flags
#       raw_mat <- as.data.frame(d_g[bl$vars], stringsAsFactors = FALSE)
#       sel_mat <- as.data.frame(lapply(bl$vars, function(v) as.integer(d_g[[v]] == bl$select_txt[[v]])))
#       names(sel_mat) <- bl$vars
# 
#       # Per-level counts (selected)
#       n_level <- colSums(sel_mat, na.rm = TRUE)
#       # Basic denominators
#       group_N <- nrow(d_g)
# 
#       # Non-missing per checkbox column
#       nonmiss_per_level <- colSums(!is.na(raw_mat), na.rm = TRUE)
# 
#       # Valid denominators (vector) according to denom
#       n_strata_valid_vec <- switch(
#         denom,
#         nonmissing = as.integer(nonmiss_per_level[bl$vars]),
#         responders = rep(sum(rowSums(sel_mat, na.rm = TRUE) > 0L, na.rm = TRUE), length(bl$vars)),
#         group      = rep(group_N, length(bl$vars))
#       )
# 
#       # Display denominators (overall column denominators)
#       n_strata_vec <- rep(group_N, length(bl$vars))
# 
#       # Sanity: names must align so we can index by bl$vars
#       stopifnot(identical(names(n_level), bl$vars))
# 
#       # Main rows (one per checkbox level)
#       df <- tibble::tibble(
#         strata         = factor(g, levels = strata_levels_all),
#         var            = bl$overall_lbl,
#         level_var      = bl$vars,                          # keep original checkbox col for per-level p
#         level          = unname(bl$labels[bl$vars]),
#         n_level        = as.integer(n_level[bl$vars]),
#         n_strata       = as.integer(n_strata_vec),
#         pct            = ifelse(n_strata_vec > 0L, n_level[bl$vars] / n_strata_vec, NA_real_),
# 
#         n_level_valid  = as.integer(n_level[bl$vars]),
#         n_strata_valid = as.integer(n_strata_valid_vec),
#         pct_valid      = ifelse(n_strata_valid_vec > 0L, n_level[bl$vars] / n_strata_valid_vec, NA_real_)
#       )
# 
#       # Optional “Any selected”
#       if (isTRUE(opts$show_any)) {
#         any_count        <- sum(rowSums(sel_mat, na.rm = TRUE) > 0L, na.rm = TRUE)
#         any_nonmissing   <- sum(rowSums(!is.na(raw_mat)) > 0L, na.rm = TRUE)
# 
#         any_display_denom <- switch(denom,
#                                     group      = group_N,
#                                     responders = any_count,
#                                     nonmissing = group_N)
#         any_valid_denom   <- switch(denom,
#                                     group      = any_display_denom,
#                                     responders = any_count,
#                                     nonmissing = any_nonmissing)
# 
#         df <- dplyr::bind_rows(
#           df,
#           tibble::tibble(
#             strata         = factor(g, levels = strata_levels_all),
#             var            = bl$overall_lbl,
#             level_var      = NA_character_,                 # NA => exclude from per-level p-vals
#             level          = "Any selected",
#             n_level        = as.integer(any_count),
#             n_strata       = as.integer(any_display_denom),
#             pct            = ifelse(any_display_denom > 0, any_count / any_display_denom, NA_real_),
# 
#             n_level_valid  = as.integer(any_count),
#             n_strata_valid = as.integer(any_valid_denom),
#             pct_valid      = ifelse(any_valid_denom > 0, any_count / any_valid_denom, NA_real_)
#           )
#         )
#       }
# 
#       df
#     }))
#   })
# 
#   dplyr::bind_rows(out) |>
#     dplyr::mutate(
#       var_type = "categorical",
#       class    = "checkbox"
#     )
# }

# Compute counts and percents per level; optional "Any selected" row
process_checkbox_blocks_strata <- function(data, blocks, opts, strata_var, strata_levels) {
  
  # Always normalize/validate opts right here (defensive)
  opts <- normalize_checkbox_opts(opts)
  opts <- validate_checkbox_opts(opts)
  
  denom <- match.arg(tolower(opts$denom %||% "responders"),
                     c("group","nonmissing","responders"))
  
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
      # Basic denominators
      group_N <- nrow(d_g)
      
      # Non-missing per checkbox column
      nonmiss_per_level <- colSums(!is.na(raw_mat), na.rm = TRUE)
      
      # Valid denominators (vector) according to denom
      any_selected <- rowSums(sel_mat, na.rm = TRUE) > 0L
      n_resp <- as.integer(sum(any_selected, na.rm = TRUE))
      
      n_strata_valid_vec <- switch(
        denom,
        nonmissing = as.integer(nonmiss_per_level[bl$vars]),
        responders = rep(n_resp, length(bl$vars)), 
        group      = rep(as.integer(group_N), length(bl$vars))
      )
      
      # Display denominators (overall column denominators)
      n_strata_vec <- rep(as.integer(switch(denom,
                                            group = group_N,
                                            nonmissing = group_N,
                                            responders = n_resp
      )), length(bl$vars))
      
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
        n_strata_valid = as.integer(n_strata_valid_vec),
        pct_valid      = ifelse(n_strata_valid_vec > 0L, n_level[bl$vars] / n_strata_valid_vec, NA_real_),
        
        # tags & label
        var_type       = "categorical",
        class          = "checkbox",
        label          = bl$overall_lbl                  # << display label for the whole block
      )
      
      # Optional “Any selected” row (synthetic binary) 
      if (isTRUE(opts$show_any)) {
        # any_count        <- sum(rowSums(sel_mat, na.rm = TRUE) > 0L, na.rm = TRUE)
        any_count        <- n_resp
        any_nonmissing   <- sum(rowSums(!is.na(raw_mat)) > 0L, na.rm = TRUE)
        
        any_display_denom <- switch(denom,
                                    group      = group_N,
                                    responders = n_resp,
                                    nonmissing = group_N)
        
        any_valid_denom <- switch(denom,
                                  group      = any_display_denom,
                                  responders = n_resp,
                                  nonmissing = any_nonmissing)
        
        # derive lowercase stem from the FIRST var in this block
        stem_l <- tolower(sub("___.*$", "", bl$vars[1]))
        
        df <- dplyr::bind_rows(
          df,
          tibble::tibble(
            strata         = factor(g, levels = strata_levels_all),
            var            = paste0(stem_l, "___any_selected"),  # << lower-case synthetic id
            level_var      = NA_character_,                      # << exclude from per-level p-values
            level          = "Any selected",
            n_level        = as.integer(any_count),
            n_strata       = as.integer(any_display_denom),
            pct            = ifelse(any_display_denom > 0, any_count / any_display_denom, NA_real_),
            
            n_level_valid  = as.integer(any_count),
            n_strata_valid = as.integer(any_valid_denom),
            pct_valid      = ifelse(any_valid_denom > 0, any_count / any_valid_denom, NA_real_),
            
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

# Checkbox p-values computed once per checkbox variable 
# tab: output from process_checkbox_blocks_strata() (has class == "checkbox")
# data: original data
# strata_var: name of strata column in `data` (string)
# blocks: list from prepare_checkbox_blocks()
# test: "auto" | "chisq" | "fisher" (kept for API compatibility; we compute all)
# p_adjust: multiple-testing adjustment method (e.g. "none","BH","holm",...)
add_pvalues_checkbox <- function(tab,
                                 data,
                                 strata_var,
                                 blocks,
                                 denom    = "group",
                                 test     = "auto",
                                 p_adjust = "none",
                                 B        = 2000) {
  
  if (!("class" %in% names(tab))) return(tab)
  if (!any(tab$class == "checkbox")) return(tab)
  
  denom <- match.arg(denom, c("group","nonmissing","responders"))
  
  per_var <- list()
  
  # strata factor once
  grp_all <- factor(data[[strata_var]])
  
  for (bl in blocks) {
    
    # block responder indicator: any non-missing in ANY checkbox var in the block
    # raw_mat <- as.data.frame(data[bl$vars], stringsAsFactors = FALSE)
    # is_resp <- rowSums(!is.na(raw_mat)) > 0L
    sel_mat <- as.data.frame(lapply(bl$vars, function(v)
      as.integer(data[[v]] == bl$select_txt[[v]])))
    names(sel_mat) <- bl$vars
    
    is_resp <- rowSums(sel_mat, na.rm = TRUE) > 0L   # responders = any selected
    
    # per checkbox variable (per-level) 
    for (v in bl$vars) {
      
      # Build Selected/Not selected without converting NA -> Not selected
      sel_chr <- rep(NA_character_, nrow(data))
      not_na  <- !is.na(data[[v]])
      sel_chr[not_na] <- ifelse(data[[v]][not_na] == bl$select_txt[[v]],
                                "Selected", "Not selected")
      sel <- factor(sel_chr, levels = c("Not selected","Selected"))
      
      idx <- switch(
        denom,
        group      = rep(TRUE, length(is_resp)),
        responders = is_resp,
        nonmissing = not_na
      )
      
      tbl <- table(grp_all[idx], sel[idx], useNA = "no")
      
      per_var[[length(per_var) + 1]] <- tibble::tibble(
        var                         = v,
        chisq_test                  = safe_chisq(tbl,  correct = TRUE,  simulate.p.value = FALSE),
        chisq_test_no_correction    = safe_chisq(tbl,  correct = FALSE, simulate.p.value = FALSE),
        chisq_test_simulated        = safe_chisq(tbl,  correct = TRUE,  simulate.p.value = TRUE,  B = B),
        fisher_test                 = safe_fisher(tbl, simulate.p.value = FALSE),
        fisher_test_simulated       = safe_fisher(tbl, simulate.p.value = TRUE, B = B),
        check_categorical_test      = flag_chisq_ok(tbl)
      )
    }
    
    # -synthetic "Any selected" variable 
    stem    <- sub("___.*$", "", bl$vars[[1]])
    var_any <- paste0(tolower(stem), "___any_selected")
    
    # any selected across the block (NA treated as 0 in the rowSums via na.rm=TRUE)
    sel_any <- factor(
      ifelse(
        rowSums(as.data.frame(lapply(bl$vars, function(v)
          as.integer(data[[v]] == bl$select_txt[[v]]))), na.rm = TRUE) > 0L,
        "Selected", "Not selected"
      ),
      levels = c("Not selected","Selected")
    )
    
    # For "responders": only those who responded to the block
    # For "nonmissing": use responders as the natural nonmissing for the block
    idx_any <- switch(
      denom,
      group      = rep(TRUE, length(is_resp)),
      responders = is_resp,
      nonmissing = is_resp
    )
    
    tbl_any <- table(grp_all[idx_any], sel_any[idx_any], useNA = "no")
    
    per_var[[length(per_var) + 1]] <- tibble::tibble(
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
    tests <- tests %>%
      dplyr::mutate(.stem = sub("___.*$", "", .data$var)) %>%
      dplyr::group_by(.data$.stem) %>%
      dplyr::mutate(
        chisq_test               = stats::p.adjust(.data$chisq_test,               method = p_adjust),
        chisq_test_no_correction = stats::p.adjust(.data$chisq_test_no_correction, method = p_adjust),
        chisq_test_simulated     = stats::p.adjust(.data$chisq_test_simulated,     method = p_adjust),
        fisher_test              = stats::p.adjust(.data$fisher_test,              method = p_adjust),
        fisher_test_simulated    = stats::p.adjust(.data$fisher_test_simulated,    method = p_adjust)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-.data$.stem)
  }
  
  tab %>%
    dplyr::left_join(tests, by = "var")
}




.default_checkbox_opts <- function() {
  list(
    denom    = "group",
    pvals    = "per_level",
    test     = "auto",
    p_adjust = "none",
    show_any = TRUE,
    note     = "Participants could select more than one option; percentages may exceed 100%."
  )
}

normalize_checkbox_opts <- function(x) {
  defaults <- list(
    denom    = "group",
    pvals    = "per_level",  # used only when strata is present
    test     = "auto",
    p_adjust = "none",
    show_any = TRUE,
    note     = "Participants could select more than one option; percentages may exceed 100%."
  )
  
  # Treat NULL or empty list as "use defaults"
  if (is.null(x) || (is.list(x) && length(x) == 0)) return(defaults)
  
  if (!is.list(x)) {
    stop("`checkbox_opts` must be a list or NULL.", call. = FALSE)
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
  
  opts$denom    <- match.arg(opts$denom,    c("group","nonmissing","responders"))
  opts$pvals    <- match.arg(opts$pvals,    c("none","per_level"))
  opts$test     <- match.arg(opts$test,     c("auto","chisq","fisher"))
  opts$p_adjust <- match.arg(opts$p_adjust, stats::p.adjust.methods)
  
  opts$show_any <- isTRUE(opts$show_any)
  opts
}