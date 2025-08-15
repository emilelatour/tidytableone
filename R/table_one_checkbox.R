
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
  denom <- match.arg(opts$denom, c("group","nonmissing","responders"))
  
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
      n_strata_valid_vec <- switch(
        denom,
        nonmissing = as.integer(nonmiss_per_level[bl$vars]),
        responders = rep(sum(rowSums(sel_mat, na.rm = TRUE) > 0L, na.rm = TRUE), length(bl$vars)),
        group      = rep(group_N, length(bl$vars))
      )
      
      # Display denominators (overall column denominators)
      n_strata_vec <- rep(group_N, length(bl$vars))
      
      # Sanity: names must align so we can index by bl$vars
      stopifnot(identical(names(n_level), bl$vars))
      
      # ---- Per-level rows (one per checkbox column) ----
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
      
      # ---- Optional “Any selected” row (synthetic binary) ----
      if (isTRUE(opts$show_any)) {
        any_count        <- sum(rowSums(sel_mat, na.rm = TRUE) > 0L, na.rm = TRUE)
        any_nonmissing   <- sum(rowSums(!is.na(raw_mat)) > 0L, na.rm = TRUE)
        
        any_display_denom <- switch(denom,
                                    group      = group_N,
                                    responders = any_count,
                                    nonmissing = group_N)
        any_valid_denom   <- switch(denom,
                                    group      = any_display_denom,
                                    responders = any_count,
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
# add_pvalues_checkbox <- function(tab,
#                                  data,
#                                  strata_var,
#                                  blocks,
#                                  test = c("auto","chisq","fisher"),
#                                  p_adjust = "none") {
# 
#   test <- match.arg(test)
# 
#   # Ensure columns exist to hold results
#   ensure_cols <- c("chisq_test","chisq_test_no_correction","chisq_test_simulated",
#                    "fisher_test","fisher_test_simulated","check_categorical_test",
#                    "p_value_level")
#   for (nm in ensure_cols) {
#     if (!nm %in% names(tab)) tab[[nm]] <- NA_real_
#   }
#   if (!"check_categorical_test" %in% names(tab)) {
#     tab$check_categorical_test <- NA_character_
#   }
# 
#   # Small helper: choose and run a categorical test robustly
#   run_cat_test <- function(x_tab, prefer = "auto") {
#     out <- list(chisq = NA_real_, chisq_nc = NA_real_, chisq_sim = NA_real_,
#                 fisher = NA_real_, fisher_sim = NA_real_, flag = NA_character_)
#     # dimensions
#     dims <- dim(x_tab)
#     # expected counts (for chi-square decisioning)
#     exp_try <- try(stats::chisq.test(x_tab, correct = TRUE), silent = TRUE)
#     if (!inherits(exp_try, "try-error")) {
#       exp_mat <- exp_try$expected
#       small_exp <- sum(exp_mat < 5, na.rm = TRUE)
#       frac_small <- small_exp / length(exp_mat)
#     } else {
#       exp_mat <- NULL
#       frac_small <- 1 # force cautious paths
#     }
# 
#     choose_chisq <- function(sim = FALSE, correct = TRUE) {
#       tt <- try(stats::chisq.test(x_tab, correct = correct, simulate.p.value = sim, B = 2000),
#                 silent = TRUE)
#       if (!inherits(tt, "try-error")) return(tt$p.value)
#       NA_real_
#     }
#     choose_fisher <- function(sim = FALSE) {
#       # Fisher is exact for 2x2; for larger tables it can be very slow.
#       # We allow simulate.p.value = TRUE for r x c when needed.
#       tt <- try(stats::fisher.test(x_tab, simulate.p.value = sim, B = 2000),
#                 silent = TRUE)
#       if (!inherits(tt, "try-error")) return(tt$p.value)
#       NA_real_
#     }
# 
#     # Decide the primary path
#     if (prefer == "chisq") {
#       # try chi-squared (with/without correction), fall back to simulated
#       out$chisq    <- choose_chisq(sim = FALSE, correct = TRUE)
#       out$chisq_nc <- choose_chisq(sim = FALSE, correct = FALSE)
#       if (is.na(out$chisq)) out$chisq <- choose_chisq(sim = TRUE, correct = TRUE)
#       if (is.na(out$chisq_nc)) out$chisq_nc <- choose_chisq(sim = TRUE, correct = FALSE)
#       out$flag <- if (!is.null(exp_mat) && (frac_small > 0.2)) "warning" else "ok"
#       # try fisher if 2x2 (cheap) else simulated fisher for rxc
#       if (all(dims == c(2,2))) {
#         out$fisher <- choose_fisher(sim = FALSE)
#       } else {
#         out$fisher_sim <- choose_fisher(sim = TRUE)
#       }
#       return(out)
#     }
# 
#     if (prefer == "fisher") {
#       if (all(dims == c(2,2))) {
#         out$fisher <- choose_fisher(sim = FALSE)
#       } else {
#         out$fisher_sim <- choose_fisher(sim = TRUE)
#       }
#       # also provide chi-square as a reference
#       out$chisq    <- choose_chisq(sim = FALSE, correct = TRUE)
#       out$chisq_nc <- choose_chisq(sim = FALSE, correct = FALSE)
#       if (is.na(out$chisq)) out$chisq <- choose_chisq(sim = TRUE, correct = TRUE)
#       if (is.na(out$chisq_nc)) out$chisq_nc <- choose_chisq(sim = TRUE, correct = FALSE)
#       out$flag <- if (!is.null(exp_mat) && (frac_small > 0.2)) "warning" else "ok"
#       return(out)
#     }
# 
#     # prefer == "auto"
#     if (all(dims == c(2,2))) {
#       # for 2x2, use Fisher if any expected < 5; else chi-square
#       if (!is.null(exp_mat) && any(exp_mat < 5)) {
#         out$fisher <- choose_fisher(sim = FALSE)
#       } else {
#         out$chisq    <- choose_chisq(sim = FALSE, correct = TRUE)
#         out$chisq_nc <- choose_chisq(sim = FALSE, correct = FALSE)
#       }
#       out$flag <- if (!is.null(exp_mat) && (frac_small > 0.2)) "warning" else "ok"
#     } else {
#       # r x c: use chi-square; if many small expected, add simulated variants
#       out$chisq    <- choose_chisq(sim = FALSE, correct = TRUE)
#       out$chisq_nc <- choose_chisq(sim = FALSE, correct = FALSE)
#       if (!is.null(exp_mat) && (frac_small > 0.2)) {
#         out$chisq_sim <- choose_chisq(sim = TRUE, correct = FALSE)
#         out$flag <- "warning"
#       } else {
#         out$flag <- "ok"
#       }
#       # also try simulated Fisher as a backup (can be slow)
#       out$fisher_sim <- choose_fisher(sim = TRUE)
#     }
#     out
#   }
# 
#   # We’ll optionally collect p-values per block to adjust if requested
#   collect_for_adjust <- list()
# 
#   for (bl_name in names(blocks)) {
#     bl <- blocks[[bl_name]]
# 
#     # build vectors for each level
#     # working subset: drop NA strata
#     d0 <- data[!is.na(data[[strata_var]]), , drop = FALSE]
#     grp <- factor(d0[[strata_var]])
#     # map per-level rows (selected vs not)
#     pvals_this_block <- list()
# 
#     # per-level rows
#     for (level_var in bl$vars) {
#       # selected indicator from one checkbox column
#       sel_raw <- d0[[level_var]]
#       # keep rows where the checkbox value is not NA
#       keep <- !is.na(sel_raw)
#       if (!any(keep)) next
#       sel <- as.integer(sel_raw[keep] == bl$select_txt[[level_var]])
#       grp_k <- grp[keep]
# 
#       # If any group has zero count after filtering, drop those levels to keep tests defined
#       grp_k <- droplevels(grp_k)
#       if (nlevels(grp_k) < 2) next  # not testable
# 
#       # Build 2 x G table: selected vs not by group
#       x_tab <- table(factor(sel, levels = c(0,1), labels = c("No","Yes")), grp_k, useNA = "no")
# 
#       # run the test
#       res <- run_cat_test(x_tab, prefer = test)
# 
#       # stash p-values to inject later
#       pvals_this_block[[level_var]] <- list(
#         chisq    = res$chisq,
#         chisq_nc = res$chisq_nc,
#         chisq_sim= res$chisq_sim,
#         fisher   = res$fisher,
#         fisher_sim = res$fisher_sim,
#         flag = res$flag
#       )
#     }
# 
#     # "Any selected" row (if present)
#     if (isTRUE(bl$has_any_selected)) {
#       raw_mat <- as.data.frame(d0[bl$vars], stringsAsFactors = FALSE)
#       # rows with at least one non-missing checkbox in this block
#       keep_any <- rowSums(!is.na(raw_mat)) > 0
#       if (any(keep_any)) {
#         grp_a <- droplevels(grp[keep_any])
#         any_sel <- rowSums(mapply(function(v) as.integer(d0[[v]] == bl$select_txt[[v]]),
#                                   bl$vars, SIMPLIFY = TRUE)) > 0
#         x_tab <- table(factor(any_sel[keep_any], levels = c(FALSE, TRUE), labels = c("No", "Yes")),
#                        grp_a, useNA = "no")
#         res <- run_cat_test(x_tab, prefer = test)
#         pvals_this_block[["__ANY__"]] <- list(
#           chisq = res$chisq, chisq_nc = res$chisq_nc, chisq_sim = res$chisq_sim,
#           fisher = res$fisher, fisher_sim = res$fisher_sim, flag = res$flag
#         )
#       }
#     }
# 
#     # Optional p-adjust across levels in this block
#     if (!identical(p_adjust, "none") && length(pvals_this_block) > 0) {
#       raw_ps <- vapply(pvals_this_block, function(z) {
#         # prefer fisher, else chisq, else simulated variants
#         suppressWarnings(
#           if (!is.na(z$fisher)) z$fisher else
#           if (!is.na(z$chisq)) z$chisq else
#           if (!is.na(z$fisher_sim)) z$fisher_sim else
#           z$chisq_sim
#         )
#       }, numeric(1))
#       adj_ps <- stats::p.adjust(raw_ps, method = p_adjust)
#       i <- 1L
#       for (nm in names(pvals_this_block)) {
#         pvals_this_block[[nm]]$p_adj <- adj_ps[i]
#         i <- i + 1L
#       }
#     }
# 
#     # Inject into `tab`
#     # per-level rows
#     for (level_var in bl$vars) {
#       if (!length(pvals_this_block[[level_var]])) next
#       idx <- which(tab$var %in% level_var)  # var == original checkbox column name
#       if (length(idx) == 0) next
#       z <- pvals_this_block[[level_var]]
#       tab$chisq_test[idx]               <- z$chisq
#       tab$chisq_test_no_correction[idx] <- z$chisq_nc
#       tab$chisq_test_simulated[idx]     <- z$chisq_sim
#       tab$fisher_test[idx]              <- z$fisher
#       tab$fisher_test_simulated[idx]    <- z$fisher_sim
#       tab$check_categorical_test[idx]   <- z$flag
#       # single convenience column:
#       tab$p_value_level[idx] <- suppressWarnings(
#         if (!is.na(z$fisher)) z$fisher else
#         if (!is.na(z$chisq))  z$chisq  else
#         if (!is.na(z$fisher_sim)) z$fisher_sim else
#         z$chisq_sim
#       )
#       # if we adjusted:
#       if (!is.null(z$p_adj)) {
#         tab$p_value_level[idx] <- z$p_adj
#       }
#     }
# 
#     # any-selected row
#     if (length(pvals_this_block[["__ANY__"]])) {
#       z <- pvals_this_block[["__ANY__"]]
#       idx <- which(tab$var %in% paste0(bl$stem, "___any_selected"))
#       if (length(idx) > 0) {
#         tab$chisq_test[idx]               <- z$chisq
#         tab$chisq_test_no_correction[idx] <- z$chisq_nc
#         tab$chisq_test_simulated[idx]     <- z$chisq_sim
#         tab$fisher_test[idx]              <- z$fisher
#         tab$fisher_test_simulated[idx]    <- z$fisher_sim
#         tab$check_categorical_test[idx]   <- z$flag
#         tab$p_value_level[idx] <- suppressWarnings(
#           if (!is.na(z$fisher)) z$fisher else
#           if (!is.na(z$chisq))  z$chisq  else
#           if (!is.na(z$fisher_sim)) z$fisher_sim else
#           z$chisq_sim
#         )
#         if (!is.null(z$p_adj)) tab$p_value_level[idx] <- z$p_adj
#       }
#     }
#   }
# 
#   tab
# }


# ---- Checkbox p-values computed once per checkbox variable -------------------
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
                                 test     = "auto",
                                 p_adjust = "none",
                                 B        = 2000) {
  
  if (!("class" %in% names(tab))) return(tab)
  if (!any(tab$class == "checkbox")) return(tab)
  
  
  
  # Build a tidy frame of per-variable tests
  per_var <- list()
  
  for (bl in blocks) {
    # build 2×2 tables for each checkbox variable (Selected vs Not selected)
    for (v in bl$vars) {
      # strata (factor) and selected flag
      grp <- factor(data[[strata_var]])
      sel <- factor(ifelse(data[[v]] == bl$select_txt[[v]], "Selected", "Not selected"),
                    levels = c("Not selected","Selected"))
      tbl <- table(grp, sel, useNA = "no")
      
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
    
    # optional "Any selected" synthetic variable
    if (isTRUE(bl$show_any)) {
      stem <- sub("___.*$", "", bl$vars[[1]])
      var_any <- paste0(tolower(stem), "___any_selected")
      
      # any selected across the block
      sel_any <- factor(
        ifelse(rowSums(as.data.frame(lapply(bl$vars,
                                            function(v) as.integer(data[[v]] == bl$select_txt[[v]]))),
                       na.rm = TRUE) > 0L, "Selected", "Not selected"),
        levels = c("Not selected","Selected")
      )
      grp <- factor(data[[strata_var]])
      tbl_any <- table(grp, sel_any, useNA = "no")
      
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
  }
  
  tests <- dplyr::bind_rows(per_var)
  
  # optional multiple-comparison adjustment within each checkbox block
  if (!identical(p_adjust, "none")) {
    # infer block stem per var, adjust inside stems independently
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
  
  # Join onto checkbox rows; repeat the same p-values across all strata/levels
  tab %>%
    dplyr::left_join(tests, by = "var")
}
