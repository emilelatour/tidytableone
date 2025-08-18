
#### Stats helper functions --------------------------------

# get_smd
# a wrapper around tableone package function

get_smd <- function(data,
                    strata = NULL,
                    vars) {
  
  if (is.null(strata)) {
    
    tibble::tibble(var = vars,
                   smd = NA_real_)
    
  } else {
    
    tableone::CreateTableOne(vars = vars,
                             strata = strata,
                             data = data) %>%
      tableone::ExtractSmd(.) %>%
      tibble::as_tibble(.,
                        rownames = "var") %>%
      dplyr::select(1:2) %>%
      dplyr::rename(smd = 2)
    
  }
  
}

# Compute SMDs for all checkbox variables (including any_selected) across ANY # of strata
# Returns tibble: var, smd  -- ready to left_join() onto your tab
get_smd_checkbox <- function(data, strata, blocks, na_level = "(Missing)") {
  # no strata -> nothing to compute
  if (is.null(strata)) {
    return(tibble::tibble(var = character(0), smd = numeric(0)))
  }

  # normalize strata to a plain string name
  s <- rlang::as_name(rlang::ensym(strata))

  # work on a copy; make sure strata is a factor and NAs get an explicit level
  df <- data
  df[[s]] <- as.factor(df[[s]])
  if (!is.null(na_level)) {
    df[[s]] <- forcats::fct_na_value_to_level(f = df[[s]], 
                                              level = na_level)
  }

  out_vars <- character(0)

  for (bl in blocks) {
    # per-level binaries (Selected vs Not selected), reusing original var names
    for (v in bl$vars) {
      df[[v]] <- factor(
        ifelse(df[[v]] == bl$select_txt[[v]], "Selected", "Not selected"),
        levels = c("Not selected", "Selected")
      )
      out_vars <- c(out_vars, v)
    }

    # synthetic any_selected for the block (lower-case stem, matches the table rows)
    stem_l  <- tolower(sub("___.*$", "", bl$vars[[1]]))
    var_any <- paste0(stem_l, "___any_selected")

    df[[var_any]] <- factor(
      ifelse(
        rowSums(as.data.frame(lapply(bl$vars, function(v)
          as.integer(df[[v]] == "Selected"))), na.rm = TRUE) > 0L,
        "Selected", "Not selected"
      ),
      levels = c("Not selected", "Selected")
    )
    out_vars <- c(out_vars, var_any)
  }

  # Use your existing wrapper to compute SMDs (handles >2 groups)
  get_smd(
    data   = df,
    strata = s,
    vars   = unique(out_vars)
  )
}


# shapiro test
calc_shapiro_test <- function(var) {
  tryCatch(shapiro.test(var)[["p.value"]],
           error = function(err) NA)
}


# Kolmogorov-Smirnov Tests
calc_ks_test <- function(var) {
  tryCatch(ks.test(unique(var), "pnorm")[["p.value"]],
           error = function(err) NA)
}


# anderson-darling test
# The K-S test is not suitable when estimating the parameters from the data. You
# can use the following code, which relies on the Anderson-Darling test for
# normality, and does not require you to supply the mean and the stddev. This
# test is stronger in accuracy than the Lilliefors test.
calc_ad_test <- function(var) {
  tryCatch(nortest::ad.test(var)[["p.value"]],
           error = function(err) NA)
}


# oneway test (unequal variances)
calc_oneway_test_unequal <- function(data, form, var.equal = FALSE) {
  
  tryCatch(oneway.test(formula = as.formula(form),
                       data = data,
                       var.equal = var.equal) %>%
             purrr::pluck(., "p.value"),
           error = function(err) NA)
  
  
  
}


# oneway test (equal variances)
calc_oneway_test_equal <- function(data, form, var.equal = TRUE) {
  
  tryCatch(oneway.test(formula = as.formula(form),
                       data = data,
                       var.equal = var.equal) %>%
             purrr::pluck(., "p.value"),
           error = function(err) NA)
  
  
  
}


# kruskal test
calc_kruskal_test <- function(data, form) {
  
  tryCatch(kruskal.test(formula = as.formula(form), data = data) %>%
             purrr::pluck(., "p.value"),
           error = function(err) NA)
  
}


# bartlett test
calc_bartlett_test <- function(data, form) {
  
  tryCatch(bartlett.test(formula = as.formula(form), data = data) %>%
             purrr::pluck(., "p.value"),
           error = function(err) NA)
  
}


# levene test
calc_levene_test <- function(data, form) {
  
  tryCatch(car::leveneTest(as.formula(form), data = data) %>%
             purrr::pluck(., "Pr(>F)", 1),
           error = function(err) NA)
  
}


# cat_check
# cat_check <- function(tab) {
#   
#   dplyr::if_else(testit::has_warning(chisq.test(tab)),
#                  "warning",
#                  "ok")
# }
# Flag cases where chi-square approximation may be shaky
# (any expected < 1) OR (>20% of expected < 5)
cat_check <- function(tab) {
  tab <- as.matrix(tab)
  n   <- sum(tab)
  rs  <- rowSums(tab)
  cs  <- colSums(tab)
  exp <- (rs %o% cs) / n  # expected counts, no warning triggered
  
  if (any(exp < 1, na.rm = TRUE) || mean(exp < 5, na.rm = TRUE) > 0.20) "warning" else "ok"
}

# calc_fisher_test
calc_fisher_test <- function(tab,
                             simulate.p.value = FALSE,
                             B = 2000) {
  tryCatch(fisher.test(tab,
                       alternative = "two.sided",
                       conf.int = FALSE,
                       simulate.p.value = simulate.p.value,
                       B = B) %>%
             purrr::pluck(., "p.value"),
           error = function(err) NA)
}


# calc_fisher_test, simulate p-value
calc_fisher_test_sim_p <- function(tab,
                                   simulate.p.value = TRUE,
                                   B = 2000) {
  tryCatch(fisher.test(tab,
                       alternative = "two.sided",
                       conf.int = FALSE,
                       simulate.p.value = simulate.p.value,
                       B = B) %>%
             purrr::pluck(., "p.value"),
           error = function(err) NA)
}


# calc_chisq_test
calc_chisq_test <- function(tab,
                            correct = TRUE,
                            simulate.p.value = FALSE,
                            B = 2000) {
  
  safe_chisq(tbl = tab, 
             correct = correct, 
             simulate.p.value = simulate.p.value, 
             B = B)
  
}


# calc_chisq_test (no correction)
calc_chisq_test_no_correct <- function(tab,
                                       correct = FALSE,
                                       simulate.p.value = FALSE,
                                       B = 2000) {
  
  safe_chisq(tbl = tab, 
             correct = correct, 
             simulate.p.value = simulate.p.value, 
             B = B)
  
}


# calc_chisq_test (simulated p)
calc_chisq_test_sim_p <- function(tab,
                                  correct = TRUE,
                                  simulate.p.value = TRUE,
                                  B = 2000) {
  
  safe_chisq(tbl = tab, 
             correct = correct, 
             simulate.p.value = simulate.p.value, 
             B = B)
  
}


chisq_expected_flag <- function(tab) {
  expd <- tryCatch(
    suppressWarnings(stats::chisq.test(tab)$expected),
    error = function(e) NULL
  )
  # Classic rule-of-thumb: any expected < 5
  if (is.null(expd)) TRUE else any(expd < 5, na.rm = TRUE)
}


# calc_cat_htest
# calc_cat_htest <- function(data, strata, vars, b_replicates) {
#   
#   tibble::tibble(strata = strata,
#                  var = vars) %>%
#     mutate(x = purrr::map(.x = strata,
#                           .f = ~ purrr::pluck(data, .x)),
#            y = purrr::map(.x = var,
#                           .f = ~ purrr::pluck(data, .x))) %>%
#     dplyr::filter(strata != var) %>%
#     mutate(tab = purrr::map2(.x = x,
#                              .y = y,
#                              .f = ~ table(.x, .y)),
#            chisq_test = purrr::map_dbl(.x = tab,
#                                        .f = ~ calc_chisq_test(.x)),
#            chisq_test_no_correction = purrr::map_dbl(.x = tab,
#                                                      .f = ~ calc_chisq_test_no_correct(.x)),
#            chisq_test_simulated = purrr::map_dbl(.x = tab,
#                                                  .f = ~ calc_chisq_test_sim_p(.x,
#                                                                               B = b_replicates)),
#            
#            fisher_test = purrr::map_dbl(.x = tab,
#                                         .f = ~ calc_fisher_test(.x)),
#            fisher_test_simulated = purrr::map_dbl(.x = tab,
#                                                   .f = ~ calc_fisher_test_sim_p(.x,
#                                                                                 B = b_replicates)),
#            check_categorical_test = purrr::map_chr(.x = tab,
#                                                    .f = ~ cat_check(.x))) %>%
#     dplyr::select(var,
#                   chisq_test,
#                   chisq_test_no_correction,
#                   chisq_test_simulated,
#                   fisher_test,
#                   fisher_test_simulated,
#                   check_categorical_test)
# }
calc_cat_htest <- function(data, strata, vars, b_replicates) {
  
  tibble::tibble(strata = strata,
                 var = vars) %>%
    mutate(x = purrr::map(.x = strata, ~ purrr::pluck(data, .x)),
           y = purrr::map(.x = var,    ~ purrr::pluck(data, .x))) %>%
    dplyr::filter(strata != var) %>%
    mutate(
      tab = purrr::map2(x, y, ~ table(.x, .y)),
      
      # Our own assumption flag (no base warnings involved)
      check_categorical_test = purrr::map_chr(tab, ~ if (chisq_expected_flag(.x)) "warning" else "ok"),
      
      # Suppress the base warning noise; we expose issues via the flag above
      chisq_test                = purrr::map_dbl(tab, ~ suppressWarnings(calc_chisq_test(.x))),
      chisq_test_no_correction  = purrr::map_dbl(tab, ~ suppressWarnings(calc_chisq_test_no_correct(.x))),
      chisq_test_simulated      = purrr::map_dbl(tab, ~ suppressWarnings(calc_chisq_test_sim_p(.x, B = b_replicates))),
      fisher_test               = purrr::map_dbl(tab, ~ calc_fisher_test(.x)),
      fisher_test_simulated     = purrr::map_dbl(tab, ~ calc_fisher_test_sim_p(.x, B = b_replicates))
    ) %>%
    dplyr::select(var,
                  chisq_test,
                  chisq_test_no_correction,
                  chisq_test_simulated,
                  fisher_test,
                  fisher_test_simulated,
                  check_categorical_test)
}


# calc_con_htest
calc_con_htest <- function(data, strata, vars) {
  
  tibble::tibble(strata = strata,
                 var = vars) %>%
    mutate(form = glue::glue("{var} ~ {strata}")) %>%
    mutate(oneway_test_unequal_var =
             purrr::map_dbl(.x = form,
                            .f = ~ calc_oneway_test_unequal(data = data,
                                                            form = .x)),
           oneway_test_equal_var =
             purrr::map_dbl(.x = form,
                            .f = ~ calc_oneway_test_equal(data = data,
                                                          form = .x)),
           kruskal_test =
             purrr::map_dbl(.x = form,
                            .f = ~ calc_kruskal_test(data = data,
                                                     form = .x)),
           bartlett_test =
             purrr::map_dbl(.x = form,
                            .f = ~ calc_bartlett_test(data = data,
                                                      form = .x)),
           levene_test =
             purrr::map_dbl(.x = form,
                            .f = ~ calc_levene_test(data = data,
                                                    form = .x))) %>%
    dplyr::select(var,
                  oneway_test_unequal_var,
                  oneway_test_equal_var,
                  kruskal_test,
                  bartlett_test,
                  levene_test)
}


# custom_min custom_max
custom_min <- function(x, na.rm = TRUE) {
  
  if (is.integer(x)) {
    
    dflt_miss <- NA_integer_
    
  } else if (is.numeric(x)) {
    
    dflt_miss <- NA_real_
    
  } else {
    
    dflt_miss <- Inf
    
  }
  
  if (all(is.na(x))) {
    dflt_miss
  } else {
    min(x, na.rm = na.rm)
  }
}

custom_max <- function(x, na.rm = TRUE) {
  
  if (is.integer(x)) {
    
    dflt_miss <- NA_integer_
    
  } else if (is.numeric(x)) {
    
    dflt_miss <- NA_real_
    
  } else {
    
    dflt_miss <- Inf
    
  }
  
  if (all(is.na(x))) {
    dflt_miss
  } else {
    max(x, na.rm = na.rm)
  }
}


#### Safe versions -------------------------------- 

# Quietly return chisq p-value (or NA) with optional Yates correction / simulation
# safe_chisq <- function(tab, correct = TRUE, simulate.p.value = FALSE, B = 2000) {
#   res <- tryCatch(
#     suppressWarnings(stats::chisq.test(tab, correct = correct,
#                                        simulate.p.value = simulate.p.value, B = B)),
#     error = function(e) NULL
#   )
#   if (is.null(res)) NA_real_ else as.numeric(res$p.value)
# }

# safe_chisq <- function(tab, correct = TRUE, simulate.p.value = FALSE, B = 2000) {
#   
#   if (is.null(tab) && !is.null(tbl)) tab <- tbl
#   
#   withCallingHandlers(
#     {
#       res <- try(
#         stats::chisq.test(tab,
#                           correct = correct,
#                           simulate.p.value = simulate.p.value,
#                           B = B),
#         silent = TRUE
#       )
#       if (inherits(res, "try-error")) return(NA_real_)
#       as.numeric(res$p.value)
#     },
#     warning = function(w) {
#       msg <- conditionMessage(w)
#       if (grepl("Chi-squared approximation may be incorrect", msg, fixed = TRUE)) {
#         invokeRestart("muffleWarning")
#       }
#       # otherwise let unrelated warnings bubble up
#     }
#   )
# }
# 
# safe_fisher <- function(tbl, simulate.p.value = FALSE, B = 2000) {
#   out <- try(stats::fisher.test(tbl, simulate.p.value = simulate.p.value, B = B),
#              silent = TRUE)
#   if (inherits(out, "try-error")) return(NA_real_)
#   as.numeric(out$p.value)
# }
# 


# Silently compute chi-squared p-value; never emit base warnings.
safe_chisq <- function(tbl, correct = TRUE, simulate.p.value = FALSE, B = 2000) {
  # normalize & structural guards
  if (length(dim(tbl)) != 2L) return(NA_real_)
  if (anyNA(tbl)) tbl[is.na(tbl)] <- 0
  if (sum(tbl) == 0L) return(NA_real_)
  if (any(rowSums(tbl) == 0L) || any(colSums(tbl) == 0L)) return(NA_real_)  # <- key line

  out <- try(
    suppressWarnings(
      stats::chisq.test(tbl, correct = correct, simulate.p.value = simulate.p.value, B = B)
    ),
    silent = TRUE
  )
  if (inherits(out, "try-error")) return(NA_real_)
  out$p.value
}

# Silently compute Fisher’s p-value; never emit base warnings.
safe_fisher <- function(tbl, simulate.p.value = FALSE, B = 2000) {
  if (length(dim(tbl)) != 2L) return(NA_real_)
  if (anyNA(tbl)) tbl[is.na(tbl)] <- 0
  if (sum(tbl) == 0L) return(NA_real_)
  if (nrow(tbl) < 2L || ncol(tbl) < 2L) return(NA_real_)

  out <- try(
    suppressWarnings(
      stats::fisher.test(tbl, simulate.p.value = simulate.p.value, B = B)
    ),
    silent = TRUE
  )
  if (inherits(out, "try-error")) return(NA_real_)
  out$p.value
}

flag_chisq_ok <- function(tbl) {
  # Chi-square rule of thumb: all expected >= 5 and no zero rows/cols
  out <- try(suppressWarnings(stats::chisq.test(tbl, correct = FALSE)), silent = TRUE)
  if (inherits(out, "try-error")) return("warning")
  exp_ok <- all(out$expected >= 5, na.rm = TRUE)
  has_zero_row <- any(rowSums(tbl) == 0)
  has_zero_col <- any(colSums(tbl) == 0)
  if (exp_ok && !has_zero_row && !has_zero_col) "ok" else "warning"
}