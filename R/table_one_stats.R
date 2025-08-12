
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
cat_check <- function(tab) {
  
  dplyr::if_else(testit::has_warning(chisq.test(tab)),
                 "warning",
                 "ok")
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
  
  tryCatch(chisq.test(tab,
                      correct = correct,
                      simulate.p.value = simulate.p.value,
                      B = B)  %>%
             purrr::pluck(., "p.value"),
           error = function(err) NA)
  
}


# calc_chisq_test (no correction)
calc_chisq_test_no_correct <- function(tab,
                                       correct = FALSE,
                                       simulate.p.value = FALSE,
                                       B = 2000) {
  
  tryCatch(chisq.test(tab,
                      correct = correct,
                      simulate.p.value = simulate.p.value,
                      B = B)  %>%
             purrr::pluck(., "p.value"),
           error = function(err) NA)
  
}


# calc_chisq_test (simulated p)
calc_chisq_test_sim_p <- function(tab,
                                  correct = TRUE,
                                  simulate.p.value = TRUE,
                                  B = 2000) {
  
  tryCatch(chisq.test(tab,
                      correct = correct,
                      simulate.p.value = simulate.p.value,
                      B = B)  %>%
             purrr::pluck(., "p.value"),
           error = function(err) NA)
  
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