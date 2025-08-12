
# TODO
# Add a row to output for N
# include missing strata?
# name for missing level?

#' @title
#' Tidy table one
#'
#' @description
#' Creates a tidy data frame of the results that can go into a "Table 1" of
#' summary descriptive statistics of a study sample. Inspiration for this is owed
#' to the `tableone` package by Kazuki Yoshida.
#'
#' @param data A data frame or tibble containing the variables to be summarized.
#' @param strata Character vector of the stratifying (grouping) variable.
#' @param vars Character vector of the variable names to be summarized. If
#'   empty, then all variables in the given data frame are used.
#' @param na_level Character string of the text to replace `NA` in the strata
#'   variable, if any exist.
#' @param b_replicates an integer specifying the number of replicates used in
#'   the Monte Carlo test for Fisher's Exact test and Chi-square test.
#' @param ... Additional arguments. Not used.
#'
#' @importFrom car leveneTest
#' @importFrom dplyr bind_rows
#' @importFrom dplyr count
#' @importFrom dplyr count
#' @importFrom dplyr distinct
#' @importFrom dplyr everything
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_if
#' @importFrom dplyr n
#' @importFrom dplyr n_distinct
#' @importFrom dplyr one_of
#' @importFrom dplyr pull
#' @importFrom dplyr rename
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom dplyr transmute
#' @importFrom dplyr ungroup
#' @importFrom forcats fct_na_value_to_level
#' @importFrom glue glue
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @importFrom purrr map_dbl
#' @importFrom purrr map2
#' @importFrom nortest ad.test
#' @importFrom purrr pluck
#' @importFrom rlang ensym
#' @importFrom stats as.formula
#' @importFrom stats bartlett.test
#' @importFrom stats chisq.test
#' @importFrom stats fisher.test
#' @importFrom stats kruskal.test
#' @importFrom stats ks.test
#' @importFrom stats oneway.test
#' @importFrom stats quantile
#' @importFrom stats sd
#' @importFrom stats shapiro.test
#' @importFrom tableone CreateTableOne
#' @importFrom tableone ExtractSmd
#' @importFrom testit has_warning
#' @importFrom tibble as_tibble
#' @importFrom tibble tibble
#' @importFrom tidyr complete
#' @importFrom tidyr gather
#'
#' @return
#' A tibble with the following results
#' \describe{
#'   \item{strata}{Level of the stratifying variable}
#'   \item{var}{Variable/column name}
#'   \item{n}{Number of records}
#'   \item{n_distinct}{Numer of distinct values}
#'   \item{complete}{Number of non-missing observations}
#'   \item{missing}{Number of missing observations}
#'   \item{mean}{Mean}
#'   \item{sd}{Standard deviation}
#'   \item{p0}{Minimum}
#'   \item{p25}{25th percentile}
#'   \item{p50}{Median}
#'   \item{p75}{75th percentile}
#'   \item{p100}{Maximum}
#'   \item{cv}{Coefficient of variation}
#'   \item{shapiro_test}{Shapiro-Wilkes test: p-value}
#'   \item{ks_test}{Kolmogorov-Smirnov test: p-value}
#'   \item{ad_test}{Anderson-Darling test for normality: p-value}
#'   \item{level}{Level of the variable}
#'   \item{n_level}{Total number in the variable's group}
#'   \item{n_strata}{Total number in the variable group and strata}
#'   \item{chisq_test}{Chi square test: p-value, with continuity correction}
#'   \item{chisq_test_no_correction}{Chi square test: p-value, without continuity correction}
#'   \item{chisq_test_simulated}{Chi square test: p-value: simulated p-value}
#'   \item{fisher_test}{Fisher's exact test: p-value}
#'   \item{fisher_test_simulated}{Fisher's exact test: simulated p-value}
#'   \item{check_categorical_test}{Is Chi square OK? Consider Fisher}
#'   \item{oneway_test_unequal_var}{Oneway anova test: p-value, equivalent to t-test when only 2 groups, unequal variances}
#'   \item{oneway_test_equal_var}{Oneway anova test: p-value, equivalent to t-test when only 2 groups, equal variances}
#'   \item{kruskal_test}{Kruskal-Wallis Rank Sum Test: p-value, equivalent to Mann-Whitney U test when only 2 groups}
#'   \item{bartlett_test}{Bartlett's test for homogeneity of variances: p-value}
#'   \item{levene_test}{Levene's test for homogeneity of variances: p-value}
#'   \item{smd}{Standarized mean difference for all pairwise comparisons}
#' }
#' @export
#'
#' @examples
#' library(dplyr)
#' 
#' tab1 <- create_tidy_table_one(data = pbc_mayo,
#'                               strata = "trt",
#'                               vars = c("time",
#'                                        "status",
#'                                        "trt",
#'                                        "age",
#'                                        "sex",
#'                                        "ascites",
#'                                        "hepato",
#'                                        "spiders",
#'                                        "edema",
#'                                        "bili",
#'                                        "chol",
#'                                        "albumin",
#'                                        "copper",
#'                                        "alk_phos",
#'                                        "ast",
#'                                        "trig",
#'                                        "platelet",
#'                                        "protime",
#'                                        "stage"))
#' 
#' dplyr::glimpse(tab1)
#' 
#' 
#' library(ggplot2)  # diamonds data set
#' 
#' #### With strata --------------------------------
#' 
#' # Continuous and categoical
#' (t1 <- create_tidy_table_one(data = diamonds,
#'                              strata = "cut",
#'                              vars = c("carat",
#'                                       # Don't have to include the strata variable
#'                                       # "cut",
#'                                       "color",
#'                                       "clarity",
#'                                       "depth",
#'                                       "table",
#'                                       "price"))
#' )
#' 
#' dplyr::glimpse(t1)
#' 
#' t1 |>
#'   adorn_tidytableone()
#' 
#' 
#' # Continuous only
#' (t2 <- create_tidy_table_one(data = diamonds,
#'                              strata = "cut",
#'                              vars = c("carat"))
#' )
#' 
#' t2 |>
#'   adorn_tidytableone()
#' 
#' 
#' 
#' # Categorical only
#' (t3 <- create_tidy_table_one(data = diamonds,
#'                              strata = "cut",
#'                              vars = c("color")))
#' 
#' t3 |>
#'   adorn_tidytableone()
#' 
#' 
#' #### Withou strata --------------------------------
#' 
#' # Continuous and categoical
#' (t1 <- create_tidy_table_one(data = diamonds,
#'                              strata = NULL,
#'                              vars = c("carat",
#'                                       # Don't have to include the strata variable
#'                                       # "cut",
#'                                       "color",
#'                                       "clarity",
#'                                       "depth",
#'                                       "table",
#'                                       "price"))
#' )
#' 
#' t1 |>
#'   adorn_tidytableone()
#' 
#' 
#' # Continuous only
#' (t2 <- create_tidy_table_one(data = diamonds,
#'                              strata = NULL,
#'                              vars = c("carat"))
#' )
#' 
#' t2 |>
#'   adorn_tidytableone()
#' 
#' 
#' 
#' # Categorical only
#' (t3 <- create_tidy_table_one(data = diamonds,
#'                              strata = NULL,
#'                              vars = c("color")))
#' 
#' t3 |>
#'   adorn_tidytableone()

create_tidy_table_one <- function(data,
                                  strata = NULL,
                                  vars,
                                  na_level = "(Missing)",
                                  b_replicates = 2000, ...) {
  
  # Silence no visible binding for global variable
  dat <- res <- n_level_valid <- n_strata_valid <- label <- sort1 <- NULL
  
  
  # Handle no strata case by calling the no strata version of the function
  if (is.null(strata)) {
    
    res_stats <- create_tidy_table_one_no_strata(data = data,
                                                 strata = NULL,
                                                 vars = vars,
                                                 na_level = na_level,
                                                 b_replicates = b_replicates, ...)
    
    return(res_stats)
    
  }
  
  # Convert ordered factors to regular factors
  data <- data %>%
    dplyr::mutate_if(.tbl = .,
                     .predicate = ~ ("ordered" %in% class(.)),
                     .funs = ~ factor(., ordered = FALSE))
  
  
  # Convert the 'strata' argument to a symbol for use in tidy evaluation
  strata_sym <- rlang::ensym(strata)
  
  
  # Handle missing values in the strata
  
  if (any(is.na(purrr::pluck(data, strata)))) {
    
    df_omit_na_strata <- data %>%
      dplyr::filter(!is.na(!! strata_sym))
    
    data <- data %>%
      mutate(!! strata_sym := forcats::fct_na_value_to_level(!! strata_sym,
                                                             level = na_level))
  } else {
    
    df_omit_na_strata <- data
    
  }
  
  # Use all variables if vars is not provided
  if (missing(vars)) {
    vars <- names(data)
  }
  
  
  #### Get variable info --------------------------------
  
  # Extract variable labels for later use
  var_lbls <- tibble::tibble(var = names(data)) |>
    mutate(label = purrr::map_chr(.x = data[, var],
                                  .f = ~ get_var_labels(x = .x)))
  
  # Get variable types and other meta-information
  var_info <- get_var_info(data = data,
                           .vars = vars)
  
  # Create sorting variables to maintain order in the output
  var_info <- var_info |>
    mutate(sort1 = cumsum(var != dplyr::lag(var, default = dplyr::first(var))),
           sort1 = sort1 + 1) |>
    group_by(var) |>
    mutate(sort2 = dplyr::row_number()) |>
    ungroup()
  
  # Identify categorical and continuous variables
  cat_vars <- var_info |>
    dplyr::filter(var_type == "categorical") |>
    dplyr::pull(var) |>
    unique() |>
    setdiff(strata)
  
  con_vars <- var_info |>
    dplyr::filter(var_type == "continuous") |>
    dplyr::pull(var) |>
    unique() |>
    setdiff(strata)
  
  
  
  #### Get Categorical and Continuous Stats --------------------------------
  
  res_stats <- list()
  
  if (length(con_vars) > 0) {
    
    con_stats <- process_continuous(data,
                                    strata_sym,
                                    con_vars)
    
    res_stats <- dplyr::bind_rows(res_stats,
                                  con_stats)
    
  }
  
  if (length(cat_vars) > 0) {
    
    cat_stats <- process_categorical(data,
                                     strata_sym,
                                     cat_vars)
    
    res_stats <- dplyr::bind_rows(res_stats,
                                  cat_stats)
    
    
  }
  
  
  
  
  #### Calc SMD --------------------------------
  
  smd_res <- get_smd(data = df_omit_na_strata,
                     strata = strata,
                     vars = vars)
  
  # 10: In StdDiff(variable = var, group = strataVar) :
  # Variable has only NA's in at least one stratum. na.rm turned off.
  
  
  #### Hypothesis tests --------------------------------
  
  htest_res <- list()
  
  
  if (length(cat_vars) > 0) {
    
    cat_stats <- calc_cat_htest(data = df_omit_na_strata,
                                strata = strata,
                                vars = cat_vars,
                                b_replicates = b_replicates)
    
    htest_res <- dplyr::bind_rows(htest_res,
                                  cat_stats)
  }
  
  if (length(con_vars) > 0) {
    
    con_stats <- calc_con_htest(data = df_omit_na_strata,
                                strata = strata,
                                vars = con_vars)
    
    htest_res <- dplyr::bind_rows(htest_res,
                                  con_stats)
    
  }
  
  
  #### Combine results, Clean up and arrange --------------------------------
  
  res_stats <- arrange_results(res_stats,
                               htest_res,
                               smd_res,
                               var_info,
                               var_lbls, 
                               vars,
                               cat_vars,
                               con_vars,
                               data,
                               strata,
                               strata_sym)
  
  
  #### Return results --------------------------------
  
  return(res_stats)
  
}



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
                                                   dplyr::one_of(.x))),
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

# Arrange results
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
  
  
  # Silence no visible binding for global variable
  sort1 <- sort2 <- label <- strata_var <- NULL
  
  
  # Get the factor levels for the strata
  if (is.factor(purrr::pluck(data, strata))) {
    strata_lvls <- c("Overall", levels(purrr::pluck(data, strata)))
  } else {
    strata_lvls <- c("Overall", unique(purrr::pluck(data, strata)))
  }
  
  # Combine the results and convert strata to factor
  res_stats <- res_stats |>
    dplyr::left_join(htest_res,
                     by = "var") %>%
    dplyr::rename("strata" = !! strata_sym) |>
    dplyr::left_join(smd_res,
                     by = "var") |>
    mutate(strata = factor(strata, levels = strata_lvls))
  
  
  
  
  # Add variable info (class, type) and labels to the results
  class_and_type <- var_info |>
    dplyr::select(-level,
                  -sort1,
                  -sort2) |>
    dplyr::distinct()
  
  res_stats <- res_stats |>
    dplyr::left_join(class_and_type,
                     by = "var")
  
  res_stats <- res_stats |>
    dplyr::left_join(var_lbls,
                     by = "var")
  
  # Arrange results by variable and level, maintaining the original order
  if (length(cat_vars) > 0) {
    
    sort_vars <- var_info |>
      dplyr::select(var, level, sort1, sort2)
    
    res_stats <- res_stats |>
      dplyr::left_join(sort_vars,
                       by = c("var", "level")) |>
      dplyr::arrange(sort1, sort2) |>
      mutate(var = forcats::fct_inorder(var),
             level = forcats::fct_inorder(level)) |>
      dplyr::select(-sort1, -sort2) |>
      dplyr::relocate(class,
                      var_type,
                      label,
                      .after = dplyr::everything())
  } else {
    
    sort_vars <- var_info |>
      dplyr::select(var, sort1, sort2)
    
    res_stats <- res_stats |>
      dplyr::left_join(sort_vars,
                       by = c("var")) |>
      dplyr::arrange(sort1, sort2) |>
      mutate(var = forcats::fct_inorder(var)) |>
      dplyr::select(-sort1, -sort2) |>
      dplyr::relocate(class,
                      var_type,
                      label,
                      .after = dplyr::everything())
    
  }
  
  # Include the name of the strata var
  res_stats |>
    mutate(strata_var = rlang::quo_name(strata_sym)) |>
    dplyr::relocate(strata_var,
                    .before = dplyr::everything())
  
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


#### Stats helper functions --------------------------------

# get_smd
# a wrapper around tableone package function

get_smd <- function(data,
                    strata = NULL,
                    vars) {
  
  if (is.null(strata)) {
    
    tibble::tibble(var = vars,
                   smm = NA_real_)
    
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


# calc_cat_htest
calc_cat_htest <- function(data, strata, vars, b_replicates) {
  
  tibble::tibble(strata = strata,
                 var = vars) %>%
    mutate(x = purrr::map(.x = strata,
                          .f = ~ purrr::pluck(data, .x)),
           y = purrr::map(.x = var,
                          .f = ~ purrr::pluck(data, .x))) %>%
    dplyr::filter(strata != var) %>%
    mutate(tab = purrr::map2(.x = x,
                             .y = y,
                             .f = ~ table(.x, .y)),
           chisq_test = purrr::map_dbl(.x = tab,
                                       .f = ~ calc_chisq_test(.x)),
           chisq_test_no_correction = purrr::map_dbl(.x = tab,
                                                     .f = ~ calc_chisq_test_no_correct(.x)),
           chisq_test_simulated = purrr::map_dbl(.x = tab,
                                                 .f = ~ calc_chisq_test_sim_p(.x,
                                                                              B = b_replicates)),
           
           fisher_test = purrr::map_dbl(.x = tab,
                                        .f = ~ calc_fisher_test(.x)),
           fisher_test_simulated = purrr::map_dbl(.x = tab,
                                                  .f = ~ calc_fisher_test_sim_p(.x,
                                                                                B = b_replicates)),
           check_categorical_test = purrr::map_chr(.x = tab,
                                                   .f = ~ cat_check(.x))) %>%
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



