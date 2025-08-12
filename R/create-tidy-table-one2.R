#' 
#' 
#' 
#' # TODO
#' # Add a row to output for N
#' # include missing strata?
#' # name for missing level?
#' 
#' #' @title
#' #' Tidy table one
#' #'
#' #' @description
#' #' Creates a tidy data frame of the results that can go into a "Table 1" of
#' #' summary descriptive statistics of a study sample. Inspiration for this is owed
#' #' to the `tableone` package by Kazuki Yoshida.
#' #'
#' #' @param data A data frame or tibble containing the variables to be summarized.
#' #' @param strata Character vector of the stratifying (grouping) variable.
#' #' @param vars Character vector of the variable names to be summarized. If
#' #'   empty, then all variables in the given data frame are used.
#' #' @param na_level Character string of the text to replace `NA` in the strata
#' #'   variable, if any exist.
#' #' @param b_replicates an integer specifying the number of replicates used in
#' #'   the Monte Carlo test for Fisher's Exact test and Chi-square test.
#' #' @param ... Additional arguments. Not used.
#' #'
#' #' @importFrom car leveneTest
#' #' @importFrom dplyr bind_rows
#' #' @importFrom dplyr count
#' #' @importFrom dplyr count
#' #' @importFrom dplyr distinct
#' #' @importFrom dplyr everything
#' #' @importFrom dplyr filter
#' #' @importFrom dplyr group_by
#' #' @importFrom dplyr left_join
#' #' @importFrom dplyr mutate
#' #' @importFrom dplyr mutate_if
#' #' @importFrom dplyr n
#' #' @importFrom dplyr n_distinct
#' #' @importFrom dplyr all_of
#' #' @importFrom dplyr pull
#' #' @importFrom dplyr rename
#' #' @importFrom dplyr rename
#' #' @importFrom dplyr select
#' #' @importFrom dplyr summarise
#' #' @importFrom dplyr transmute
#' #' @importFrom dplyr ungroup
#' #' @importFrom forcats fct_na_value_to_level
#' #' @importFrom glue glue
#' #' @importFrom purrr map
#' #' @importFrom purrr map_chr
#' #' @importFrom purrr map_dbl
#' #' @importFrom purrr map2
#' #' @importFrom nortest ad.test
#' #' @importFrom purrr pluck
#' #' @importFrom rlang ensym
#' #' @importFrom stats as.formula
#' #' @importFrom stats bartlett.test
#' #' @importFrom stats chisq.test
#' #' @importFrom stats fisher.test
#' #' @importFrom stats kruskal.test
#' #' @importFrom stats ks.test
#' #' @importFrom stats oneway.test
#' #' @importFrom stats quantile
#' #' @importFrom stats sd
#' #' @importFrom stats shapiro.test
#' #' @importFrom tableone CreateTableOne
#' #' @importFrom tableone ExtractSmd
#' #' @importFrom testit has_warning
#' #' @importFrom tibble as_tibble
#' #' @importFrom tibble tibble
#' #' @importFrom tidyr complete
#' #' @importFrom tidyr pivot_longer
#' #'
#' #' @return
#' #' A tibble with the following results
#' #' \describe{
#' #'   \item{strata}{Level of the stratifying variable}
#' #'   \item{var}{Variable/column name}
#' #'   \item{n}{Number of records}
#' #'   \item{n_distinct}{Numer of distinct values}
#' #'   \item{complete}{Number of non-missing observations}
#' #'   \item{missing}{Number of missing observations}
#' #'   \item{mean}{Mean}
#' #'   \item{sd}{Standard deviation}
#' #'   \item{p0}{Minimum}
#' #'   \item{p25}{25th percentile}
#' #'   \item{p50}{Median}
#' #'   \item{p75}{75th percentile}
#' #'   \item{p100}{Maximum}
#' #'   \item{cv}{Coefficient of variation}
#' #'   \item{shapiro_test}{Shapiro-Wilkes test: p-value}
#' #'   \item{ks_test}{Kolmogorov-Smirnov test: p-value}
#' #'   \item{ad_test}{Anderson-Darling test for normality: p-value}
#' #'   \item{level}{Level of the variable}
#' #'   \item{n_level}{Total number in the variable's group}
#' #'   \item{n_strata}{Total number in the variable group and strata}
#' #'   \item{chisq_test}{Chi square test: p-value, with continuity correction}
#' #'   \item{chisq_test_no_correction}{Chi square test: p-value, without continuity correction}
#' #'   \item{chisq_test_simulated}{Chi square test: p-value: simulated p-value}
#' #'   \item{fisher_test}{Fisher's exact test: p-value}
#' #'   \item{fisher_test_simulated}{Fisher's exact test: simulated p-value}
#' #'   \item{check_categorical_test}{Is Chi square OK? Consider Fisher}
#' #'   \item{oneway_test_unequal_var}{Oneway anova test: p-value, equivalent to t-test when only 2 groups, unequal variances}
#' #'   \item{oneway_test_equal_var}{Oneway anova test: p-value, equivalent to t-test when only 2 groups, equal variances}
#' #'   \item{kruskal_test}{Kruskal-Wallis Rank Sum Test: p-value, equivalent to Mann-Whitney U test when only 2 groups}
#' #'   \item{bartlett_test}{Bartlett's test for homogeneity of variances: p-value}
#' #'   \item{levene_test}{Levene's test for homogeneity of variances: p-value}
#' #'   \item{smd}{Standarized mean difference for all pairwise comparisons}
#' #' }
#' #' @export
#' #'
#' #' @examples
#' #' library(dplyr)
#' #'
#' #' tab1 <- create_tidy_table_one(data = pbc_mayo,
#' #'                               strata = "trt",
#' #'                               vars = c("time",
#' #'                                        "status",
#' #'                                        "trt",
#' #'                                        "age",
#' #'                                        "sex",
#' #'                                        "ascites",
#' #'                                        "hepato",
#' #'                                        "spiders",
#' #'                                        "edema",
#' #'                                        "bili",
#' #'                                        "chol",
#' #'                                        "albumin",
#' #'                                        "copper",
#' #'                                        "alk_phos",
#' #'                                        "ast",
#' #'                                        "trig",
#' #'                                        "platelet",
#' #'                                        "protime",
#' #'                                        "stage"))
#' #'
#' #' dplyr::glimpse(tab1)
#' #'
#' #'
#' #' library(ggplot2)  # diamonds data set
#' #'
#' #' #### With strata --------------------------------
#' #'
#' #' # Continuous and categoical
#' #' (t1 <- create_tidy_table_one(data = diamonds,
#' #'                              strata = "cut",
#' #'                              vars = c("carat",
#' #'                                       # Don't have to include the strata variable
#' #'                                       # "cut",
#' #'                                       "color",
#' #'                                       "clarity",
#' #'                                       "depth",
#' #'                                       "table",
#' #'                                       "price"))
#' #' )
#' #'
#' #' dplyr::glimpse(t1)
#' #'
#' #' t1 |>
#' #'   adorn_tidytableone()
#' #'
#' #'
#' #' # Continuous only
#' #' (t2 <- create_tidy_table_one(data = diamonds,
#' #'                              strata = "cut",
#' #'                              vars = c("carat"))
#' #' )
#' #'
#' #' t2 |>
#' #'   adorn_tidytableone()
#' #'
#' #'
#' #'
#' #' # Categorical only
#' #' (t3 <- create_tidy_table_one(data = diamonds,
#' #'                              strata = "cut",
#' #'                              vars = c("color")))
#' #'
#' #' t3 |>
#' #'   adorn_tidytableone()
#' #'
#' #'
#' #' #### Withou strata --------------------------------
#' #'
#' #' # Continuous and categoical
#' #' (t1 <- create_tidy_table_one(data = diamonds,
#' #'                              strata = NULL,
#' #'                              vars = c("carat",
#' #'                                       # Don't have to include the strata variable
#' #'                                       # "cut",
#' #'                                       "color",
#' #'                                       "clarity",
#' #'                                       "depth",
#' #'                                       "table",
#' #'                                       "price"))
#' #' )
#' #'
#' #' t1 |>
#' #'   adorn_tidytableone()
#' #'
#' #'
#' #' # Continuous only
#' #' (t2 <- create_tidy_table_one(data = diamonds,
#' #'                              strata = NULL,
#' #'                              vars = c("carat"))
#' #' )
#' #'
#' #' t2 |>
#' #'   adorn_tidytableone()
#' #'
#' #'
#' #'
#' #' # Categorical only
#' #' (t3 <- create_tidy_table_one(data = diamonds,
#' #'                              strata = NULL,
#' #'                              vars = c("color")))
#' #'
#' #' t3 |>
#' #'   adorn_tidytableone()
#' 
#' create_tidy_table_one2 <- function(data,
#'                                    strata = NULL,
#'                                    vars,
#'                                    na_level = "(Missing)",
#'                                    b_replicates = 2000,
#'                                    checkbox = NULL,                 # <- NEW: user-supplied tibble
#'                                    checkbox_opts = list(            # <- NEW: options for multi-response blocks
#'                                      denom = "group",               # "group", "nonmissing", "responders"
#'                                      pvals = "per_level",           # "none", "per_level"
#'                                      test  = "auto",                # "auto", "fisher", "chisq"
#'                                      p_adjust = "none",             # "none", "holm", "bonferroni"
#'                                      show_any = TRUE,               # add "Any selected" row
#'                                      note = "Participants could select more than one option; percentages may exceed 100%."
#'                                    ),
#'                                    ...) {
#'   
#'   # Silence no visible binding for global variable
#'   dat <- res <- n_level_valid <- n_strata_valid <- label <- sort1 <- NULL
#'   
#'   
#'   # Handle no strata case by calling the no strata version of the function
#'   if (is.null(strata)) {
#'     
#'     res_stats <- create_tidy_table_one_no_strata(data = data,
#'                                                  strata = NULL,
#'                                                  vars = vars,
#'                                                  na_level = na_level,
#'                                                  b_replicates = b_replicates, ...)
#'     
#'     return(res_stats)
#'     
#'   }
#'   
#'   # Convert ordered factors to regular factors
#'   data <- data %>%
#'     dplyr::mutate_if(.tbl = .,
#'                      .predicate = ~ ("ordered" %in% class(.)),
#'                      .funs = ~ factor(., ordered = FALSE))
#'   
#'   
#'   # Convert the 'strata' argument to a symbol for use in tidy evaluation
#'   strata_sym <- rlang::ensym(strata)
#'   
#'   # Coerce strata to factor once up front to avoid warnings later
#'   if (!rlang::quo_is_null(rlang::enquo(strata))) {
#'     data[[rlang::as_name(strata_sym)]] <- as.factor(data[[rlang::as_name(strata_sym)]])
#'   }
#'   
#'   
#'   # Handle missing values in the strata
#'   
#'   if (any(is.na(purrr::pluck(data, strata)))) {
#'     
#'     df_omit_na_strata <- data %>%
#'       dplyr::filter(!is.na(!! strata_sym))
#'     
#'     data <- data %>%
#'       mutate(!! strata_sym := forcats::fct_na_value_to_level(!! strata_sym,
#'                                                              level = na_level))
#'   } else {
#'     
#'     df_omit_na_strata <- data
#'     
#'   }
#'   
#'   # Use all variables if vars is not provided
#'   if (missing(vars)) {
#'     vars <- names(data)
#'   }
#'   
#'   
#'   #### Get variable info --------------------------------
#'   
#'   # Extract variable labels for later use
#'   var_lbls <- tibble::tibble(var = names(data)) |>
#'     mutate(label = purrr::map_chr(.x = data[, var],
#'                                   .f = ~ get_var_labels(x = .x)))
#'   
#'   # Get variable types and other meta-information
#'   var_info <- get_var_info(data = data,
#'                            .vars = vars)
#'   
#'   # Create sorting variables to maintain order in the output
#'   var_info <- var_info |>
#'     mutate(sort1 = cumsum(var != dplyr::lag(var, default = dplyr::first(var))),
#'            sort1 = sort1 + 1) |>
#'     group_by(var) |>
#'     mutate(sort2 = dplyr::row_number()) |>
#'     ungroup()
#'   
#'   
#'   # Identify Checkbox (multi-response) spec
#'   # Normalize and validate the checkbox spec (tibble with columns:
#'   # var, overall_lbl, checkbox_lbl, checkbox_txt)
#'   cb_spec <- NULL
#'   
#'   if (!is.null(checkbox)) {
#'     cb_spec <- validate_checkbox_spec(
#'       checkbox,
#'       data = data,
#'       vars  = vars
#'     )
#'     
#'     # Build a list of blocks: one element per overall_lbl
#'     cb_blocks <- prepare_checkbox_blocks(cb_spec)
#'     
#'     # Remove checkbox vars from the standard categorical path
#'     cb_vars <- unique(cb_spec$var)
#'   } else {
#'     cb_blocks <- list()
#'     cb_vars <- character(0)
#'   }
#'   
#'   if (!is.null(checkbox)) {
#'     cb_vi <- tibble::tibble(
#'       var = names(cb_blocks),
#'       level = NA_character_,
#'       var_type = "categorical",
#'       class = "checkbox",
#'       sort1 = match(var, vars),  # order by first mention in `vars`
#'       sort2 = 0L
#'     )
#'     var_info <- dplyr::bind_rows(var_info, cb_vi)
#'   }
#'   
#'   
#'   # Identify categorical and continuous variables
#'   cat_vars <- var_info |>
#'     dplyr::filter(var_type == "categorical",
#'                   is.na(.data$class) | .data$class != "checkbox") |>
#'     dplyr::pull(var) |>
#'     unique() |>
#'     setdiff(strata) |>
#'     setdiff(cb_vars)   # still exclude the individual checkbox columns
#'   
#'   con_vars <- var_info |>
#'     dplyr::filter(var_type == "continuous") |>
#'     dplyr::pull(var) |>
#'     unique() |>
#'     setdiff(strata)
#'   
#'   
#'   
#'   
#'   #### Get Categorical and Continuous Stats --------------------------------
#'   
#'   res_stats <- list()
#'   
#'   if (length(con_vars) > 0) {
#'     
#'     con_stats <- process_continuous(data,
#'                                     strata_sym,
#'                                     con_vars)
#'     
#'     res_stats <- dplyr::bind_rows(res_stats,
#'                                   con_stats)
#'     
#'   }
#'   
#'   if (length(cat_vars) > 0) {
#'     
#'     cat_stats <- process_categorical(data,
#'                                      strata_sym,
#'                                      cat_vars)
#'     
#'     res_stats <- dplyr::bind_rows(res_stats,
#'                                   cat_stats)
#'     
#'     
#'   }
#'   
#'   
#'   
#'   
#'   #### Calc SMD --------------------------------
#'   
#'   smd_res <- get_smd(data = df_omit_na_strata,
#'                      strata = strata,
#'                      vars = vars)
#'   
#'   # 10: In StdDiff(variable = var, group = strataVar) :
#'   # Variable has only NA's in at least one stratum. na.rm turned off.
#'   
#'   
#'   #### Hypothesis tests --------------------------------
#'   
#'   htest_res <- list()
#'   
#'   
#'   if (length(cat_vars) > 0) {
#'     
#'     cat_stats <- calc_cat_htest(data = df_omit_na_strata,
#'                                 strata = strata,
#'                                 vars = cat_vars,
#'                                 b_replicates = b_replicates)
#'     
#'     htest_res <- dplyr::bind_rows(htest_res,
#'                                   cat_stats)
#'   }
#'   
#'   if (length(con_vars) > 0) {
#'     
#'     con_stats <- calc_con_htest(data = df_omit_na_strata,
#'                                 strata = strata,
#'                                 vars = con_vars)
#'     
#'     htest_res <- dplyr::bind_rows(htest_res,
#'                                   con_stats)
#'     
#'   }
#'   
#'   
#'   #### Checkbox blocks (multi-response) --------------------------------
#'   res_checkbox <- NULL
#'   
#'   if (length(cb_blocks) > 0) {
#'     res_checkbox <- process_checkbox_blocks(
#'       data        = data,
#'       strata_var  = rlang::as_name(strata_sym),  # <- pass name, not values
#'       blocks      = cb_blocks,
#'       opts        = checkbox_opts
#'     )
#'     
#'     res_checkbox <- res_checkbox |>
#'       dplyr::rename(!! rlang::as_name(strata_sym) := .strata)
#'     
#'     # If requested, compute per-level p-values (2x2 selected vs not by strata)
#'     if (!is.null(strata) && isTRUE(checkbox_opts$pvals %in% c("per_level"))) {
#'       res_checkbox <- add_pvalues_checkbox(
#'         res_checkbox,
#'         data        = data,
#'         strata_var  = rlang::as_name(strata_sym),  # <- pass name, not values
#'         blocks      = cb_blocks,
#'         test        = checkbox_opts$test,
#'         p_adjust    = checkbox_opts$p_adjust
#'       )
#'     }
#'     
#'     # Tag as categorical for downstream p-value formatting (keeps compat)
#'     res_checkbox <- res_checkbox |>
#'       dplyr::mutate(var_type = "categorical", class = "checkbox")  # <- add class tag
#'   }
#'   
#'   # Bind checkbox rows with the rest (if present)
#'   if (!is.null(res_checkbox)) {
#'     res_stats <- dplyr::bind_rows(res_stats, res_checkbox)
#'   }
#'   
#'   # Attach footnote & map so adorn_* can indent, print note, etc.
#'   if (length(cb_blocks) > 0) {
#'     attr(res_stats, "checkbox_blocks") <- cb_blocks
#'     attr(res_stats, "checkbox_opts")   <- checkbox_opts
#'   }
#'   
#'   
#'   #### Combine results, Clean up and arrange --------------------------------
#'   
#'   res_stats <- arrange_results(res_stats,
#'                                htest_res,
#'                                smd_res,
#'                                var_info,
#'                                var_lbls,
#'                                vars,
#'                                cat_vars,
#'                                con_vars,
#'                                data,
#'                                strata,
#'                                strata_sym)
#'   
#'   
#'   #### Return results --------------------------------
#'   
#'   return(res_stats)
#'   
#' }
#' 
#' 
#' 
#' #### Helper functions --------------------------------
#' 
#' # Process categorical variables
#' process_categorical <- function(data,
#'                                 strata_sym,
#'                                 cat_vars) {
#'   
#'   # Silence no visible binding for global variable
#'   dat <- res <- n_level_valid <- n_strata_valid <- NULL
#'   
#'   
#'   
#'   suppressWarnings(
#'     cat_strata <- tibble::tibble(var = cat_vars) |>
#'       mutate(dat = purrr::map(.x = var,
#'                               .f = ~ dplyr::select(data,
#'                                                    !! strata_sym,
#'                                                    dplyr::all_of(.x))),
#'              res = purrr::map(.x = dat,
#'                               .f = ~ do_one_cat_strata(x = .x,
#'                                                        strata_sym = strata_sym))) |>
#'       dplyr::select(res) |>
#'       tidyr::unnest(res)
#'   )
#'   
#'   cat_overall <- cat_strata %>%
#'     dplyr::group_by(var, level) %>%
#'     mutate(n_level = sum(n_level, na.rm = TRUE),
#'            n_level_valid = dplyr::if_else(is.na(level), NA_integer_, n_level)) %>%
#'     group_by(!! strata_sym, var) %>%
#'     mutate(n_strata = sum(n_level, na.rm = TRUE),
#'            n_strata_valid = sum(n_level_valid, na.rm = TRUE)) %>%
#'     ungroup() %>%
#'     mutate(!! strata_sym := "Overall") %>%
#'     dplyr::distinct()
#'   
#'   cat_stats <- dplyr::bind_rows(cat_overall,
#'                                 cat_strata)
#'   
#'   # Calc percentage
#'   cat_stats <- cat_stats |>
#'     mutate(pct = n_level / n_strata,
#'            pct_valid = n_level_valid / n_strata_valid)
#'   
#' }
#' 
#' # Process continuous variables
#' process_continuous <- function(data,
#'                                strata_sym,
#'                                con_vars) {
#'   
#'   con_strata <- data %>%
#'     dplyr::select(!! strata_sym,
#'                   dplyr::all_of(con_vars)) %>%
#'     tidyr::pivot_longer(data = .,
#'                         cols = - !! strata_sym,
#'                         names_to = "var",
#'                         values_to = "value") %>%
#'     group_by(!! strata_sym, var) %>%
#'     summarise(n = dplyr::n(),
#'               n_distinct = dplyr::n_distinct(value),
#'               complete = sum(!is.na(value)),
#'               missing = sum(is.na(value)),
#'               mean = mean(value, na.rm = TRUE),
#'               sd = sd(value, na.rm = TRUE),
#'               p0 = custom_min(value, na.rm = TRUE),
#'               p25 = quantile(value, probs = 0.25, na.rm = TRUE),
#'               p50 = quantile(value, probs = 0.50, na.rm = TRUE),
#'               p75 = quantile(value, probs = 0.75, na.rm = TRUE),
#'               p100 = custom_max(value, na.rm = TRUE),
#'               cv = sd / mean,
#'               shapiro_test = calc_shapiro_test(var = value),
#'               ks_test = calc_ks_test(var = value),
#'               ad_test = calc_ad_test(var = value),
#'               .groups = "drop") %>%
#'     ungroup() %>%
#'     mutate(!! strata_sym := as.character(!! strata_sym))
#'   
#'   con_overall <- data %>%
#'     dplyr::select(!! strata_sym, dplyr::all_of(con_vars)) %>%
#'     tidyr::pivot_longer(data = .,
#'                         cols = - !! strata_sym,
#'                         names_to = "var",
#'                         values_to = "value") %>%
#'     group_by(var) %>%
#'     summarise(n = dplyr::n(),
#'               n_distinct = dplyr::n_distinct(value),
#'               complete = sum(!is.na(value)),
#'               missing = sum(is.na(value)),
#'               mean = mean(value, na.rm = TRUE),
#'               sd = sd(value, na.rm = TRUE),
#'               p0 = custom_min(value, na.rm = TRUE),
#'               p25 = quantile(value, probs = 0.25, na.rm = TRUE),
#'               p50 = quantile(value, probs = 0.50, na.rm = TRUE),
#'               p75 = quantile(value, probs = 0.75, na.rm = TRUE),
#'               p100 = custom_max(value, na.rm = TRUE),
#'               cv = sd / mean,
#'               shapiro_test = calc_shapiro_test(var = value),
#'               ks_test = calc_ks_test(var = value),
#'               ad_test = calc_ad_test(var = value),
#'               .groups = "drop") %>%
#'     ungroup() %>%
#'     mutate(!! strata_sym := "Overall") %>%
#'     dplyr::select(!! strata_sym, dplyr::everything())
#'   
#'   dplyr::bind_rows(con_overall,
#'                    con_strata)
#'   
#' }
#' 
#' # Safely coalesce a set of candidate columns into `target` (only if present),
#' # then drop the redundant source columns.
#' safe_merge_cols <- function(df, target, candidates) {
#'   present <- intersect(candidates, names(df))
#'   if (length(present) == 0L) return(df)
#'   
#'   # Build a list of the present columns as vectors
#'   vecs <- lapply(present, function(nm) df[[nm]])
#'   
#'   # If only one source, just copy it (unless it's already the target)
#'   if (length(vecs) == 1L) {
#'     src <- present[[1]]
#'     if (!identical(src, target)) df[[target]] <- vecs[[1]]
#'   } else {
#'     # Coalesce left-to-right
#'     df[[target]] <- Reduce(dplyr::coalesce, vecs)
#'   }
#'   
#'   # Drop the redundant source columns (keep the target)
#'   drop_cols <- setdiff(present, target)
#'   if (length(drop_cols)) df <- dplyr::select(df, -dplyr::all_of(drop_cols))
#'   
#'   df
#' }
#' 
#' 
#' # Arrange results
#' arrange_results <- function(res_stats,
#'                             htest_res,
#'                             smd_res,
#'                             var_info,
#'                             var_lbls,
#'                             vars,
#'                             cat_vars,
#'                             con_vars,
#'                             data,
#'                             strata,
#'                             strata_sym) {
#'   
#'   
#'   # Silence no visible binding for global variable
#'   sort1 <- sort2 <- label <- strata_var <- NULL
#'   
#'   
#'   # Get the factor levels for the strata
#'   if (is.factor(purrr::pluck(data, strata))) {
#'     strata_lvls <- c("Overall", levels(purrr::pluck(data, strata)))
#'   } else {
#'     strata_lvls <- c("Overall", unique(purrr::pluck(data, strata)))
#'   }
#'   
#'   # Combine the results and convert strata to factor
#'   res_stats <- res_stats |>
#'     dplyr::left_join(htest_res,
#'                      by = "var") %>%
#'     dplyr::rename("strata" = !! strata_sym) |>
#'     dplyr::left_join(smd_res,
#'                      by = "var") |>
#'     mutate(strata = factor(strata, levels = strata_lvls))
#'   
#'   
#'   
#'   
#'   # Add variable info (class, type) and labels to the results
#'   class_and_type <- var_info |>
#'     dplyr::select(-level,
#'                   -sort1,
#'                   -sort2) |>
#'     dplyr::distinct()
#'   
#'   res_stats <- res_stats |>
#'     dplyr::left_join(class_and_type,
#'                      by = "var")
#'   
#'   res_stats <- res_stats |>
#'     dplyr::left_join(var_lbls,
#'                      by = "var")
#'   
#'   # Ensure expected columns exist (robust to partial joins)
#'   for (nm in c("class", "var_type", "label")) {
#'     if (!nm %in% names(res_stats)) {
#'       res_stats[[nm]] <- if (nm == "label") NA_character_ else NA_character_
#'     }
#'   }
#'   
#'   # Arrange results by variable and level, maintaining the original order
#'   if (length(cat_vars) > 0) {
#'     
#'     sort_vars <- var_info |>
#'       dplyr::select(var, level, sort1, sort2)
#'     
#'     res_stats <- res_stats |>
#'       dplyr::left_join(sort_vars,
#'                        by = c("var", "level")) |>
#'       dplyr::arrange(sort1, sort2) |>
#'       mutate(var = forcats::fct_inorder(var),
#'              level = forcats::fct_inorder(level)) |>
#'       dplyr::select(-sort1, -sort2) |>
#'       dplyr::relocate(class,
#'                       var_type,
#'                       label,
#'                       .after = dplyr::everything())
#'   } else {
#'     
#'     sort_vars <- var_info |>
#'       dplyr::select(var, sort1, sort2)
#'     
#'     res_stats <- res_stats |>
#'       dplyr::left_join(sort_vars,
#'                        by = c("var")) |>
#'       dplyr::arrange(sort1, sort2) |>
#'       mutate(var = forcats::fct_inorder(var)) |>
#'       dplyr::select(-sort1, -sort2) |>
#'       dplyr::relocate(class,
#'                       var_type,
#'                       label,
#'                       .after = dplyr::everything())
#'     
#'   }
#'   
#'   # ---- Coalesce duplicated columns from joins and drop helpers ----
#'   res_stats <- res_stats |>
#'     safe_merge_cols("label",    c("label.x", "label.y", "label")) |>
#'     safe_merge_cols("var_type", c("var_type.x", "var_type.y", "var_type")) |>
#'     safe_merge_cols("class",    c("class.x", "class.y", "class")) |>
#'     safe_merge_cols("chisq_test", c("chisq_test.x", "chisq_test.y", "chisq_test")) |>
#'     safe_merge_cols("chisq_test_no_correction",
#'                     c("chisq_test_no_correction.x", "chisq_test_no_correction.y", "chisq_test_no_correction")) |>
#'     safe_merge_cols("chisq_test_simulated",
#'                     c("chisq_test_simulated.x", "chisq_test_simulated.y", "chisq_test_simulated")) |>
#'     safe_merge_cols("fisher_test", c("fisher_test.x", "fisher_test.y", "fisher_test")) |>
#'     safe_merge_cols("fisher_test_simulated",
#'                     c("fisher_test_simulated.x", "fisher_test_simulated.y", "fisher_test_simulated")) |>
#'     safe_merge_cols("check_categorical_test",
#'                     c("check_categorical_test.x", "check_categorical_test.y", "check_categorical_test")) |>
#'     # drop internal checkbox helpers if they exist
#'     dplyr::select(-dplyr::any_of(c(".strata", "level_var", "group_n", "p_value")))
#'   
#'   # (optional) tidy column order; keep your existing relocate if you like
#'   res_stats <- res_stats |>
#'     dplyr::relocate(
#'       strata_var, strata, var, level,
#'       n, n_distinct, complete, missing,
#'       n_level, n_strata, n_level_valid, n_strata_valid,
#'       mean, sd, p0, p25, p50, p75, p100, cv,
#'       pct, pct_valid,
#'       chisq_test, chisq_test_no_correction, chisq_test_simulated,
#'       fisher_test, fisher_test_simulated, check_categorical_test,
#'       oneway_test_unequal_var, oneway_test_equal_var,
#'       kruskal_test, bartlett_test, levene_test, smd,
#'       .before = dplyr::everything()
#'     ) |>
#'     dplyr::relocate(class, var_type, label, .after = dplyr::last_col())
#'   
#'   # Include the name of the strata var
#'   res_stats |>
#'     mutate(strata_var = rlang::quo_name(strata_sym)) |>
#'     dplyr::relocate(strata_var,
#'                     .before = dplyr::everything())
#'   
#' }
#' 
#' # Extracts the label attribute from a variable, if it exists.
#' # If the label is not found, returns NA as a character string.
#' get_var_labels <- function(x) {
#'   
#'   x_lbl <- attr(x,
#'                 which = "label",
#'                 exact = TRUE)
#'   
#'   if (is.null(x_lbl)) {
#'     NA_character_
#'   } else {
#'     x_lbl
#'   }
#'   
#' }
#' 
#' 
#' # Processes a categorical variable across strata, creating a count of each level within the strata.
#' # The function calculates the total counts and valid counts (non-missing) within each stratum,
#' # ensuring that all levels are included even if some are missing in certain strata.
#' # The result is a tibble with counts per level and strata, including handling for missing values.
#' do_one_cat_strata <- function(x, strata_sym) {
#'   
#'   # Silence no visible binding for global variable
#'   n_level_valid <- NULL
#'   
#'   if (ncol(x) <= 1L) {
#'     nm <- rlang::as_name(strata_sym)
#'     empty <- setNames(list(character()), nm)
#'     return(tibble::as_tibble(empty) |>
#'              dplyr::mutate(
#'                var = factor(),
#'                level = factor(),
#'                n_level = integer(),
#'                n_strata = integer(),
#'                n_level_valid = integer(),
#'                n_strata_valid = integer()
#'              ))
#'   }
#'   
#'   x |>
#'     tidyr::pivot_longer(cols = - !! strata_sym,
#'                         names_to = "var",
#'                         values_to = "level") |>
#'     dplyr::count(!! strata_sym, var, level,
#'                  name = "n_level",
#'                  .drop = FALSE)  |>
#'     group_by(var) |>
#'     tidyr::complete(!! strata_sym, level,
#'                     fill = list(n_level = 0)) |>
#'     group_by(!! strata_sym, var) |>
#'     mutate(n_strata = sum(n_level, na.rm = TRUE),
#'            n_level_valid = dplyr::if_else(is.na(level), NA_integer_, n_level),
#'            n_strata_valid = sum(n_level_valid, na.rm = TRUE)) |>
#'     ungroup() |>
#'     dplyr::select(!! strata_sym, dplyr::everything()) |>
#'     mutate(!! strata_sym := as.character(!! strata_sym))
#'   
#' }
#' 
#' 
#' #### Stats helper functions --------------------------------
#' 
#' # get_smd
#' # a wrapper around tableone package function
#' 
#' get_smd <- function(data,
#'                     strata = NULL,
#'                     vars) {
#'   
#'   if (is.null(strata)) {
#'     
#'     tibble::tibble(var = vars,
#'                    smd = NA_real_)
#'     
#'   } else {
#'     
#'     tableone::CreateTableOne(vars = vars,
#'                              strata = strata,
#'                              data = data) %>%
#'       tableone::ExtractSmd(.) %>%
#'       tibble::as_tibble(.,
#'                         rownames = "var") %>%
#'       dplyr::select(1:2) %>%
#'       dplyr::rename(smd = 2)
#'     
#'   }
#'   
#' }
#' 
#' 
#' # shapiro test
#' calc_shapiro_test <- function(var) {
#'   tryCatch(shapiro.test(var)[["p.value"]],
#'            error = function(err) NA)
#' }
#' 
#' 
#' # Kolmogorov-Smirnov Tests
#' calc_ks_test <- function(var) {
#'   tryCatch(ks.test(unique(var), "pnorm")[["p.value"]],
#'            error = function(err) NA)
#' }
#' 
#' 
#' # anderson-darling test
#' # The K-S test is not suitable when estimating the parameters from the data. You
#' # can use the following code, which relies on the Anderson-Darling test for
#' # normality, and does not require you to supply the mean and the stddev. This
#' # test is stronger in accuracy than the Lilliefors test.
#' calc_ad_test <- function(var) {
#'   tryCatch(nortest::ad.test(var)[["p.value"]],
#'            error = function(err) NA)
#' }
#' 
#' 
#' # oneway test (unequal variances)
#' calc_oneway_test_unequal <- function(data, form, var.equal = FALSE) {
#'   
#'   tryCatch(oneway.test(formula = as.formula(form),
#'                        data = data,
#'                        var.equal = var.equal) %>%
#'              purrr::pluck(., "p.value"),
#'            error = function(err) NA)
#'   
#'   
#'   
#' }
#' 
#' 
#' # oneway test (equal variances)
#' calc_oneway_test_equal <- function(data, form, var.equal = TRUE) {
#'   
#'   tryCatch(oneway.test(formula = as.formula(form),
#'                        data = data,
#'                        var.equal = var.equal) %>%
#'              purrr::pluck(., "p.value"),
#'            error = function(err) NA)
#'   
#'   
#'   
#' }
#' 
#' 
#' # kruskal test
#' calc_kruskal_test <- function(data, form) {
#'   
#'   tryCatch(kruskal.test(formula = as.formula(form), data = data) %>%
#'              purrr::pluck(., "p.value"),
#'            error = function(err) NA)
#'   
#' }
#' 
#' 
#' # bartlett test
#' calc_bartlett_test <- function(data, form) {
#'   
#'   tryCatch(bartlett.test(formula = as.formula(form), data = data) %>%
#'              purrr::pluck(., "p.value"),
#'            error = function(err) NA)
#'   
#' }
#' 
#' 
#' # levene test
#' calc_levene_test <- function(data, form) {
#'   
#'   tryCatch(car::leveneTest(as.formula(form), data = data) %>%
#'              purrr::pluck(., "Pr(>F)", 1),
#'            error = function(err) NA)
#'   
#' }
#' 
#' 
#' # cat_check
#' cat_check <- function(tab) {
#'   
#'   dplyr::if_else(testit::has_warning(chisq.test(tab)),
#'                  "warning",
#'                  "ok")
#' }
#' 
#' 
#' # calc_fisher_test
#' calc_fisher_test <- function(tab,
#'                              simulate.p.value = FALSE,
#'                              B = 2000) {
#'   tryCatch(fisher.test(tab,
#'                        alternative = "two.sided",
#'                        conf.int = FALSE,
#'                        simulate.p.value = simulate.p.value,
#'                        B = B) %>%
#'              purrr::pluck(., "p.value"),
#'            error = function(err) NA)
#' }
#' 
#' 
#' # calc_fisher_test, simulate p-value
#' calc_fisher_test_sim_p <- function(tab,
#'                                    simulate.p.value = TRUE,
#'                                    B = 2000) {
#'   tryCatch(fisher.test(tab,
#'                        alternative = "two.sided",
#'                        conf.int = FALSE,
#'                        simulate.p.value = simulate.p.value,
#'                        B = B) %>%
#'              purrr::pluck(., "p.value"),
#'            error = function(err) NA)
#' }
#' 
#' 
#' # calc_chisq_test
#' calc_chisq_test <- function(tab,
#'                             correct = TRUE,
#'                             simulate.p.value = FALSE,
#'                             B = 2000) {
#'   
#'   tryCatch(chisq.test(tab,
#'                       correct = correct,
#'                       simulate.p.value = simulate.p.value,
#'                       B = B)  %>%
#'              purrr::pluck(., "p.value"),
#'            error = function(err) NA)
#'   
#' }
#' 
#' 
#' # calc_chisq_test (no correction)
#' calc_chisq_test_no_correct <- function(tab,
#'                                        correct = FALSE,
#'                                        simulate.p.value = FALSE,
#'                                        B = 2000) {
#'   
#'   tryCatch(chisq.test(tab,
#'                       correct = correct,
#'                       simulate.p.value = simulate.p.value,
#'                       B = B)  %>%
#'              purrr::pluck(., "p.value"),
#'            error = function(err) NA)
#'   
#' }
#' 
#' 
#' # calc_chisq_test (simulated p)
#' calc_chisq_test_sim_p <- function(tab,
#'                                   correct = TRUE,
#'                                   simulate.p.value = TRUE,
#'                                   B = 2000) {
#'   
#'   tryCatch(chisq.test(tab,
#'                       correct = correct,
#'                       simulate.p.value = simulate.p.value,
#'                       B = B)  %>%
#'              purrr::pluck(., "p.value"),
#'            error = function(err) NA)
#'   
#' }
#' 
#' 
#' # calc_cat_htest
#' calc_cat_htest <- function(data, strata, vars, b_replicates) {
#'   
#'   tibble::tibble(strata = strata,
#'                  var = vars) %>%
#'     mutate(x = purrr::map(.x = strata,
#'                           .f = ~ purrr::pluck(data, .x)),
#'            y = purrr::map(.x = var,
#'                           .f = ~ purrr::pluck(data, .x))) %>%
#'     dplyr::filter(strata != var) %>%
#'     mutate(tab = purrr::map2(.x = x,
#'                              .y = y,
#'                              .f = ~ table(.x, .y)),
#'            chisq_test = purrr::map_dbl(.x = tab,
#'                                        .f = ~ calc_chisq_test(.x)),
#'            chisq_test_no_correction = purrr::map_dbl(.x = tab,
#'                                                      .f = ~ calc_chisq_test_no_correct(.x)),
#'            chisq_test_simulated = purrr::map_dbl(.x = tab,
#'                                                  .f = ~ calc_chisq_test_sim_p(.x,
#'                                                                               B = b_replicates)),
#'            
#'            fisher_test = purrr::map_dbl(.x = tab,
#'                                         .f = ~ calc_fisher_test(.x)),
#'            fisher_test_simulated = purrr::map_dbl(.x = tab,
#'                                                   .f = ~ calc_fisher_test_sim_p(.x,
#'                                                                                 B = b_replicates)),
#'            check_categorical_test = purrr::map_chr(.x = tab,
#'                                                    .f = ~ cat_check(.x))) %>%
#'     dplyr::select(var,
#'                   chisq_test,
#'                   chisq_test_no_correction,
#'                   chisq_test_simulated,
#'                   fisher_test,
#'                   fisher_test_simulated,
#'                   check_categorical_test)
#' }
#' 
#' 
#' # calc_con_htest
#' calc_con_htest <- function(data, strata, vars) {
#'   
#'   tibble::tibble(strata = strata,
#'                  var = vars) %>%
#'     mutate(form = glue::glue("{var} ~ {strata}")) %>%
#'     mutate(oneway_test_unequal_var =
#'              purrr::map_dbl(.x = form,
#'                             .f = ~ calc_oneway_test_unequal(data = data,
#'                                                             form = .x)),
#'            oneway_test_equal_var =
#'              purrr::map_dbl(.x = form,
#'                             .f = ~ calc_oneway_test_equal(data = data,
#'                                                           form = .x)),
#'            kruskal_test =
#'              purrr::map_dbl(.x = form,
#'                             .f = ~ calc_kruskal_test(data = data,
#'                                                      form = .x)),
#'            bartlett_test =
#'              purrr::map_dbl(.x = form,
#'                             .f = ~ calc_bartlett_test(data = data,
#'                                                       form = .x)),
#'            levene_test =
#'              purrr::map_dbl(.x = form,
#'                             .f = ~ calc_levene_test(data = data,
#'                                                     form = .x))) %>%
#'     dplyr::select(var,
#'                   oneway_test_unequal_var,
#'                   oneway_test_equal_var,
#'                   kruskal_test,
#'                   bartlett_test,
#'                   levene_test)
#' }
#' 
#' 
#' # custom_min custom_max
#' custom_min <- function(x, na.rm = TRUE) {
#'   
#'   if (is.integer(x)) {
#'     
#'     dflt_miss <- NA_integer_
#'     
#'   } else if (is.numeric(x)) {
#'     
#'     dflt_miss <- NA_real_
#'     
#'   } else {
#'     
#'     dflt_miss <- Inf
#'     
#'   }
#'   
#'   if (all(is.na(x))) {
#'     dflt_miss
#'   } else {
#'     min(x, na.rm = na.rm)
#'   }
#' }
#' 
#' custom_max <- function(x, na.rm = TRUE) {
#'   
#'   if (is.integer(x)) {
#'     
#'     dflt_miss <- NA_integer_
#'     
#'   } else if (is.numeric(x)) {
#'     
#'     dflt_miss <- NA_real_
#'     
#'   } else {
#'     
#'     dflt_miss <- Inf
#'     
#'   }
#'   
#'   if (all(is.na(x))) {
#'     dflt_miss
#'   } else {
#'     max(x, na.rm = na.rm)
#'   }
#' }
#' 
#' # ---- Checkbox helpers -------------------------------------------------
#' 
#' validate_checkbox_spec <- function(x, data, vars) {
#'   req_cols <- c("var","overall_lbl","checkbox_lbl","checkbox_txt")
#'   if (!all(req_cols %in% names(x))) {
#'     stop("`checkbox` must include columns: ", paste(req_cols, collapse = ", "), call. = FALSE)
#'   }
#'   x <- tibble::as_tibble(x)
#'   
#'   # All vars must exist in data AND be in the selected vars list
#'   missing_in_data <- setdiff(unique(x$var), names(data))
#'   if (length(missing_in_data)) {
#'     stop("Checkbox vars not found in data: ", paste(missing_in_data, collapse = ", "), call. = FALSE)
#'   }
#'   missing_in_vars <- setdiff(unique(x$var), vars)
#'   if (length(missing_in_vars)) {
#'     stop("Checkbox vars must also be present in `vars`: ", paste(missing_in_vars, collapse = ", "), call. = FALSE)
#'   }
#'   
#'   # one label per row; allow repeated overall_lbl to form blocks
#'   x$overall_lbl <- as.character(x$overall_lbl)
#'   x$checkbox_lbl <- as.character(x$checkbox_lbl)
#'   x$checkbox_txt <- as.character(x$checkbox_txt)
#'   
#'   # de-duplicate rows if user passed dupes
#'   dplyr::distinct(x, var, overall_lbl, checkbox_lbl, checkbox_txt)
#' }
#' 
#' prepare_checkbox_blocks <- function(spec) {
#'   sp <- split(spec, spec$overall_lbl)
#'   out <- lapply(names(sp), function(nm) {
#'     df <- sp[[nm]]
#'     list(
#'       overall_lbl = nm,
#'       vars        = df$var,
#'       labels      = stats::setNames(df$checkbox_lbl, df$var),
#'       select_txt  = stats::setNames(df$checkbox_txt, df$var)
#'     )
#'   })
#'   rlang::set_names(out, names(sp))
#' }
#' 
#' # Compute counts and percents per level; optional "Any selected" row
#' process_checkbox_blocks <- function(data, strata_var, blocks, opts) {
#'   # denom options: "group","nonmissing","responders"
#'   denom <- match.arg(opts$denom, c("group","nonmissing","responders"))
#'   
#'   out <- lapply(blocks, function(bl) {
#'     
#'     # Build 0/1 selection matrix for this block (NAME FIRST, then tibble)
#'     sel <- stats::setNames(
#'       lapply(bl$vars, function(v) as.integer(data[[v]] == bl$select_txt[[v]])),
#'       bl$vars
#'     )
#'     sel <- tibble::as_tibble(sel)
#'     names(sel) <- bl$vars
#'     
#'     # strata as character for joins/printing
#'     df <- tibble::tibble(.strata = as.character(data[[strata_var]])) |>
#'       dplyr::bind_cols(sel)
#'     
#'     # per-group denominators
#'     grp_n <- df |>
#'       dplyr::count(.strata, name = "group_n")
#'     
#'     any_selected <- as.integer(rowSums(sel, na.rm = TRUE) > 0L)
#'     
#'     denom_df <- switch(
#'       denom,
#'       group = grp_n,
#'       nonmissing = {
#'         # (placeholder: treat NAs as not selected; if you add true NA handling,
#'         # update this branch to compute a reduced denominator)
#'         grp_n
#'       },
#'       responders = {
#'         df |>
#'           dplyr::mutate(.any = any_selected) |>
#'           dplyr::group_by(.strata) |>
#'           dplyr::summarise(group_n = sum(.any, na.rm = TRUE), .groups = "drop")
#'       }
#'     )
#'     
#'     # Per-strata counts per level (selected == 1)
#'     long <- df |>
#'       tidyr::pivot_longer(cols = dplyr::all_of(bl$vars),
#'                           names_to = "level_var",
#'                           values_to = "selected") |>
#'       dplyr::group_by(.strata, level_var) |>
#'       dplyr::summarise(n_level = sum(selected, na.rm = TRUE), .groups = "drop") |>
#'       dplyr::left_join(denom_df, by = ".strata") |>
#'       dplyr::mutate(
#'         pct   = ifelse(group_n > 0, n_level / group_n, NA_real_),
#'         var   = bl$overall_lbl,
#'         level = bl$labels[level_var]
#'       )
#'     
#'     # Overall denominator
#'     overall_denom <- switch(
#'       denom,
#'       group = nrow(data),
#'       nonmissing = nrow(data),             # refine if adding true NA handling
#'       responders = sum(any_selected, na.rm = TRUE)
#'     )
#'     
#'     # Overall counts per level
#'     overall <- long |>
#'       dplyr::group_by(level_var) |>
#'       dplyr::summarise(n_level = sum(n_level, na.rm = TRUE), .groups = "drop") |>
#'       dplyr::mutate(
#'         .strata = "Overall",
#'         group_n = overall_denom,
#'         pct     = ifelse(group_n > 0, n_level / group_n, NA_real_),
#'         var     = bl$overall_lbl,
#'         level   = bl$labels[level_var]
#'       )
#'     
#'     # Optional "Any selected"
#'     any_row <- NULL
#'     if (isTRUE(opts$show_any)) {
#'       any_counts <- df |>
#'         dplyr::mutate(.any = any_selected) |>
#'         dplyr::group_by(.strata) |>
#'         dplyr::summarise(n_level = sum(.any, na.rm = TRUE), .groups = "drop") |>
#'         dplyr::left_join(denom_df, by = ".strata") |>
#'         dplyr::mutate(
#'           pct   = ifelse(group_n > 0, n_level / group_n, NA_real_),
#'           var   = bl$overall_lbl,
#'           level = "Any selected",
#'           level_var = NA_character_
#'         )
#'       
#'       any_overall <- tibble::tibble(
#'         .strata  = "Overall",
#'         n_level  = sum(any_selected, na.rm = TRUE),
#'         group_n  = overall_denom,
#'         pct      = ifelse(group_n > 0, n_level / group_n, NA_real_),
#'         var      = bl$overall_lbl,
#'         level    = "Any selected",
#'         level_var = NA_character_
#'       )
#'       
#'       any_row <- dplyr::bind_rows(any_counts, any_overall)
#'     }
#'     
#'     dplyr::bind_rows(long, overall, any_row)
#'   })
#'   
#'   out <- dplyr::bind_rows(out)
#'   
#'   # Shape to match rest of pipeline
#'   out |>
#'     dplyr::rename(n_strata = n_level) |>
#'     dplyr::mutate(
#'       n_level    = NA_integer_,     # computed later for overall style if needed
#'       label      = var,
#'       pct_valid  = pct,             # same as pct for checkbox rows
#'       chisq_test = NA_real_,
#'       chisq_test_no_correction = NA_real_,
#'       chisq_test_simulated = NA_real_,
#'       fisher_test = NA_real_,
#'       fisher_test_simulated = NA_real_,
#'       check_categorical_test = NA_character_
#'     )
#' }
#' 
#' # Add per-level Fisher/χ² p-values (2x2 selected vs not by strata)
#' add_pvalues_checkbox <- function(tab, data, strata_var, blocks, test, p_adjust = "none") {
#'   
#'   needs <- tab |>
#'     dplyr::filter(!is.na(.data$level_var)) |>
#'     dplyr::select(var, level, level_var) |>
#'     dplyr::distinct()
#'   
#'   p_res <- needs |>
#'     dplyr::rowwise() |>
#'     dplyr::mutate(p_raw = {
#'       vv <- level_var
#'       x <- tibble::tibble(
#'         sel   = as.integer(data[[vv]] == blocks[[var]]$select_txt[[vv]]),
#'         group = as.character(data[[strata_var]])
#'       )
#'       tt <- table(x$sel, x$group, useNA = "no")
#'       
#'       if (identical(test, "fisher")) {
#'         calc_fisher_test(tt)
#'       } else if (identical(test, "chisq")) {
#'         calc_chisq_test(tt)
#'       } else {
#'         ex <- tryCatch(suppressWarnings(stats::chisq.test(tt)$expected), error = function(e) NULL)
#'         if (!is.null(ex) && any(ex < 5)) calc_fisher_test(tt) else calc_chisq_test(tt)
#'       }
#'     }) |>
#'     dplyr::ungroup()
#'   
#'   if (!identical(p_adjust, "none")) {
#'     p_res <- p_res |>
#'       dplyr::group_by(var) |>
#'       dplyr::mutate(p_value = p.adjust(p_raw, method = p_adjust)) |>
#'       dplyr::ungroup()
#'   } else {
#'     p_res <- dplyr::mutate(p_res, p_value = p_raw)
#'   }
#'   
#'   tab |>
#'     dplyr::left_join(dplyr::select(p_res, var, level, p_value),
#'                      by = c("var","level")) |>
#'     dplyr::mutate(p_value_level = p_value)
#' }
#' 
#' 
#' 
#' 
#' 
#' 
#' #### TESTING --------------------------------
#' 
#' library(dplyr)
#' library(tibble)
#' library(tidyr)
#' 
#' df <- tibble::tibble(
#'   id = c(1:100),
#'   group = sample(c("A", "B", "C"), size = 100, replace = TRUE),
#'   preferred_color___1 = sample(c("Checked", "Unchecked"), size = 100, replace = TRUE),
#'   preferred_color___2 = sample(c("Checked", "Unchecked"), size = 100, replace = TRUE),
#'   preferred_color___3 = sample(c("Checked", "Unchecked"), size = 100, replace = TRUE),
#'   preferred_color___4 = sample(c("Checked", "Unchecked"), size = 100, replace = TRUE),
#'   preferred_color___5 = sample(c("Checked", "Unchecked"), size = 100, replace = TRUE)) %>%
#'   mutate(group = factor(group,
#'                         levels = c("A", "B", "C")))
#' 
#' df
#' 
#' 
#' var_labels <- tibble::tribble(
#'   ~field_name, ~checkbox_choice,
#'   "preferred_color___1",           "Blue",
#'   "preferred_color___2",          "Green",
#'   "preferred_color___3",         "Orange",
#'   "preferred_color___4",            "Red",
#'   "preferred_color___5",         "Yellow"
#' )
#' 
#' 
#' df2 <- tibble::tibble(
#'   record_id = c(1:100),
#'   gender = sample(c("Female", "Male"),
#'                   size = 100,
#'                   replace = TRUE),
#'   gender_other = NA,
#'   age = sample(c(18:85),
#'                size = 100,
#'                replace = TRUE),
#'   education = sample(c("High-school", "College", "Graduate school"),
#'                      size = 100,
#'                      replace = TRUE),
#'   ethnicity = sample(c("Hispanic", "Non-hispanic"),
#'                      size = 100,
#'                      replace = TRUE),
#'   key = sample(c("race___1", "race___2", "race___3",
#'                  "race___4", "race___5", "race___6", "race___98"),
#'                size = 100,
#'                replace = TRUE),
#'   value = "Checked",
#'   income = sample(c(20000:120000),
#'                   size = 100,
#'                   replace = TRUE),
#'   marital_status = sample(c("Married", "Single"),
#'                           size = 100,
#'                           replace = TRUE),
#'   survey_complete = sample(c("Complete", "Not complete"),
#'                            size = 100,
#'                            replace = TRUE),
#'   group = sample(c("Treatment", "Control"),
#'                  size = 100,
#'                  replace = TRUE)) %>%
#'   mutate(key = factor(key,
#'                       levels = c("race___1", "race___2", "race___3",
#'                                  "race___4", "race___5", "race___6",
#'                                  "race___98"))) %>%
#'   tidyr::spread(.,
#'                 key = "key",
#'                 value = "value") %>%
#'   mutate_at(.vars = vars(race___1:race___98),
#'             .funs = list(~ tidyr::replace_na(., "Unchecked")))
#' 
#' 
#' data_labels <- tibble::tribble(
#'   ~field_name,                      ~checkbox_choice,
#'   "record_id",                                    NA,
#'   "gender",                                    NA,
#'   "gender_other",                                    NA,
#'   "age",                                    NA,
#'   "education",                                    NA,
#'   "ethnicity",                                    NA,
#'   "race___1",                               "White",
#'   "race___2",           "Black or African-American",
#'   "race___3",    "American Indian or Alaska Native",
#'   "race___4",                               "Asian",
#'   "race___5", "Native Hawaiian or Pacific Islander",
#'   "race___6",                               "Other",
#'   "race___98",                "Prefer not to answer",
#'   "race_other",                                    NA,
#'   "income",                                    NA,
#'   "marital_status",                                    NA,
#'   "survey_complete",                                    NA
#' )
#' 
#' 
#' df2
#' 
#' 
#' create_tidy_table_one2(data = df2,
#'                        strata = "group",
#'                        vars = c("gender",
#'                                 "age",
#'                                 "education",
#'                                 "ethnicity",
#'                                 "income",
#'                                 "marital_status",
#'                                 "race___1",
#'                                 "race___2",
#'                                 "race___3",
#'                                 "race___4",
#'                                 "race___5",
#'                                 "race___6",
#'                                 "race___98"))
#' 
#' 
#' create_tidy_table_one2(data = df2,
#'                        strata = "group",
#'                        vars = c("gender",
#'                                 "age",
#'                                 "education",
#'                                 "ethnicity",
#'                                 "income",
#'                                 "marital_status",
#'                                 "race___1",
#'                                 "race___2",
#'                                 "race___3",
#'                                 "race___4",
#'                                 "race___5",
#'                                 "race___6",
#'                                 "race___98"),
#'                        checkbox = tibble::tribble(
#'                          ~var, ~overall_lbl,                         ~checkbox_lbl, ~checkbox_txt,
#'                          "race___1",       "Race",                               "White",     "Checked",
#'                          "race___2",       "Race",           "Black or African-American",     "Checked",
#'                          "race___3",       "Race",    "American Indian or Alaska Native",     "Checked",
#'                          "race___4",       "Race",                               "Asian",     "Checked",
#'                          "race___5",       "Race", "Native Hawaiian or Pacific Islander",     "Checked",
#'                          "race___6",       "Race",                               "Other",     "Checked",
#'                          "race___98",       "Race",                "Prefer not to answer",     "Checked"
#'                        )
#'                        
#' )
#' 
#' 
#' var_labels <- tibble::tribble(
#'                              ~var,                                         ~lbl,
#'                       "record_id",                                         "ID",
#'                          "gender",                                     "Gender",
#'                    "gender_other",                              "Gender, Other",
#'                             "age",                              "Age, in years",
#'                       "education",                                  "Education",
#'                       "ethnicity",                                  "Ethnicity",
#'                          "income",                                     "Income",
#'                  "marital_status",                             "Marital status",
#'                 "survey_complete",                                           NA,
#'                           "group",                                      "Group",
#'                        "race___1",                               "Race (White)",
#'                        "race___2",           "Race (Black or African-American)",
#'                        "race___3",    "Race (American Indian or Alaska Native)",
#'                        "race___4",                               "Race (Asian)",
#'                        "race___5", "Race (Native Hawaiian or Pacific Islander)",
#'                        "race___6",                               "Race (Other)",
#'                       "race___98",                "Race (Prefer not to answer)"
#'                 )
#' 
#' 
#' df2 <- df2 |> 
#'   lamisc::apply_data_labels(vars = var_labels$var, 
#'                             labels = var_labels$lbl)
#' 
#' 
#' create_tidy_table_one2(data = df2,
#'                        strata = "group",
#'                        vars = c("gender",
#'                                 "age",
#'                                 "education",
#'                                 "ethnicity",
#'                                 "income",
#'                                 "marital_status",
#'                                 "race___1",
#'                                 "race___2",
#'                                 "race___3",
#'                                 "race___4",
#'                                 "race___5",
#'                                 "race___6",
#'                                 "race___98"),
#'                        checkbox = tibble::tribble(
#'                          ~var, ~overall_lbl,                         ~checkbox_lbl, ~checkbox_txt,
#'                          "race___1",       "Race",                               "White",     "Checked",
#'                          "race___2",       "Race",           "Black or African-American",     "Checked",
#'                          "race___3",       "Race",    "American Indian or Alaska Native",     "Checked",
#'                          "race___4",       "Race",                               "Asian",     "Checked",
#'                          "race___5",       "Race", "Native Hawaiian or Pacific Islander",     "Checked",
#'                          "race___6",       "Race",                               "Other",     "Checked",
#'                          "race___98",       "Race",                "Prefer not to answer",     "Checked"
#'                        )
#'                        
#' )
#' 
#' 
#' 
#' create_tidy_table_one2(data = df2,
#'                        strata = "group",
#'                        vars = c("gender",
#'                                 "age",
#'                                 "education",
#'                                 "ethnicity",
#'                                 "income",
#'                                 "marital_status",
#'                                 "race___1",
#'                                 "race___2",
#'                                 "race___3",
#'                                 "race___4",
#'                                 "race___5",
#'                                 "race___6",
#'                                 "race___98")
#'                        
#' )
#' 
#' 
#' 
#' t1 <- create_tidy_table_one2(data = df2,
#'                        strata = "group",
#'                        vars = c("gender",
#'                                 "age",
#'                                 "education",
#'                                 "ethnicity",
#'                                 "income",
#'                                 "marital_status",
#'                                 "race___1",
#'                                 "race___2",
#'                                 "race___3",
#'                                 "race___4",
#'                                 "race___5",
#'                                 "race___6",
#'                                 "race___98"),
#'                        checkbox = tibble::tribble(
#'                          ~var, ~overall_lbl,                         ~checkbox_lbl, ~checkbox_txt,
#'                          "race___1",       "Race",                               "White",     "Checked",
#'                          "race___2",       "Race",           "Black or African-American",     "Checked",
#'                          "race___3",       "Race",    "American Indian or Alaska Native",     "Checked",
#'                          "race___4",       "Race",                               "Asian",     "Checked",
#'                          "race___5",       "Race", "Native Hawaiian or Pacific Islander",     "Checked",
#'                          "race___6",       "Race",                               "Other",     "Checked",
#'                          "race___98",       "Race",                "Prefer not to answer",     "Checked"
#'                        )
#'                        
#' )
#' 
#' 
#' tidytableone::adorn_tidytableone(t1)
