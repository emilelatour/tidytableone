
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
#' @param checkbox A tibble/data.frame specifying checkbox blocks:
#'   columns `var` (the checkbox column name), `overall_lbl` (group label to show),
#'   `checkbox_lbl` (label for each option), and `checkbox_txt` (the selected text to match, e.g., "Checked").
#' @param checkbox_opts A list of options for checkbox processing. Recognized fields:
#'   `denom` = c("group","nonmissing","responders"), `pvals` (ignored for no-strata),
#'   `test` (ignored for no-strata), `p_adjust` (method passed to stats::p.adjust),
#'   `show_any` (logical; add “Any selected” row), `note` (character footnote).
#'
#' @importFrom car leveneTest
#' @importFrom dplyr bind_rows
#' @importFrom dplyr count
#' @importFrom dplyr cur_column
#' @importFrom dplyr distinct
#' @importFrom dplyr everything
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_if
#' @importFrom dplyr n
#' @importFrom dplyr n_distinct
#' @importFrom dplyr all_of
#' @importFrom dplyr pull
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
#' @importFrom rlang %||%
#' @importFrom rlang ensym
#' @importFrom stats as.formula
#' @importFrom stats bartlett.test
#' @importFrom stats chisq.test
#' @importFrom stats fisher.test
#' @importFrom stats kruskal.test
#' @importFrom stats ks.test
#' @importFrom stats oneway.test
#' @importFrom stats p.adjust
#' @importFrom stats quantile
#' @importFrom stats sd
#' @importFrom stats shapiro.test
#' @importFrom tableone CreateTableOne
#' @importFrom tableone ExtractSmd
#' @importFrom testit has_warning
#' @importFrom tibble as_tibble
#' @importFrom tibble tibble
#' @importFrom tidyr complete
#' @importFrom tidyr pivot_longer
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
#' # Minimal smoke test (always runs)
#' set.seed(1)
#' tiny <- tibble::tibble(
#'   cut   = sample(c("Fair","Good"), 20, TRUE),
#'   color = sample(c("E","H"), 20, TRUE),
#'   carat = runif(20, 0.2, 1.5)
#' )
#' create_tidytableone(
#'   data = tiny,
#'   strata = "cut",
#'   vars = c("carat","color")
#' )
#'
#' @examplesIf interactive()
#' # Larger, fully worked examples (run only in interactive sessions)
#' library(dplyr)
#' 
#' library(ggplot2)  # for diamonds data
#'
#' #### With strata --------------------------------
#' # Continuous + categorical
#' t1 <- create_tidytableone(
#'   data = ggplot2::diamonds,
#'   strata = "cut",
#'   vars = c("carat","color","clarity","depth","table","price")
#' )
#' dplyr::glimpse(t1)
#' # If you have an adorn helper:
#' # t1 |> adorn_tidytableone()
#'
#' # Continuous only
#' t2 <- create_tidytableone(
#'   data = ggplot2::diamonds,
#'   strata = "cut",
#'   vars = c("carat")
#' )
#' # t2 |> adorn_tidytableone()
#'
#' # Categorical only
#' t3 <- create_tidytableone(
#'   data = ggplot2::diamonds,
#'   strata = "cut",
#'   vars = c("color")
#' )
#' # t3 |> adorn_tidytableone()
#'
#' #### Without strata --------------------------------
#' # Continuous + categorical
#' t4 <- create_tidytableone(
#'   data = ggplot2::diamonds,
#'   strata = NULL,
#'   vars = c("carat","color","clarity","depth","table","price")
#' )
#' # t4 |> adorn_tidytableone()
#'
#' # Continuous only
#' t5 <- create_tidytableone(
#'   data = ggplot2::diamonds,
#'   strata = NULL,
#'   vars = c("carat")
#' )
#' # t5 |> adorn_tidytableone()
#'
#' # Categorical only
#' t6 <- create_tidytableone(
#'   data = ggplot2::diamonds,
#'   strata = NULL,
#'   vars = c("color")
#' )
#' # t6 |> adorn_tidytableone()
#' 
#' 
#' #### With Built in data set --------------------------------
#'
#' tab1 <- create_tidytableone(data = pbc_mayo,
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

#' @keywords internal
.create_tidytableone_core <- function(data,
                                      strata = NULL,
                                      vars,
                                      na_level = "(Missing)",
                                      b_replicates = 2000,
                                      checkbox = NULL,
                                      checkbox_opts = list(
                                        denom = "group",
                                        pvals = "per_level",
                                        test  = "auto",
                                        p_adjust = "none",
                                        show_any = TRUE,
                                        note = "Participants could select more than one option; percentages may exceed 100%."
                                      ),
                                      ...) {
  
  # Silence no visible binding for global variable
  dat <- res <- n_level_valid <- n_strata_valid <- label <- sort1 <- NULL
  
  
  # Handle no strata case by calling the no strata version of the function
  # inside .create_tidytableone_core(), replace the current no‑strata block:
  if (is.null(strata)) {
    if (is.null(checkbox)) {
      res_stats <- create_tidytableone_no_strata(
        data = data,
        vars = vars,
        na_level = na_level,
        b_replicates = b_replicates,
        ...
      )
    } else {
      res_stats <- create_tidytableone_no_strata_checkbox(
        data = data,
        vars = vars,
        na_level = na_level,
        b_replicates = b_replicates,
        checkbox = checkbox,
        checkbox_opts = checkbox_opts,
        ...
      )
    }
    return(res_stats)
  }
  
  # Convert ordered factors to regular factors
  data <- data %>%
    dplyr::mutate_if(.tbl = .,
                     .predicate = ~ ("ordered" %in% class(.)),
                     .funs = ~ factor(., ordered = FALSE))
  
  
  # Convert the 'strata' argument to a symbol for use in tidy evaluation
  strata_sym <- rlang::ensym(strata)
  
  # Coerce strata to factor once up front to avoid warnings later
  if (!rlang::quo_is_null(rlang::enquo(strata))) {
    data[[rlang::as_name(strata_sym)]] <- as.factor(data[[rlang::as_name(strata_sym)]])
  }
  
  
  # Extract variable labels for later use BEFORE any coercion that could drop them
  var_lbls <- tibble::tibble(var = names(data)) |>
    mutate(label = purrr::map_chr(.x = data[, var],
                                  .f = ~ get_var_labels(x = .x)))
  
  
  # normalize variable types (preserve intended levels) ----
  # Characters -> factor with levels in order of appearance (stable)
  # Logicals   -> factor with levels FALSE, TRUE
  # Ordered    -> already converted above to plain factor (keeps existing level order)
  data[vars] <- lapply(vars, function(v) {
    x <- data[[v]]
    lbl <- attr(x, "label", exact = TRUE)
    
    y <- if (is.character(x)) {
      # For plain characters, convert to factor using levels in the *original* data
      # not the subset.
      lv <- sort(unique(data[[v]][!is.na(data[[v]])]))
      factor(x, levels = lv)
    } else if (is.logical(x)) {
      factor(x, levels = c(FALSE, TRUE))
    } else if (is.factor(x)) {
      # Preserve ALL original factor levels unchanged
      factor(x, levels = levels(x))
    } else {
      x
    }
    
    if (!is.null(lbl)) attr(y, "label") <- lbl
    y
  })
  
  
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
  
  
  # Identify Checkbox (multi-response) spec
  # Normalize and validate the checkbox spec (tibble with columns:
  # var, overall_lbl, checkbox_lbl, checkbox_txt)
  cb_spec <- NULL
  
  if (!is.null(checkbox)) {
    cb_spec <- validate_checkbox_spec(
      checkbox,
      data = data,
      vars  = vars
    )
    
    # Build a list of blocks: one element per overall_lbl
    cb_blocks <- prepare_checkbox_blocks(cb_spec)
    
    # Remove checkbox vars from the standard categorical path
    cb_vars <- unique(cb_spec$var)
  } else {
    cb_blocks <- list()
    cb_vars <- character(0)
  }
  
  if (!is.null(checkbox)) {
    cb_vi <- tibble::tibble(
      var = names(cb_blocks),
      level = NA_character_,
      var_type = "categorical",
      class = "checkbox",
      sort1 = match(var, vars),  # order by first mention in `vars`
      sort2 = 0L
    )
    
    var_info <- dplyr::bind_rows(var_info, cb_vi)
    
  }
  
  # Identify categorical and continuous variables
  cat_vars <- var_info |>
    dplyr::filter(var_type == "categorical",
                  is.na(class) | class != "checkbox") |>
    dplyr::pull(var) |>
    unique() |>
    setdiff(strata) |>
    setdiff(cb_vars)   # still exclude the individual checkbox columns
  
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
  
  htest_res <- tibble::tibble()
  
  
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
  
  # # Emit a friendly warning once per call if chi-squared looks shaky
  # if (isTRUE(getOption("tidytableone.warn_chisq", TRUE))) {
  #   if (nrow(htest_res) > 0 && "check_categorical_test" %in% names(htest_res)) {
  #     bad <- unique(htest_res$var[htest_res$check_categorical_test == "warning"])
  #     if (length(bad) > 0) {
  #       warning(
  #         glue::glue(
  #           "Chi-squared assumptions may be violated for: {paste(bad, collapse = ', ')}. ",
  #           "Consider Fisher's exact test (returned in `fisher_test`)."
  #         ),
  #         call. = FALSE
  #       )
  #     }
  #   }
  # }
  
  
  #### Checkbox blocks (multi-response) --------------------------------
  res_checkbox <- NULL
  
  if (length(cb_blocks) > 0) {
    res_checkbox <- process_checkbox_blocks_strata(
      data          = data,
      blocks        = cb_blocks,
      opts          = checkbox_opts,
      strata_var    = rlang::as_name(strata_sym),
      strata_levels = levels(data[[rlang::as_name(strata_sym)]]) %||%
        unique(as.character(data[[rlang::as_name(strata_sym)]]))
    ) %>%
      dplyr::rename(!! rlang::as_name(strata_sym) := strata)
    
    # Always provide the column so downstream code/tests can rely on it
    if (!"p_value_level" %in% names(res_checkbox)) {
      res_checkbox <- dplyr::mutate(res_checkbox, p_value_level = NA_real_)
    }
    
    # If per-level p-values were requested, fill them in once
    if (!is.null(strata) && isTRUE(checkbox_opts$pvals %in% c("per_level"))) {
      res_checkbox <- add_pvalues_checkbox(
        tab        = res_checkbox,
        data       = data,
        strata_var = rlang::as_name(strata_sym),
        blocks     = cb_blocks,
        test       = checkbox_opts$test    %||% "auto",
        p_adjust   = checkbox_opts$p_adjust %||% "none",
        B          = 2000
      )
    }
    
    # Get the SMD
    smd_cb <- get_smd_checkbox(
      data   = data,
      strata = strata,   # e.g., "group"
      blocks = cb_blocks
    )
    
    # res_checkbox <- res_checkbox %>%
    #   dplyr::left_join(smd_cb, by = "var", suffix = c("", ".cb")) %>%
    #   dplyr::mutate(smd = dplyr::coalesce(smd, smd.cb)) %>%
    #   dplyr::select(-dplyr::any_of("smd.cb"))
    
    res_checkbox <- res_checkbox %>%
      dplyr::left_join(smd_cb, by = "var")
    
    
    # Tag as categorical for downstream p-value formatting (keeps compat)
    res_checkbox <- res_checkbox %>%
      dplyr::mutate(var_type = "categorical", class = "checkbox")
  }
  
  # Bind checkbox rows with the rest (if present)
  if (!is.null(res_checkbox)) {
    res_stats <- dplyr::bind_rows(res_stats, res_checkbox)
  }
  
  # Attach footnote & map so adorn_* can indent, print note, etc.
  if (length(cb_blocks) > 0) {
    attr(res_stats, "checkbox_blocks") <- cb_blocks
    attr(res_stats, "checkbox_opts")   <- checkbox_opts
  }
  
  #### Ensure zero-count rows exist for unobserved categorical levels --------------------------------
  
  if (length(cat_vars) > 0) {
    
    # Desired levels per categorical var from the *normalized* data
    lvl_tbl_cat <- purrr::map_dfr(cat_vars, function(v) {
      lv <- levels(data[[v]])
      tibble::tibble(var = v, level = lv, level_order = seq_along(lv))
    })
    
    # Real strata column name (e.g., "healed_f")
    strata_col  <- rlang::as_name(strata_sym)
    strata_lvls <- if (!is.null(strata)) levels(data[[strata_col]]) else character(0)
    all_strata  <- c("Overall", strata_lvls)
    
    # --- robust categorical slice (works with/without `class`) ---
    has_class <- "class" %in% names(res_stats)
    n_rs <- nrow(res_stats)
    
    cond_cat <- if (n_rs > 0) (res_stats$var %in% cat_vars) else logical(0)
    cond_not_checkbox <- if (has_class && n_rs > 0) {
      is.na(res_stats$class) | res_stats$class != "checkbox"
    } else if (n_rs > 0) {
      rep(TRUE, n_rs)
    } else {
      logical(0)
    }
    is_plain_cat <- if (n_rs > 0) cond_cat & cond_not_checkbox else logical(0)
    
    # Categorical piece to scaffold
    res_cat <- if (n_rs > 0) res_stats[is_plain_cat, , drop = FALSE] else res_stats
    
    # ---- Ensure columns needed post-join exist on res_cat (types matter) ----
    if (!"var_type"        %in% names(res_cat)) res_cat$var_type        <- NA_character_
    if (!"n_level"         %in% names(res_cat)) res_cat$n_level         <- NA_integer_
    if (!"n_level_valid"   %in% names(res_cat)) res_cat$n_level_valid   <- NA_integer_
    if (!"pct"             %in% names(res_cat)) res_cat$pct             <- NA_real_
    if (!"n_strata"        %in% names(res_cat)) res_cat$n_strata        <- NA_integer_
    if (!"n_strata_valid"  %in% names(res_cat)) res_cat$n_strata_valid  <- NA_integer_
    if (!"label"          %in% names(res_cat)) res_cat$label          <- NA_character_  
    
    # Full grid (var x level x strata) using actual strata column
    scaffold <- lvl_tbl_cat %>%
      dplyr::select(var, level) %>%
      tidyr::crossing(!!rlang::sym(strata_col) := all_strata)
    
    join_keys <- c("var", "level", strata_col)
    
    res_cat_completed <- scaffold %>%
      dplyr::left_join(res_cat, by = join_keys) %>%
      # bring in data-defined labels and fill if missing
      dplyr::left_join(var_lbls, by = "var", suffix = c("", ".from_data")) %>%
      dplyr::mutate(
        label          = dplyr::coalesce(label, label.from_data),
        var_type       = dplyr::coalesce(var_type, "categorical"),
        n_level        = dplyr::coalesce(n_level, 0L),
        n_level_valid  = dplyr::coalesce(n_level_valid, 0L),
        pct            = dplyr::coalesce(pct, 0)
        # keep n_strata / n_strata_valid as-is (may remain NA here)
      ) %>%
      dplyr::select(-dplyr::any_of("label.from_data")) %>%
      dplyr::arrange(factor(var, levels = vars), level, !!rlang::sym(strata_col))
    
    # Everything else (continuous + checkbox) unchanged
    res_other <- if (n_rs > 0) res_stats[!is_plain_cat, , drop = FALSE] else res_stats
    
    # Recombine
    res_stats <- dplyr::bind_rows(res_other, res_cat_completed)
    
    # Drop any ghost rows with missing var name
    res_stats <- res_stats |> dplyr::filter(!is.na(var) & var != "")
    
  }
  
  
  #### Arrange, combine, and clean up results --------------------------------
  
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
  
  
  #### Return warnings (if any) --------------------------------
  
  if (isTRUE(getOption("tidytableone.warn_chisq", TRUE))) {
    if ("check_categorical_test" %in% names(res_stats)) {
      
      flagged_rows <- res_stats %>%
        dplyr::filter(check_categorical_test == "warning")
      
      if (nrow(flagged_rows) > 0) {
        vars_to_report <- flagged_rows %>%
          dplyr::mutate(
            var_for_msg = dplyr::if_else(
              class == "checkbox" & !is.na(level_var) & level_var != "",
              level_var,                      # e.g., "race___5"
              as.character(var)               # e.g., "gender"
            )
          ) %>%
          dplyr::pull(var_for_msg) %>%
          unique() %>%
          stats::na.omit()
        
        if (length(vars_to_report) > 0) {
          warning(
            glue::glue(
              "Chi-squared assumptions may be violated for: {paste(vars_to_report, collapse = ', ')}. ",
              "Consider Fisher's exact test (returned in `fisher_test`)."
            ),
            call. = FALSE
          )
        }
      }
    }
  }
  
  
  #### One last tidying -------------------------------- 
  
  
  res_stats <- res_stats |> 
    dplyr::select(-level_var)
  
  # fix_smd_cols <- function(df) {
  #   # coalesce any of these, in this order, into `smd`, then drop the extras
  #   candidates <- c("smd", "smd.cb", "smd.y", "smd.x")
  #   have <- intersect(candidates, names(df))
  #   if (length(have) == 0) return(df)
  #   df %>%
  #     dplyr::mutate(smd = dplyr::coalesce(!!!rlang::syms(have))) %>%
  #     dplyr::select(-dplyr::any_of(setdiff(have, "smd")))
  # }
  # 
  # # use it:
  # res_stats <- fix_smd_cols(res_stats)
  
  if ("smd.x" %in% names(res_stats) || "smd.y" %in% names(res_stats)) {
    res_stats <- res_stats %>%
      dplyr::mutate(
        smd = dplyr::coalesce(smd, smd.y, smd.x)
      ) %>%
      dplyr::select(-dplyr::any_of(c("smd.y", "smd.x")))
  }
  
  #### Final ordering: respect user-supplied `vars` -------------------------------- 
  
  # strata ordering (Overall first)
  if ("strata" %in% names(res_stats)) {
    strata_levels <- unique(as.character(res_stats$strata))
    strata_levels <- c("Overall", setdiff(strata_levels, "Overall"))
    res_stats <- res_stats |>
      dplyr::mutate(strata = factor(strata, levels = strata_levels))
  }
  
  # variable ordering (vars vector)
  res_stats <- res_stats |>
    dplyr::mutate(
      var_order = match(var, vars),
      var_order = dplyr::if_else(is.na(var_order), 1e9, var_order)
    )
  
  # within-var ordering: if level_order exists, use it; otherwise preserve current row order
  has_level_order <- "level_order" %in% names(res_stats)
  
  res_stats <- res_stats |>
    dplyr::group_by(var) |>
    dplyr::mutate(
      .row_in_var = dplyr::row_number(),
      level_order2 = if (has_level_order) dplyr::coalesce(level_order, .row_in_var) else .row_in_var
    ) |>
    dplyr::ungroup()
  
  res_stats <- res_stats |>
    dplyr::arrange(var_order, level_order2, strata) |>
    dplyr::select(-var_order, -.row_in_var, -dplyr::any_of("level_order2"))
  
  
  
  #### Return results --------------------------------
  
  return(res_stats)
  
}




#' Create a tidy “Table 1”
#'
#' @inheritParams .create_tidytableone_core
#' @export
#' @examples
#' # Basic Table 1 using built-in example data
#' tab1 <- create_tidytableone(
#'   data = pbc_mayo,
#'   strata = "trt",
#'   vars = c(
#'     "time","status","trt","age","sex","ascites","hepato","spiders","edema",
#'     "bili","chol","albumin","copper","alk_phos","ast","trig","platelet",
#'     "protime","stage"
#'   )
#' )
#' tab1
#'
#' @examplesIf interactive()
#' library(dplyr)
#'
#' dplyr::glimpse(tab1)
#' tab1 |> adorn_tidytableone()
#'
#' # With checkboxes (multi-select style)
#' set.seed(1)
#' pbc_mayo2 <- pbc_mayo |>
#'   mutate(
#'     race___1 = sample(c("Checked","Unchecked"), n(), TRUE),
#'     race___2 = sample(c("Checked","Unchecked"), n(), TRUE)
#'   )
#'
#' tab2 <- create_tidytableone(
#'   data = pbc_mayo2,
#'   strata = "trt",
#'   vars = c("trt","age","sex","race___1","race___2"),
#'   checkbox = tibble::tribble(
#'     ~var,       ~overall_lbl, ~checkbox_lbl, ~checkbox_txt,
#'     "race___1", "Race",       "White",       "Checked",
#'     "race___2", "Race",       "Black",       "Checked"
#'   )
#' )
#'
#' dplyr::glimpse(tab2)
#' tab2 |> adorn_tidytableone()
#'
#' # With variable labels
#' library(sjlabelled)
#' pbc_mayo2 <- sjlabelled::var_labels(
#'   x = pbc_mayo2,
#'   sex = "Gender",
#'   age = "Age (years)"
#' )
#'
#' tab3 <- create_tidytableone(
#'   data = pbc_mayo2,
#'   strata = "trt",
#'   vars = c("trt","age","sex","race___1","race___2"),
#'   checkbox = tibble::tribble(
#'     ~var,       ~overall_lbl, ~checkbox_lbl, ~checkbox_txt,
#'     "race___1", "Race",       "White",       "Checked",
#'     "race___2", "Race",       "Black",       "Checked"
#'   )
#' )
#'
#' tab3 |> adorn_tidytableone()
create_tidytableone <- function(data,
                                strata = NULL,
                                vars,
                                na_level = "(Missing)",
                                b_replicates = 2000,
                                checkbox = NULL,
                                checkbox_opts = NULL, 
                                ...) {
  
  # # ensure defaults if user didn't supply checkbox_opts
  # if (is.null(checkbox_opts)) {
  #   checkbox_opts <- list(
  #     denom    = "group",
  #     pvals    = "per_level",
  #     test     = "auto",
  #     p_adjust = "none",
  #     show_any = TRUE,
  #     note     = "Participants could select more than one option; percentages may exceed 100%."
  #   )
  # }
  
  checkbox_opts <- normalize_checkbox_opts(checkbox_opts)
  
  checkbox_opts <- validate_checkbox_opts(checkbox_opts)
  
  .create_tidytableone_core(
    data = data,
    strata = strata,
    vars = vars,
    na_level = na_level,
    b_replicates = b_replicates,
    checkbox = checkbox,          # <-- pass through
    checkbox_opts = checkbox_opts,
    ...
  )
}


#' Create tidy Table 1 with checkbox variables
#'
#' This is a wrapper for [create_tidytableone()] that supports
#' multi-select (checkbox) variables, such as REDCap checkboxes.
#'
#' @inheritParams .create_tidytableone_core
#' @param checkbox A tibble with columns:
#'   \itemize{
#'     \item `var` — checkbox variable name in `data` (e.g., `"race___1"`).
#'     \item `overall_lbl` — Label for the collapsed group in Table 1
#'       (e.g., `"Race"`).
#'     \item `checkbox_lbl` — Label for the individual checkbox level
#'       (e.g., `"White"`).
#'     \item `checkbox_txt` — Text used to indicate selection (e.g., `"Checked"`).
#'   }
#' @param checkbox_opts A list controlling checkbox processing. Can include:
#'   \describe{
#'     \item{denom}{Character: `"group"` or `"overall"` denominator.}
#'     \item{pvals}{Logical: whether to show p-values.}
#'     \item{test}{Test type for categorical variables (`"chisq"`, `"fisher"`).}
#'     \item{p_adjust}{Adjustment method for p-values.}
#'     \item{show_any}{Logical: whether to add an "Any selected" row.}
#'     \item{note}{Character: note text to include.}
#'   }
#' @param ... Additional arguments passed to [create_tidytableone()].
#'
#' @details
#' This function first calls [create_tidytableone()] to produce the
#' standard Table 1, then expands and relabels multi-select (checkbox)
#' variables so each selection appears as a separate row.
#'
#' **Example `checkbox` tibble:**
#' ```r
#' tibble::tribble(
#'   ~var, ~overall_lbl, ~checkbox_lbl, ~checkbox_txt,
#'   "race___1", "Race", "White", "Checked",
#'   "race___2", "Race", "Black or African-American", "Checked"
#' )
#' ```
#'
#' @seealso [create_tidytableone()]
#'
#' @return A tibble in the same format as [create_tidytableone()].
#' @export
#'
#' @examples
#' # Minimal smoke test (always runs quickly)
#' set.seed(123)
#' tiny <- tibble::tibble(
#'   group = sample(c("A","B"), 20, TRUE),
#'   gender = sample(c("Female","Male"), 20, TRUE),
#'   age = sample(18:80, 20, TRUE),
#'   race___1 = sample(c("Checked","Unchecked"), 20, TRUE),
#'   race___2 = sample(c("Checked","Unchecked"), 20, TRUE)
#' )
#'
#' create_tidytableone_checkbox(
#'   data = tiny,
#'   strata = "group",
#'   vars = c("gender","age","race___1","race___2"),
#'   checkbox = tibble::tribble(
#'     ~var,       ~overall_lbl, ~checkbox_lbl, ~checkbox_txt,
#'     "race___1", "Race",       "White",       "Checked",
#'     "race___2", "Race",       "Black",       "Checked"
#'   )
#' )
#'
#' @examplesIf interactive()
#' # Larger, fully worked example (runs only in interactive sessions)
#' 
#' library(dplyr)
#' library(sjlabelled)
#' library(tibble)
#' library(tidyr)
#' 
#' set.seed(42)
#' df2 <- tibble(
#'   record_id = 1:100,
#'   gender = sample(c("Female", "Male"), 100, TRUE),
#'   age = sample(18:85, 100, TRUE),
#'   education = sample(c("High-school", "College", "Graduate school"), 100, TRUE),
#'   ethnicity = sample(c("Hispanic", "Non-hispanic"), 100, TRUE),
#'   income = sample(20000:120000, 100, TRUE),
#'   marital_status = sample(c("Married", "Single"), 100, TRUE),
#'   group = sample(c("Treatment", "Control"), 100, TRUE),
#'   key = sample(paste0("race___", c(1:6, 98)), 100, TRUE),
#'   value = "Checked"
#' ) %>%
#'   mutate(key = factor(key, levels = paste0("race___", c(1:6, 98)))) %>%
#'   spread(key, value) %>%
#'   mutate(across(starts_with("race___"), ~ replace_na(., "Unchecked")))
#' 
#' # Checkbox mapping
#' cb_map <- tribble(
#'   ~var,        ~overall_lbl, ~checkbox_lbl,                              ~checkbox_txt,
#'   "race___1",  "Race",       "White",                                    "Checked",
#'   "race___2",  "Race",       "Black or African-American",                "Checked",
#'   "race___3",  "Race",       "American Indian or Alaska Native",         "Checked",
#'   "race___4",  "Race",       "Asian",                                    "Checked",
#'   "race___5",  "Race",       "Native Hawaiian or Pacific Islander",      "Checked",
#'   "race___6",  "Race",       "Other",                                    "Checked",
#'   "race___98", "Race",       "Prefer not to answer",                     "Checked"
#' )
#' 
#' # Basic table with checkboxes
#' create_tidytableone_checkbox(
#'   data = df2,
#'   strata = "group",
#'   vars = c("gender", "age", "education", "ethnicity", "income", "marital_status",
#'            paste0("race___", c(1:6, 98))),
#'   checkbox = cb_map
#' )
#' 
#' # With variable labels
#' df2_labelled <- sjlabelled::var_labels(
#'   df2,
#'   gender = "Gender", age = "Age (years)", education = "Education",
#'   ethnicity = "Ethnicity", income = "Income", marital_status = "Marital status",
#'   race___1 = "Race (White)", race___2 = "Race (Black)", race___3 = "Race (American Indian)",
#'   race___4 = "Race (Asian)", race___5 = "Race (Native Hawaiian)", race___6 = "Race (Other)",
#'   race___98 = "Race (Prefer not to answer)"
#' )
#' 
#' create_tidytableone_checkbox(
#'   data = df2_labelled,
#'   strata = "group",
#'   vars = c("gender", "age", "education", "ethnicity", "income", "marital_status",
#'            paste0("race___", c(1:6, 98))),
#'   checkbox = cb_map
#' )
#' 
#'
create_tidytableone_checkbox <- function(data,
                                         strata = NULL,
                                         vars,
                                         na_level = "(Missing)",
                                         b_replicates = 2000,
                                         checkbox = NULL,
                                         checkbox_opts = list(
                                           denom   = "group",
                                           pvals   = "per_level",
                                           test    = "auto",
                                           p_adjust = "none",
                                           show_any = TRUE,
                                           note     = "Participants could select more than one option; percentages may exceed 100%."
                                         ),
                                         ...) {
  .create_tidytableone_core(
    data = data,
    strata = strata,
    vars = vars,
    na_level = na_level,
    b_replicates = b_replicates,
    checkbox = checkbox,
    checkbox_opts = checkbox_opts,
    ...
  )
}


#' @rdname tidytableone-deprecated
#' @export
create_tidy_table_one <- function(...) {
  # .Deprecated("create_tidytableone")
  .Defunct("create_tidytableone")
  dots <- list(...)
  if (!is.null(dots$.vars)) {
    dots$vars <- dots$.vars
    dots$.vars <- NULL
  }
  do.call(create_tidytableone, dots)
}

#' @rdname tidytableone-deprecated
#' @export
create_tidy_table_one_checkbox <- function(...) {
  # .Deprecated("create_tidytableone_checkbox")
  .Defunct("create_tidytableone_checkbox")
  dots <- list(...)
  if (!is.null(dots$.vars)) {
    dots$vars <- dots$.vars
    dots$.vars <- NULL
  }
  do.call(create_tidytableone_checkbox, dots)
}








