#' tidytableone: Tidy construction of "Table 1"
#'
#' A tidy approach to creating the "Table 1" of study characteristics common
#' in biomedical research. The two main functions are
#' [create_tidytableone()] (which produces a tibble of summary statistics and
#' hypothesis-test results) and [adorn_tidytableone()] (which formats that
#' tibble into a printable Table 1).
#'
#' @keywords internal
"_PACKAGE"

# ---------------------------------------------------------------------------
# Package-level constants
# ---------------------------------------------------------------------------
# Internal constants used throughout the package. Prefer these in new code
# rather than re-typing the literal strings. Existing code still uses the
# literals; they'll be migrated over during the refactor.

.tidytableone_overall_label       <- "Overall"
.tidytableone_missing_level       <- "(Missing)"
.tidytableone_any_selected_label  <- "Any selected"
.tidytableone_any_selected_suffix <- "___any_selected"

# ---------------------------------------------------------------------------
# globalVariables: silence R CMD check warnings for NSE / tidy-eval columns
# ---------------------------------------------------------------------------
# Consolidated from the previous `globalVariables` block in tidytableone.R
# and the supplementary globals.R. We'll prune this as functions migrate to
# .data / .env pronouns in later phases.

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".",
    # base columns
    "var", "level", "level_var", "level_chr", "level_id",
    "strata", "strata_var", "strata_order",
    # counts / percents
    "n", "n_level", "n_strata", "n_level_valid", "n_strata_valid",
    "pct", "pct_valid", "group_n",
    # continuous stats
    "mean", "sd", "p0", "p25", "p50", "p75", "p100", "cv",
    "shapiro_test", "ks_test", "ad_test",
    # hypothesis tests
    "bartlett_test", "chisq_test", "chisq_test_no_correction",
    "chisq_test_simulated", "check_categorical_test",
    "fisher_test", "fisher_test_simulated",
    "kruskal_test", "levene_test",
    "oneway_test", "oneway_test_equal_var", "oneway_test_unequal_var",
    "chisq_test_ht", "chisq_test_no_correction_ht", "chisq_test_simulated_ht",
    "fisher_test_ht", "fisher_test_simulated_ht",
    # metadata
    "class", "var_type", "var_level", "label", "smd",
    "smd.x", "smd.y", "label.from_data",
    # checkbox spec
    "checkbox_lbl", "checkbox_txt", "overall_lbl",
    # adorn intermediates
    "glue_formula", "num_not_miss", "p_value", "p_raw",
    "var_for_msg", "is_any",
    # internal helpers
    "form", "tab", "tbl", "value", "x", "y", "selected",
    "sort1", "sort2",
    "var_order", "level_order", "level_order2",
    ".any", ".denom", ".strata",
    ".level_first_row", ".level_id", ".orig_row",
    ".row_in_var", ".is_any", ".is_missing",
    ".is_missing_level", ".is_any_selected",
    ".base_max_num", ".base_max_int",
    # legacy / grouping helpers (will go away in Phases 2-4)
    "group_id", "group_label_first", "var_pos", "first_pos"
  ))
}
