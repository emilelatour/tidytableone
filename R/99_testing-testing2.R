# library(dplyr)
# library(labelled)
# library(tibble)
# library(tidyr)
# 
# set.seed(42)
# df2 <- tibble(
#   record_id = 1:100,
#   gender = sample(c("Female", "Male"), 100, TRUE),
#   age = sample(18:85, 100, TRUE),
#   education = sample(c("High-school", "College", "Graduate school"), 100, TRUE),
#   ethnicity = sample(c("Hispanic", "Non-hispanic"), 100, TRUE),
#   income = sample(20000:120000, 100, TRUE),
#   marital_status = sample(c("Married", "Single"), 100, TRUE),
#   group = sample(c("Treatment", "Control"), 100, TRUE),
#   key = sample(paste0("race___", c(1:6, 98)), 100, TRUE),
#   value = "Checked"
# ) %>%
#   mutate(key = factor(key, levels = paste0("race___", c(1:6, 98)))) %>%
#   spread(key, value) %>%
#   mutate(across(starts_with("race___"), ~ replace_na(., "Unchecked")))
# 
# # Add some NAs
# df2 <- df2 |>
#   mutate(race___1 = dplyr::if_else(record_id %in% sample(1:100, size = 20), NA_character_, race___1))
# 
# df2 <- df2 |>
#   mutate(gender = dplyr::if_else(record_id %in% sample(1:100, size = 10), NA_character_, gender))
# 
# # Checkbox mapping
# cb_map <- tribble(
#   ~var,        ~overall_lbl, ~checkbox_lbl,                              ~checkbox_txt,
#   "race___1",  "Race",       "White",                                    "Checked",
#   "race___2",  "Race",       "Black or African-American",                "Checked",
#   "race___3",  "Race",       "American Indian or Alaska Native",         "Checked",
#   "race___4",  "Race",       "Asian",                                    "Checked",
#   "race___5",  "Race",       "Native Hawaiian or Pacific Islander",      "Checked",
#   "race___6",  "Race",       "Other",                                    "Checked",
#   "race___98", "Race",       "Prefer not to answer",                     "Checked"
# )
# 
# 
# # With variable labels
# df2_labelled <- set_variable_labels(
#   df2,
#   gender = "Gender", age = "Age (years)", education = "Education",
#   ethnicity = "Ethnicity", income = "Income", marital_status = "Marital status",
#   race___1 = "Race (White)", race___2 = "Race (Black)", race___3 = "Race (American Indian)",
#   race___4 = "Race (Asian)", race___5 = "Race (Native Hawaiian)", race___6 = "Race (Other)",
#   race___98 = "Race (Prefer not to answer)"
# )
# 
# 
# skimr::skim(df2_labelled)
# 
# # Strata and checkboxes
# create_tidytableone(
#   data = df2_labelled,
#   strata = "group",
#   vars = c("gender", "age", "education", "ethnicity", "income", "marital_status",
#            paste0("race___", c(1:6, 98))),
#   checkbox = cb_map
# )
# 
# # No strata and checkboxes
# create_tidytableone(
#   data = df2_labelled,
#   strata = NULL,
#   vars = c("gender", "age", "education", "ethnicity", "income", "marital_status",
#            paste0("race___", c(1:6, 98))),
#   checkbox = cb_map
# )
# 
# # Strata and No checkboxes
# create_tidytableone(
#   data = df2_labelled,
#   strata = "group",
#   vars = c("gender", "age", "education", "ethnicity", "income", "marital_status",
#            paste0("race___", c(1:6, 98)))
# )
# 
# # No strata and  no checkboxes
# create_tidytableone(
#   data = df2_labelled,
#   strata = NULL,
#   vars = c("gender", "age", "education", "ethnicity", "income", "marital_status",
#            paste0("race___", c(1:6, 98)))
# )
# 
# 
# # Strata and checkboxes
# create_tidytableone(
#   data = df2_labelled,
#   strata = "group",
#   vars = c("gender", "age", "education", "ethnicity", "income", "marital_status",
#            paste0("race___", c(1:6, 98))),
#   checkbox = cb_map
# ) |>
#   adorn_tidytableone()
# 
# # No strata and checkboxes
# create_tidytableone(
#   data = df2_labelled,
#   strata = NULL,
#   vars = c("gender", "age", "education", "ethnicity", "income", "marital_status",
#            paste0("race___", c(1:6, 98))),
#   checkbox = cb_map
# ) |>
#   adorn_tidytableone()
# 
# # Strata and No checkboxes
# create_tidytableone(
#   data = df2_labelled,
#   strata = "group",
#   vars = c("gender", "age", "education", "ethnicity", "income", "marital_status",
#            paste0("race___", c(1:6, 98)))
# ) |>
#   adorn_tidytableone()
# # Not working
# 
# # No strata and  no checkboxes
# create_tidytableone(
#   data = df2_labelled,
#   strata = NULL,
#   vars = c("gender", "age", "education", "ethnicity", "income", "marital_status",
#            paste0("race___", c(1:6, 98)))
# ) |>
#   adorn_tidytableone()
# # Not working either
# 
# #### Next steps for robustness --------------------------------
# 
# # Other variations I can think of are more “edge conditions” than new categories:
# # 	•	Empty vars vector → should fail gracefully.
# # 	•	Strata with only 1 level → make sure it still runs and produces sensible “Overall”.
# # 	•	Checkbox map doesn’t match vars → e.g. passing a checkbox block that’s not in the vars list. Right now I think you warn/error, but that’s a test worth running.
# # 	•	Continuous-only or categorical-only tables → ensures stats flow right.
# # 	•	All missing in a variable → confirms NA handling and adorn won’t break.
# # 	•	Factor with unused levels → should either drop or display cleanly.
# 
# 
# 
# 
# 
# foo <- create_tidytableone(
#   data = df2_labelled,
#   strata = "group",
#   vars = c("gender", "age", "education", "ethnicity", "income", "marital_status",
#            paste0("race___", c(1:6, 98))),
#   checkbox = cb_map
# )
# 
# 
# 
# foo
# 
# 
# tab = foo$tab
#                                  data = foo$data
#                                  strata_var = foo$strata_var
#                                  blocks = foo$blocks
#                                  test = foo$test
#                                  p_adjust = foo$p_adjust
#                                  B = foo$B
# 
# 
# 
# if (!("class" %in% names(tab))) return(tab)
#   if (!any(tab$class == "checkbox")) return(tab)
# 
# 
# 
#   # Build a tidy frame of per-variable tests
#   per_var <- list()
# 
#   for (bl in blocks) {
#     # build 2×2 tables for each checkbox variable (Selected vs Not selected)
#     for (v in bl$vars) {
#       # strata (factor) and selected flag
#       grp <- factor(data[[strata_var]])
#       sel <- factor(ifelse(data[[v]] == bl$select_txt[[v]], "Selected", "Not selected"),
#                     levels = c("Not selected","Selected"))
#       tbl <- table(grp, sel, useNA = "no")
# 
#       per_var[[length(per_var) + 1]] <- tibble::tibble(
#         var                         = v,
#         chisq_test                  = safe_chisq(tbl,  correct = TRUE,  simulate.p.value = FALSE),
#         chisq_test_no_correction    = safe_chisq(tbl,  correct = FALSE, simulate.p.value = FALSE),
#         chisq_test_simulated        = safe_chisq(tbl,  correct = TRUE,  simulate.p.value = TRUE,  B = B),
#         fisher_test                 = safe_fisher(tbl, simulate.p.value = FALSE),
#         fisher_test_simulated       = safe_fisher(tbl, simulate.p.value = TRUE, B = B),
#         check_categorical_test      = flag_chisq_ok(tbl)
#       )
#     }
# 
#     # ---- synthetic "Any selected" variable ----
#     # Build the synthetic var name and only compute if that row is present in `tab`
#       stem <- sub("___.*$", "", bl$vars[[1]])
#       var_any <- paste0(tolower(stem), "___any_selected")
# 
#       # any selected across the block
#       sel_any <- factor(
#         ifelse(rowSums(as.data.frame(lapply(bl$vars,
#                                             function(v) as.integer(data[[v]] == bl$select_txt[[v]]))),
#                        na.rm = TRUE) > 0L, "Selected", "Not selected"),
#         levels = c("Not selected","Selected")
#       )
#       grp <- factor(data[[strata_var]])
#       tbl_any <- table(grp, sel_any, useNA = "no")
# 
#       per_var[[length(per_var) + 1]] <- tibble::tibble(
#         var                         = var_any,
#         chisq_test                  = safe_chisq(tbl_any,  correct = TRUE,  simulate.p.value = FALSE),
#         chisq_test_no_correction    = safe_chisq(tbl_any,  correct = FALSE, simulate.p.value = FALSE),
#         chisq_test_simulated        = safe_chisq(tbl_any,  correct = TRUE,  simulate.p.value = TRUE,  B = B),
#         fisher_test                 = safe_fisher(tbl_any, simulate.p.value = FALSE),
#         fisher_test_simulated       = safe_fisher(tbl_any, simulate.p.value = TRUE, B = B),
#         check_categorical_test      = flag_chisq_ok(tbl_any)
#       )
#     
#   }
# 
#   tests <- dplyr::bind_rows(per_var)
# 
#   # optional multiple-comparison adjustment within each checkbox block
#   if (!identical(p_adjust, "none")) {
#     # infer block stem per var, adjust inside stems independently
#     tests <- tests %>%
#       dplyr::mutate(.stem = sub("___.*$", "", .data$var)) %>%
#       dplyr::group_by(.data$.stem) %>%
#       dplyr::mutate(
#         chisq_test               = stats::p.adjust(.data$chisq_test,               method = p_adjust),
#         chisq_test_no_correction = stats::p.adjust(.data$chisq_test_no_correction, method = p_adjust),
#         chisq_test_simulated     = stats::p.adjust(.data$chisq_test_simulated,     method = p_adjust),
#         fisher_test              = stats::p.adjust(.data$fisher_test,              method = p_adjust),
#         fisher_test_simulated    = stats::p.adjust(.data$fisher_test_simulated,    method = p_adjust)
#       ) %>%
#       dplyr::ungroup() %>%
#       dplyr::select(-.data$.stem)
#   }
# 
#   # Join onto checkbox rows; repeat the same p-values across all strata/levels
#   tab %>%
#     dplyr::left_join(tests, by = "var")
#   