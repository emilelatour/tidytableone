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