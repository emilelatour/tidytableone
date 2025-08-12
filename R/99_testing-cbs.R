# 
# # devtools::load_all(".")
# 
# 
# #### TESTING --------------------------------
# 
# library(dplyr)
# library(tibble)
# library(tidyr)
# 
# df <- tibble::tibble(
#   id = c(1:100),
#   group = sample(c("A", "B", "C"), size = 100, replace = TRUE),
#   preferred_color___1 = sample(c("Checked", "Unchecked"), size = 100, replace = TRUE),
#   preferred_color___2 = sample(c("Checked", "Unchecked"), size = 100, replace = TRUE),
#   preferred_color___3 = sample(c("Checked", "Unchecked"), size = 100, replace = TRUE),
#   preferred_color___4 = sample(c("Checked", "Unchecked"), size = 100, replace = TRUE),
#   preferred_color___5 = sample(c("Checked", "Unchecked"), size = 100, replace = TRUE)) %>%
#   mutate(group = factor(group,
#                         levels = c("A", "B", "C")))
# 
# df
# 
# 
# var_labels <- tibble::tribble(
#   ~field_name, ~checkbox_choice,
#   "preferred_color___1",           "Blue",
#   "preferred_color___2",          "Green",
#   "preferred_color___3",         "Orange",
#   "preferred_color___4",            "Red",
#   "preferred_color___5",         "Yellow"
# )
# 
# 
# 
# 
# 
# data_labels <- tibble::tribble(
#   ~field_name,                      ~checkbox_choice,
#   "record_id",                                    NA,
#   "gender",                                    NA,
#   "gender_other",                                    NA,
#   "age",                                    NA,
#   "education",                                    NA,
#   "ethnicity",                                    NA,
#   "race___1",                               "White",
#   "race___2",           "Black or African-American",
#   "race___3",    "American Indian or Alaska Native",
#   "race___4",                               "Asian",
#   "race___5", "Native Hawaiian or Pacific Islander",
#   "race___6",                               "Other",
#   "race___98",                "Prefer not to answer",
#   "race_other",                                    NA,
#   "income",                                    NA,
#   "marital_status",                                    NA,
#   "survey_complete",                                    NA
# )
# 
# 
# 
# library(dplyr)
# library(tibble)
# library(labelled)
# 
# 
# df2 <- tibble::tibble(
#   record_id = c(1:100),
#   gender = sample(c("Female", "Male"),
#                   size = 100,
#                   replace = TRUE),
#   gender_other = NA,
#   age = sample(c(18:85),
#                size = 100,
#                replace = TRUE),
#   education = sample(c("High-school", "College", "Graduate school"),
#                      size = 100,
#                      replace = TRUE),
#   ethnicity = sample(c("Hispanic", "Non-hispanic"),
#                      size = 100,
#                      replace = TRUE),
#   key = sample(c("race___1", "race___2", "race___3",
#                  "race___4", "race___5", "race___6", "race___98"),
#                size = 100,
#                replace = TRUE),
#   value = "Checked",
#   income = sample(c(20000:120000),
#                   size = 100,
#                   replace = TRUE),
#   marital_status = sample(c("Married", "Single"),
#                           size = 100,
#                           replace = TRUE),
#   survey_complete = sample(c("Complete", "Not complete"),
#                            size = 100,
#                            replace = TRUE),
#   group = sample(c("Treatment", "Control"),
#                  size = 100,
#                  replace = TRUE)) %>%
#   mutate(key = factor(key,
#                       levels = c("race___1", "race___2", "race___3",
#                                  "race___4", "race___5", "race___6",
#                                  "race___98"))) %>%
#   tidyr::spread(.,
#                 key = "key",
#                 value = "value") %>%
#   mutate_at(.vars = vars(race___1:race___98),
#             .funs = list(~ tidyr::replace_na(., "Unchecked")))
# 
# 
# dplyr::glimpse(df2)
# 
# 
# 
# 
# 
# t1 <- create_tidy_table_one_checkbox(data = df2,
#                        strata = "group",
#                        vars = c("gender",
#                                 "age",
#                                 "education",
#                                 "ethnicity",
#                                 "income",
#                                 "marital_status",
#                                 "race___1",
#                                 "race___2",
#                                 "race___3",
#                                 "race___4",
#                                 "race___5",
#                                 "race___6",
#                                 "race___98"),
#                        checkbox = tibble::tribble(
#                          ~var, ~overall_lbl,                         ~checkbox_lbl, ~checkbox_txt,
#                          "race___1",       "Race",                               "White",     "Checked",
#                          "race___2",       "Race",           "Black or African-American",     "Checked",
#                          "race___3",       "Race",    "American Indian or Alaska Native",     "Checked",
#                          "race___4",       "Race",                               "Asian",     "Checked",
#                          "race___5",       "Race", "Native Hawaiian or Pacific Islander",     "Checked",
#                          "race___6",       "Race",                               "Other",     "Checked",
#                          "race___98",       "Race",                "Prefer not to answer",     "Checked"
#                        )
# 
# )
# 
# t1
# 
# 
# # With labels
# var_labels <- tibble::tribble(
#                              ~var,                                         ~lbl,
#                       "record_id",                                         "ID",
#                          "gender",                                     "Gender",
#                    "gender_other",                              "Gender, Other",
#                             "age",                              "Age, in years",
#                       "education",                                  "Education",
#                       "ethnicity",                                  "Ethnicity",
#                          "income",                                     "Income",
#                  "marital_status",                             "Marital status",
#                 "survey_complete",                                           NA,
#                           "group",                                      "Group",
#                        "race___1",                               "Race (White)",
#                        "race___2",           "Race (Black or African-American)",
#                        "race___3",    "Race (American Indian or Alaska Native)",
#                        "race___4",                               "Race (Asian)",
#                        "race___5", "Race (Native Hawaiian or Pacific Islander)",
#                        "race___6",                               "Race (Other)",
#                       "race___98",                "Race (Prefer not to answer)"
#                 )
# 
# 
# 
# lbls_list <- stats::setNames(as.list(var_labels$lbl), var_labels$var)
# 
# 
# df2_labelled <- df2 |>
#   labelled::set_variable_labels(.labels = lbls_list)
# 
# labelled::var_label(df2_labelled)
# 
# t1 <- create_tidy_table_one_checkbox(data = df2_labelled,
#                        strata = "group",
#                        vars = c("gender",
#                                 "age",
#                                 "education",
#                                 "ethnicity",
#                                 "income",
#                                 "marital_status",
#                                 "race___1",
#                                 "race___2",
#                                 "race___3",
#                                 "race___4",
#                                 "race___5",
#                                 "race___6",
#                                 "race___98"),
#                        checkbox = tibble::tribble(
#                          ~var, ~overall_lbl,                         ~checkbox_lbl, ~checkbox_txt,
#                          "race___1",       "Race",                               "White",     "Checked",
#                          "race___2",       "Race",           "Black or African-American",     "Checked",
#                          "race___3",       "Race",    "American Indian or Alaska Native",     "Checked",
#                          "race___4",       "Race",                               "Asian",     "Checked",
#                          "race___5",       "Race", "Native Hawaiian or Pacific Islander",     "Checked",
#                          "race___6",       "Race",                               "Other",     "Checked",
#                          "race___98",       "Race",                "Prefer not to answer",     "Checked"
#                        )
# 
# )
# 
# 
# dplyr::glimpse(t1)
