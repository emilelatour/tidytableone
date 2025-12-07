# 
# 
# #### Load the data -------------------------------- 
# 
# file_path <- "/Users/latour/Library/CloudStorage/OneDrive-OregonHealth&ScienceUniversity/emile-temp/physical-function-in-pg/test.rds"
# 
# df <- readr::read_rds(file = file_path)
# 
# dplyr::glimpse(df)
# 
# # Check the levels for the factors to see the order
# levels(df$pg_type)
# 
# levels(df$age_cat)
# 
# levels(df$bmi_c)
# 
# # variable names for table 1
# t1_vars <- c("age_at_bsl",
#              "age_cat",
#              "bmi_c",
#              "pg_type")
# 
# # t1_vars <- c("age_at_bsl",
# #              "age_cat",
# #              "gender",
# #              "race_ethn",
# #              "bmi_c",
# #              "pg_type",
# #              "le_ulcer", 
# #              "arrival", 
# #              "ambulation", 
# #              "ulcer_size_1",
# #              "pga04", 
# #              "painmedication",
# #              "opioid_use",
# #              "skin_bother",
# #              "emotional_bother",
# #              "activity_difficulty",
# #              "skin_status",
# #              "skin_pain", 
# #              "worst_skin_pain",
# #              "adl_difficulty", 
# #              "ulcer_drainage")
# 
# # Test table one function below. I just select strata:level to be able to show
# # the issue with the level column clearly
# 
# 
# # Level column is class Character and the order doesn't match the factor levels
# create_tidytableone(data = dplyr::filter(df, is_baseline), 
#                                         strata = NULL, 
#                                         vars = t1_vars) |> 
#   tidytableone::adorn_tidytableone(default_continuous = "{mean} ({sd})\n{median} [{range}]", 
#                                        missing = "ifany") |> 
#   print(n = 200)
# 
# 
# # Level column is class factor and things are in the right order.
# create_tidytableone(data = dplyr::filter(df, is_baseline), 
#                                         strata = "gender", 
#                                         vars = t1_vars) |> 
#   dplyr::select(strata:level) |> 
#   print(n = 100)
# 
# 
# 
# 
# 
# 
# set.seed(123)
# 
# dfc <- tibble::tibble(
#   id       = 1:40,
#   group = factor(sample(c("A", "B"), 40, TRUE)),
#   age      = round(rnorm(40, 50, 10), 1),
#   gender   = factor(sample(c("Female", "Male"), 40, TRUE)),
#   # checkbox cols
#   race___1 = sample(c("Checked", "Unchecked"), 40, TRUE, prob = c(0.6, 0.4)),  # White
#   race___2 = sample(c("Checked", "Unchecked"), 40, TRUE, prob = c(0.2, 0.8)),  # Black
#   race___3 = sample(c("Checked", "Unchecked"), 40, TRUE, prob = c(0.1, 0.9))   # Asian
# )
# 
# dfc <- dfc |> 
#   mutate(gender = factor(gender, 
#                          levels = c("Male", "Female")), 
#          dplyr::across(.cols = dplyr::starts_with("race___"), 
#                        .fns = \(x) factor(x, 
#                                           levels = c("Unchecked", 
#                                                      "Checked"))))
# 
# 
# 
# cb <- tibble::tibble(
#   var          = c("race___1","race___2","race___3"),
#   overall_lbl  = "Race",
#   checkbox_lbl = c("White", "Black or African American", "Asian"),
#   checkbox_txt = "Checked"
# )
# 
# tab_cb <- tidytableone::create_tidytableone(
#   strata = NULL, 
#   data = dfc,
#   vars = c("age","gender","race___1","race___2","race___3"),
#   checkbox = cb,
#   checkbox_opts = list(
#     denom    = "group",
#     show_any = TRUE
#   )
# )
# 
# tab_cb |> dplyr::select(strata, var, level, n_strata, n_level, pct, class, var_type)
# 
# 
# tidytableone::create_tidytableone(
#   strata = "group", 
#   data = dfc,
#   vars = c("age","gender","race___1","race___2","race___3"),
#   checkbox = cb,
#   checkbox_opts = list(
#     denom    = "group",
#     show_any = TRUE
#   )
# ) |> dplyr::select(strata, var, level, n_strata, n_level, pct, class, var_type)
# 
# 
# 
# tidytableone::create_tidytableone(
#   strata = "gender", 
#   data = dfc,
#   vars = c("age","group","race___1","race___2","race___3"),
#   checkbox = cb,
#   checkbox_opts = list(
#     denom    = "group",
#     show_any = TRUE
#   )
# ) |> dplyr::select(strata, var, level, n_strata, n_level, pct, class, var_type)
