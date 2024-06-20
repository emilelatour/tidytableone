#
# library(dplyr)
# library(tidytableone)
#
# pbc_mayo2 <- pbc_mayo |>
#   mutate(dplyr::across(
#     .cols = all_of(c("time")),
#     .fns = ~ ifelse(row_number(.x) %in% sample(1:n(), size = (10 * n(
#     ) / 100)), NA, .x)
#   ))
#
# create_tidy_table_one(data = pbc_mayo2,
#                       strata = "trt",
#                       vars = c("time"))
#
#
# create_tidy_table_one(data = pbc_mayo2,
#                       strata = "trt",
#                       vars = c("status"))
#
#
#
# con_res <- create_tidy_table_one(data = pbc_mayo2,
#                                  strata = "trt",
#                                  vars = c("time"))
#
#
# cat_res <- create_tidy_table_one(data = pbc_mayo2,
#                       strata = "trt",
#                       vars = c("status"))
#
# names(con_res)[!names(con_res) %in% names(cat_res)]
#
#
# names(cat_res)[!names(cat_res) %in% names(con_res)]
#
#
#
# # > names(con_res)[!names(con_res) %in% names(cat_res)]
# # [1] "n"                       "n_distinct"              "complete"                "missing"
# # [5] "mean"                    "sd"                      "p0"                      "p25"
# # [9] "p50"                     "p75"                     "p100"                    "cv"
# # [13] "shapiro_test"            "ks_test"                 "ad_test"                 "oneway_test_unequal_var"
# # [17] "oneway_test_equal_var"   "kruskal_test"            "bartlett_test"           "levene_test"
# # > names(cat_res)[!names(cat_res) %in% names(con_res)]
# # [1] "level"                    "n_level"                  "n_strata"                 "pct"
# # [5] "chisq_test"               "chisq_test_no_correction" "chisq_test_simulated"     "fisher_test"
# # [9] "fisher_test_simulated"    "check_categorical_test"
