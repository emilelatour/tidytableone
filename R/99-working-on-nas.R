#
#
# # What to do about NAs
#
# # Error message for outcome
# # 106 observations missing `trt` have been removed. To include these observations, use `forcats::fct_na_value_to_level()` on `trt` column before passing to `tbl_summary()`
#
# # Missing column should not be included in p-value
#
# pbc_mayo2 <- pbc_mayo |>
#   mutate(dplyr::across(
#          .cols = all_of(c("time")),
#          .fns = ~ ifelse(row_number(.x) %in% sample(1:n(), size = (10 * n(
#          ) / 100)), NA, .x)
#        ))
#
# tab1 <- create_tidy_table_one(data = pbc_mayo2,
#                               strata = "trt",
#                               vars = c("time",
#                                        "status",
#                                        # "age",
#                                        # "sex",
#                                        # "ascites",
#                                        # "hepato",
#                                        # "spiders",
#                                        # "edema",
#                                        # "bili",
#                                        # "chol",
#                                        # "albumin",
#                                        # "copper",
#                                        # "alk_phos",
#                                        # "ast",
#                                        # "trig",
#                                        # "platelet",
#                                        # "protime",
#                                        "stage"))
#
# dplyr::glimpse(tab1)
#
# adorn_tidytableone(tab1)
#
#
#
#
#
# pbc_mayo2 |>
#   dplyr::select(trt,
#                 time,
#                 status,
#                 stage) |>
# gtsummary::tbl_summary(by = trt)
#
#
# pbc_mayo2 |>
#   mutate(trt = forcats::fct_na_value_to_level(trt,
#                                               level = "(Missing)")) |>
#   dplyr::select(trt,
#                 time,
#                 status,
#                 stage) |>
# gtsummary::tbl_summary(by = trt,
#                        missing = "ifany")
#
# pbc_mayo2 |>
#   mutate(trt = forcats::fct_na_value_to_level(trt,
#                                               level = "(Missing)")) |>
#   dplyr::select(trt,
#                 time,
#                 status,
#                 stage) |>
# gtsummary::tbl_summary(by = trt)
#
#
# pbc_mayo2 |>
#   # mutate(trt = forcats::fct_na_value_to_level(trt,
#   #                                             level = "(Missing)")) |>
#   dplyr::select(trt,
#                 time,
#                 status,
#                 stage) |>
# gtsummary::tbl_summary(by = trt,
#                        missing = "always") |>
#   gtsummary::add_p()
#
#
# # https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.html
#
#
# #### Table1 --------------------------------
#
# table1::table1(~ time + status + stage | trt, data = pbc_mayo2)
# table1::table1(~ time + status + stage | trt, data = dplyr::filter(pbc_mayo2, !is.na(trt)))
# # Note that when contains missing values (here weight), be it continuous or
# # categorical, these are reported as a distinct category (with count and
# # percent).
#
#
# #### For tidytableone --------------------------------
#
#
# # Missing == "no"
#
# tab1 |>
#   dplyr::filter(!(is.na(level) & var_type == "categorical")) |>
#   adorn_tidytableone()
#
# # Missing == "ifany"
#
# tab1 |>
#   group_by(var) |>
#   dplyr::filter(any(missing > 0) | is.na(level)) |>
#   mutate(n = dplyr::coalesce(n, n_strata),
#          missing = dplyr::coalesce(missing, n_level)) |>
#   dplyr::select(strata, var, n, missing) |>
#   ungroup() |>
#   mutate(pct = missing / n,
#          pct = lamisc::fmt_pct(pct),
#          n_missing = glue::glue("{missing} ({pct})"))
#
# # Missing == "always"
#
# tab1 |>
#   group_by(var) |>
#   mutate(n_level = dplyr::if_else(is.na(level), n_level, 0),
#          n = dplyr::coalesce(n, n_strata),
#          missing = dplyr::coalesce(missing, n_level)) |>
#   group_by(var,
#            strata,
#            n) |>
#   summarise(missing = sum(missing, na.rm = TRUE),
#             .groups = "drop") |>
#   mutate(pct = missing / n,
#          pct = lamisc::fmt_pct(pct),
#          n_missing = glue::glue("{missing} ({pct})")) |>
#   lamisc::print_pipe(n = Inf)
