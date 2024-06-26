#
# #### Load the packages --------------------------------
#
# library(dplyr)
# library(tidytableone)
#
#
# #### Create some missing --------------------------------
#
# pbc_mayo2 <- pbc_mayo |>
#   mutate(dplyr::across(
#     .cols = all_of(c("time")),
#     .fns = ~ ifelse(row_number(.x) %in% sample(1:n(), size = (10 * n(
#     ) / 100)), NA, .x)
#   ))
#
#
# #### Tidy table ones --------------------------------
#
# ## Strata, Continous ----------------
#
# create_tidy_table_one(data = pbc_mayo2,
#                       strata = "trt",
#                       vars = c("time"))
#
# ## Strata, categorical ----------------
#
# create_tidy_table_one(data = pbc_mayo2,
#                       strata = "trt",
#                       vars = c("stage"))
#
# ## Strata, both ----------------
#
# create_tidy_table_one(data = pbc_mayo2,
#                       strata = "trt",
#                       vars = c("time",
#                                "stage"))
#
# ## Strata, both, more vars ----------------
#
# create_tidy_table_one(data = pbc_mayo2,
#                       strata = "trt",
#                       vars = c("time",
#                                "stage",
#                                "bili",
#                                "status"))
#
# ## No strata, Continous ----------------
#
# create_tidy_table_one(data = pbc_mayo2,
#                       # strata = "trt",
#                       vars = c("time"))
#
#
# ## No strata, categorical ----------------
#
# create_tidy_table_one(data = pbc_mayo2,
#                       # strata = "trt",
#                       vars = c("stage"))
#
# ## No strata, both ----------------
#
# create_tidy_table_one(data = pbc_mayo2,
#                       # strata = "trt",
#                       vars = c("time",
#                                "stage"))
#
# ## No strata, both, more vars ----------------
#
# create_tidy_table_one(data = pbc_mayo2,
#                       # strata = "trt",
#                       vars = c("time",
#                                "stage",
#                                "bili",
#                                "status"))
#
#
#
# #### Adorn tidy table ones --------------------------------
#
# ## Strata, Continous ----------------
#
# create_tidy_table_one(data = pbc_mayo2,
#                       strata = "trt",
#                       vars = c("time")) |>
#   adorn_tidytableone()
#
# ## Strata, categorical ----------------
#
# create_tidy_table_one(data = pbc_mayo2,
#                       strata = "trt",
#                       vars = c("stage")) |>
#   adorn_tidytableone()
#
# pbc_mayo2 |>
#   mutate(trt = forcats::fct_na_value_to_level(trt,
#                                               level = "(Missing)")) %>%
# latable::create_table_one(data = .,
#                           strata = "trt",
#                           vars = c("stage"))
#
# pbc_mayo2 %>%
#   latable::create_table_one(data = .,
#                             strata = "trt",
#                             vars = c("stage"))
#
#
#
# ## Strata, both ----------------
#
# create_tidy_table_one(data = pbc_mayo2,
#                       strata = "trt",
#                       vars = c("time",
#                                "stage")) |>
#   adorn_tidytableone()
#
# ## Strata, both, more vars ----------------
#
# create_tidy_table_one(data = pbc_mayo2,
#                       strata = "trt",
#                       vars = c("time",
#                                "stage",
#                                "bili",
#                                "status")) |>
#   adorn_tidytableone()
#
# ## No strata, Continous ----------------
#
# create_tidy_table_one(data = pbc_mayo2,
#                       # strata = "trt",
#                       vars = c("time")) |>
#   adorn_tidytableone()
#
#
# ## No strata, categorical ----------------
#
# create_tidy_table_one(data = pbc_mayo2,
#                       # strata = "trt",
#                       vars = c("stage")) |>
#   adorn_tidytableone()
#
# ## No strata, both ----------------
#
# create_tidy_table_one(data = pbc_mayo2,
#                       # strata = "trt",
#                       vars = c("time",
#                                "stage")) |>
#   adorn_tidytableone()
#
# ## No strata, both, more vars ----------------
#
# create_tidy_table_one(data = pbc_mayo2,
#                       # strata = "trt",
#                       vars = c("time",
#                                "stage",
#                                "bili",
#                                "status")) |>
#   adorn_tidytableone()
#
#
#
# #### Adorn tidy table ones --------------------------------
#
# ## Strata, Continous ----------------
#
# create_tidy_table_one(data = pbc_mayo2,
#                       strata = "trt",
#                       vars = c("time")) |>
#   adorn_tidytableone(missing = "ifany",
#                      combine_level_col = FALSE)
#
# ## Strata, categorical ----------------
#
# create_tidy_table_one(data = pbc_mayo2,
#                       strata = "trt",
#                       vars = c("stage")) |>
#   adorn_tidytableone(missing = "ifany")
#
#
# pbc_mayo2 |>
#   mutate(trt = forcats::fct_na_value_to_level(trt,
#                                               level = "(Missing)")) %>%
#   latable::create_table_one(data = .,
#                             strata = "trt",
#                             vars = c("stage"))
#
# pbc_mayo2 %>%
#   latable::create_table_one(data = .,
#                             strata = "trt",
#                             vars = c("stage"))
#
#
#
# ## Strata, both ----------------
#
# create_tidy_table_one(data = pbc_mayo2,
#                       strata = "trt",
#                       vars = c("time",
#                                "stage")) |>
#   adorn_tidytableone(missing = "ifany")
#
# ## Strata, both, more vars ----------------
#
# create_tidy_table_one(data = pbc_mayo2,
#                       strata = "trt",
#                       vars = c("time",
#                                "stage",
#                                "bili",
#                                "status")) |>
#   adorn_tidytableone(missing = "ifany")
#
# ## No strata, Continous ----------------
#
# create_tidy_table_one(data = pbc_mayo2,
#                       # strata = "trt",
#                       vars = c("time")) |>
#   adorn_tidytableone(missing = "ifany")
#
#
# ## No strata, categorical ----------------
#
# create_tidy_table_one(data = pbc_mayo2,
#                       # strata = "trt",
#                       vars = c("stage")) |>
#   adorn_tidytableone(missing = "ifany")
#
# ## No strata, both ----------------
#
# create_tidy_table_one(data = pbc_mayo2,
#                       # strata = "trt",
#                       vars = c("time",
#                                "stage")) |>
#   adorn_tidytableone(missing = "ifany")
#
# ## No strata, both, more vars ----------------
#
# create_tidy_table_one(data = pbc_mayo2,
#                       # strata = "trt",
#                       vars = c("time",
#                                "stage",
#                                "bili",
#                                "status")) |>
#   adorn_tidytableone(missing = "ifany")
#
#
#
#
#
# #### Adorn tidy table ones --------------------------------
#
# ## Strata, Continous ----------------
#
# create_tidy_table_one(data = pbc_mayo2,
#                       strata = "trt",
#                       vars = c("time")) |>
#   adorn_tidytableone(missing = "always",
#                      combine_level_col = FALSE)
#
# ## Strata, categorical ----------------
#
# create_tidy_table_one(data = pbc_mayo2,
#                       strata = "trt",
#                       vars = c("stage")) |>
#   adorn_tidytableone(missing = "always")
#
#
# ## Strata, both ----------------
#
# create_tidy_table_one(data = pbc_mayo2,
#                       strata = "trt",
#                       vars = c("time",
#                                "stage")) |>
#   adorn_tidytableone(missing = "always")
#
# ## Strata, both, more vars ----------------
#
# create_tidy_table_one(data = pbc_mayo2,
#                       strata = "trt",
#                       vars = c("time",
#                                "stage",
#                                "bili",
#                                "status")) |>
#   adorn_tidytableone(missing = "always")
#
#
# create_tidy_table_one(data = pbc_mayo2,
#                       strata = "trt",
#                       vars = c("time",
#                                "stage",
#                                "bili",
#                                "status")) |>
#   adorn_tidytableone(missing = "always",
#                      combine_level_col = FALSE) |>
#   lamisc::flex_print()
#
#
# ## No strata, Continous ----------------
#
# create_tidy_table_one(data = pbc_mayo2,
#                       # strata = "trt",
#                       vars = c("time")) |>
#   adorn_tidytableone(missing = "always")
#
#
# ## No strata, categorical ----------------
#
# create_tidy_table_one(data = pbc_mayo2,
#                       # strata = "trt",
#                       vars = c("stage")) |>
#   adorn_tidytableone(missing = "always")
#
# ## No strata, both ----------------
#
# create_tidy_table_one(data = pbc_mayo2,
#                       # strata = "trt",
#                       vars = c("time",
#                                "stage")) |>
#   adorn_tidytableone(missing = "always")
#
# ## No strata, both, more vars ----------------
#
# create_tidy_table_one(data = pbc_mayo2,
#                       # strata = "trt",
#                       vars = c("time",
#                                "stage",
#                                "bili",
#                                "status")) |>
#   adorn_tidytableone(missing = "always")
#
# # if (any(tidy_t1$var_type == "continuous") & any(tidy_t1$var_type == "categorical")) {
# #
# #
# # } else if (any(tidy_t1$var_type == "continuous")) {
# #
# #
# #
# # } else if (any(tidy_t1$var_type == "categorical")) {
# #
# #
# # }
#
