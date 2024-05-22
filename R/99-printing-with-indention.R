# #### Print with indentions --------------------------------
#
#
#
# ### Packages -----------------------------
#
# pacman::p_load(
#   tidyverse,     # packages ggplot2, dplyr, tidyr, readr, purrr, tibble,
#   # stringr, and forcats
#   broom,         # functions tidy(), glance(), augment()
#   fs,            # Cross-platform interface to file system operations
#   glue,          # Glue strings to data in R
#   here,          # Constructs paths to your project's files
#   janitor,       # for working with dirty data
#   khroma,        # Colour Schemes for Scientific Data Visualization
#   lubridate,     # Functions to work with date-times and time-spans
#   mice,          # Multiple imputation using Fully Conditional Specification
#   naniar,        # structures, summaries, and visualisations for missing data
#   readxl,        # read in excel files
#   scales,        # Scale functions for visualization
#   install = FALSE
# )
#
# #### Other packages -----------------------------
#
# library(lamisc)       # devtools::install_github("emilelatour/lamisc")
# library(latable)      # devtools::install_github("emilelatour/latable")
# library(laviz)        # devtools::install_github("emilelatour/laviz")
# library(purposeful)   # devtools::install_github("emilelatour/purposeful")
# library(redcaptools)  # devtools::install_github("emilelatour/redcaptools")
#
# # devtools::install_github("emilelatour/tidytableone")
# library(tidytableone)
#
#
# library(dplyr)
#
# var_labels <- tibble::tribble(
#        ~vars,                                      ~labels,
#         "id",                                "Case Number",
#       "time",          "Number of days since registration",
#     "status",                         "Status at endpoint",
#        "trt",                            "Treatment group",
#        "age",                              "Age, in years",
#        "sex",                                        "Sex",
#    "ascites",                        "Presence of ascites",
#     "hepato", "Presence of hepatomegaly or enlarged liver",
#    "spiders",     "Blood vessel malformations in the skin",
#      "edema",                          "Presence of edema",
#       "bili",                   "Serum bilirunbin (mg/dl)",
#       "chol",                  "Serum cholesterol (mg/dl)",
#    "albumin",                       "Serum albumin (g/dl)",
#     "copper",                      "Urine copper (ug/day)",
#   "alk_phos",             "Alkaline phosphotase (U/liter)",
#        "ast",          "Aspartate aminotransferase (U/ml)",
#       "trig",                      "Triglycerides (mg/dl)",
#   "platelet",                             "Platelet count",
#    "protime",           "Standardised blood clotting time",
#      "stage", "Histologic stage of disease (needs biopsy)"
#   )
#
#
# pbc_mayo2 <- lamisc::apply_data_labels(data = pbc_mayo,
#                                        vars = var_labels$vars,
#                                        labels = var_labels$labels)
#
# tab1 <- create_tidy_table_one(data = pbc_mayo2,
#                               strata = "trt",
#                               vars = c("time",
#                                        "status",
#                                        "age",
#                                        "sex",
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
#
# library(gt)
# library(ftExtra)
#
# foo <- adorn_tidytableone(tidy_t1 = tab1,
#                    use_labels = TRUE)
# foo
#
# foo |>
#   lamisc::flex_print() |>
#   flextable::padding(i = which(stringr::str_detect(foo$var, pattern = "^\\s{2,}")),
#                      j = 1,
#                      padding.left = 20)
#
#
# adorn_tidytableone(tidy_t1 = tab1,
#                    use_labels = TRUE) |>
#   mutate(var = stringr::str_replace(var, "^\\s{2,}", "&emsp;")) |>
#   lamisc::flex_print() |>
#   ftExtra::colformat_md(j = 1)
#
#
# adorn_tidytableone(tidy_t1 = tab1,
#                    use_labels = TRUE) |>
#   mutate(var = stringr::str_replace(var, "^\\s{2,}", "&emsp;")) |>
#   gt::gt() |>
#   gt::cols_align(align = "left",
#                  columns = var) |>
#   gt::fmt_markdown(columns = var)
#
#
#
