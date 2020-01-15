#
# library(tidytableone)
#
# tab1 <- create_tidy_table_one(data = pbc_mayo,
#                               strata = "trt",
#                               .vars = c("time", "status", "trt", "age", "sex", "ascites", "hepato",
#                                         "spiders", "edema", "bili", "chol", "albumin", "copper", "alk_phos",
#                                         "ast", "trig", "platelet", "protime", "stage"))
#
# dplyr::glimpse(tab1)
#
# #### Packages --------------------------------
#
# library(dplyr)
# library(scales)
# library(stringr)
#
# foo <- tab1
#
# #### Arguments --------------------------------
#
# exact
# nonnormal
# format
# The default is "fp" frequency (percentage). You can also choose from "f" frequency only, "p" percentage only, and "pf" percentage (frequency).
#
# accuracy_cont = 0.01
# big_mark = ","
# to_mark = "-"
# # suffix
# # trim
# accuracy_cat = 0.1
# accuracy_pval = 0.001
# accuracy_cv_smd = 0.001
#
#
# #### Format p-values, cv, and smd --------------------------------
#
# pad_pvalue <- function(p) {
#
#   if (stringr::str_detect(string = p,
#                           pattern = "<") & !is.na(p)) {
#     stringr::str_replace(string = p,
#                          pattern = "<",
#                          replacement = "< ")
#   } else if (!is.na(p)) {
#     paste0("  ", p)
#   } else {
#     p
#   }
# }
#
# wrap_pad_pvalue <- function(p) {
#
#   purrr::map_chr(.x = p,
#                  .f = ~ pad_pvalue(.x))
#
# }
#
# foo <- foo %>%
#   dplyr::mutate_at(.vars = dplyr::vars(cv,
#                                        smd),
#                    .funs = list(~ scales::number(x = .,
#                                                  accuracy = accuracy_cv_smd,
#                                                  big.mark = big_mark))) %>%
#   dplyr::mutate_at(.vars = dplyr::vars(shapiro_test,
#                                        ks_test,
#                                        ad_test,
#                                        chisq_test,
#                                        fisher_test,
#                                        oneway_test,
#                                        kruskal_test,
#                                        bartlett_test,
#                                        levene_test),
#                    .funs = list(~ scales::pvalue(x = .,
#                                                  accuracy = accuracy_pval))) %>%
#   dplyr::mutate_at(.vars = dplyr::vars(shapiro_test,
#                                        ks_test,
#                                        ad_test,
#                                        chisq_test,
#                                        fisher_test,
#                                        oneway_test,
#                                        kruskal_test,
#                                        bartlett_test,
#                                        levene_test),
#                    .funs = list(~ wrap_pad_pvalue(p = .)))
#
# foo %>%
#   dplyr::glimpse()
#
#
#
# #### Format contiuous variables --------------------------------
#
# foo <- foo %>%
#   dplyr::mutate_at(.vars = dplyr::vars(mean,
#                                        sd,
#                                        p0,
#                                        p25,
#                                        p50,
#                                        p75,
#                                        p100),
#                    .funs = list(~ scales::number(x = .,
#                                                  accuracy = accuracy_cont,
#                                                  big.mark = big_mark))) %>%
#   mutate(mean_sd = glue::glue("{mean} ({sd})"),
#          mean_min_max = glue::glue("{mean} [{p0} {to_mark} {p100}]"),
#          mean_iqr = glue::glue("{mean} [{p25} {to_mark} {p75}]")) %>%
#   mutate(median_sd = glue::glue("{p50} ({sd})"),
#          median_min_max = glue::glue("{p50} [{p0} {to_mark} {p100}]"),
#          median_iqr = glue::glue("{p50} [{p25} {to_mark} {p75}]"))
#
# foo %>%
#   dplyr::glimpse()
#
#
# #### Format whole numbers --------------------------------
#
# foo <- foo %>%
#   mutate_at(.vars = dplyr::vars(n,
#                                 n_distinct,
#                                 complete,
#                                 missing),
#             .funs = list(~ scales::number(x = .,
#                                           accuracy = 1.0,
#                                           big.mark = big_mark)))
#
# foo %>%
#   dplyr::glimpse()
#
#
# #### Format categorical variables --------------------------------
#
# foo %>%
#   dplyr::filter(var_type == "categorical") %>%
#   dplyr::glimpse()
#
# foo %>%
#   dplyr::filter(var_type == "categorical") %>%
#   mutate(pct = n_level / n_strata) %>%
#   mutate_at(.vars = dplyr::vars(n_level,
#                                 n_strata),
#             .funs = list(~ scales::number(x = .,
#                                           accuracy = 1.0,
#                                           big.mark = big_mark,
#                                           trim = FALSE))) %>%
#   mutate(freq = n_level,
#          pct = scales::percent(x = pct,
#                                accuracy = accuracy_cat,
#                                suffix = "%",
#                                big.mark = big_mark,
#                                trim = FALSE),
#          pct_freq = glue::glue("{pct} ({freq})"),
#          freq_pct = glue::glue("{freq} ({pct})")) %>%
#   dplyr::glimpse()
#
#
# # Option to trim or not
# # Start setting up how to select
