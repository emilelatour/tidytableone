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
#
#
#
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
#
#
#
#
#
#
# make_t1_pretty2 <- function(t1,
#                             default_continuous = "{mean} ({sd})",
#                             default_categorical = "{n} ({p})",
#                             fmt_vars = NULL,
#                             con_accuracy = 0.1,
#                             cat_accuracy = 0.1,
#                             prefix = "",
#                             suffix = "",
#                             big_mark = "",
#                             decimal_mark = ".",
#                             style_positive = "none",
#                             style_negative = "hyphen",
#                             scale_cut = NULL,
#                             con_trim = TRUE,
#                             cat_trim = FALSE,
#                             show_pct = TRUE,
#                             missing = "ifany", ...) {
#
#   # Silence no visible binding for global variable
#   glue_formula <- pct <- cv <- strata <- glue_formula2 <- NULL
#
#   # Percentage suffix
#   if (show_pct) {
#     pct_suffix = "%"
#   } else {
#     pct_suffix = ""
#   }
#
#
#   #### Glue formulae --------------------------------
#
#   if (is.null(fmt_vars)) {
#
#     formula_for_table <- t1 |>
#       dplyr::distinct(var,
#                       var_type) |>
#       mutate(glue_formula = dplyr::case_when(
#         var_type == "continuous" ~ default_continuous,
#         var_type == "categorical" ~ default_categorical,
#         .default = NA_character_))
#
#   } else {
#
#     override_formulae <- tibble::tibble(var = names(fmt_vars),
#                                         glue_formula = purrr::map_chr(.x = var,
#                                                                       .f = ~ fmt_vars[[.x]]))
#     formula_for_table <- t1 |>
#       dplyr::distinct(var,
#                       var_type) |>
#       mutate(glue_formula = dplyr::case_when(
#         var_type == "continuous" ~ default_continuous,
#         var_type == "categorical" ~ default_categorical,
#         .default = NA_character_)) |>
#       dplyr::rows_update(override_formulae,
#                          by = "var")
#
#   }
#
#   ## Fix formula ----------------
#
#
#   formula_for_table <- formula_for_table |>
#     mutate(glue_formula = stringr::str_replace_all(string = glue_formula,
#                                                    pattern = "median|Median|med|med",
#                                                    replacement = "p50")) |>
#     mutate(glue_formula = stringr::str_replace_all(string = glue_formula,
#                                                    pattern = "min|Min|minimum|Minimum",
#                                                    replacement = "p0")) |>
#     mutate(glue_formula = stringr::str_replace_all(string = glue_formula,
#                                                    pattern = "max|Max|maximum|Maximum",
#                                                    replacement = "p100")) |>
#     mutate(glue_formula = stringr::str_replace_all(string = glue_formula,
#                                                    pattern = "max|Max|maximum|Maximum",
#                                                    replacement = "p100")) |>
#     mutate(glue_formula = stringr::str_replace_all(string = glue_formula,
#                                                    pattern = "\\{p\\}",
#                                                    replacement = "{pct}")) |>
#     mutate(glue_formula = stringr::str_replace_all(string = glue_formula,
#                                                    pattern = "\\{n\\}",
#                                                    replacement = "{n_level}")) |>
#     mutate(glue_formula = stringr::str_replace_all(string = glue_formula,
#                                                    pattern = "\\{N\\}",
#                                                    replacement = "{n_strata}"))
#
#
#
#   #### Make the pretty t1 --------------------------------
#
#   if (any(t1$var_type == "continuous") & any(t1$var_type == "categorical")) {
#
#     pretty_t1 <- t1 |>
#       mutate(pct = n_level / n_strata) |>
#       # Format counts for strata
#       mutate(dplyr::across(.cols = c(n_level,
#                                      n_strata),
#                            .fns = ~ scales::number(x = .,
#                                                    accuracy = 1.0,
#                                                    scale = 1,
#                                                    prefix = "",
#                                                    suffix = "",
#                                                    big.mark = "",
#                                                    decimal.mark = ".",
#                                                    style_positive = "none",
#                                                    style_negative = "hyphen",
#                                                    scale_cut = NULL,
#                                                    trim = FALSE))) |>
#       # Format categorical Percentages
#       mutate(pct = scales::percent(x = pct,
#                                    accuracy = cat_accuracy,
#                                    scale = 100,
#                                    prefix = prefix,
#                                    suffix = pct_suffix,
#                                    big.mark = big_mark,
#                                    decimal.mark = decimal_mark,
#                                    style_positive = style_positive,
#                                    style_negative = style_negative,
#                                    scale_cut = scale_cut,
#                                    trim = cat_trim)) |>
#       # Format for continuous data
#       mutate(dplyr::across(.cols = c(mean:cv),
#                            .fns = ~ scales::number(x = .,
#                                                    accuracy = con_accuracy,
#                                                    scale = 1,
#                                                    prefix = prefix,
#                                                    suffix = suffix,
#                                                    big.mark = big_mark,
#                                                    decimal.mark = decimal_mark,
#                                                    style_positive = style_positive,
#                                                    style_negative = style_negative,
#                                                    scale_cut = scale_cut,
#                                                    trim = con_trim)))
#
#
#   } else if (any(t1$var_type == "categorical")) {
#
#     pretty_t1 <- t1 |>
#       mutate(pct = n_level / n_strata) |>
#       # Format counts for strata
#       mutate(dplyr::across(.cols = c(n_level,
#                                      n_strata),
#                            .fns = ~ scales::number(x = .,
#                                                    accuracy = 1.0,
#                                                    scale = 1,
#                                                    prefix = "",
#                                                    suffix = "",
#                                                    big.mark = "",
#                                                    decimal.mark = ".",
#                                                    style_positive = "none",
#                                                    style_negative = "hyphen",
#                                                    scale_cut = NULL,
#                                                    trim = FALSE))) |>
#       # Format categorical Percentages
#       mutate(pct = scales::percent(x = pct,
#                                    accuracy = cat_accuracy,
#                                    scale = 100,
#                                    prefix = prefix,
#                                    suffix = pct_suffix,
#                                    big.mark = big_mark,
#                                    decimal.mark = decimal_mark,
#                                    style_positive = style_positive,
#                                    style_negative = style_negative,
#                                    scale_cut = scale_cut,
#                                    trim = cat_trim))
#
#   } else if (any(t1$var_type == "continuous")) {
#
#     # Format continuous stats: mean, sd, median, etc.
#     pretty_t1 <- t1 |>
#       mutate(dplyr::across(.cols = c(mean:cv),
#                            .fns = ~ scales::number(x = .,
#                                                    accuracy = con_accuracy,
#                                                    scale = 1,
#                                                    prefix = prefix,
#                                                    suffix = suffix,
#                                                    big.mark = big_mark,
#                                                    decimal.mark = decimal_mark,
#                                                    style_positive = style_positive,
#                                                    style_negative = style_negative,
#                                                    scale_cut = scale_cut,
#                                                    trim = con_trim)))
#
#   }
#
#
#   if (!any(names(pretty_t1) == "level")) {
#
#     pretty_t1 <- pretty_t1 |>
#       mutate(level = "")
#   }
#
#
#
#   pretty_t1 <- pretty_t1 |>
#     dplyr::left_join(formula_for_table,
#                      by = c("var",
#                             "var_type")) |>
#     dplyr::rowwise() |>
#     mutate(glue_formula2 = glue::glue(glue_formula)) |>
#     dplyr::select(strata,
#                   var,
#                   level,
#                   var_type,
#                   glue_formula,
#                   glue_formula2) |>
#     tidyr::pivot_wider(names_from = strata,
#                        values_from = glue_formula2) |>
#     tidyr::separate_longer_delim(cols = c(-var, -var_type),
#                                  delim = "\n") |>
#     # Replace labels for stats
#     mutate(glue_formula = stringr::str_replace(glue_formula,
#                                                pattern = "\\{mean\\}",
#                                                replacement = "Mean"),
#            glue_formula = stringr::str_replace(glue_formula,
#                                                pattern = "\\{sd\\}",
#                                                replacement = "SD"),
#            glue_formula = stringr::str_replace(glue_formula,
#                                                pattern = "\\{p50\\}",
#                                                replacement = "Median"),
#            glue_formula = stringr::str_replace(glue_formula,
#                                                pattern = "\\{p0\\}",
#                                                replacement = "Min."),
#            glue_formula = stringr::str_replace(glue_formula,
#                                                pattern = "\\{p100\\}",
#                                                replacement = "Max."),
#            glue_formula = stringr::str_replace(glue_formula,
#                                                pattern = "\\{n_level\\}",
#                                                replacement = "n"),
#            glue_formula = stringr::str_replace(glue_formula,
#                                                pattern = "\\{n_strata\\}",
#                                                replacement = "N"),
#            glue_formula = stringr::str_replace(glue_formula,
#                                                pattern = "\\{pct\\}",
#                                                replacement = "%"))
#
#
#   return(pretty_t1)
#
#
# }
#
#
# create_tidy_table_one(data = pbc_mayo2,
#                       strata = "trt",
#                       vars = c("time", "stage")) |>
#   make_t1_pretty2()
#
#
# create_tidy_table_one(data = pbc_mayo2,
#                       strata = "trt",
#                       vars = c("time", "stage")) |>
#   glimpse()
#
#
#
#
#
#
# #### Missing --------------------------------
#
# table1::table1(~ time + status + stage | trt, data = pbc_mayo2)
# table1::table1(~ time + status + stage | trt, data = dplyr::filter(pbc_mayo2, !is.na(trt)))
# # Note that when contains missing values (here weight), be it continuous or
# # categorical, these are reported as a distinct category (with count and
# # percent).
#
#
# pbc_mayo2 |>
#   mutate(trt = forcats::fct_na_value_to_level(trt,
#                                               level = "(Missing)")) %>%
#   table1::table1(~ time + status + stage | trt, data = .)
#
#
# #### For tidytableone --------------------------------
#
# ## Strata ----------------
#
# tab1 <- create_tidy_table_one(data = pbc_mayo2,
#                               strata = "trt",
#                               vars = c("time",
#                                        # "status",
#                                        "stage"))
#
# get_miss <- function(t1,
#                      missing = "no",
#                      missing_text = "(Missing)",
#                      default_miss = "{n}",
#                      cat_accuracy = 0.1,
#                      prefix = "",
#                      suffix = "",
#                      big_mark = "",
#                      decimal_mark = ".",
#                      style_positive = "none",
#                      style_negative = "hyphen",
#                      scale_cut = NULL,
#                      con_trim = TRUE,
#                      cat_trim = FALSE,
#                      show_pct = TRUE, ...) {
#
#   # Silence no visible binding for global variable
#   glue_formula <- pct <- cv <- strata <- glue_formula2 <- NULL
#
#   # Percentage suffix
#   if (show_pct) {
#     pct_suffix = "%"
#   } else {
#     pct_suffix = ""
#   }
#
#
#   if (any(t1$var_type == "continuous") & any(t1$var_type == "categorical")) {
#
#     t1 <- t1
#
#   } else if (any(t1$var_type == "continuous")) {
#
#     t1 <- t1 |>
#       mutate(n_strata = NA_integer_,
#              n_strata_valid = NA_integer_,
#              level = NA_character_)
#
#   } else if (any(t1$var_type == "categorical")) {
#
#     t1 <- t1 |>
#       mutate(n = NA_integer_,
#              complete = NA_integer_,
#              missing = NA_integer_)
#
#   }
#
#
#   if (missing == "no") {
#
#     miss_tab <- t1 |>
#       dplyr::filter(strata == "Overall") |>
#       dplyr::distinct(var,
#                       n,
#                       complete,
#                       n_strata,
#                       n_strata_valid,
#                       var_type) |>
#       mutate(n = dplyr::coalesce(n, n_strata),
#              n_available = dplyr::coalesce(complete, n_strata_valid),
#              p_available = n_available / n) |>
#       dplyr::select(var, n, n_available, p_available)
#
#
#     miss_tab <- miss_tab |>
#       # Format counts for strata
#       mutate(dplyr::across(.cols = c(n,
#                                      n_available),
#                            .fns = ~ scales::number(x = .,
#                                                    accuracy = 1.0,
#                                                    scale = 1,
#                                                    prefix = "",
#                                                    suffix = "",
#                                                    big.mark = "",
#                                                    decimal.mark = ".",
#                                                    style_positive = "none",
#                                                    style_negative = "hyphen",
#                                                    scale_cut = NULL,
#                                                    trim = FALSE))) |>
#       # Format categorical Percentages
#       mutate(p_available = scales::percent(x = p_available,
#                                            accuracy = cat_accuracy,
#                                            scale = 100,
#                                            prefix = prefix,
#                                            suffix = pct_suffix,
#                                            big.mark = big_mark,
#                                            decimal.mark = decimal_mark,
#                                            style_positive = style_positive,
#                                            style_negative = style_negative,
#                                            scale_cut = scale_cut,
#                                            trim = cat_trim))
#
#   } else if (missing == "ifany") {
#
#     any_miss <- t1 |>
#       dplyr::filter(strata == "Overall") |>
#       mutate(n = dplyr::coalesce(n, n_strata),
#              missing = dplyr::if_else(var_type == "continuous",
#                                       missing,
#                                       n_strata - n_strata_valid)) |>
#       dplyr::filter(missing > 0) |>
#       dplyr::pull(var)
#
#     miss_tab <- t1 |>
#       dplyr::filter(var %in% any_miss) |>
#       dplyr::select(strata,
#                     var,
#                     n,
#                     missing,
#                     level,
#                     n_strata,
#                     n_strata_valid,
#                     var_type) |>
#       mutate(n = dplyr::coalesce(n, n_strata),
#              missing = dplyr::if_else(var_type == "continuous",
#                                       missing,
#                                       n_strata - n_strata_valid),
#              missing_p = missing / n) |>
#       dplyr::distinct(strata,
#                       var,
#                       n,
#                       missing,
#                       missing_p,
#                       var_type)
#
#
#     miss_tab <- miss_tab |>
#       # Format counts for strata
#       mutate(dplyr::across(.cols = c(n,
#                                      missing),
#                            .fns = ~ scales::number(x = .,
#                                                    accuracy = 1.0,
#                                                    scale = 1,
#                                                    prefix = "",
#                                                    suffix = "",
#                                                    big.mark = "",
#                                                    decimal.mark = ".",
#                                                    style_positive = "none",
#                                                    style_negative = "hyphen",
#                                                    scale_cut = NULL,
#                                                    trim = FALSE))) |>
#       # Format categorical Percentages
#       mutate(missing_p = scales::percent(x = missing_p,
#                                          accuracy = cat_accuracy,
#                                          scale = 100,
#                                          prefix = prefix,
#                                          suffix = pct_suffix,
#                                          big.mark = big_mark,
#                                          decimal.mark = decimal_mark,
#                                          style_positive = style_positive,
#                                          style_negative = style_negative,
#                                          scale_cut = scale_cut,
#                                          trim = cat_trim))
#
#
#
#   } else if (missing == "always") {
#
#     miss_tab <- t1 |>
#       dplyr::select(strata,
#                     var,
#                     n,
#                     missing,
#                     level,
#                     n_strata,
#                     n_strata_valid,
#                     var_type) |>
#       mutate(n = dplyr::coalesce(n, n_strata),
#              missing = dplyr::if_else(var_type == "continuous",
#                                       missing,
#                                       n_strata - n_strata_valid),
#              missing_p = missing / n) |>
#       dplyr::distinct(strata,
#                       var,
#                       n,
#                       missing,
#                       missing_p,
#                       var_type)
#
#
#     miss_tab <- miss_tab |>
#       # Format counts for strata
#       mutate(dplyr::across(.cols = c(n,
#                                      missing),
#                            .fns = ~ scales::number(x = .,
#                                                    accuracy = 1.0,
#                                                    scale = 1,
#                                                    prefix = "",
#                                                    suffix = "",
#                                                    big.mark = "",
#                                                    decimal.mark = ".",
#                                                    style_positive = "none",
#                                                    style_negative = "hyphen",
#                                                    scale_cut = NULL,
#                                                    trim = FALSE))) |>
#       # Format categorical Percentages
#       mutate(missing_p = scales::percent(x = missing_p,
#                                          accuracy = cat_accuracy,
#                                          scale = 100,
#                                          prefix = prefix,
#                                          suffix = pct_suffix,
#                                          big.mark = big_mark,
#                                          decimal.mark = decimal_mark,
#                                          style_positive = style_positive,
#                                          style_negative = style_negative,
#                                          scale_cut = scale_cut,
#                                          trim = cat_trim))
#
#
#
#   }
#
#
#
#   # Format results
#
#   if (missing == "no") {
#
#     miss_tab <- miss_tab |>
#       mutate(glue_formula = default_miss,
#              glue_formula = stringr::str_replace_all(string = glue_formula,
#                                                      pattern = "\\{n\\}",
#                                                      replacement = "{n_available}"),
#              glue_formula = stringr::str_replace_all(string = glue_formula,
#                                                      pattern = "\\{p\\}",
#                                                      replacement = "{p_available}")) |>
#       dplyr::rowwise() |>
#       mutate(num_not_miss = glue::glue(glue_formula)) |>
#       dplyr::select(var, num_not_miss)
#
#   }
#
#
#
#
#   return(miss_tab)
#
#
# }
#
#
# get_miss(t1 = tab1,
#          missing = "no")
#
# # Continuous only
# create_tidy_table_one(data = pbc_mayo2,
#                       strata = "trt",
#                       vars = c("time")) |>
#   get_miss(missing = "no")
#
#
# # Categorical only
# create_tidy_table_one(data = pbc_mayo2,
#                       strata = "trt",
#                       vars = c("stage")) |>
#   get_miss(missing = "no")
#
#
#
#
#
# foo <- create_tidy_table_one(data = pbc_mayo2,
#                       strata = "trt",
#                       vars = c("time"))
#
# foo |>
#   make_t1_pretty2()
#
# foo |>
#   get_miss(missing = "no") |>
#   mutate(glue_formula = "{n} ({p})",
#          glue_formula = stringr::str_replace_all(string = glue_formula,
#                                                  pattern = "\\{n\\}",
#                                                  replacement = "{n_available}"),
#          glue_formula = stringr::str_replace_all(string = glue_formula,
#                                                  pattern = "\\{p\\}",
#                                                  replacement = "{p_available}")) |>
#   dplyr::rowwise() |>
#   mutate(num_not_miss = glue::glue(glue_formula)) |>
#   dplyr::select(var, num_not_miss)
#
#
# foo <- create_tidy_table_one(data = pbc_mayo2,
#                              strata = "trt",
#                              vars = c("stage"))
#
# foo |>
#   adorn_tidytableone()
#
# foo |>
#   get_miss(missing = "no")
#
#
# foo <- create_tidy_table_one(data = pbc_mayo2,
#                              strata = "trt",
#                              vars = c("time", "stage"))
#
# foo |>
#   group_by(var) |>
#   dplyr::group_split() |>
#   purrr::map_df(.f = ~ get_miss(t1 = .x,
#                              missing = "no"))
#   mutate(var2 = var) |>
#   tidyr::nest(.by = var2)|>
#   mutate(miss = purrr::map(.x = data,
#                               .f = ~ get_miss(t1 = .x,
#                                               missing = "no"))) |>
#
#
#
#
# get_miss(t1 = tab1,
#          missing = "ifany")
#
# # Continuous only
# create_tidy_table_one(data = pbc_mayo2,
#                       strata = "trt",
#                       vars = c("time")) |>
#   get_miss(missing = "ifany")
#
#
# # Categorical only
# create_tidy_table_one(data = pbc_mayo2,
#                       strata = "trt",
#                       vars = c("stage")) |>
#   get_miss(missing = "ifany")
#
#
# create_tidy_table_one(data = pbc_mayo2,
#                       strata = "trt",
#                       vars = c("time", "stage")) |>
#   get_miss(missing = "ifany")
#
#
# get_miss(t1 = tab1,
#          missing = "always")
#
# # Continuous only
# create_tidy_table_one(data = pbc_mayo2,
#                       strata = "trt",
#                       vars = c("time")) |>
#   get_miss(missing = "always")
#
#
# # Categorical only
# create_tidy_table_one(data = pbc_mayo2,
#                       strata = "trt",
#                       vars = c("stage")) |>
#   get_miss(missing = "always")
#
#
# create_tidy_table_one(data = pbc_mayo2,
#                       strata = "trt",
#                       vars = c("time",
#                                "status",
#                                "stage")) |>
#   get_miss(missing = "always")
#
#
#
# ## No strata ----------------
#
# tab2 <- create_tidy_table_one(data = pbc_mayo2,
#                               # strata = "trt",
#                               vars = c("time",
#                                        # "status",
#                                        "stage"))
#
#
# get_miss_no_strata <- function(t1,
#                                missing = "no",
#                                missing_text = "(Missing)",
#                                default_categorical = "{n} ({p})",
#                                cat_accuracy = 0.1,
#                                prefix = "",
#                                suffix = "",
#                                big_mark = "",
#                                decimal_mark = ".",
#                                style_positive = "none",
#                                style_negative = "hyphen",
#                                scale_cut = NULL,
#                                con_trim = TRUE,
#                                cat_trim = FALSE,
#                                show_pct = TRUE, ...) {
#
#   # Silence no visible binding for global variable
#   glue_formula <- pct <- cv <- strata <- glue_formula2 <- NULL
#
#   # Percentage suffix
#   if (show_pct) {
#     pct_suffix = "%"
#   } else {
#     pct_suffix = ""
#   }
#
#
#   if (any(t1$var_type == "continuous") & any(t1$var_type == "categorical")) {
#
#     t1 <- t1
#
#   } else if (any(t1$var_type == "continuous")) {
#
#     t1 <- t1 |>
#       mutate(n_strata = NA_integer_,
#              n_strata_valid = NA_integer_,
#              level = NA_character_)
#
#   } else if (any(t1$var_type == "categorical")) {
#
#     t1 <- t1 |>
#       mutate(n = NA_integer_,
#              complete = NA_integer_,
#              missing = NA_integer_)
#   }
#
#   if (missing == "no") {
#
#     miss_tab <- t1 |>
#       dplyr::distinct(var,
#                       n,
#                       complete,
#                       n_strata,
#                       n_strata_valid,
#                       var_type) |>
#       mutate(n = dplyr::coalesce(n, n_strata),
#              n_available = dplyr::coalesce(complete, n_strata_valid),
#              p_available = n_available / n) |>
#       dplyr::select(var, n, n_available, p_available)
#
#
#     miss_tab <- miss_tab |>
#       # Format counts for strata
#       mutate(dplyr::across(.cols = c(n,
#                                      n_available),
#                            .fns = ~ scales::number(x = .,
#                                                    accuracy = 1.0,
#                                                    scale = 1,
#                                                    prefix = "",
#                                                    suffix = "",
#                                                    big.mark = "",
#                                                    decimal.mark = ".",
#                                                    style_positive = "none",
#                                                    style_negative = "hyphen",
#                                                    scale_cut = NULL,
#                                                    trim = FALSE))) |>
#       # Format categorical Percentages
#       mutate(p_available = scales::percent(x = p_available,
#                                            accuracy = cat_accuracy,
#                                            scale = 100,
#                                            prefix = prefix,
#                                            suffix = pct_suffix,
#                                            big.mark = big_mark,
#                                            decimal.mark = decimal_mark,
#                                            style_positive = style_positive,
#                                            style_negative = style_negative,
#                                            scale_cut = scale_cut,
#                                            trim = cat_trim))
#
#
#
#   } else if (missing == "ifany") {
#
#     any_miss <- t1 |>
#       mutate(n = dplyr::coalesce(n, n_strata),
#              missing = dplyr::if_else(var_type == "continuous",
#                                       missing,
#                                       n_strata - n_strata_valid)) |>
#       dplyr::filter(missing > 0) |>
#       dplyr::pull(var)
#
#     miss_tab <- t1 |>
#       dplyr::filter(var %in% any_miss) |>
#       dplyr::select(var,
#                     n,
#                     missing,
#                     level,
#                     n_strata,
#                     n_strata_valid,
#                     var_type) |>
#       mutate(n = dplyr::coalesce(n, n_strata),
#              missing = dplyr::if_else(var_type == "continuous",
#                                       missing,
#                                       n_strata - n_strata_valid),
#              missing_p = missing / n) |>
#       dplyr::distinct(var,
#                       n,
#                       missing,
#                       missing_p,
#                       var_type)
#
#
#     miss_tab <- miss_tab |>
#       # Format counts for strata
#       mutate(dplyr::across(.cols = c(n,
#                                      missing),
#                            .fns = ~ scales::number(x = .,
#                                                    accuracy = 1.0,
#                                                    scale = 1,
#                                                    prefix = "",
#                                                    suffix = "",
#                                                    big.mark = "",
#                                                    decimal.mark = ".",
#                                                    style_positive = "none",
#                                                    style_negative = "hyphen",
#                                                    scale_cut = NULL,
#                                                    trim = FALSE))) |>
#       # Format categorical Percentages
#       mutate(missing_p = scales::percent(x = missing_p,
#                                          accuracy = cat_accuracy,
#                                          scale = 100,
#                                          prefix = prefix,
#                                          suffix = pct_suffix,
#                                          big.mark = big_mark,
#                                          decimal.mark = decimal_mark,
#                                          style_positive = style_positive,
#                                          style_negative = style_negative,
#                                          scale_cut = scale_cut,
#                                          trim = cat_trim))
#
#
#
#   } else if (missing == "always") {
#
#     miss_tab <- t1 |>
#       dplyr::select(var,
#                     n,
#                     missing,
#                     level,
#                     n_strata,
#                     n_strata_valid,
#                     var_type) |>
#       mutate(n = dplyr::coalesce(n, n_strata),
#              missing = dplyr::if_else(var_type == "continuous",
#                                       missing,
#                                       n_strata - n_strata_valid),
#              missing_p = missing / n) |>
#       dplyr::distinct(var,
#                       n,
#                       missing,
#                       missing_p,
#                       var_type)
#
#
#     miss_tab <- miss_tab |>
#       # Format counts for strata
#       mutate(dplyr::across(.cols = c(n,
#                                      missing),
#                            .fns = ~ scales::number(x = .,
#                                                    accuracy = 1.0,
#                                                    scale = 1,
#                                                    prefix = "",
#                                                    suffix = "",
#                                                    big.mark = "",
#                                                    decimal.mark = ".",
#                                                    style_positive = "none",
#                                                    style_negative = "hyphen",
#                                                    scale_cut = NULL,
#                                                    trim = FALSE))) |>
#       # Format categorical Percentages
#       mutate(missing_p = scales::percent(x = missing_p,
#                                          accuracy = cat_accuracy,
#                                          scale = 100,
#                                          prefix = prefix,
#                                          suffix = pct_suffix,
#                                          big.mark = big_mark,
#                                          decimal.mark = decimal_mark,
#                                          style_positive = style_positive,
#                                          style_negative = style_negative,
#                                          scale_cut = scale_cut,
#                                          trim = cat_trim))
#
#
#
#   }
#
#
#
#   return(miss_tab)
#
#
# }
#
# get_miss_no_strata(t1 = tab2,
#                    missing = "no")
#
#
# # Continuous only
# create_tidy_table_one(data = pbc_mayo2,
#                       # strata = "trt",
#                       vars = c("time")) |>
#   get_miss_no_strata(missing = "no")
#
# # Categorical only
# create_tidy_table_one(data = pbc_mayo2,
#                       # strata = "trt",
#                       vars = c("stage")) |>
#   get_miss_no_strata(missing = "no")
#
#
# get_miss_no_strata(t1 = tab2,
#          missing = "ifany")
#
# # Continuous only
# create_tidy_table_one(data = pbc_mayo2,
#                       # strata = "trt",
#                       vars = c("time")) |>
#   get_miss_no_strata(missing = "ifany")
#
#
# # Categorical only
# create_tidy_table_one(data = pbc_mayo2,
#                       # strata = "trt",
#                       vars = c("stage")) |>
#   get_miss_no_strata(missing = "ifany")
#
#
# create_tidy_table_one(data = pbc_mayo2,
#                       # strata = "trt",
#                       vars = c("time", "status", "stage")) |>
#   get_miss_no_strata(missing = "ifany")
#
#
# get_miss_no_strata(t1 = tab2,
#          missing = "always")
#
# # Continuous only
# create_tidy_table_one(data = pbc_mayo2,
#                       # strata = "trt",
#                       vars = c("time")) |>
#   get_miss_no_strata(missing = "always")
#
#
# # Categorical only
# create_tidy_table_one(data = pbc_mayo2,
#                       # strata = "trt",
#                       vars = c("stage")) |>
#   get_miss_no_strata(missing = "always")
#
#
# create_tidy_table_one(data = pbc_mayo2,
#                       # strata = "trt",
#                       vars = c("time",
#                                "status",
#                                "stage")) |>
#   get_miss_no_strata(missing = "always")
#
#
#
#
# # TODO -- pick up with if any. Think ahead to formatting. To show the percent or
# # the number or both... Maybe jsut the number for now. Maybe don't format until after.
#
#
# # Missing == "ifany"
#
# tab1 |>
#   dplyr::select(strata,
#                 var,
#                 n,
#                 missing,
#                 level,
#                 n_level:n_strata_valid,
#                 var_type) |>
#   mutate(n = dplyr::coalesce(n, n_strata),
#          missing = dplyr::if_else(var_type == "continuous",
#                                   missing,
#                                   n_strata - n_strata_valid),
#          missing_p = missing / n) |>
#   dplyr::distinct(strata,
#                   var,
#                   n,
#                   missing,
#                   missing_p,
#                   var_type)
#
#
#
# tab2 |>
#   dplyr::select(var,
#                 n,
#                 missing,
#                 level,
#                 n_level:n_strata_valid,
#                 var_type) |>
#   mutate(n = dplyr::coalesce(n, n_strata),
#          missing = dplyr::if_else(var_type == "continuous",
#                                   missing,
#                                   n_strata - n_strata_valid),
#          missing_p = missing / n) |>
#   dplyr::distinct(var,
#                   n,
#                   missing,
#                   missing_p,
#                   var_type)
#
#
#
#
#
# pbc_mayo2 |>
#   mutate(trt = forcats::fct_na_value_to_level(trt,
#                                               level = "(Missing)")) |>
#   dplyr::select(trt,
#                 time,
#                 status,
#                 stage) |>
#   gtsummary::tbl_summary(by = trt,
#                          missing = "ifany")
#
#
#
#
#
#
# latable::create_table_one(data = pbc_mayo2,
#                           strata = "trt",
#                           vars = c("time",
#                                    "status",
#                                    "stage"))
#
#
# tab1 |>
#   dplyr::filter(!(is.na(level) & var_type == "categorical")) |>
#   adorn_tidytableone(default_continuous = "{mean} ({sd})\n{median} [{p25} to {p75}]\n{p0} to {p100}")
#
# pbc_mayo2 |>
#   mutate(trt = forcats::fct_na_value_to_level(trt,
#                                               level = "(Missing)")) |>
#   dplyr::select(trt,
#                 time,
#                 status,
#                 stage) |>
#   gtsummary::tbl_summary(by = trt,
#                          missing = "no")
