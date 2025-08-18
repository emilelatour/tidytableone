

library(dplyr)
library(labelled)
library(tibble)
library(tidyr)

set.seed(42)
df2 <- tibble(
  record_id = 1:100,
  gender = sample(c("Female", "Male"), 100, TRUE),
  age = sample(18:85, 100, TRUE),
  education = sample(c("High-school", "College", "Graduate school"), 100, TRUE),
  ethnicity = sample(c("Hispanic", "Non-hispanic"), 100, TRUE),
  income = sample(20000:120000, 100, TRUE),
  marital_status = sample(c("Married", "Single"), 100, TRUE),
  group = sample(c("Treatment", "Control"), 100, TRUE),
  key = sample(paste0("race___", c(1:6, 98)), 100, TRUE),
  value = "Checked"
) %>%
  mutate(key = factor(key, levels = paste0("race___", c(1:6, 98)))) %>%
  spread(key, value) %>%
  mutate(across(starts_with("race___"), ~ replace_na(., "Unchecked")))

# Add some NAs
df2 <- df2 |>
  mutate(race___1 = dplyr::if_else(record_id %in% sample(1:100, size = 20), NA_character_, race___1))

df2 <- df2 |>
  mutate(gender = dplyr::if_else(record_id %in% sample(1:100, size = 10), NA_character_, gender))

# Checkbox mapping
cb_map <- tribble(
  ~var,        ~overall_lbl, ~checkbox_lbl,                              ~checkbox_txt,
  "race___1",  "Race",       "White",                                    "Checked",
  "race___2",  "Race",       "Black or African-American",                "Checked",
  "race___3",  "Race",       "American Indian or Alaska Native",         "Checked",
  "race___4",  "Race",       "Asian",                                    "Checked",
  "race___5",  "Race",       "Native Hawaiian or Pacific Islander",      "Checked",
  "race___6",  "Race",       "Other",                                    "Checked",
  "race___98", "Race",       "Prefer not to answer",                     "Checked"
)


# With variable labels
df2_labelled <- set_variable_labels(
  df2,
  gender = "Gender", age = "Age (years)", education = "Education",
  ethnicity = "Ethnicity", income = "Income", marital_status = "Marital status",
  race___1 = "Race (White)", race___2 = "Race (Black)", race___3 = "Race (American Indian)",
  race___4 = "Race (Asian)", race___5 = "Race (Native Hawaiian)", race___6 = "Race (Other)",
  race___98 = "Race (Prefer not to answer)"
)

t1 <- create_tidy_table_one_checkbox(
  data = df2_labelled,
  strata = "group",
  vars = c("gender", "age", "education", "ethnicity", "income", "marital_status",
           paste0("race___", c(1:6, 98))),
  checkbox = cb_map
)




create_tidy_table_one(
  data = df2_labelled,
  strata = "group",
  vars = c("gender", "age", "education", "ethnicity", "income", "marital_status",
           paste0("race___", c(1:6, 98))),
  checkbox = cb_map
)

t1 |>
  # dplyr::select(-level_var) |>
  # mutate(level_var = dplyr::case_when(
  #   level == "Any selected" & is.na(level_var) ~ paste0(gsub("[[:digit:]]", "", dplyr::lag(level_var)), "_any_selected"),
  #   .default = level_var),
  #   label = dplyr::if_else(class == "checkbox",
  #                               var,
  #                               label),
  #        var = dplyr::if_else(class == "checkbox",
  #                               level_var,
  #                               var)) |>
adorn_tidytableone(missing = "always") |>
  print(n = Inf)


create_tidy_table_one_checkbox(
  data = df2_labelled,
  strata = "group",
  vars = c("gender", "age", "education", "ethnicity", "income", "marital_status",
           paste0("race___", c(1:6, 98))),
  checkbox = cb_map
)



process_categorical(df2_labelled,
                                     rlang::sym("group"),
                                     "race___1")

with(df2_labelled,
     fisher.test(group,
                 race___1))


create_tidy_table_one_checkbox(
  data = df2_labelled,
  strata = "group",
  vars = c("gender", "age", "education", "ethnicity", "income", "marital_status",
           paste0("race___", c(1:6, 98))),
  checkbox = cb_map
) |>
  print(n = Inf)










# Basic table with checkboxes
create_tidy_table_one_checkbox(
  data = df2,
  strata = "group",
  vars = c("gender", "age", "education", "ethnicity", "income", "marital_status",
           paste0("race___", c(1:6, 98))),
  checkbox = cb_map
)

create_tidy_table_one_checkbox(
  data = df2,
  strata = "group",
  vars = c("gender", "age", "education", "ethnicity", "income", "marital_status",
           paste0("race___", c(1:6, 98)))
)

create_tidy_table_one(
  data = df2,
  strata = "group",
  vars = c("gender", "age", "education", "ethnicity", "income", "marital_status",
           paste0("race___", c(1:6, 98))),
  checkbox = cb_map
)


library(tableone)

tableone::CreateTableOne(data = df2,
  strata = "group",
  vars = c("gender", "age", "education", "ethnicity", "income", "marital_status",
           paste0("race___", c(1:6, 98)))) |>
  print(showAllLevels = TRUE, formatOptions = list(big.mark = ","))




  flex_print_tidytableone()

adorn_tidytableone(t1, show_test = TRUE, checkbox_p = "per_level")

adorn_tidytableone(t1, show_test = TRUE)

t1 %>% filter(var == "race___1")


adorn_tidytableone(t1,
                   default_continuous = "{mean} ({sd})\n{median} [{range}]") |>
  flex_print_tidytableone()






# Per-level p-values for checkbox rows (printed on each level line)
tab_pvals_cb <- tidy_t1 %>%
  dplyr::filter((.data$class %||% "") == "checkbox") %>%
  dplyr::transmute(
    var   = as.character(.data$var),
    level = as.character(.data$level),
    p_raw = .data$p_value_level
  ) %>%
  dplyr::group_by(.data$var) %>%
  dplyr::mutate(
    p_adj = if (identical(checkbox_p_adjust, "none")) p_raw
            else stats::p.adjust(p_raw, method = checkbox_p_adjust)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::transmute(
    var, level,
    p_value = scales::pvalue(p_adj, accuracy = p_accuracy,
                             decimal.mark = ".", add_p = FALSE)
  )





df2 |>
  create_tidy_table_one(strata = NULL,
                        vars = )
