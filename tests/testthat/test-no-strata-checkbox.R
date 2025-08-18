test_that("no-strata checkbox block aggregates and labels correctly", {
  set.seed(3)
  # build a REDCap-like wide checkbox set
  n <- 60
  df <- tibble::tibble(
    age = sample(18:85, n, TRUE),
    race___1  = sample(c("Checked","Unchecked"), n, TRUE, prob = c(.3,.7)),
    race___2  = sample(c("Checked","Unchecked"), n, TRUE, prob = c(.2,.8)),
    race___3  = sample(c("Checked","Unchecked"), n, TRUE, prob = c(.1,.9)),
    race___98 = sample(c("Checked","Unchecked"), n, TRUE, prob = c(.05,.95))
  )

  spec <- tibble::tribble(
    ~var,        ~overall_lbl, ~checkbox_lbl,            ~checkbox_txt,
    "race___1",  "Race",       "White",                  "Checked",
    "race___2",  "Race",       "Black or African-American","Checked",
    "race___3",  "Race",       "American Indian or Alaska Native","Checked",
    "race___98", "Race",       "Prefer not to answer",   "Checked"
  )

  tt <- create_tidytableone_no_strata_checkbox(
    data = df,
    vars = c("age","race___1","race___2","race___3","race___98"),
    checkbox = spec
  )

  # should include the collapsed 'Race' variable with per-level rows + Any selected
  race_rows <- tt %>%
  dplyr::filter(.data$class == "checkbox", .data$label == "Race")
  expect_gt(nrow(race_rows), 0)
  expect_true(all(c("level","pct","n_strata") %in% names(race_rows)))
  expect_true(any(race_rows$level == "Any selected"))

  # overall denominator should be N for denom = "group" default
  expect_true(all(race_rows$n_strata[!is.na(race_rows$n_strata)] %in% n))

  # percent bounds sane
  expect_true(all(is.na(race_rows$pct) | (race_rows$pct >= 0 & race_rows$pct <= 1)))
})