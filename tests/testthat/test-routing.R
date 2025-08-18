test_that("create_tidy_table_one routes to checkbox path (strata present)", {
  df <- tibble::tibble(
    group = rep(c("A","B"), each = 15),
    race___1 = sample(c("Checked","Unchecked"), 30, TRUE),
    age = rnorm(30)
  )
  cb <- tibble::tribble(
    ~var,       ~overall_lbl, ~checkbox_lbl, ~checkbox_txt,
    "race___1", "Race",       "White",       "Checked"
  )
  tab <- create_tidy_table_one(
    data = df, strata = "group", vars = c("age","race___1"),
    checkbox = cb
  )
  expect_true(any(tab$class == "checkbox"))
  expect_true("p_value_level" %in% names(tab))
})

test_that("create_tidy_table_one routes to no-strata checkbox path", {
  df <- tibble::tibble(
    race___1 = sample(c("Checked","Unchecked"), 20, TRUE),
    y = rnorm(20)
  )
  cb <- tibble::tribble(
    ~var,       ~overall_lbl, ~checkbox_lbl, ~checkbox_txt,
    "race___1", "Race",       "White",       "Checked"
  )
  tab <- create_tidy_table_one(df, vars = c("y","race___1"), checkbox = cb)
  expect_true(any(tab$class == "checkbox"))
  expect_true(any(tab$strata == "Overall"))
})