

test_that("no-strata returns required columns and no join leftovers", {
  set.seed(123)
  df <- tibble::tibble(
    gender = sample(c("Female","Male"), 100, TRUE),
    age    = sample(18:85, 100, TRUE),
    income = sample(20000:120000, 100, TRUE)
  )

  tt <- create_tidy_table_one_no_strata(
    data = df,
    vars = c("gender","age","income")
  )

  expect_s3_class(tt, "tbl_df")

  # must exist regardless of var mix
  expect_true(all(c(
    "strata","var","n","complete","missing","class","var_type","label"
  ) %in% names(tt)))

  # no join leftovers
  leftovers <- c("label.x","label.y","var_type.x","var_type.y","class.x","class.y")
  expect_false(any(leftovers %in% names(tt)))
})