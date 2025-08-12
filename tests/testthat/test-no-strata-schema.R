test_that("no-strata output has full schema and no join leftovers", {
  set.seed(1)
  df <- tibble::tibble(
    x_num = rnorm(20),
    x_cat = sample(c("A","B","C", NA), 20, TRUE)
  )

  tt <- create_tidy_table_one_no_strata(
    data = df,
    vars = c("x_num","x_cat")
  )

  # Required columns exist (spot-check plus no *.x/*.y remnants)
  must_have <- c(
    "strata_var","strata","var","level",
    "n","n_distinct","complete","missing",
    "n_level","n_strata","n_level_valid","n_strata_valid",
    "mean","sd","p0","p25","p50","p75","p100","cv",
    "pct","pct_valid",
    "chisq_test","chisq_test_no_correction","chisq_test_simulated",
    "fisher_test","fisher_test_simulated","check_categorical_test",
    "oneway_test_unequal_var","oneway_test_equal_var",
    "kruskal_test","bartlett_test","levene_test","smd",
    "class","var_type","label"
  )
  expect_true(all(must_have %in% names(tt)))
  expect_false(any(grepl("\\.(x|y)$", names(tt))))
})