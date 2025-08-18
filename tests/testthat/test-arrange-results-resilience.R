test_that("arrange_results() pathway ensures columns referenced in relocate exist", {
  # Only continuous path:
  tt_num <- create_tidytableone(
    data = tiny_df,
    strata = "group",
    vars = "x_num"
  )
  must_exist <- c(
    "n","n_distinct","complete","missing",
    "mean","sd","p0","p25","p50","p75","p100","cv",
    "chisq_test","fisher_test", # will be NA but columns must exist
    "smd"
  )
  expect_true(all(must_exist %in% names(tt_num)))

  # Only categorical path:
  tt_cat <- create_tidytableone(
    data = tiny_df,
    strata = "group",
    vars = "x_cat"
  )
  must_exist2 <- c("level","n_level","n_strata","pct")
  expect_true(all(must_exist2 %in% names(tt_cat)))
})