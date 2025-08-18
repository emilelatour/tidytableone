test_that("no-strata handles continuous only without error", {
  set.seed(1)
  df <- tibble::tibble(x = rnorm(50), y = rnorm(50))

  expect_no_error({
    tt <- create_tidytableone_no_strata(
      data = df,
      vars = c("x","y")
    )
  })

  tt <- create_tidytableone_no_strata(df, vars = c("x","y"))
  # core numeric summaries present
  expect_true(all(c("mean","sd","p50") %in% names(tt)))
  # no 'level' required for continuous-only rows
  # if present (from helpers), it should just be NA
  if ("level" %in% names(tt)) {
    expect_true(all(is.na(tt$level)))
  }
})