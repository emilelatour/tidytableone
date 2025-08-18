test_that("no-strata handles categorical only and computes pct", {
  set.seed(2)
  df <- tibble::tibble(
    a = sample(c("A","B","C"), 40, TRUE),
    b = sample(c("Yes","No"),  40, TRUE)
  )

  tt <- create_tidytableone_no_strata(
    data = df,
    vars = c("a","b")
  )

  expect_true(all(c("level","n_level","n_strata","pct","pct_valid") %in% names(tt)))
  # overall denominator should be row count
  expect_true(all(tt$n_strata[!is.na(tt$n_strata)] %in% nrow(df)))
  # pct should be n_level / n_strata
  ok <- is.na(tt$pct) | abs(tt$pct - tt$n_level/tt$n_strata) < 1e-12
  expect_true(all(ok))
})