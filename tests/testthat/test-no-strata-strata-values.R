test_that("no-strata uses strata=='Overall' and strata_var==NA", {
  set.seed(2)
  df <- tibble::tibble(
    a = rnorm(10),
    b = sample(c("Y","N"), 10, TRUE)
  )

  tt <- create_tidytableone_no_strata(
    data = df,
    vars = c("a","b")
  )

  expect_true(is.factor(tt$strata))
  expect_identical(levels(tt$strata), "Overall")
  expect_true(all(as.character(tt$strata) == "Overall"))
  expect_true(all(is.na(tt$strata_var)))
})