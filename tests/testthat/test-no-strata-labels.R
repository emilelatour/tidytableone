test_that("no-strata respects variable label attribute when present", {
  df <- tibble::tibble(
    age = structure(sample(18:90, 30, TRUE), label = "Age (years)"),
    sex = structure(sample(c("F","M"), 30, TRUE),    label = "Sex at birth")
  )

  tt <- create_tidy_table_one_no_strata(
    data = df,
    vars = c("age","sex")
  )

  # labels column exists and contains our text for both vars
  lab <- dplyr::distinct(dplyr::select(tt, var, label))
  expect_true("Age (years)" %in% lab$label)
  expect_true("Sex at birth" %in% lab$label)
})