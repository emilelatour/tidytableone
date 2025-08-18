# tests/testthat/test-warnings.R

test_that("only our custom chi-sq warning is emitted (when enabled)", {
  withr::local_options(list(tidytableone.warn_chisq = TRUE))

  msgs <- character()
  withCallingHandlers({
    create_tidytableone_checkbox(
      data = tiny_df,
      strata = "group",
      vars = c("x_cat", "race___1", "race___2"),
      checkbox = cb_spec
    )
  }, warning = function(w) {
    msgs <<- c(msgs, conditionMessage(w))
    invokeRestart("muffleWarning")
  })

  # exactly one warning and it's ours
  expect_length(msgs, 1)
  expect_match(msgs, "Chi-squared assumptions may be violated", fixed = TRUE)
})

test_that("no chi-sq warning emitted when custom warning is disabled", {
  withr::local_options(list(tidytableone.warn_chisq = FALSE))

  msgs <- character()
  withCallingHandlers({
    create_tidytableone_checkbox(
      data = tiny_df,
      strata = "group",
      vars = c("x_cat", "race___1", "race___2"),
      checkbox = cb_spec
    )
  }, warning = function(w) {
    msgs <<- c(msgs, conditionMessage(w))
    invokeRestart("muffleWarning")
  })

  # nothing captured
  expect_length(msgs, 0)
})

test_that("legacy create_tidy_table_one() triggers a deprecation warning", {
  msgs <- character()
  withCallingHandlers({
    create_tidy_table_one(
      data = tiny_df,
      strata = "group",
      vars = c("x_cat", "race___1", "race___2")
    )
  }, warning = function(w) {
    msgs <<- c(msgs, conditionMessage(w))
    invokeRestart("muffleWarning")
  })

  expect_true(any(grepl("is deprecated", msgs)))
})