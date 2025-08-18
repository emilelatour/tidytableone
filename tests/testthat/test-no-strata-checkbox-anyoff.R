test_that("no-strata checkbox: show_any = FALSE removes the 'Any selected' row", {
  set.seed(4)
  n <- 25
  df <- tibble::tibble(
    race___1  = sample(c("Checked","Unchecked"), n, TRUE),
    race___2  = sample(c("Checked","Unchecked"), n, TRUE)
  )

  cb_spec <- tibble::tribble(
    ~var,        ~overall_lbl, ~checkbox_lbl, ~checkbox_txt,
    "race___1",  "Race",       "White",       "Checked",
    "race___2",  "Race",       "Black",       "Checked"
  )

  tt <- create_tidytableone_no_strata_checkbox(
    data = df,
    vars = c("race___1","race___2"),
    checkbox = cb_spec,
    checkbox_opts = list(
      denom   = "group",
      show_any = FALSE
    )
  )

  expect_false(any(dplyr::filter(tt, var == "Race")$level == "Any selected"))
})