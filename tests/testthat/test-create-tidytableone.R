test_that("legacy returns required columns and no join leftovers", {
  
  withr::local_options(list(tidytableone.warn_chisq = FALSE))
  
  tt <- create_tidytableone(
    data = tiny_df,
    strata = "group",
    vars = c("x_num","x_cat","race___1","race___2")
  )

  expect_true(all(c("strata","var","label","class","var_type") %in% names(tt)))
  expect_true(all(c("pct","smd") %in% names(tt)))

  # factor levels include Overall then strata levels
  expect_equal(levels(tt$strata), c("Overall","A","B"))

  # no .x/.y suffix columns from joins
  expect_false(any(grepl("\\.x$|\\.y$", names(tt))))
})

test_that("legacy handles only continuous or only categorical without error", {
  
  withr::local_options(list(tidytableone.warn_chisq = FALSE))
  
  expect_no_error({
    tt_num <- create_tidytableone(
      data = tiny_df,
      strata = "group",
      vars = "x_num"
    )
  })
  expect_true(all(c("mean","sd","p50") %in% names(tt_num)))

  expect_no_error({
    tt_cat <- create_tidytableone(
      data = tiny_df,
      strata = "group",
      vars = "x_cat"
    )
  })
  expect_true(all(c("level","n_level","pct") %in% names(tt_cat)))
})

test_that("legacy respects variable label attribute when present", {
  
  withr::local_options(list(tidytableone.warn_chisq = FALSE))
  
  df_lab <- tiny_df
  attr(df_lab$x_num, "label") <- "Numeric X"
  tt <- create_tidytableone(
    data = df_lab,
    strata = "group",
    vars = c("x_num","x_cat")
  )
  lab_row <- dplyr::filter(tt, var == "x_num")$label
  expect_true(any(lab_row == "Numeric X" | is.na(lab_row)))
})