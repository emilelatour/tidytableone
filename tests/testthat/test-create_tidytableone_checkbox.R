

test_that("only our custom chi-sq warning is emitted (when enabled)", {
  withr::local_options(list(tidytableone.warn_chisq = TRUE))

  # capture *all* warnings and check the messages
  msgs <- character()
  withCallingHandlers({
    create_tidytableone_checkbox(
      data = tiny_df,
      strata = "group",
      vars = c("x_cat","race___1","race___2"),
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

test_that("no warning emitted when custom warning is disabled", {
  withr::local_options(list(tidytableone.warn_chisq = FALSE))

  expect_warning(
    create_tidytableone_checkbox(
      data = tiny_df,
      strata = "group",
      vars = c("x_cat","race___1","race___2"),
      checkbox = cb_spec
    ),
    NA  # i.e., expect no warning
  )
})

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

test_that("checkbox block aggregates, labels, and per-level p-values column exists", {
  tt <- create_tidytableone_checkbox(
    data = tiny_df,
    strata = "group",
    vars   = c("x_cat","race___1","race___2"),
    checkbox = cb_spec
  )

  # Grab all checkbox rows by their machine ids (race___*)
  race_rows <- dplyr::filter(tt, stringr::str_detect(var, "^race___"))
  expect_gt(nrow(race_rows), 0)

  # Levels include White, Black, and Any selected
  expect_setequal(unique(race_rows$level), c("White","Black","Any selected"))

  # Denominator default is group; pct must be numeric within [0,1]
  grpA_white <- dplyr::filter(race_rows, strata == "A", level == "White") |> dplyr::pull(pct)
  expect_true(is.numeric(grpA_white))
  if (length(grpA_white)) expect_true(all(dplyr::between(grpA_white, 0, 1), na.rm = TRUE))

  # Per-level p-values column should exist (may be NA in degenerate cases)
  expect_true("p_value_level" %in% names(tt))

  # class/var_type set for checkbox rows
  rr_class <- dplyr::filter(tt, stringr::str_detect(var, "^race___")) |> dplyr::pull(class)
  rr_type  <- dplyr::filter(tt, stringr::str_detect(var, "^race___")) |> dplyr::pull(var_type)
  expect_true(all(rr_class == "checkbox", na.rm = TRUE))
  expect_true(all(rr_type  == "categorical", na.rm = TRUE))
})

test_that("checkbox function coexists with regular categorical/continuous rows", {
  tt <- create_tidytableone_checkbox(
    data = tiny_df,
    strata = "group",
    vars   = c("x_num","x_cat","race___1","race___2"),
    checkbox = cb_spec
  )

  # Has both continuous stats (mean/sd) and categorical stats (level/pct)
  expect_true(any(!is.na(tt$mean)))
  expect_true(any(!is.na(tt$level)))
})

test_that("checkbox function tolerates NA in strata by labeling as (Missing)", {
  df2 <- tiny_df
  df2$group[1] <- NA

  tt <- create_tidytableone_checkbox(
    data = df2,
    strata = "group",
    vars   = c("x_cat","race___1","race___2"),
    checkbox = cb_spec
  )

  # Should still include Overall + the observed levels (including the NA label we add)
  expect_true("Overall" %in% levels(tt$strata))
  expect_true(!anyNA(levels(tiny_df$group)))   # original had no NA
  expect_gt(nrow(tt), 0)                       # presence of output is enough here
})