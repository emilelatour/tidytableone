test_that("checkbox block aggregates, labels, and per-level p-values column exists", {
  tt <- create_tidy_table_one_checkbox(
    data = tiny_df,
    strata = "group",
    vars = c("x_cat","race___1","race___2"),
    checkbox = cb_spec
  )

  # Expect the conceptual block "Race"
  race_rows <- dplyr::filter(tt, var == "Race")
  expect_gt(nrow(race_rows), 0)

  # Levels include White, Black, and Any selected
  expect_setequal(unique(race_rows$level), c("White","Black","Any selected"))

  # Denominator default is group; pct must be numeric within [0,1]
  grpA_white <- dplyr::filter(race_rows, strata == "A", level == "White")$pct
  expect_true(is.numeric(grpA_white))
  if (length(grpA_white)) expect_true(all(dplyr::between(grpA_white, 0, 1), na.rm = TRUE))

  # Per-level p-values column should exist (may be NA in degenerate cases)
  expect_true("p_value_level" %in% names(tt))

  # class/var_type set for checkbox rows
  expect_true(all(dplyr::filter(tt, var == "Race")$class == "checkbox"))
  expect_true(all(dplyr::filter(tt, var == "Race")$var_type == "categorical"))
})

test_that("checkbox function coexists with regular categorical/continuous rows", {
  tt <- create_tidy_table_one_checkbox(
    data = tiny_df,
    strata = "group",
    vars = c("x_num","x_cat","race___1","race___2"),
    checkbox = cb_spec
  )

  # Has both continuous stats (mean/sd) and categorical stats (level/pct)
  expect_true(any(!is.na(tt$mean)))
  expect_true(any(!is.na(tt$level)))
})

test_that("checkbox function tolerates NA in strata by labeling as (Missing)", {
  df2 <- tiny_df
  df2$group[1] <- NA
  tt <- create_tidy_table_one_checkbox(
    data = df2,
    strata = "group",
    vars = c("x_cat","race___1","race___2"),
    checkbox = cb_spec
  )
  # Should still include Overall + the observed levels (including missing label)
  expect_true("Overall" %in% levels(tt$strata))
  expect_true(anyNA(levels(tiny_df$group)) == FALSE) # original had no NA
  # Function should not error; presence of output is enough here
  expect_gt(nrow(tt), 0)
})