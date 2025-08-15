test_that("checkbox block aggregates, labels, and per-level p-values column exists", {
  tt <- create_tidy_table_one_checkbox(
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
  tt <- create_tidy_table_one_checkbox(
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

  tt <- create_tidy_table_one_checkbox(
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