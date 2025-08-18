test_that("no-strata checkbox: class tag, any-row, and denominators", {
  set.seed(3)
  n <- 30
  df <- tibble::tibble(
    # Two checkbox columns; “Checked” marks selection
    race___1  = sample(c("Checked","Unchecked"), n, TRUE, prob = c(0.4,0.6)),
    race___2  = sample(c("Checked","Unchecked"), n, TRUE, prob = c(0.2,0.8)),
    # One regular var to ensure coexistence
    age = round(rnorm(n, 50, 10))
  )

  cb_spec <- tibble::tribble(
    ~var,       ~overall_lbl, ~checkbox_lbl, ~checkbox_txt,
    "race___1", "Race",       "White",       "Checked",
    "race___2", "Race",       "Black",       "Checked"
  )

  # --- group denominator (N = nrow(data)) ---
  tt_g <- create_tidytableone_no_strata_checkbox(
    data = df,
    vars = c("age","race___1","race___2"),
    checkbox = cb_spec,
    checkbox_opts = list(
      denom    = "group",
      show_any = TRUE
    )
  )

  # All checkbox rows flagged and for the right block
  race_rows <- dplyr::filter(tt_g, .data$class == "checkbox", .data$label == "Race")
  expect_true(nrow(race_rows) > 0L)
  expect_true(all(race_rows$class == "checkbox"))

  # Contains “Any selected”
  expect_true(any(race_rows$level == "Any selected"))

  # Denominator equals full N for group denom
  expect_true(all(stats::na.omit(race_rows$n_strata) == n))

  # Pct calculation aligns with counts / N (for non-NA, non-"Any selected" rows)
  non_any <- dplyr::filter(race_rows, !is.na(.data$level) & .data$level != "Any selected")
  expect_true(all(abs(non_any$pct - (non_any$n_level / n)) < 1e-12))

  # --- responders denominator (N = #rows with any selection) ---
  any_sel <- as.integer(df$race___1 == "Checked" | df$race___2 == "Checked")
  n_resp  <- sum(any_sel, na.rm = TRUE)

  tt_r <- create_tidytableone_no_strata_checkbox(
    data = df,
    vars = c("age","race___1","race___2"),
    checkbox = cb_spec,
    checkbox_opts = list(
      denom    = "responders",
      show_any = TRUE
    )
  )

  race_rows_r <- dplyr::filter(tt_r, .data$class == "checkbox", .data$label == "Race")
  expect_true(nrow(race_rows_r) > 0L)

  # For responders denom, all n_strata should equal number of responders
  expect_true(all(stats::na.omit(race_rows_r$n_strata) == n_resp))

  # “Any selected” row count equals responders and pct == 1
  any_row <- dplyr::filter(race_rows_r, .data$level == "Any selected")
  expect_equal(nrow(any_row), 1L)
  expect_equal(any_row$n_level[[1]],  n_resp)
  expect_equal(any_row$n_strata[[1]], n_resp)
  expect_equal(any_row$pct[[1]], 1)
})