# Stable snapshot tests for create_tidytableone() covering the 2x2 grid:
#
#   strata   in {present, NULL}
#   checkbox in {NULL, present}
#
# These exist primarily as a refactor safety net: if a later cleanup
# silently changes output, the snapshot diff will catch it. They are NOT
# meant to be a complete behavioural spec — they're a regression tripwire.
#
# If a snapshot changes intentionally, review the diff with
#   testthat::snapshot_review()
# and accept with
#   testthat::snapshot_accept().

# ---------------------------------------------------------------------------
# Test fixtures (inlined; no reliance on helper-*.R files)
# ---------------------------------------------------------------------------

pbc_mayo_test_vars <- c(
  "time", "status", "age", "sex", "ascites", "hepato",
  "spiders", "edema", "bili", "chol", "albumin", "stage"
)

# A small REDCap-style checkbox dataset.
make_checkbox_example <- function(seed = 20251101, n = 60) {
  set.seed(seed)
  tibble::tibble(
    id    = seq_len(n),
    group = sample(c("A", "B"), n, replace = TRUE),
    age   = round(stats::rnorm(n, 50, 12), 1),
    sex   = sample(c("Female", "Male"), n, replace = TRUE),
    race___1  = sample(c("Checked", "Unchecked"), n, TRUE, prob = c(0.55, 0.45)),
    race___2  = sample(c("Checked", "Unchecked"), n, TRUE, prob = c(0.20, 0.80)),
    race___3  = sample(c("Checked", "Unchecked"), n, TRUE, prob = c(0.10, 0.90)),
    race___98 = sample(c("Checked", "Unchecked"), n, TRUE, prob = c(0.05, 0.95))
  )
}

checkbox_example_spec <- function() {
  tibble::tribble(
    ~var,        ~overall_lbl, ~checkbox_lbl,                          ~checkbox_txt,
    "race___1",  "Race",       "White",                                "Checked",
    "race___2",  "Race",       "Black or African-American",            "Checked",
    "race___3",  "Race",       "American Indian or Alaska Native",     "Checked",
    "race___98", "Race",       "Prefer not to answer",                 "Checked"
  )
}

# A non-REDCap-style multi-block checkbox example (mixed variable names,
# "Yes"/"No" values), modelled on Emile's real-world PG-recurrence usage.
make_mixed_checkbox_example <- function(seed = 20251102, n = 80) {
  set.seed(seed)
  yn <- function(p) sample(c("Yes", "No"), n, replace = TRUE, prob = c(p, 1 - p))
  tibble::tibble(
    id       = seq_len(n),
    group    = sample(c("Recur", "No recur"), n, replace = TRUE),
    age      = round(stats::rnorm(n, 55, 14), 1),
    sex      = sample(c("Female", "Male"), n, replace = TRUE),
    rheum_dx = yn(0.15),
    heme_dx  = yn(0.08),
    solid_ca = yn(0.10),
    ckd      = yn(0.20),
    chf      = yn(0.12)
  )
}

mixed_checkbox_spec <- function() {
  tibble::tribble(
    ~var,        ~overall_lbl,                         ~checkbox_lbl,             ~checkbox_txt,
    "rheum_dx",  "PG-associated past medical history", "Rheumatologic disease",   "Yes",
    "heme_dx",   "PG-associated past medical history", "Hematologic malignancy",  "Yes",
    "solid_ca",  "PG-associated past medical history", "Solid cancer",            "Yes",
    "ckd",       "Past medical history",               "Chronic kidney disease",  "Yes",
    "chf",       "Past medical history",               "Congestive heart failure","Yes"
  )
}

# ---------------------------------------------------------------------------
# Strata present, no checkbox
# ---------------------------------------------------------------------------
test_that("create_tidytableone is stable with strata (pbc_mayo)", {
  set.seed(20251101)
  tab <- create_tidytableone(
    data   = pbc_mayo,
    strata = "trt",
    vars   = pbc_mayo_test_vars
  )
  expect_snapshot(print(tab, n = Inf, width = 200))
})

# ---------------------------------------------------------------------------
# No strata, no checkbox
# ---------------------------------------------------------------------------
test_that("create_tidytableone is stable without strata (pbc_mayo)", {
  set.seed(20251101)
  tab <- create_tidytableone(
    data   = pbc_mayo,
    strata = NULL,
    vars   = pbc_mayo_test_vars
  )
  expect_snapshot(print(tab, n = Inf, width = 200))
})

# ---------------------------------------------------------------------------
# Strata present, checkbox present (REDCap-style)
# ---------------------------------------------------------------------------
test_that("create_tidytableone is stable with checkbox + strata", {
  set.seed(20251101)
  dat  <- make_checkbox_example()
  spec <- checkbox_example_spec()
  tab <- create_tidytableone(
    data     = dat,
    strata   = "group",
    vars     = c("age", "sex",
                 "race___1", "race___2", "race___3", "race___98"),
    checkbox = spec
  )
  expect_snapshot(print(tab, n = Inf, width = 200))
})

# Regression test for a bug where, when no plain-categorical variables are
# present (cat_vars empty) but checkbox blocks are, arrange_results's
# fallback `else` branch joined sort_vars by="var" only.  var_info has one
# row per categorical *level*, so a checkbox column like `race___1` (whose
# raw values are "Checked"/"Unchecked") had two rows in sort_vars, doubling
# every checkbox row.  The fix deduplicates sort_vars by var in that branch.
test_that("create_tidytableone with checkbox + strata + no plain categorical does not duplicate rows", {
  set.seed(42)
  n <- 200
  dat <- tibble::tibble(
    treatment = sample(c("Drug", "Placebo"), n, replace = TRUE),
    age       = round(rnorm(n, 55, 12)),
    race___1  = sample(c("Checked", "Unchecked"), n, replace = TRUE),
    race___2  = sample(c("Checked", "Unchecked"), n, replace = TRUE),
    race___3  = sample(c("Checked", "Unchecked"), n, replace = TRUE)
  )
  spec <- tibble::tribble(
    ~var,        ~overall_lbl, ~checkbox_lbl,
    "race___1",  "Race",       "White",
    "race___2",  "Race",       "Black",
    "race___3",  "Race",       "Other"
  )
  tab <- create_tidytableone(
    data     = dat,
    strata   = "treatment",
    vars     = c("age", "race___1", "race___2", "race___3"),
    checkbox = spec
  )

  # Every (strata, var, level) triple should occur at most once
  dup_check <- dplyr::count(tab, strata, var, level, name = "n_rows")
  expect_true(all(dup_check$n_rows == 1L))
})

# Regression test for an attribute-preservation bug: checkbox_opts is
# attached as an attribute on the result tibble so that adorn_tidytableone()
# can append the block's `note` to the header label. But dplyr operations
# drop non-standard attributes, so attaching the attribute in the middle
# of the pipeline (as a former version did) lost it before return. The
# fix attaches at the very last step, after all dplyr ops are done.
test_that("checkbox_opts attribute survives to the final tibble", {
  set.seed(42)
  n <- 100
  dat <- tibble::tibble(
    treatment = sample(c("Drug", "Placebo"), n, replace = TRUE),
    age       = round(rnorm(n, 55, 12)),
    race___1  = sample(c("Checked", "Unchecked"), n, replace = TRUE),
    race___2  = sample(c("Checked", "Unchecked"), n, replace = TRUE)
  )
  spec <- tibble::tribble(
    ~var,        ~overall_lbl, ~checkbox_lbl,
    "race___1",  "Race",       "White",
    "race___2",  "Race",       "Black"
  )

  # With strata
  tab_s <- create_tidytableone(
    data     = dat,
    strata   = "treatment",
    vars     = c("age", "race___1", "race___2"),
    checkbox = spec
  )
  expect_false(is.null(attr(tab_s, "checkbox_opts")))
  expect_false(is.null(attr(tab_s, "checkbox_blocks")))
  expect_equal(attr(tab_s, "checkbox_opts")$note,
               "More than one response allowed")

  # Without strata
  tab_n <- create_tidytableone(
    data     = dat,
    strata   = NULL,
    vars     = c("age", "race___1", "race___2"),
    checkbox = spec
  )
  expect_false(is.null(attr(tab_n, "checkbox_opts")))
  expect_false(is.null(attr(tab_n, "checkbox_blocks")))
})

test_that("adorn_tidytableone appends the checkbox note to the header label", {
  set.seed(42)
  n <- 100
  dat <- tibble::tibble(
    treatment = sample(c("Drug", "Placebo"), n, replace = TRUE),
    age       = round(rnorm(n, 55, 12)),
    race___1  = sample(c("Checked", "Unchecked"), n, replace = TRUE),
    race___2  = sample(c("Checked", "Unchecked"), n, replace = TRUE)
  )
  spec <- tibble::tribble(
    ~var,        ~overall_lbl, ~checkbox_lbl,
    "race___1",  "Race",       "White",
    "race___2",  "Race",       "Black"
  )

  tab <- create_tidytableone(
    data     = dat,
    strata   = "treatment",
    vars     = c("age", "race___1", "race___2"),
    checkbox = spec
  )
  adorned <- adorn_tidytableone(tab)

  # The "Race" header row's var column should contain the note suffix
  expect_true(
    any(grepl("Race, More than one response allowed", adorned$var))
  )
})

# ---------------------------------------------------------------------------
# No strata, checkbox present (REDCap-style)
# ---------------------------------------------------------------------------
test_that("create_tidytableone is stable with checkbox, no strata", {
  set.seed(20251101)
  dat  <- make_checkbox_example()
  spec <- checkbox_example_spec()
  tab <- create_tidytableone(
    data     = dat,
    strata   = NULL,
    vars     = c("age", "sex",
                 "race___1", "race___2", "race___3", "race___98"),
    checkbox = spec
  )
  expect_snapshot(print(tab, n = Inf, width = 200))
})

# ---------------------------------------------------------------------------
# Mixed (non-REDCap) checkbox names, multiple blocks, with strata
# ---------------------------------------------------------------------------
test_that("create_tidytableone is stable with multi-block mixed-name checkboxes", {
  set.seed(20251102)
  dat  <- make_mixed_checkbox_example()
  spec <- mixed_checkbox_spec()
  tab <- create_tidytableone(
    data     = dat,
    strata   = "group",
    vars     = c("age", "sex", "rheum_dx", "heme_dx", "solid_ca",
                 "ckd", "chf"),
    checkbox = spec
  )
  expect_snapshot(print(tab, n = Inf, width = 200))
})

# ---------------------------------------------------------------------------
# Schema sanity checks: column names + types, separate from value content.
# These are extra-sensitive to structural drift.
# ---------------------------------------------------------------------------
test_that("create_tidytableone schema is stable (strata)", {
  tab <- create_tidytableone(
    data   = pbc_mayo,
    strata = "trt",
    vars   = pbc_mayo_test_vars
  )
  expect_snapshot({
    cat("Columns:\n")
    print(names(tab))
    cat("\nTypes:\n")
    print(vapply(tab, function(x) paste(class(x), collapse = "/"),
                 character(1)))
  })
})

test_that("create_tidytableone schema is stable (no strata)", {
  tab <- create_tidytableone(
    data   = pbc_mayo,
    strata = NULL,
    vars   = pbc_mayo_test_vars
  )
  expect_snapshot({
    cat("Columns:\n")
    print(names(tab))
    cat("\nTypes:\n")
    print(vapply(tab, function(x) paste(class(x), collapse = "/"),
                 character(1)))
  })
})
