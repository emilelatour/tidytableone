# Stable snapshot tests for adorn_tidytableone() covering the same 2x2 grid
# as the create tests, plus a few of the key knob combinations.
#
# Same intent as the create-tidytableone tests: a regression tripwire.

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
# Default settings across the 2x2 grid
# ---------------------------------------------------------------------------
test_that("adorn_tidytableone is stable with strata (pbc_mayo, defaults)", {
  set.seed(20251101)
  tab <- create_tidytableone(
    data   = pbc_mayo,
    strata = "trt",
    vars   = pbc_mayo_test_vars
  )
  adorned <- adorn_tidytableone(tab)
  expect_snapshot(print(adorned, n = Inf, width = 200))
})

test_that("adorn_tidytableone is stable without strata (pbc_mayo, defaults)", {
  set.seed(20251101)
  tab <- create_tidytableone(
    data   = pbc_mayo,
    strata = NULL,
    vars   = pbc_mayo_test_vars
  )
  adorned <- adorn_tidytableone(tab)
  expect_snapshot(print(adorned, n = Inf, width = 200))
})

test_that("adorn_tidytableone is stable with checkbox + strata", {
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
  adorned <- adorn_tidytableone(tab)
  expect_snapshot(print(adorned, n = Inf, width = 200))
})

test_that("adorn_tidytableone is stable with checkbox, no strata", {
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
  adorned <- adorn_tidytableone(tab)
  expect_snapshot(print(adorned, n = Inf, width = 200))
})

test_that("adorn_tidytableone is stable with multi-block mixed-name checkboxes", {
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
  adorned <- adorn_tidytableone(tab)
  expect_snapshot(print(adorned, n = Inf, width = 200))
})

# ---------------------------------------------------------------------------
# Key option combinations
# ---------------------------------------------------------------------------
test_that("adorn_tidytableone respects show_test and show_smd", {
  set.seed(20251101)
  tab <- create_tidytableone(
    data   = pbc_mayo,
    strata = "trt",
    vars   = pbc_mayo_test_vars
  )
  adorned <- adorn_tidytableone(tab, show_test = TRUE, show_smd = TRUE)
  expect_snapshot(print(adorned, n = Inf, width = 200))
})

test_that("adorn_tidytableone respects missing = 'ifany'", {
  set.seed(20251101)
  tab <- create_tidytableone(
    data   = pbc_mayo,
    strata = "trt",
    vars   = pbc_mayo_test_vars
  )
  adorned <- adorn_tidytableone(tab, missing = "ifany")
  expect_snapshot(print(adorned, n = Inf, width = 200))
})

test_that("adorn_tidytableone respects missing = 'always'", {
  set.seed(20251101)
  tab <- create_tidytableone(
    data   = pbc_mayo,
    strata = "trt",
    vars   = pbc_mayo_test_vars
  )
  adorned <- adorn_tidytableone(tab, missing = "always")
  expect_snapshot(print(adorned, n = Inf, width = 200))
})

test_that("adorn_tidytableone respects exact / nonnormal hints", {
  set.seed(20251101)
  tab <- create_tidytableone(
    data   = pbc_mayo,
    strata = "trt",
    vars   = pbc_mayo_test_vars
  )
  adorned <- adorn_tidytableone(
    tab,
    exact     = c("status", "ascites"),
    nonnormal = c("bili", "chol")
  )
  expect_snapshot(print(adorned, n = Inf, width = 200))
})

test_that("adorn_tidytableone respects combine_level_col = FALSE", {
  set.seed(20251101)
  tab <- create_tidytableone(
    data   = pbc_mayo,
    strata = "trt",
    vars   = pbc_mayo_test_vars
  )
  adorned <- adorn_tidytableone(tab, combine_level_col = FALSE)
  expect_snapshot(print(adorned, n = Inf, width = 200))
})
