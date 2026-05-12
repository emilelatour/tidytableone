# tidytableone 0.1.0

First coherent release after a major refactor.

## Breaking changes (checkbox API)

* `checkbox_opts$denom`, `checkbox_opts$test`, and `:::create_tidytableone_checkbox()` removed.
  Checkbox percentages are always group-based; test selection happens at adorn time
  via `exact` / `monte_carlo_p`.
* `checkbox_opts$pvals` renamed to `checkbox_opts$show_pvalues` (logical).
* `checkbox_opts$show_any` default changed from `TRUE` to `FALSE`. Pass `show_any = TRUE`
  explicitly when you want the "Any selected" row.
* `checkbox_opts$note` default changed to `"More than one response allowed"` and is now
  appended to the checkbox block header label (e.g., "Race, More than one response allowed").
  Set `note = ""` to suppress.

## Other API changes

* New `default_checkbox_txt = "Checked"` argument on `create_tidytableone()`. The
  `checkbox_txt` column in the spec tibble is now optional.
* `create_tidytableone()` and `adorn_tidytableone()` now error on unknown `...` arguments
  with a helpful message that suggests the right destination function.

## Internal

* Eliminated parallel no-strata code paths; one codepath for both stratified and
  unstratified usage.
* Canonical output schema centralized in `.t1_schema_defaults()`.
* Synthetic `___any_selected` variable name now derives from the block's `overall_lbl`
  (slugified), fixing misleading names for mixed-name blocks.
* New snapshot test suite covering both create and adorn behavior.