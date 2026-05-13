
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidytableone

<!-- badges: start -->

<!-- badges: end -->

> Tidy construction of “Table 1” study-characteristic summaries for
> biomedical research.

`tidytableone` produces the descriptive “Table 1” that opens nearly
every clinical paper: a stratified summary of study characteristics with
appropriate continuous and categorical summaries, statistical tests, and
missing-data handling. The output comes in two forms — a long tidy
tibble for programmatic use, or a polished publication-style table for
direct inclusion in reports.

## Installation

``` r
# install.packages("pak")
pak::pak("emilelatour/tidytableone")
```

## Usage

The two-function workflow is `create_tidytableone()` →
`adorn_tidytableone()`:

- `create_tidytableone()` — computes summaries, tests, and standardized
  mean differences; returns a long tidy tibble of results.
- `adorn_tidytableone()` — turns that tidy tibble into a
  publication-ready wide table with formatted cells, indented level
  rows, and a single p-value column.

``` r
library(tidytableone)

tidy_t1 <- create_tidytableone(
  data   = pbc_mayo,
  strata = "trt",
  vars   = c("age", "sex", "ascites", "hepato", "spiders", "edema",
             "bili", "albumin", "stage")
)

adorn_tidytableone(tidy_t1)
#> # A tibble: 41 × 7
#>    var        num_not_miss Overall `D-penicillamine` Placebo `(Missing)` p_value
#>    <chr>      <chr>        <chr>   <chr>             <chr>   <chr>       <chr>  
#>  1 "n"        ""           "418"   "158"             "154"   "106"       ""     
#>  2 ""         ""           ""      ""                ""      ""          ""     
#>  3 "age  "    "418"        ""      ""                ""      ""          "0.018"
#>  4 "  Mean (… ""           "50.7 … "51.4 (11.0)"     "48.6 … "52.9 (9.8… ""     
#>  5 "  Median… ""           "51.0 … "51.9 [26.3 – 78… "48.1 … "53.0 [33.… ""     
#>  6 "  "       ""           ""      ""                ""      ""          ""     
#>  7 "sex, n (… "418"        ""      ""                ""      ""          "0.421"
#>  8 "  Male"   ""           " 44 (… " 21 (13.3%)"     " 15 (… "  8 ( 7.5… ""     
#>  9 "  Female" ""           "374 (… "137 (86.7%)"     "139 (… " 98 (92.5… ""     
#> 10 "  "       ""           ""      ""                ""      ""          ""     
#> # ℹ 31 more rows
```

For an unstratified table, omit `strata`:

``` r
create_tidytableone(
  data = pbc_mayo,
  vars = c("age", "sex", "bili")
) |>
  adorn_tidytableone()
#> # A tibble: 14 × 3
#>    var                      num_not_miss Overall             
#>    <chr>                    <chr>        <chr>               
#>  1 "n"                      ""           "418"               
#>  2 ""                       ""           ""                  
#>  3 "age  "                  "418"        ""                  
#>  4 "  Mean (SD)"            ""           "50.7 (10.4)"       
#>  5 "  Median [Min. – Max.]" ""           "51.0 [26.3 – 78.4]"
#>  6 "  "                     ""           ""                  
#>  7 "sex, n (%)  "           "418"        ""                  
#>  8 "  Male"                 ""           " 44 (10.5%)"       
#>  9 "  Female"               ""           "374 (89.5%)"       
#> 10 "  "                     ""           ""                  
#> 11 "bili  "                 "418"        ""                  
#> 12 "  Mean (SD)"            ""           "3.2 (4.4)"         
#> 13 "  Median [Min. – Max.]" ""           "1.4 [0.3 – 28.0]"  
#> 14 "  "                     ""           ""
```

## Checkbox-style multi-response variables

REDCap-style “check all that apply” fields are common in clinical data
and awkward to summarize with most table-one packages. `tidytableone`
handles them as first-class citizens. A small synthetic example:

``` r
library(tibble)
library(dplyr)

set.seed(42)
n <- 200 

cb_data <- tibble(
  treatment = sample(c("Drug", "Placebo"), n, replace = TRUE),
  age       = round(rnorm(n, 55, 12)),
  race___1  = sample(c("Checked", "Unchecked"), n, replace = TRUE, prob = c(0.55, 0.45)),
  race___2  = sample(c("Checked", "Unchecked"), n, replace = TRUE, prob = c(0.20, 0.80)),
  race___3  = sample(c("Checked", "Unchecked"), n, replace = TRUE, prob = c(0.10, 0.90)),
  race___98 = sample(c("Checked", "Unchecked"), n, replace = TRUE, prob = c(0.05, 0.95))
)

checkbox_spec <- tribble(
  ~var,        ~overall_lbl, ~checkbox_lbl,
  "race___1",  "Race",       "White",
  "race___2",  "Race",       "Black or African-American",
  "race___3",  "Race",       "American Indian or Alaska Native",
  "race___98", "Race",       "Prefer not to answer"
)
```

The block renders with a single labeled header (“Race, More than one
response allowed”) followed by indented per-option rows, with
group-based percentages (denominator = stratum N) and per-level
p-values. Multiple blocks per table are supported, including blocks
whose member variables don’t share a common stem.

## Statistical tests

`create_tidytableone()` computes every candidate p-value;
`adorn_tidytableone()` picks one per variable for display:

| Variable type | Default test | Override options |
|----|----|----|
| Continuous, 2 groups | Welch’s t-test | `equal_variance = TRUE` → Student’s t; `nonnormal` → Wilcoxon rank-sum |
| Continuous, 3+ groups | Welch’s ANOVA | `equal_variance = TRUE` → one-way ANOVA; `nonnormal` → Kruskal-Wallis |
| Categorical | Chi-squared | `exact` → Fisher’s exact; `monte_carlo_p` → simulated p-values |

The tidy tibble contains every test variant (Welch’s, Student’s,
Kruskal, Bartlett, Levene, chi-squared with and without continuity
correction, Fisher’s, and simulated versions), so you can switch
displays without recomputing.

## Output

`create_tidytableone()` returns a long tibble with one row per (stratum
× variable × level) combination. Key columns:

| Column | What it is |
|----|----|
| `strata_var`, `strata` | Stratification variable name and level (`"Overall"` row included) |
| `var`, `level`, `label` | Variable name, factor level (for categorical), display label |
| `var_type`, `class` | `"continuous"` / `"categorical"`; `"checkbox"` flag for multi-response |
| `n`, `complete`, `missing` | Counts |
| `mean`, `sd`, `p0`–`p100`, `cv` | Continuous summaries |
| `n_level`, `n_strata`, `pct` | Categorical counts and percentages |
| `chisq_test`, `fisher_test`, `oneway_test_*`, `kruskal_test`, … | Test p-values |
| `smd` | Standardized mean difference |

`adorn_tidytableone()` returns a wide tibble suitable for direct
printing or piping into `gt`, `flextable`, `kableExtra`, etc.

## What this isn’t

`tidytableone` is one of several R packages for descriptive tables;
alternatives include
[`tableone`](https://CRAN.R-project.org/package=tableone),
[`gtsummary`](https://www.danieldsjoberg.com/gtsummary/), and
[`compareGroups`](https://CRAN.R-project.org/package=compareGroups),
each with different design priorities. Pick what fits your workflow.

## License

MIT
