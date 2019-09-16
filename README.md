
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidytableone

<!-- badges: start -->

<!-- badges: end -->

The goal of `tidytableone` is to enable a tidy approach to creating the
“Table 1” of study characteristics common in bimoedical research.

## Installation

You can install the released version of tidytableone from
[CRAN](https://CRAN.R-project.org) with:

``` r
# install.packages("devtools")
devtools::install_github("emilelatour/tidytableone)
```

## Example

Load the packages for the example

``` r
library(tidytableone)
library(tidyverse)
#> ── Attaching packages ───────────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──
#> ✔ ggplot2 3.2.1     ✔ purrr   0.3.2
#> ✔ tibble  2.1.3     ✔ dplyr   0.8.3
#> ✔ tidyr   1.0.0     ✔ stringr 1.4.0
#> ✔ readr   1.3.1     ✔ forcats 0.4.0
#> ── Conflicts ──────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
library(knitr)
```

Take a glimpse at the data that comes in the `tidytableone` package:

``` r
dplyr::glimpse(pbc_mayo)
#> Observations: 418
#> Variables: 20
#> $ id       <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, …
#> $ time     <int> 400, 4500, 1012, 1925, 1504, 2503, 1832, 2466, 2400, 51…
#> $ status   <fct> Dead, Alive, Dead, Dead, Liver transplant, Dead, Alive,…
#> $ trt      <fct> D-penicillamine, D-penicillamine, D-penicillamine, D-pe…
#> $ age      <dbl> 58.76523, 56.44627, 70.07255, 54.74059, 38.10541, 66.25…
#> $ sex      <fct> Female, Female, Male, Female, Female, Female, Female, F…
#> $ ascites  <fct> Yes, No, No, No, No, No, No, No, No, Yes, No, No, No, Y…
#> $ hepato   <fct> Yes, Yes, No, Yes, Yes, Yes, Yes, No, No, No, Yes, No, …
#> $ spiders  <fct> Yes, Yes, No, Yes, Yes, No, No, No, Yes, Yes, Yes, Yes,…
#> $ edema    <fct> "Edema despite diuretic therapy", "No edema and no diur…
#> $ bili     <dbl> 14.5, 1.1, 1.4, 1.8, 3.4, 0.8, 1.0, 0.3, 3.2, 12.6, 1.4…
#> $ chol     <int> 261, 302, 176, 244, 279, 248, 322, 280, 562, 200, 259, …
#> $ albumin  <dbl> 2.60, 4.14, 3.48, 2.54, 3.53, 3.98, 4.09, 4.00, 3.08, 2…
#> $ copper   <int> 156, 54, 210, 64, 143, 50, 52, 52, 79, 140, 46, 94, 40,…
#> $ alk_phos <dbl> 1718.0, 7394.8, 516.0, 6121.8, 671.0, 944.0, 824.0, 465…
#> $ ast      <dbl> 137.95, 113.52, 96.10, 60.63, 113.15, 93.00, 60.45, 28.…
#> $ trig     <int> 172, 88, 55, 92, 72, 63, 213, 189, 88, 143, 79, 95, 130…
#> $ platelet <int> 190, 221, 151, 183, 136, NA, 204, 373, 251, 302, 258, 7…
#> $ protime  <dbl> 12.2, 10.6, 12.0, 10.3, 10.9, 11.0, 9.7, 11.0, 11.0, 11…
#> $ stage    <chr> "4", "3", "4", "4", "3", "3", "3", "3", "2", "4", "4", …
```

Use the main function of this package to make a tidy table one

``` r
tab1 <- tidytableone::create_tidy_table_one(data = pbc_mayo, 
                                            strata = "trt", 
                                            .vars = c("time", "status", "trt", "age", "sex", "ascites", "hepato",
                                                      "spiders", "edema", "bili", "chol", "albumin", "copper", "alk_phos",
                                                      "ast", "trig", "platelet", "protime", "stage"))
```

Use the `dplyr::glimpse` command to see the tidy table one

``` r
dplyr::glimpse(tab1)
#> Observations: 132
#> Variables: 27
#> $ strata                 <fct> Overall, D-penicillamine, Placebo, (Missi…
#> $ var                    <fct> time, time, time, time, status, status, s…
#> $ n                      <int> 418, 158, 154, 106, NA, NA, NA, NA, NA, N…
#> $ n_distinct             <int> 399, 155, 151, 106, NA, NA, NA, NA, NA, N…
#> $ complete               <int> 418, 158, 154, 106, NA, NA, NA, NA, NA, N…
#> $ missing                <int> 0, 0, 0, 0, NA, NA, NA, NA, NA, NA, NA, N…
#> $ mean                   <dbl> 1917.78230, 2015.62025, 1996.86364, 1657.…
#> $ sd                     <dbl> 1104.672992, 1094.123315, 1155.928911, 10…
#> $ p0                     <dbl> 41.00000, 41.00000, 51.00000, 41.00000, N…
#> $ p25                    <dbl> 1092.75000, 1231.00000, 1153.00000, 998.0…
#> $ p50                    <dbl> 1730.00000, 1895.00000, 1811.00000, 1397.…
#> $ p75                    <dbl> 2613.50000, 2632.50000, 2771.25000, 2262.…
#> $ p100                   <dbl> 4795.00000, 4556.00000, 4523.00000, 4795.…
#> $ cv                     <dbl> 0.5760158, 0.5428221, 0.5788722, 0.608644…
#> $ shapiro_test           <dbl> 8.291488e-08, 3.809631e-03, 1.249396e-03,…
#> $ level                  <chr> NA, NA, NA, NA, "Alive", "Dead", "Liver t…
#> $ n_level                <dbl> NA, NA, NA, NA, 232, 161, 25, 83, 65, 10,…
#> $ n_strata               <dbl> NA, NA, NA, NA, 418, 418, 418, 158, 158, …
#> $ chisq_test             <dbl> NA, NA, NA, NA, 0.8935097, 0.8935097, 0.8…
#> $ fisher_test            <dbl> NA, NA, NA, NA, 0.8842219, 0.8842219, 0.8…
#> $ check_categorical_test <chr> NA, NA, NA, NA, "ok", "ok", "ok", "ok", "…
#> $ oneway_test            <dbl> 0.88312949, 0.88312949, 0.88312949, 0.883…
#> $ kruskal_test           <dbl> 0.82661809, 0.82661809, 0.82661809, 0.826…
#> $ bartlett_test          <dbl> 0.4946294, 0.4946294, 0.4946294, 0.494629…
#> $ smd                    <dbl> 0.01666588, 0.01666588, 0.01666588, 0.016…
#> $ class                  <chr> "integer", "integer", "integer", "integer…
#> $ var_type               <chr> "continuous", "continuous", "continuous",…
```

Some information is only available for categorical variables and some
information for continuous variables.

``` r

tab1 %>% 
  dplyr::filter(var_type == "categorical") %>% 
  dplyr::select(strata, var, level:check_categorical_test, smd:var_type)
#> # A tibble: 88 x 11
#>    strata var   level n_level n_strata chisq_test fisher_test
#>    <fct>  <fct> <chr>   <dbl>    <dbl>      <dbl>       <dbl>
#>  1 Overa… stat… Alive     232      418      0.894       0.884
#>  2 Overa… stat… Dead      161      418      0.894       0.884
#>  3 Overa… stat… Live…      25      418      0.894       0.884
#>  4 D-pen… stat… Alive      83      158      0.894       0.884
#>  5 D-pen… stat… Dead       65      158      0.894       0.884
#>  6 D-pen… stat… Live…      10      158      0.894       0.884
#>  7 Place… stat… Alive      85      154      0.894       0.884
#>  8 Place… stat… Dead       60      154      0.894       0.884
#>  9 Place… stat… Live…       9      154      0.894       0.884
#> 10 (Miss… stat… Alive      64      106      0.894       0.884
#> # … with 78 more rows, and 4 more variables: check_categorical_test <chr>,
#> #   smd <dbl>, class <chr>, var_type <chr>
```

``` r

tab1 %>% 
  dplyr::filter(var_type == "continuous") %>% 
  dplyr::select(strata, var, n:shapiro_test, oneway_test:bartlett_test, smd:var_type)
#> # A tibble: 44 x 21
#>    strata var       n n_distinct complete missing   mean     sd    p0
#>    <fct>  <fct> <int>      <int>    <int>   <int>  <dbl>  <dbl> <dbl>
#>  1 Overa… time    418        399      418       0 1.92e3 1.10e3  41  
#>  2 D-pen… time    158        155      158       0 2.02e3 1.09e3  41  
#>  3 Place… time    154        151      154       0 2.00e3 1.16e3  51  
#>  4 (Miss… time    106        106      106       0 1.66e3 1.01e3  41  
#>  5 Overa… age     418        344      418       0 5.07e1 1.04e1  26.3
#>  6 D-pen… age     158        156      158       0 5.14e1 1.10e1  26.3
#>  7 Place… age     154        154      154       0 4.86e1 9.96e0  30.6
#>  8 (Miss… age     106         37      106       0 5.29e1 9.78e0  33.0
#>  9 Overa… bili    418         98      418       0 3.22e0 4.41e0   0.3
#> 10 D-pen… bili    158         58      158       0 2.87e0 3.63e0   0.3
#> # … with 34 more rows, and 12 more variables: p25 <dbl>, p50 <dbl>,
#> #   p75 <dbl>, p100 <dbl>, cv <dbl>, shapiro_test <dbl>,
#> #   oneway_test <dbl>, kruskal_test <dbl>, bartlett_test <dbl>, smd <dbl>,
#> #   class <chr>, var_type <chr>
```

Then with some simple data manipulation we can reshape to make a table
one…

``` r
tab1 %>% 
  mutate(strata = forcats::fct_inorder(strata), 
         n_pct = scales::percent(n_level / n_strata), 
         mean = scales::number(mean, accuracy = 0.1), 
         value = dplyr::if_else(mean == "NA", n_pct, mean), 
         p_value = dplyr::coalesce(oneway_test, chisq_test), 
         p_value = scales::pvalue(p_value)) %>% 
  dplyr::distinct(strata, 
                  var, 
                  level, 
                  value, 
                  p_value) %>% 
  tidyr::spread(., 
                key = strata, 
                value = value) %>% 
  dplyr::select(-p_value, dplyr::everything(), p_value) %>% 
  knitr::kable()
```

| var       | level                                                           | Overall | D-penicillamine | Placebo | (Missing) | p\_value |
| :-------- | :-------------------------------------------------------------- | :------ | :-------------- | :------ | :-------- | :------- |
| time      | NA                                                              | 1 917.8 | 2 015.6         | 1 996.9 | 1 657.1   | 0.883    |
| status    | Alive                                                           | 56%     | 53%             | 55%     | 60%       | 0.894    |
| status    | Dead                                                            | 39%     | 41%             | 39%     | 34%       | 0.894    |
| status    | Liver transplant                                                | 6%      | 6%              | 6%      | 6%        | 0.894    |
| age       | NA                                                              | 50.7    | 51.4            | 48.6    | 52.9      | 0.018    |
| sex       | Female                                                          | 89%     | 87%             | 90%     | 92%       | 0.421    |
| sex       | Male                                                            | 11%     | 13%             | 10%     | 8%        | 0.421    |
| ascites   | No                                                              | 69%     | 91%             | 94%     | 0%        | 0.567    |
| ascites   | Yes                                                             | 6%      | 9%              | 6%      | 0%        | 0.567    |
| ascites   | NA                                                              | 25%     | 0%              | 0%      | 100%      | 0.567    |
| hepato    | No                                                              | 36%     | 54%             | 44%     | 0%        | 0.088    |
| hepato    | Yes                                                             | 38%     | 46%             | 56%     | 0%        | 0.088    |
| hepato    | NA                                                              | 25%     | 0%              | 0%      | 100%      | 0.088    |
| spiders   | No                                                              | 53%     | 72%             | 71%     | 0%        | 0.985    |
| spiders   | Yes                                                             | 22%     | 28%             | 29%     | 0%        | 0.985    |
| spiders   | NA                                                              | 25%     | 0%              | 0%      | 100%      | 0.985    |
| edema     | Edema despite diuretic therapy                                  | 5%      | 6%              | 6%      | 0%        | 0.877    |
| edema     | Edema present without diuretics, or edema resolved by diuretics | 11%     | 10%             | 8%      | 14%       | 0.877    |
| edema     | No edema and no diuretic therapy for edema                      | 85%     | 84%             | 85%     | 86%       | 0.877    |
| bili      | NA                                                              | 3.2     | 2.9             | 3.6     | 3.1       | 0.133    |
| chol      | NA                                                              | 369.5   | 365.0           | 373.9   | NaN       | 0.747    |
| albumin   | NA                                                              | 3.5     | 3.5             | 3.5     | 3.4       | 0.874    |
| copper    | NA                                                              | 97.6    | 97.6            | 97.7    | NaN       | 0.999    |
| alk\_phos | NA                                                              | 1 982.7 | 2 021.3         | 1 943.0 | NaN       | 0.747    |
| ast       | NA                                                              | 122.6   | 120.2           | 125.0   | NaN       | 0.460    |
| trig      | NA                                                              | 124.7   | 124.1           | 125.3   | NaN       | 0.886    |
| platelet  | NA                                                              | 257.0   | 258.8           | 265.2   | 241.7     | 0.554    |
| protime   | NA                                                              | 10.7    | 10.7            | 10.8    | 10.8      | 0.199    |
| stage     | 1                                                               | 5%      | 8%              | 3%      | 5%        | 0.201    |
| stage     | 2                                                               | 22%     | 22%             | 21%     | 24%       | 0.201    |
| stage     | 3                                                               | 37%     | 35%             | 42%     | 33%       | 0.201    |
| stage     | 4                                                               | 34%     | 35%             | 35%     | 33%       | 0.201    |
| stage     | NA                                                              | 1%      | 0%              | 0%      | 6%        | 0.201    |

This is rough, but you get the idea. The plan is to next create a
function to make the table based on specifications.

Or possibly with some nesting…

``` r
tab1 %>% 
  tidyr::nest(con_res = c(n:shapiro_test, oneway_test:bartlett_test)) %>% 
  tidyr::nest(cat_res = c(n_level:check_categorical_test)) %>% 
  mutate(mean_sd = purrr::map_chr(.x = con_res, 
                                  .f = ~ paste0(round(.x$mean, 1), 
                                                " (", 
                                                round(.x$sd, 1), 
                                                ")"))) %>% 
  mutate(n_pct = purrr::map_chr(.x = cat_res, 
                                .f = ~ tryCatch(suppressWarnings(paste0(.x$n_level, 
                                                       " (", 
                                                       scales::percent(.x$n_level / .x$n_strata), 
                                                       ")")), 
                                                error = function(err) NA))) %>% 
  mutate(value = dplyr::if_else(var_type == "continuous", mean_sd, n_pct)) %>% 
  dplyr::select(-mean_sd, -n_pct, -class)
#> # A tibble: 132 x 8
#>    strata    var    level        smd var_type    con_res   cat_res value   
#>    <fct>     <fct>  <chr>      <dbl> <chr>    <list<df[> <list<df> <chr>   
#>  1 Overall   time   <NA>      0.0167 continu…   [1 × 16]   [1 × 5] 1917.8 …
#>  2 D-penici… time   <NA>      0.0167 continu…   [1 × 16]   [1 × 5] 2015.6 …
#>  3 Placebo   time   <NA>      0.0167 continu…   [1 × 16]   [1 × 5] 1996.9 …
#>  4 (Missing) time   <NA>      0.0167 continu…   [1 × 16]   [1 × 5] 1657.1 …
#>  5 Overall   status Alive     0.0538 categor…   [1 × 16]   [1 × 5] 232 (55…
#>  6 Overall   status Dead      0.0538 categor…   [1 × 16]   [1 × 5] 161 (38…
#>  7 Overall   status Liver tr… 0.0538 categor…   [1 × 16]   [1 × 5] 25 (5.9…
#>  8 D-penici… status Alive     0.0538 categor…   [1 × 16]   [1 × 5] 83 (52.…
#>  9 D-penici… status Dead      0.0538 categor…   [1 × 16]   [1 × 5] 65 (41.…
#> 10 D-penici… status Liver tr… 0.0538 categor…   [1 × 16]   [1 × 5] 10 (6.3…
#> # … with 122 more rows
```
