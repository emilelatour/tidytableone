
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
#> Variables: 25
#> $ strata                 <chr> "Overall", "Overall", "Overall", "Overall…
#> $ var                    <chr> "age", "albumin", "alk_phos", "ast", "bil…
#> $ n                      <int> 418, 418, 418, 418, 418, 418, 418, 418, 4…
#> $ n_distinct             <int> 344, 154, 296, 180, 98, 202, 159, 244, 49…
#> $ complete               <int> 418, 418, 312, 312, 418, 284, 310, 407, 4…
#> $ missing                <int> 0, 0, 106, 106, 0, 134, 108, 11, 2, 0, 13…
#> $ mean                   <dbl> 50.741551, 3.497440, 1982.655769, 122.556…
#> $ sd                     <dbl> 10.4472144, 0.4249716, 2140.3888245, 56.6…
#> $ p0                     <dbl> 26.27789, 1.96000, 289.00000, 26.35000, 0…
#> $ p25                    <dbl> 42.83231, 3.24250, 871.50000, 80.60000, 0…
#> $ p50                    <dbl> 51.00068, 3.53000, 1259.00000, 114.70000,…
#> $ p75                    <dbl> 58.24093, 3.77000, 1980.00000, 151.90000,…
#> $ p100                   <dbl> 78.43943, 4.64000, 13862.40000, 457.25000…
#> $ cv                     <dbl> 0.20589072, 0.12150933, 1.07955645, 0.462…
#> $ shapiro_test           <dbl> 8.918234e-03, 6.386612e-04, 6.849605e-26,…
#> $ level                  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ n_level                <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ n_strata               <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ chisq_test             <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ fisher_test            <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ check_categorical_test <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ oneway_test            <dbl> 0.01753178, 0.87370003, 0.74714193, 0.460…
#> $ kruskal_test           <dbl> 0.01962155, 0.95045176, 0.81198200, 0.458…
#> $ bartlett_test          <dbl> 2.136378e-01, 1.599496e-01, 6.354011e-01,…
#> $ smd                    <dbl> 0.2702619258, 0.0180021838, 0.0365323630,…
```

Some information is only available for categorical variables and some
information for continous variables.

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
| age       | NA                                                              | 50.7    | 51.4            | 48.6    | 52.9      | 0.018    |
| albumin   | NA                                                              | 3.5     | 3.5             | 3.5     | 3.4       | 0.874    |
| alk\_phos | NA                                                              | 1 982.7 | 2 021.3         | 1 943.0 | NaN       | 0.747    |
| ascites   | No                                                              | 69%     | 91%             | 94%     | 0%        | 0.567    |
| ascites   | Yes                                                             | 6%      | 9%              | 6%      | 0%        | 0.567    |
| ascites   | NA                                                              | 25%     | 0%              | 0%      | 100%      | 0.567    |
| ast       | NA                                                              | 122.6   | 120.2           | 125.0   | NaN       | 0.460    |
| bili      | NA                                                              | 3.2     | 2.9             | 3.6     | 3.1       | 0.133    |
| chol      | NA                                                              | 369.5   | 365.0           | 373.9   | NaN       | 0.747    |
| copper    | NA                                                              | 97.6    | 97.6            | 97.7    | NaN       | 0.999    |
| edema     | Edema despite diuretic therapy                                  | 5%      | 6%              | 6%      | 0%        | 0.877    |
| edema     | Edema present without diuretics, or edema resolved by diuretics | 11%     | 10%             | 8%      | 14%       | 0.877    |
| edema     | No edema and no diuretic therapy for edema                      | 85%     | 84%             | 85%     | 86%       | 0.877    |
| hepato    | No                                                              | 36%     | 54%             | 44%     | 0%        | 0.088    |
| hepato    | Yes                                                             | 38%     | 46%             | 56%     | 0%        | 0.088    |
| hepato    | NA                                                              | 25%     | 0%              | 0%      | 100%      | 0.088    |
| platelet  | NA                                                              | 257.0   | 258.8           | 265.2   | 241.7     | 0.554    |
| protime   | NA                                                              | 10.7    | 10.7            | 10.8    | 10.8      | 0.199    |
| sex       | Female                                                          | 89%     | 87%             | 90%     | 92%       | 0.421    |
| sex       | Male                                                            | 11%     | 13%             | 10%     | 8%        | 0.421    |
| spiders   | No                                                              | 53%     | 72%             | 71%     | 0%        | 0.985    |
| spiders   | Yes                                                             | 22%     | 28%             | 29%     | 0%        | 0.985    |
| spiders   | NA                                                              | 25%     | 0%              | 0%      | 100%      | 0.985    |
| stage     | 1                                                               | 5%      | 8%              | 3%      | 5%        | 0.201    |
| stage     | 2                                                               | 22%     | 22%             | 21%     | 24%       | 0.201    |
| stage     | 3                                                               | 37%     | 35%             | 42%     | 33%       | 0.201    |
| stage     | 4                                                               | 34%     | 35%             | 35%     | 33%       | 0.201    |
| stage     | NA                                                              | 1%      | 0%              | 0%      | 6%        | 0.201    |
| status    | Alive                                                           | 56%     | 53%             | 55%     | 60%       | 0.894    |
| status    | Dead                                                            | 39%     | 41%             | 39%     | 34%       | 0.894    |
| status    | Liver transplant                                                | 6%      | 6%              | 6%      | 6%        | 0.894    |
| time      | NA                                                              | 1 917.8 | 2 015.6         | 1 996.9 | 1 657.1   | 0.883    |
| trig      | NA                                                              | 124.7   | 124.1           | 125.3   | NaN       | 0.886    |

This is rough, but you get the idea. The plan is to next create a
function to make the table based on specifications.
