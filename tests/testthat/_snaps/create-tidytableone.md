# create_tidytableone is stable with strata (pbc_mayo)

    Code
      print(tab, n = Inf, width = 200)
    Output
      # A tibble: 92 x 40
         strata_var strata          var     level                n n_distinct complete
         <chr>      <fct>           <fct>   <fct>            <int>      <int>    <int>
       1 trt        Overall         time    <NA>               418        399      418
       2 trt        D-penicillamine time    <NA>               158        155      158
       3 trt        Placebo         time    <NA>               154        151      154
       4 trt        (Missing)       time    <NA>               106        106      106
       5 trt        Overall         status  Alive               NA         NA       NA
       6 trt        D-penicillamine status  Alive               NA         NA       NA
       7 trt        Placebo         status  Alive               NA         NA       NA
       8 trt        (Missing)       status  Alive               NA         NA       NA
       9 trt        Overall         status  Liver transplant    NA         NA       NA
      10 trt        D-penicillamine status  Liver transplant    NA         NA       NA
      11 trt        Placebo         status  Liver transplant    NA         NA       NA
      12 trt        (Missing)       status  Liver transplant    NA         NA       NA
      13 trt        Overall         status  Dead                NA         NA       NA
      14 trt        D-penicillamine status  Dead                NA         NA       NA
      15 trt        Placebo         status  Dead                NA         NA       NA
      16 trt        (Missing)       status  Dead                NA         NA       NA
      17 trt        Overall         age     <NA>               418        344      418
      18 trt        D-penicillamine age     <NA>               158        156      158
      19 trt        Placebo         age     <NA>               154        154      154
      20 trt        (Missing)       age     <NA>               106         37      106
      21 trt        Overall         sex     Male                NA         NA       NA
      22 trt        D-penicillamine sex     Male                NA         NA       NA
      23 trt        Placebo         sex     Male                NA         NA       NA
      24 trt        (Missing)       sex     Male                NA         NA       NA
      25 trt        Overall         sex     Female              NA         NA       NA
      26 trt        D-penicillamine sex     Female              NA         NA       NA
      27 trt        Placebo         sex     Female              NA         NA       NA
      28 trt        (Missing)       sex     Female              NA         NA       NA
      29 trt        Overall         ascites No                  NA         NA       NA
      30 trt        D-penicillamine ascites No                  NA         NA       NA
      31 trt        Placebo         ascites No                  NA         NA       NA
      32 trt        (Missing)       ascites No                  NA         NA       NA
      33 trt        Overall         ascites Yes                 NA         NA       NA
      34 trt        D-penicillamine ascites Yes                 NA         NA       NA
      35 trt        Placebo         ascites Yes                 NA         NA       NA
      36 trt        (Missing)       ascites Yes                 NA         NA       NA
      37 trt        Overall         hepato  No                  NA         NA       NA
      38 trt        D-penicillamine hepato  No                  NA         NA       NA
      39 trt        Placebo         hepato  No                  NA         NA       NA
      40 trt        (Missing)       hepato  No                  NA         NA       NA
      41 trt        Overall         hepato  Yes                 NA         NA       NA
      42 trt        D-penicillamine hepato  Yes                 NA         NA       NA
      43 trt        Placebo         hepato  Yes                 NA         NA       NA
      44 trt        (Missing)       hepato  Yes                 NA         NA       NA
      45 trt        Overall         spiders No                  NA         NA       NA
      46 trt        D-penicillamine spiders No                  NA         NA       NA
      47 trt        Placebo         spiders No                  NA         NA       NA
      48 trt        (Missing)       spiders No                  NA         NA       NA
      49 trt        Overall         spiders Yes                 NA         NA       NA
      50 trt        D-penicillamine spiders Yes                 NA         NA       NA
      51 trt        Placebo         spiders Yes                 NA         NA       NA
      52 trt        (Missing)       spiders Yes                 NA         NA       NA
      53 trt        Overall         edema   No edema and no~    NA         NA       NA
      54 trt        D-penicillamine edema   No edema and no~    NA         NA       NA
      55 trt        Placebo         edema   No edema and no~    NA         NA       NA
      56 trt        (Missing)       edema   No edema and no~    NA         NA       NA
      57 trt        Overall         edema   Edema present w~    NA         NA       NA
      58 trt        D-penicillamine edema   Edema present w~    NA         NA       NA
      59 trt        Placebo         edema   Edema present w~    NA         NA       NA
      60 trt        (Missing)       edema   Edema present w~    NA         NA       NA
      61 trt        Overall         edema   Edema despite d~    NA         NA       NA
      62 trt        D-penicillamine edema   Edema despite d~    NA         NA       NA
      63 trt        Placebo         edema   Edema despite d~    NA         NA       NA
      64 trt        (Missing)       edema   Edema despite d~    NA         NA       NA
      65 trt        Overall         bili    <NA>               418         98      418
      66 trt        D-penicillamine bili    <NA>               158         58      158
      67 trt        Placebo         bili    <NA>               154         59      154
      68 trt        (Missing)       bili    <NA>               106         49      106
      69 trt        Overall         chol    <NA>               418        202      284
      70 trt        D-penicillamine chol    <NA>               158        123      140
      71 trt        Placebo         chol    <NA>               154        118      144
      72 trt        (Missing)       chol    <NA>               106          1        0
      73 trt        Overall         albumin <NA>               418        154      418
      74 trt        D-penicillamine albumin <NA>               158         99      158
      75 trt        Placebo         albumin <NA>               154         90      154
      76 trt        (Missing)       albumin <NA>               106         75      106
      77 trt        Overall         stage   1                   NA         NA       NA
      78 trt        D-penicillamine stage   1                   NA         NA       NA
      79 trt        Placebo         stage   1                   NA         NA       NA
      80 trt        (Missing)       stage   1                   NA         NA       NA
      81 trt        Overall         stage   2                   NA         NA       NA
      82 trt        D-penicillamine stage   2                   NA         NA       NA
      83 trt        Placebo         stage   2                   NA         NA       NA
      84 trt        (Missing)       stage   2                   NA         NA       NA
      85 trt        Overall         stage   3                   NA         NA       NA
      86 trt        D-penicillamine stage   3                   NA         NA       NA
      87 trt        Placebo         stage   3                   NA         NA       NA
      88 trt        (Missing)       stage   3                   NA         NA       NA
      89 trt        Overall         stage   4                   NA         NA       NA
      90 trt        D-penicillamine stage   4                   NA         NA       NA
      91 trt        Placebo         stage   4                   NA         NA       NA
      92 trt        (Missing)       stage   4                   NA         NA       NA
         missing n_level n_strata n_level_valid n_strata_valid    mean       sd     p0
           <int>   <int>    <int>         <int>          <int>   <dbl>    <dbl>  <dbl>
       1       0      NA       NA            NA             NA 1918.   1105.     41   
       2       0      NA       NA            NA             NA 2016.   1094.     41   
       3       0      NA       NA            NA             NA 1997.   1156.     51   
       4       0      NA       NA            NA             NA 1657.   1009.     41   
       5      NA     232      418           232            418   NA      NA      NA   
       6      NA      83      158            83            158   NA      NA      NA   
       7      NA      85      154            85            154   NA      NA      NA   
       8      NA      64      106            64            106   NA      NA      NA   
       9      NA      25      418            25            418   NA      NA      NA   
      10      NA      10      158            10            158   NA      NA      NA   
      11      NA       9      154             9            154   NA      NA      NA   
      12      NA       6      106             6            106   NA      NA      NA   
      13      NA     161      418           161            418   NA      NA      NA   
      14      NA      65      158            65            158   NA      NA      NA   
      15      NA      60      154            60            154   NA      NA      NA   
      16      NA      36      106            36            106   NA      NA      NA   
      17       0      NA       NA            NA             NA   50.7    10.4    26.3 
      18       0      NA       NA            NA             NA   51.4    11.0    26.3 
      19       0      NA       NA            NA             NA   48.6     9.96   30.6 
      20       0      NA       NA            NA             NA   52.9     9.78   33.0 
      21      NA      44      418            44            418   NA      NA      NA   
      22      NA      21      158            21            158   NA      NA      NA   
      23      NA      15      154            15            154   NA      NA      NA   
      24      NA       8      106             8            106   NA      NA      NA   
      25      NA     374      418           374            418   NA      NA      NA   
      26      NA     137      158           137            158   NA      NA      NA   
      27      NA     139      154           139            154   NA      NA      NA   
      28      NA      98      106            98            106   NA      NA      NA   
      29      NA     288      418           288            312   NA      NA      NA   
      30      NA     144      158           144            158   NA      NA      NA   
      31      NA     144      154           144            154   NA      NA      NA   
      32      NA       0      106             0              0   NA      NA      NA   
      33      NA      24      418            24            312   NA      NA      NA   
      34      NA      14      158            14            158   NA      NA      NA   
      35      NA      10      154            10            154   NA      NA      NA   
      36      NA       0      106             0              0   NA      NA      NA   
      37      NA     152      418           152            312   NA      NA      NA   
      38      NA      85      158            85            158   NA      NA      NA   
      39      NA      67      154            67            154   NA      NA      NA   
      40      NA       0      106             0              0   NA      NA      NA   
      41      NA     160      418           160            312   NA      NA      NA   
      42      NA      73      158            73            158   NA      NA      NA   
      43      NA      87      154            87            154   NA      NA      NA   
      44      NA       0      106             0              0   NA      NA      NA   
      45      NA     222      418           222            312   NA      NA      NA   
      46      NA     113      158           113            158   NA      NA      NA   
      47      NA     109      154           109            154   NA      NA      NA   
      48      NA       0      106             0              0   NA      NA      NA   
      49      NA      90      418            90            312   NA      NA      NA   
      50      NA      45      158            45            158   NA      NA      NA   
      51      NA      45      154            45            154   NA      NA      NA   
      52      NA       0      106             0              0   NA      NA      NA   
      53      NA     354      418           354            418   NA      NA      NA   
      54      NA     132      158           132            158   NA      NA      NA   
      55      NA     131      154           131            154   NA      NA      NA   
      56      NA      91      106            91            106   NA      NA      NA   
      57      NA      44      418            44            418   NA      NA      NA   
      58      NA      16      158            16            158   NA      NA      NA   
      59      NA      13      154            13            154   NA      NA      NA   
      60      NA      15      106            15            106   NA      NA      NA   
      61      NA      20      418            20            418   NA      NA      NA   
      62      NA      10      158            10            158   NA      NA      NA   
      63      NA      10      154            10            154   NA      NA      NA   
      64      NA       0      106             0            106   NA      NA      NA   
      65       0      NA       NA            NA             NA    3.22    4.41    0.3 
      66       0      NA       NA            NA             NA    2.87    3.63    0.3 
      67       0      NA       NA            NA             NA    3.65    5.28    0.3 
      68       0      NA       NA            NA             NA    3.12    4.04    0.4 
      69     134      NA       NA            NA             NA  370.    232.    120   
      70      18      NA       NA            NA             NA  365.    210.    127   
      71      10      NA       NA            NA             NA  374.    252.    120   
      72     106      NA       NA            NA             NA  NaN      NA      NA   
      73       0      NA       NA            NA             NA    3.50    0.425   1.96
      74       0      NA       NA            NA             NA    3.52    0.443   2.1 
      75       0      NA       NA            NA             NA    3.52    0.396   1.96
      76       0      NA       NA            NA             NA    3.43    0.435   2.31
      77      NA      21      418            21            412   NA      NA      NA   
      78      NA      12      158            12            158   NA      NA      NA   
      79      NA       4      154             4            154   NA      NA      NA   
      80      NA       5      106             5            100   NA      NA      NA   
      81      NA      92      418            92            412   NA      NA      NA   
      82      NA      35      158            35            158   NA      NA      NA   
      83      NA      32      154            32            154   NA      NA      NA   
      84      NA      25      106            25            100   NA      NA      NA   
      85      NA     155      418           155            412   NA      NA      NA   
      86      NA      56      158            56            158   NA      NA      NA   
      87      NA      64      154            64            154   NA      NA      NA   
      88      NA      35      106            35            100   NA      NA      NA   
      89      NA     144      418           144            412   NA      NA      NA   
      90      NA      55      158            55            158   NA      NA      NA   
      91      NA      54      154            54            154   NA      NA      NA   
      92      NA      35      106            35            100   NA      NA      NA   
              p25     p50     p75    p100     cv     pct pct_valid chisq_test
            <dbl>   <dbl>   <dbl>   <dbl>  <dbl>   <dbl>     <dbl>      <dbl>
       1 1093.    1730    2614.   4795     0.576 NA        NA         NA     
       2 1231     1895    2632.   4556     0.543 NA        NA         NA     
       3 1153     1811    2771.   4523     0.579 NA        NA         NA     
       4  998     1397    2262.   4795     0.609 NA        NA         NA     
       5   NA       NA      NA      NA    NA      0.555     0.555      0.894 
       6   NA       NA      NA      NA    NA      0.525     0.525      0.894 
       7   NA       NA      NA      NA    NA      0.552     0.552      0.894 
       8   NA       NA      NA      NA    NA      0.604     0.604      0.894 
       9   NA       NA      NA      NA    NA      0.0598    0.0598     0.894 
      10   NA       NA      NA      NA    NA      0.0633    0.0633     0.894 
      11   NA       NA      NA      NA    NA      0.0584    0.0584     0.894 
      12   NA       NA      NA      NA    NA      0.0566    0.0566     0.894 
      13   NA       NA      NA      NA    NA      0.385     0.385      0.894 
      14   NA       NA      NA      NA    NA      0.411     0.411      0.894 
      15   NA       NA      NA      NA    NA      0.390     0.390      0.894 
      16   NA       NA      NA      NA    NA      0.340     0.340      0.894 
      17   42.8     51.0    58.2    78.4   0.206 NA        NA         NA     
      18   43.0     51.9    58.9    78.4   0.214 NA        NA         NA     
      19   41.4     48.1    55.8    74.5   0.205 NA        NA         NA     
      20   46.0     53.0    61.0    75.0   0.185 NA        NA         NA     
      21   NA       NA      NA      NA    NA      0.105     0.105      0.421 
      22   NA       NA      NA      NA    NA      0.133     0.133      0.421 
      23   NA       NA      NA      NA    NA      0.0974    0.0974     0.421 
      24   NA       NA      NA      NA    NA      0.0755    0.0755     0.421 
      25   NA       NA      NA      NA    NA      0.895     0.895      0.421 
      26   NA       NA      NA      NA    NA      0.867     0.867      0.421 
      27   NA       NA      NA      NA    NA      0.903     0.903      0.421 
      28   NA       NA      NA      NA    NA      0.925     0.925      0.421 
      29   NA       NA      NA      NA    NA      0.689     0.923      0.567 
      30   NA       NA      NA      NA    NA      0.911     0.911      0.567 
      31   NA       NA      NA      NA    NA      0.935     0.935      0.567 
      32   NA       NA      NA      NA    NA      0        NA          0.567 
      33   NA       NA      NA      NA    NA      0.0574    0.0769     0.567 
      34   NA       NA      NA      NA    NA      0.0886    0.0886     0.567 
      35   NA       NA      NA      NA    NA      0.0649    0.0649     0.567 
      36   NA       NA      NA      NA    NA      0        NA          0.567 
      37   NA       NA      NA      NA    NA      0.364     0.487      0.0882
      38   NA       NA      NA      NA    NA      0.538     0.538      0.0882
      39   NA       NA      NA      NA    NA      0.435     0.435      0.0882
      40   NA       NA      NA      NA    NA      0        NA          0.0882
      41   NA       NA      NA      NA    NA      0.383     0.513      0.0882
      42   NA       NA      NA      NA    NA      0.462     0.462      0.0882
      43   NA       NA      NA      NA    NA      0.565     0.565      0.0882
      44   NA       NA      NA      NA    NA      0        NA          0.0882
      45   NA       NA      NA      NA    NA      0.531     0.712      0.985 
      46   NA       NA      NA      NA    NA      0.715     0.715      0.985 
      47   NA       NA      NA      NA    NA      0.708     0.708      0.985 
      48   NA       NA      NA      NA    NA      0        NA          0.985 
      49   NA       NA      NA      NA    NA      0.215     0.288      0.985 
      50   NA       NA      NA      NA    NA      0.285     0.285      0.985 
      51   NA       NA      NA      NA    NA      0.292     0.292      0.985 
      52   NA       NA      NA      NA    NA      0        NA          0.985 
      53   NA       NA      NA      NA    NA      0.847     0.847      0.877 
      54   NA       NA      NA      NA    NA      0.835     0.835      0.877 
      55   NA       NA      NA      NA    NA      0.851     0.851      0.877 
      56   NA       NA      NA      NA    NA      0.858     0.858      0.877 
      57   NA       NA      NA      NA    NA      0.105     0.105      0.877 
      58   NA       NA      NA      NA    NA      0.101     0.101      0.877 
      59   NA       NA      NA      NA    NA      0.0844    0.0844     0.877 
      60   NA       NA      NA      NA    NA      0.142     0.142      0.877 
      61   NA       NA      NA      NA    NA      0.0478    0.0478     0.877 
      62   NA       NA      NA      NA    NA      0.0633    0.0633     0.877 
      63   NA       NA      NA      NA    NA      0.0649    0.0649     0.877 
      64   NA       NA      NA      NA    NA      0         0          0.877 
      65    0.8      1.4     3.4    28     1.37  NA        NA         NA     
      66    0.8      1.4     3.2    20     1.26  NA        NA         NA     
      67    0.725    1.3     3.6    28     1.45  NA        NA         NA     
      68    0.725    1.4     3.08   18     1.30  NA        NA         NA     
      69  250.     310.    400    1775     0.628 NA        NA         NA     
      70  248.     316.    417    1712     0.574 NA        NA         NA     
      71  254.     304.    377    1775     0.675 NA        NA         NA     
      72   NA       NA      NA      NA    NA     NA        NA         NA     
      73    3.24     3.53    3.77    4.64  0.122 NA        NA         NA     
      74    3.21     3.56    3.83    4.64  0.126 NA        NA         NA     
      75    3.34     3.54    3.78    4.38  0.112 NA        NA         NA     
      76    3.12     3.47    3.72    4.52  0.127 NA        NA         NA     
      77   NA       NA      NA      NA    NA      0.0502    0.0510     0.201 
      78   NA       NA      NA      NA    NA      0.0759    0.0759     0.201 
      79   NA       NA      NA      NA    NA      0.0260    0.0260     0.201 
      80   NA       NA      NA      NA    NA      0.0472    0.05       0.201 
      81   NA       NA      NA      NA    NA      0.220     0.223      0.201 
      82   NA       NA      NA      NA    NA      0.222     0.222      0.201 
      83   NA       NA      NA      NA    NA      0.208     0.208      0.201 
      84   NA       NA      NA      NA    NA      0.236     0.25       0.201 
      85   NA       NA      NA      NA    NA      0.371     0.376      0.201 
      86   NA       NA      NA      NA    NA      0.354     0.354      0.201 
      87   NA       NA      NA      NA    NA      0.416     0.416      0.201 
      88   NA       NA      NA      NA    NA      0.330     0.35       0.201 
      89   NA       NA      NA      NA    NA      0.344     0.350      0.201 
      90   NA       NA      NA      NA    NA      0.348     0.348      0.201 
      91   NA       NA      NA      NA    NA      0.351     0.351      0.201 
      92   NA       NA      NA      NA    NA      0.330     0.35       0.201 
      # i 17 more variables: chisq_test_no_correction <dbl>, chisq_test_simulated <dbl>, fisher_test <dbl>, fisher_test_simulated <dbl>, check_categorical_test <chr>, oneway_test_unequal_var <dbl>,
      #   oneway_test_equal_var <dbl>, kruskal_test <dbl>, bartlett_test <dbl>, levene_test <dbl>, smd <dbl>, shapiro_test <dbl>, ks_test <dbl>, ad_test <dbl>, class <chr>, var_type <chr>, label <chr>

# create_tidytableone is stable without strata (pbc_mayo)

    Code
      print(tab, n = Inf, width = 200)
    Output
      # A tibble: 23 x 40
         strata_var strata  var     level        n n_distinct complete missing n_level
         <chr>      <fct>   <fct>   <fct>    <int>      <int>    <int>   <int>   <int>
       1 <NA>       Overall time    <NA>       418        399      418       0      NA
       2 <NA>       Overall status  Alive       NA         NA       NA      NA     232
       3 <NA>       Overall status  Liver t~    NA         NA       NA      NA      25
       4 <NA>       Overall status  Dead        NA         NA       NA      NA     161
       5 <NA>       Overall age     <NA>       418        344      418       0      NA
       6 <NA>       Overall sex     Male        NA         NA       NA      NA      44
       7 <NA>       Overall sex     Female      NA         NA       NA      NA     374
       8 <NA>       Overall ascites No          NA         NA       NA      NA     288
       9 <NA>       Overall ascites Yes         NA         NA       NA      NA      24
      10 <NA>       Overall hepato  No          NA         NA       NA      NA     152
      11 <NA>       Overall hepato  Yes         NA         NA       NA      NA     160
      12 <NA>       Overall spiders No          NA         NA       NA      NA     222
      13 <NA>       Overall spiders Yes         NA         NA       NA      NA      90
      14 <NA>       Overall edema   No edem~    NA         NA       NA      NA     354
      15 <NA>       Overall edema   Edema p~    NA         NA       NA      NA      44
      16 <NA>       Overall edema   Edema d~    NA         NA       NA      NA      20
      17 <NA>       Overall bili    <NA>       418         98      418       0      NA
      18 <NA>       Overall chol    <NA>       418        202      284     134      NA
      19 <NA>       Overall albumin <NA>       418        154      418       0      NA
      20 <NA>       Overall stage   1           NA         NA       NA      NA      21
      21 <NA>       Overall stage   2           NA         NA       NA      NA      92
      22 <NA>       Overall stage   3           NA         NA       NA      NA     155
      23 <NA>       Overall stage   4           NA         NA       NA      NA     144
         n_strata n_level_valid n_strata_valid    mean       sd     p0     p25     p50
            <int>         <int>          <int>   <dbl>    <dbl>  <dbl>   <dbl>   <dbl>
       1       NA            NA             NA 1918.   1105.     41    1093.   1730   
       2      418           232            418   NA      NA      NA      NA      NA   
       3      418            25            418   NA      NA      NA      NA      NA   
       4      418           161            418   NA      NA      NA      NA      NA   
       5       NA            NA             NA   50.7    10.4    26.3    42.8    51.0 
       6      418            44            418   NA      NA      NA      NA      NA   
       7      418           374            418   NA      NA      NA      NA      NA   
       8      418           288            312   NA      NA      NA      NA      NA   
       9      418            24            312   NA      NA      NA      NA      NA   
      10      418           152            312   NA      NA      NA      NA      NA   
      11      418           160            312   NA      NA      NA      NA      NA   
      12      418           222            312   NA      NA      NA      NA      NA   
      13      418            90            312   NA      NA      NA      NA      NA   
      14      418           354            418   NA      NA      NA      NA      NA   
      15      418            44            418   NA      NA      NA      NA      NA   
      16      418            20            418   NA      NA      NA      NA      NA   
      17       NA            NA             NA    3.22    4.41    0.3     0.8     1.4 
      18       NA            NA             NA  370.    232.    120     250.    310.  
      19       NA            NA             NA    3.50    0.425   1.96    3.24    3.53
      20      418            21            412   NA      NA      NA      NA      NA   
      21      418            92            412   NA      NA      NA      NA      NA   
      22      418           155            412   NA      NA      NA      NA      NA   
      23      418           144            412   NA      NA      NA      NA      NA   
             p75    p100     cv     pct pct_valid chisq_test chisq_test_no_correction
           <dbl>   <dbl>  <dbl>   <dbl>     <dbl>      <dbl>                    <dbl>
       1 2614.   4795     0.576 NA        NA              NA                       NA
       2   NA      NA    NA      0.555     0.555          NA                       NA
       3   NA      NA    NA      0.0598    0.0598         NA                       NA
       4   NA      NA    NA      0.385     0.385          NA                       NA
       5   58.2    78.4   0.206 NA        NA              NA                       NA
       6   NA      NA    NA      0.105     0.105          NA                       NA
       7   NA      NA    NA      0.895     0.895          NA                       NA
       8   NA      NA    NA      0.689     0.923          NA                       NA
       9   NA      NA    NA      0.0574    0.0769         NA                       NA
      10   NA      NA    NA      0.364     0.487          NA                       NA
      11   NA      NA    NA      0.383     0.513          NA                       NA
      12   NA      NA    NA      0.531     0.712          NA                       NA
      13   NA      NA    NA      0.215     0.288          NA                       NA
      14   NA      NA    NA      0.847     0.847          NA                       NA
      15   NA      NA    NA      0.105     0.105          NA                       NA
      16   NA      NA    NA      0.0478    0.0478         NA                       NA
      17    3.4    28     1.37  NA        NA              NA                       NA
      18  400    1775     0.628 NA        NA              NA                       NA
      19    3.77    4.64  0.122 NA        NA              NA                       NA
      20   NA      NA    NA      0.0502    0.0510         NA                       NA
      21   NA      NA    NA      0.220     0.223          NA                       NA
      22   NA      NA    NA      0.371     0.376          NA                       NA
      23   NA      NA    NA      0.344     0.350          NA                       NA
      # i 16 more variables: chisq_test_simulated <dbl>, fisher_test <dbl>, fisher_test_simulated <dbl>, check_categorical_test <chr>, oneway_test_unequal_var <dbl>, oneway_test_equal_var <dbl>,
      #   kruskal_test <dbl>, bartlett_test <dbl>, levene_test <dbl>, smd <dbl>, shapiro_test <dbl>, ks_test <dbl>, ad_test <dbl>, class <chr>, var_type <chr>, label <chr>

# create_tidytableone is stable with checkbox + strata

    Code
      print(tab, n = Inf, width = 200)
    Output
      # A tibble: 21 x 41
         strata_var strata  var       level                      n n_distinct complete
         <chr>      <fct>   <fct>     <fct>                  <int>      <int>    <int>
       1 group      Overall age       <NA>                      60         58       60
       2 group      A       age       <NA>                      29         28       29
       3 group      B       age       <NA>                      31         30       31
       4 group      Overall sex       Female                    NA         NA       NA
       5 group      A       sex       Female                    NA         NA       NA
       6 group      B       sex       Female                    NA         NA       NA
       7 group      Overall sex       Male                      NA         NA       NA
       8 group      A       sex       Male                      NA         NA       NA
       9 group      B       sex       Male                      NA         NA       NA
      10 group      Overall race___1  White                     NA         NA       NA
      11 group      A       race___1  White                     NA         NA       NA
      12 group      B       race___1  White                     NA         NA       NA
      13 group      Overall race___2  Black or African-Amer~    NA         NA       NA
      14 group      A       race___2  Black or African-Amer~    NA         NA       NA
      15 group      B       race___2  Black or African-Amer~    NA         NA       NA
      16 group      Overall race___3  American Indian or Al~    NA         NA       NA
      17 group      A       race___3  American Indian or Al~    NA         NA       NA
      18 group      B       race___3  American Indian or Al~    NA         NA       NA
      19 group      Overall race___98 Prefer not to answer      NA         NA       NA
      20 group      A       race___98 Prefer not to answer      NA         NA       NA
      21 group      B       race___98 Prefer not to answer      NA         NA       NA
         missing n_level n_strata n_level_valid n_strata_valid  mean    sd    p0   p25
           <int>   <int>    <int>         <int>          <int> <dbl> <dbl> <dbl> <dbl>
       1       0      NA       NA            NA             NA  48.8  11.6  28    40.0
       2       0      NA       NA            NA             NA  46.2  11.2  28    37.9
       3       0      NA       NA            NA             NA  51.3  11.6  34.5  43.4
       4      NA      24       60            24             60  NA    NA    NA    NA  
       5      NA      14       29            14             29  NA    NA    NA    NA  
       6      NA      10       31            10             31  NA    NA    NA    NA  
       7      NA      36       60            36             60  NA    NA    NA    NA  
       8      NA      15       29            15             29  NA    NA    NA    NA  
       9      NA      21       31            21             31  NA    NA    NA    NA  
      10      NA      34       60            34             60  NA    NA    NA    NA  
      11      NA      16       29            16             29  NA    NA    NA    NA  
      12      NA      18       31            18             31  NA    NA    NA    NA  
      13      NA      14       60            14             60  NA    NA    NA    NA  
      14      NA       6       29             6             29  NA    NA    NA    NA  
      15      NA       8       31             8             31  NA    NA    NA    NA  
      16      NA       5       60             5             60  NA    NA    NA    NA  
      17      NA       1       29             1             29  NA    NA    NA    NA  
      18      NA       4       31             4             31  NA    NA    NA    NA  
      19      NA       3       60             3             60  NA    NA    NA    NA  
      20      NA       2       29             2             29  NA    NA    NA    NA  
      21      NA       1       31             1             31  NA    NA    NA    NA  
           p50   p75  p100     cv     pct pct_valid chisq_test chisq_test_no_correct~1
         <dbl> <dbl> <dbl>  <dbl>   <dbl>     <dbl>      <dbl>                   <dbl>
       1  48.8  56.4  85.5  0.238 NA        NA          NA                      NA    
       2  45.3  54.1  67.5  0.243 NA        NA          NA                      NA    
       3  50.7  58.0  85.5  0.227 NA        NA          NA                      NA    
       4  NA    NA    NA   NA      0.4       0.4         0.316                   0.206
       5  NA    NA    NA   NA      0.483     0.483       0.316                   0.206
       6  NA    NA    NA   NA      0.323     0.323       0.316                   0.206
       7  NA    NA    NA   NA      0.6       0.6         0.316                   0.206
       8  NA    NA    NA   NA      0.517     0.517       0.316                   0.206
       9  NA    NA    NA   NA      0.677     0.677       0.316                   0.206
      10  NA    NA    NA   NA      0.567     0.567       1                       0.821
      11  NA    NA    NA   NA      0.552     0.552       1                       0.821
      12  NA    NA    NA   NA      0.581     0.581       1                       0.821
      13  NA    NA    NA   NA      0.233     0.233       0.871                   0.640
      14  NA    NA    NA   NA      0.207     0.207       0.871                   0.640
      15  NA    NA    NA   NA      0.258     0.258       0.871                   0.640
      16  NA    NA    NA   NA      0.0833    0.0833      0.392                   0.185
      17  NA    NA    NA   NA      0.0345    0.0345      0.392                   0.185
      18  NA    NA    NA   NA      0.129     0.129       0.392                   0.185
      19  NA    NA    NA   NA      0.05      0.05        0.953                   0.514
      20  NA    NA    NA   NA      0.0690    0.0690      0.953                   0.514
      21  NA    NA    NA   NA      0.0323    0.0323      0.953                   0.514
      # i abbreviated name: 1: chisq_test_no_correction
      # i 17 more variables: chisq_test_simulated <dbl>, fisher_test <dbl>, fisher_test_simulated <dbl>, check_categorical_test <chr>, oneway_test_unequal_var <dbl>, oneway_test_equal_var <dbl>,
      #   kruskal_test <dbl>, bartlett_test <dbl>, levene_test <dbl>, smd <dbl>, shapiro_test <dbl>, ks_test <dbl>, ad_test <dbl>, p_value_level <dbl>, class <chr>, var_type <chr>, label <chr>

# create_tidytableone is stable with checkbox, no strata

    Code
      print(tab, n = Inf, width = 200)
    Output
      # A tibble: 7 x 40
        strata_var strata  var       level                       n n_distinct complete
        <chr>      <fct>   <fct>     <fct>                   <int>      <int>    <int>
      1 <NA>       Overall age       <NA>                       60         58       60
      2 <NA>       Overall sex       Female                     NA         NA       NA
      3 <NA>       Overall sex       Male                       NA         NA       NA
      4 <NA>       Overall race___1  White                      NA         NA       NA
      5 <NA>       Overall race___2  Black or African-Ameri~    NA         NA       NA
      6 <NA>       Overall race___3  American Indian or Ala~    NA         NA       NA
      7 <NA>       Overall race___98 Prefer not to answer       NA         NA       NA
        missing n_level n_strata n_level_valid n_strata_valid  mean    sd    p0   p25
          <int>   <int>    <int>         <int>          <int> <dbl> <dbl> <dbl> <dbl>
      1       0      NA       NA            NA             NA  48.8  11.6    28  40.0
      2      NA      24       60            24             60  NA    NA      NA  NA  
      3      NA      36       60            36             60  NA    NA      NA  NA  
      4      NA      34       60            34             60  NA    NA      NA  NA  
      5      NA      14       60            14             60  NA    NA      NA  NA  
      6      NA       5       60             5             60  NA    NA      NA  NA  
      7      NA       3       60             3             60  NA    NA      NA  NA  
          p50   p75  p100     cv     pct pct_valid chisq_test chisq_test_no_correction
        <dbl> <dbl> <dbl>  <dbl>   <dbl>     <dbl>      <dbl>                    <dbl>
      1  48.8  56.4  85.5  0.238 NA        NA              NA                       NA
      2  NA    NA    NA   NA      0.4       0.4            NA                       NA
      3  NA    NA    NA   NA      0.6       0.6            NA                       NA
      4  NA    NA    NA   NA      0.567     0.567          NA                       NA
      5  NA    NA    NA   NA      0.233     0.233          NA                       NA
      6  NA    NA    NA   NA      0.0833    0.0833         NA                       NA
      7  NA    NA    NA   NA      0.05      0.05           NA                       NA
      # i 16 more variables: chisq_test_simulated <dbl>, fisher_test <dbl>, fisher_test_simulated <dbl>, check_categorical_test <chr>, oneway_test_unequal_var <dbl>, oneway_test_equal_var <dbl>,
      #   kruskal_test <dbl>, bartlett_test <dbl>, levene_test <dbl>, smd <dbl>, shapiro_test <dbl>, ks_test <dbl>, ad_test <dbl>, class <chr>, var_type <chr>, label <chr>

# create_tidytableone is stable with multi-block mixed-name checkboxes

    Code
      print(tab, n = Inf, width = 200)
    Output
      # A tibble: 24 x 41
         strata_var strata   var      level                      n n_distinct complete
         <chr>      <fct>    <fct>    <fct>                  <int>      <int>    <int>
       1 group      Overall  age      <NA>                      80         72       80
       2 group      No recur age      <NA>                      39         37       39
       3 group      Recur    age      <NA>                      41         38       41
       4 group      Overall  sex      Female                    NA         NA       NA
       5 group      No recur sex      Female                    NA         NA       NA
       6 group      Recur    sex      Female                    NA         NA       NA
       7 group      Overall  sex      Male                      NA         NA       NA
       8 group      No recur sex      Male                      NA         NA       NA
       9 group      Recur    sex      Male                      NA         NA       NA
      10 group      Overall  rheum_dx Rheumatologic disease     NA         NA       NA
      11 group      No recur rheum_dx Rheumatologic disease     NA         NA       NA
      12 group      Recur    rheum_dx Rheumatologic disease     NA         NA       NA
      13 group      Overall  heme_dx  Hematologic malignancy    NA         NA       NA
      14 group      No recur heme_dx  Hematologic malignancy    NA         NA       NA
      15 group      Recur    heme_dx  Hematologic malignancy    NA         NA       NA
      16 group      Overall  solid_ca Solid cancer              NA         NA       NA
      17 group      No recur solid_ca Solid cancer              NA         NA       NA
      18 group      Recur    solid_ca Solid cancer              NA         NA       NA
      19 group      Overall  ckd      Chronic kidney disease    NA         NA       NA
      20 group      No recur ckd      Chronic kidney disease    NA         NA       NA
      21 group      Recur    ckd      Chronic kidney disease    NA         NA       NA
      22 group      Overall  chf      Congestive heart fail~    NA         NA       NA
      23 group      No recur chf      Congestive heart fail~    NA         NA       NA
      24 group      Recur    chf      Congestive heart fail~    NA         NA       NA
         missing n_level n_strata n_level_valid n_strata_valid  mean    sd    p0   p25
           <int>   <int>    <int>         <int>          <int> <dbl> <dbl> <dbl> <dbl>
       1       0      NA       NA            NA             NA  52.9  15.4  15.9  42.7
       2       0      NA       NA            NA             NA  50.6  16.4  15.9  41.6
       3       0      NA       NA            NA             NA  55.1  14.2  23.4  48.9
       4      NA      37       80            37             80  NA    NA    NA    NA  
       5      NA      14       39            14             39  NA    NA    NA    NA  
       6      NA      23       41            23             41  NA    NA    NA    NA  
       7      NA      43       80            43             80  NA    NA    NA    NA  
       8      NA      25       39            25             39  NA    NA    NA    NA  
       9      NA      18       41            18             41  NA    NA    NA    NA  
      10      NA       8       80             8             80  NA    NA    NA    NA  
      11      NA       5       39             5             39  NA    NA    NA    NA  
      12      NA       3       41             3             41  NA    NA    NA    NA  
      13      NA       4       80             4             80  NA    NA    NA    NA  
      14      NA       3       39             3             39  NA    NA    NA    NA  
      15      NA       1       41             1             41  NA    NA    NA    NA  
      16      NA       6       80             6             80  NA    NA    NA    NA  
      17      NA       3       39             3             39  NA    NA    NA    NA  
      18      NA       3       41             3             41  NA    NA    NA    NA  
      19      NA      16       80            16             80  NA    NA    NA    NA  
      20      NA       9       39             9             39  NA    NA    NA    NA  
      21      NA       7       41             7             41  NA    NA    NA    NA  
      22      NA      11       80            11             80  NA    NA    NA    NA  
      23      NA       4       39             4             39  NA    NA    NA    NA  
      24      NA       7       41             7             41  NA    NA    NA    NA  
           p50   p75  p100     cv     pct pct_valid chisq_test chisq_test_no_correct~1
         <dbl> <dbl> <dbl>  <dbl>   <dbl>     <dbl>      <dbl>                   <dbl>
       1  53.4  62.8  86.5  0.291 NA        NA          NA                     NA     
       2  51.8  58.7  86.5  0.324 NA        NA          NA                     NA     
       3  58.4  63.4  84.5  0.258 NA        NA          NA                     NA     
       4  NA    NA    NA   NA      0.462     0.462       0.113                  0.0701
       5  NA    NA    NA   NA      0.359     0.359       0.113                  0.0701
       6  NA    NA    NA   NA      0.561     0.561       0.113                  0.0701
       7  NA    NA    NA   NA      0.538     0.538       0.113                  0.0701
       8  NA    NA    NA   NA      0.641     0.641       0.113                  0.0701
       9  NA    NA    NA   NA      0.439     0.439       0.113                  0.0701
      10  NA    NA    NA   NA      0.1       0.1         0.655                  0.412 
      11  NA    NA    NA   NA      0.128     0.128       0.655                  0.412 
      12  NA    NA    NA   NA      0.0732    0.0732      0.655                  0.412 
      13  NA    NA    NA   NA      0.05      0.05        0.572                  0.281 
      14  NA    NA    NA   NA      0.0769    0.0769      0.572                  0.281 
      15  NA    NA    NA   NA      0.0244    0.0244      0.572                  0.281 
      16  NA    NA    NA   NA      0.075     0.075       1                      0.949 
      17  NA    NA    NA   NA      0.0769    0.0769      1                      0.949 
      18  NA    NA    NA   NA      0.0732    0.0732      1                      0.949 
      19  NA    NA    NA   NA      0.2       0.2         0.695                  0.502 
      20  NA    NA    NA   NA      0.231     0.231       0.695                  0.502 
      21  NA    NA    NA   NA      0.171     0.171       0.695                  0.502 
      22  NA    NA    NA   NA      0.138     0.138       0.575                  0.376 
      23  NA    NA    NA   NA      0.103     0.103       0.575                  0.376 
      24  NA    NA    NA   NA      0.171     0.171       0.575                  0.376 
      # i abbreviated name: 1: chisq_test_no_correction
      # i 17 more variables: chisq_test_simulated <dbl>, fisher_test <dbl>, fisher_test_simulated <dbl>, check_categorical_test <chr>, oneway_test_unequal_var <dbl>, oneway_test_equal_var <dbl>,
      #   kruskal_test <dbl>, bartlett_test <dbl>, levene_test <dbl>, smd <dbl>, shapiro_test <dbl>, ks_test <dbl>, ad_test <dbl>, p_value_level <dbl>, class <chr>, var_type <chr>, label <chr>

# create_tidytableone schema is stable (strata)

    Code
      cat("Columns:\n")
    Output
      Columns:
    Code
      print(names(tab))
    Output
       [1] "strata_var"               "strata"                  
       [3] "var"                      "level"                   
       [5] "n"                        "n_distinct"              
       [7] "complete"                 "missing"                 
       [9] "n_level"                  "n_strata"                
      [11] "n_level_valid"            "n_strata_valid"          
      [13] "mean"                     "sd"                      
      [15] "p0"                       "p25"                     
      [17] "p50"                      "p75"                     
      [19] "p100"                     "cv"                      
      [21] "pct"                      "pct_valid"               
      [23] "chisq_test"               "chisq_test_no_correction"
      [25] "chisq_test_simulated"     "fisher_test"             
      [27] "fisher_test_simulated"    "check_categorical_test"  
      [29] "oneway_test_unequal_var"  "oneway_test_equal_var"   
      [31] "kruskal_test"             "bartlett_test"           
      [33] "levene_test"              "smd"                     
      [35] "shapiro_test"             "ks_test"                 
      [37] "ad_test"                  "class"                   
      [39] "var_type"                 "label"                   
    Code
      cat("\nTypes:\n")
    Output
      
      Types:
    Code
      print(vapply(tab, function(x) paste(class(x), collapse = "/"), character(1)))
    Output
                    strata_var                   strata                      var 
                   "character"                 "factor"                 "factor" 
                         level                        n               n_distinct 
                      "factor"                "integer"                "integer" 
                      complete                  missing                  n_level 
                     "integer"                "integer"                "integer" 
                      n_strata            n_level_valid           n_strata_valid 
                     "integer"                "integer"                "integer" 
                          mean                       sd                       p0 
                     "numeric"                "numeric"                "numeric" 
                           p25                      p50                      p75 
                     "numeric"                "numeric"                "numeric" 
                          p100                       cv                      pct 
                     "numeric"                "numeric"                "numeric" 
                     pct_valid               chisq_test chisq_test_no_correction 
                     "numeric"                "numeric"                "numeric" 
          chisq_test_simulated              fisher_test    fisher_test_simulated 
                     "numeric"                "numeric"                "numeric" 
        check_categorical_test  oneway_test_unequal_var    oneway_test_equal_var 
                   "character"                "numeric"                "numeric" 
                  kruskal_test            bartlett_test              levene_test 
                     "numeric"                "numeric"                "numeric" 
                           smd             shapiro_test                  ks_test 
                     "numeric"                "numeric"                "numeric" 
                       ad_test                    class                 var_type 
                     "numeric"              "character"              "character" 
                         label 
                   "character" 

# create_tidytableone schema is stable (no strata)

    Code
      cat("Columns:\n")
    Output
      Columns:
    Code
      print(names(tab))
    Output
       [1] "strata_var"               "strata"                  
       [3] "var"                      "level"                   
       [5] "n"                        "n_distinct"              
       [7] "complete"                 "missing"                 
       [9] "n_level"                  "n_strata"                
      [11] "n_level_valid"            "n_strata_valid"          
      [13] "mean"                     "sd"                      
      [15] "p0"                       "p25"                     
      [17] "p50"                      "p75"                     
      [19] "p100"                     "cv"                      
      [21] "pct"                      "pct_valid"               
      [23] "chisq_test"               "chisq_test_no_correction"
      [25] "chisq_test_simulated"     "fisher_test"             
      [27] "fisher_test_simulated"    "check_categorical_test"  
      [29] "oneway_test_unequal_var"  "oneway_test_equal_var"   
      [31] "kruskal_test"             "bartlett_test"           
      [33] "levene_test"              "smd"                     
      [35] "shapiro_test"             "ks_test"                 
      [37] "ad_test"                  "class"                   
      [39] "var_type"                 "label"                   
    Code
      cat("\nTypes:\n")
    Output
      
      Types:
    Code
      print(vapply(tab, function(x) paste(class(x), collapse = "/"), character(1)))
    Output
                    strata_var                   strata                      var 
                   "character"                 "factor"                 "factor" 
                         level                        n               n_distinct 
                      "factor"                "integer"                "integer" 
                      complete                  missing                  n_level 
                     "integer"                "integer"                "integer" 
                      n_strata            n_level_valid           n_strata_valid 
                     "integer"                "integer"                "integer" 
                          mean                       sd                       p0 
                     "numeric"                "numeric"                "numeric" 
                           p25                      p50                      p75 
                     "numeric"                "numeric"                "numeric" 
                          p100                       cv                      pct 
                     "numeric"                "numeric"                "numeric" 
                     pct_valid               chisq_test chisq_test_no_correction 
                     "numeric"                "numeric"                "numeric" 
          chisq_test_simulated              fisher_test    fisher_test_simulated 
                     "numeric"                "numeric"                "numeric" 
        check_categorical_test  oneway_test_unequal_var    oneway_test_equal_var 
                   "character"                "numeric"                "numeric" 
                  kruskal_test            bartlett_test              levene_test 
                     "numeric"                "numeric"                "numeric" 
                           smd             shapiro_test                  ks_test 
                     "numeric"                "numeric"                "numeric" 
                       ad_test                    class                 var_type 
                     "numeric"              "character"              "character" 
                         label 
                   "character" 

