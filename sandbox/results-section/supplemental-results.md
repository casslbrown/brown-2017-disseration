# Supplemental results
Date: `r Sys.Date()`  
<!-- These two chunks should be added in the beginning of every .Rmd that you want to source an .R script -->
<!--  The 1st mandatory chunck  -->
<!--  Set the working directory to the repository's base directory -->



<!--  The 2nd mandatory chunck  -->
<!-- Set the report-wide options, and point to the external code file. -->


<!-- Load 'sourced' R files.  Suppress the output when loading packages. --> 



<!-- Load the sources.  Suppress the output when loading sources. --> 


# Immediate Word Recall
## Immediate Word Recall Model Fit Indices


## Immediate Word Recall LCM Parameters

```
    Parameter    Est    SE p_value
1     MeansIA  5.467 0.015       0
2     MeansSA -0.083 0.002       0
3 VariancesIA  1.295 0.034       0
4 VariancesSA  0.005 0.001       0
```

## Immediate Word Recall Unconditional ATL Model

```
          paramHeader param    est    se  est_se    pval
1                IA.|  A_01  1.000 0.000 999.000 999.000
2                IA.|  A_02  1.000 0.000 999.000 999.000
3                IA.|  A_03  1.000 0.000 999.000 999.000
4                IA.|  A_04  1.000 0.000 999.000 999.000
5                IA.|  A_05  1.000 0.000 999.000 999.000
6                IA.|  A_06  1.000 0.000 999.000 999.000
7                SA.|  A_01  0.000 0.000 999.000 999.000
8                SA.|  A_02  2.000 0.000 999.000 999.000
9                SA.|  A_03  4.000 0.000 999.000 999.000
10               SA.|  A_04  6.000 0.000 999.000 999.000
11               SA.|  A_05  8.000 0.000 999.000 999.000
12               SA.|  A_06 10.000 0.000 999.000 999.000
13            A_02.ON  A_01  0.005 0.005   1.053   0.292
14            A_03.ON  A_02  0.008 0.007   1.079   0.281
15            A_04.ON  A_03 -0.023 0.010  -2.245   0.025
16            A_05.ON  A_04 -0.025 0.015  -1.709   0.087
17            A_06.ON  A_05 -0.033 0.019  -1.738   0.082
18            SA.WITH    IA  0.007 0.004   1.877   0.061
19              Means    IA  5.438 0.020 268.008   0.000
20              Means    SA -0.081 0.009  -8.823   0.000
21         Intercepts  A_01  0.000 0.000 999.000 999.000
22         Intercepts  A_02  0.000 0.000 999.000 999.000
23         Intercepts  A_03  0.000 0.000 999.000 999.000
24         Intercepts  A_04  0.000 0.000 999.000 999.000
25         Intercepts  A_05  0.000 0.000 999.000 999.000
26         Intercepts  A_06  0.000 0.000 999.000 999.000
27          Variances    IA  1.042 0.036  28.757   0.000
28          Variances    SA  0.004 0.001   5.117   0.000
29 Residual.Variances  A_01  1.203 0.041  29.709   0.000
30 Residual.Variances  A_02  1.266 0.035  35.764   0.000
31 Residual.Variances  A_03  1.267 0.033  38.897   0.000
32 Residual.Variances  A_04  1.424 0.044  32.139   0.000
33 Residual.Variances  A_05  1.304 0.049  26.608   0.000
34 Residual.Variances  A_06  1.313 0.049  26.724   0.000
```


# Delayed Word Recall
## Delayed Word Recall Model Fit Indices

```
                        Title Observations ChiSqM_Value ChiSqM_DF ChiSqM_ScalingCorrection CM          TRd df_diff
1  Autoregressive, univariate        11556     3750.568        10                   1.2284  -           NA      NA
2                         LGM        11556      186.355        16                   1.1095  -           NA      NA
3              LGM, quadratic        11556      184.980        15                   1.1157  -           NA      NA
4             ALT, full model        11556      129.719        11                   1.1064  3  -55.0796998      -4
5          LGM, nested in ALT        11556      186.355        16                   1.1095  3    0.3725396       1
6      ALT, no slope variance        11556      273.213        13                   1.1264  3   96.8933109      -2
7               ALT, no slope        11556     1222.846        14                   1.1593  3 2397.1169242      -1
8      ALT, fixed regressions        11556      185.800        15                   1.1112  3    0.0000000       0
    CFI   TLI RMSEA_Estimate  SRMR               Filename
1 0.714 0.572          0.180 0.237 u01_nocov_wrectotd.out
2 0.987 0.988          0.030 0.016 u02_nocov_wrectotd.out
3 0.987 0.987          0.031 0.016 u03_nocov_wrectotd.out
4 0.991 0.988          0.031 0.016 u04_nocov_wrectotd.out
5 0.987 0.988          0.030 0.016 u05_nocov_wrectotd.out
6 0.980 0.977          0.042 0.025 u06_nocov_wrectotd.out
7 0.908 0.901          0.086 0.052 u07_nocov_wrectotd.out
8 0.987 0.987          0.031 0.016 u08_nocov_wrectotd.out
```

## Delayed Word Recall LCM Parameters

```
    Parameter    Est    SE p_value
1     MeansIA  4.419 0.019       0
2     MeansSA -0.092 0.002       0
3 VariancesIA  2.055 0.051       0
4 VariancesSA  0.009 0.001       0
```

## Delayed Word Recall Unconditional ATL Model

```
          paramHeader param     est    se  est_se    pval
1                IA.|  A_01   1.000 0.000 999.000 999.000
2                IA.|  A_02   1.000 0.000 999.000 999.000
3                IA.|  A_03   1.000 0.000 999.000 999.000
4                IA.|  A_04   1.000 0.000 999.000 999.000
5                IA.|  A_05   1.000 0.000 999.000 999.000
6                IA.|  A_06   1.000 0.000 999.000 999.000
7                SA.|  A_01   0.000 0.000 999.000 999.000
8                SA.|  A_02   2.000 0.000 999.000 999.000
9                SA.|  A_03   4.000 0.000 999.000 999.000
10               SA.|  A_04   6.000 0.000 999.000 999.000
11               SA.|  A_05   8.000 0.000 999.000 999.000
12               SA.|  A_06  10.000 0.000 999.000 999.000
13               QA.|  A_01   0.000 0.000 999.000 999.000
14               QA.|  A_02   4.000 0.000 999.000 999.000
15               QA.|  A_03  16.000 0.000 999.000 999.000
16               QA.|  A_04  36.000 0.000 999.000 999.000
17               QA.|  A_05  64.000 0.000 999.000 999.000
18               QA.|  A_06 100.000 0.000 999.000 999.000
19            SA.WITH    IA  -0.007 0.005  -1.223   0.221
20              Means    IA   4.424 0.021 212.587   0.000
21              Means    SA  -0.096 0.007 -14.100   0.000
22              Means    QA   0.000 0.001   0.596   0.551
23         Intercepts  A_01   0.000 0.000 999.000 999.000
24         Intercepts  A_02   0.000 0.000 999.000 999.000
25         Intercepts  A_03   0.000 0.000 999.000 999.000
26         Intercepts  A_04   0.000 0.000 999.000 999.000
27         Intercepts  A_05   0.000 0.000 999.000 999.000
28         Intercepts  A_06   0.000 0.000 999.000 999.000
29          Variances    IA   2.058 0.051  40.021   0.000
30          Variances    SA   0.009 0.001  10.302   0.000
31          Variances    QA   0.000 0.000 999.000 999.000
32 Residual.Variances  A_01   1.687 0.048  35.286   0.000
33 Residual.Variances  A_02   1.850 0.042  44.320   0.000
34 Residual.Variances  A_03   1.904 0.040  48.115   0.000
35 Residual.Variances  A_04   1.908 0.041  46.431   0.000
36 Residual.Variances  A_05   1.923 0.045  42.919   0.000
37 Residual.Variances  A_06   1.735 0.050  34.768   0.000
```

# Mental Status
## Mental Status Model Fit Indices

```
                        Title Observations ChiSqM_Value ChiSqM_DF ChiSqM_ScalingCorrection CM        TRd df_diff   CFI
1  Autoregressive, univariate         9470     1434.986        10                   1.5943  -         NA      NA 0.815
2                         LGM         9470      495.892        16                   1.5963  -         NA      NA 0.938
3              LGM, quadratic         9470      467.512        15                   1.6251  -         NA      NA 0.941
4             ALT, full model         9470       91.786        11                   1.7539  3  -471.1388      -4 0.990
5          LGM, nested in ALT         9470      495.892        16                   1.5963  2        NaN       0 0.938
6      ALT, no slope variance         9470      187.146        13                   1.7355  3  -479.2968      -2 0.977
7               ALT, no slope         9470     1323.689        14                   1.7996  3 -1983.5640      -1 0.830
8      ALT, fixed regressions         9470      387.547        15                   1.6242  3     0.0000       0 0.952
    TLI RMSEA_Estimate  SRMR                       Filename
1 0.723          0.123 0.172 u01_nocov_mentalstatus_tot.out
2 0.942          0.056 0.034 u02_nocov_mentalstatus_tot.out
3 0.941          0.056 0.034 u03_nocov_mentalstatus_tot.out
4 0.986          0.028 0.026 u04_nocov_mentalstatus_tot.out
5 0.942          0.056 0.034 u05_nocov_mentalstatus_tot.out
6 0.974          0.038 0.034 u06_nocov_mentalstatus_tot.out
7 0.818          0.099 0.105 u07_nocov_mentalstatus_tot.out
8 0.952          0.051 0.034 u08_nocov_mentalstatus_tot.out
```

## Mental Status LCM Parameters

```
    Parameter    Est    SE p_value
1     MeansIA  8.526 0.009   0.000
2     MeansSA -0.059 0.004   0.000
3     MeansQA -0.001 0.000   0.001
4 VariancesIA  0.434 0.023   0.000
5 VariancesSA  0.012 0.001   0.000
```

## Mental Status Unconditional ATL Model

```
          paramHeader    param    est    se  est_se pval
1                 I.| MENT2004  1.000 0.000 999.000  999
2                 I.| MENT2006  1.000 0.000 999.000  999
3                 I.| MENT2008  1.000 0.000 999.000  999
4                 I.| MENT2010  1.000 0.000 999.000  999
5                 I.| MENT2012  1.000 0.000 999.000  999
6                 I.| MENT2014  1.000 0.000 999.000  999
7                 S.| MENT2004  0.000 0.000 999.000  999
8                 S.| MENT2006  2.000 0.000 999.000  999
9                 S.| MENT2008  4.000 0.000 999.000  999
10                S.| MENT2010  6.000 0.000 999.000  999
11                S.| MENT2012  8.000 0.000 999.000  999
12                S.| MENT2014 10.000 0.000 999.000  999
13        MENT2006.ON MENT2004  0.043 0.005   7.769    0
14        MENT2008.ON MENT2006  0.085 0.011   7.679    0
15        MENT2010.ON MENT2008  0.097 0.017   5.848    0
16        MENT2012.ON MENT2010  0.153 0.023   6.582    0
17        MENT2014.ON MENT2012  0.193 0.029   6.597    0
18             S.WITH        I -0.016 0.004  -4.161    0
19              Means        I  8.364 0.010 848.723    0
20              Means        S -0.216 0.023  -9.470    0
21         Intercepts MENT2004  0.000 0.000 999.000  999
22         Intercepts MENT2006  0.000 0.000 999.000  999
23         Intercepts MENT2008  0.000 0.000 999.000  999
24         Intercepts MENT2010  0.000 0.000 999.000  999
25         Intercepts MENT2012  0.000 0.000 999.000  999
26         Intercepts MENT2014  0.000 0.000 999.000  999
27          Variances        I  0.776 0.033  23.758    0
28          Variances        S  0.009 0.001  10.639    0
29 Residual.Variances MENT2004  0.431 0.020  21.078    0
30 Residual.Variances MENT2006  0.569 0.023  24.436    0
31 Residual.Variances MENT2008  0.677 0.025  27.042    0
32 Residual.Variances MENT2010  0.725 0.028  25.800    0
33 Residual.Variances MENT2012  0.681 0.031  22.180    0
34 Residual.Variances MENT2014  0.667 0.033  20.511    0
```

