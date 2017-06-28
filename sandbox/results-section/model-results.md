# Results
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

```
                                         Title ChiSqM_Value ChiSqM_DF   CFI   TLI RMSEA_Estimate  SRMR
1                       ATL, nested LCM model      3246.864        17 0.803 0.826          0.112 0.151
2                Initial univariate ATL model        83.787        11 0.996 0.994          0.021 0.015
3                Initial univariate ATL model     16416.903        15 0.000 0.000          0.269 0.410
4                LCM, quadratic unconditional       152.050        15 0.989 0.989          0.025 0.014
5 LCM, quadratic unconditional, word list tvc      3154.169        42 0.840 0.805          0.070 0.182
6                          LCM, unconditional       153.056        16 0.989 0.990          0.024 0.014
7                LCM, unconditional, list tvc      2851.835        46 0.834 0.816          0.063 0.195
8          Unconditional autoregressive model      4602.230        10 0.720 0.580          0.174 0.231
```

## Immediate Word Recall LCM Parameters

```
           Parameter    Est    SE p_value
1          Intercept  5.252 0.014       0
2              Slope -0.070 0.002       0
3 Intercept Variance  1.631 0.037       0
4     Slope Variance  0.006 0.001       0
```

## Immediate Word Recall Unconditional ATL Model

```
          paramHeader   param    est    se  est_se    pval
1                 I.| WRI2004  1.000 0.000 999.000 999.000
2                 I.| WRI2006  1.000 0.000 999.000 999.000
3                 I.| WRI2008  1.000 0.000 999.000 999.000
4                 I.| WRI2010  1.000 0.000 999.000 999.000
5                 I.| WRI2012  1.000 0.000 999.000 999.000
6                 I.| WRI2014  1.000 0.000 999.000 999.000
7                 S.| WRI2004  0.000 0.000 999.000 999.000
8                 S.| WRI2006  2.000 0.000 999.000 999.000
9                 S.| WRI2008  4.000 0.000 999.000 999.000
10                S.| WRI2010  6.000 0.000 999.000 999.000
11                S.| WRI2012  8.000 0.000 999.000 999.000
12                S.| WRI2014 10.000 0.000 999.000 999.000
13         WRI2006.ON WRI2004 -0.003 0.004  -0.852   0.394
14         WRI2008.ON WRI2006 -0.007 0.005  -1.209   0.227
15         WRI2010.ON WRI2008 -0.039 0.008  -4.980   0.000
16         WRI2012.ON WRI2010 -0.053 0.011  -4.857   0.000
17         WRI2014.ON WRI2012 -0.053 0.014  -3.679   0.000
18             S.WITH       I -0.001 0.004  -0.213   0.832
19              Means       I  5.228 0.016 317.071   0.000
20              Means       S -0.041 0.007  -5.899   0.000
21         Intercepts WRI2004  0.000 0.000 999.000 999.000
22         Intercepts WRI2006  0.000 0.000 999.000 999.000
23         Intercepts WRI2008  0.000 0.000 999.000 999.000
24         Intercepts WRI2010  0.000 0.000 999.000 999.000
25         Intercepts WRI2012  0.000 0.000 999.000 999.000
26         Intercepts WRI2014  0.000 0.000 999.000 999.000
27          Variances       I  1.618 0.038  42.845   0.000
28          Variances       S  0.007 0.001   9.175   0.000
29 Residual.Variances WRI2004  1.233 0.030  40.782   0.000
30 Residual.Variances WRI2006  1.338 0.025  53.526   0.000
31 Residual.Variances WRI2008  1.347 0.024  55.652   0.000
32 Residual.Variances WRI2010  1.430 0.027  52.615   0.000
33 Residual.Variances WRI2012  1.436 0.033  43.691   0.000
34 Residual.Variances WRI2014  1.363 0.035  38.680   0.000
```

# Delayed Word Recall
## Delayed Word Recall Model Fit Indices

```
                                                  Title ChiSqM_Value ChiSqM_DF   CFI   TLI RMSEA_Estimate  SRMR
1                                    LCM Unconditional       194.639        16 0.987 0.988          0.027 0.015
2 linear growth model for loneliness, word list as tvc      2938.911        46 0.852 0.836          0.065 0.180
3                              Unconditional ATL model       141.203        11 0.993 0.990          0.028 0.019
```

## Delayed Word Recall LCM Parameters

```
           Parameter    Est    SE p_value
1          Intercept  4.179 0.017       0
2              Slope -0.077 0.002       0
3 Intercept Variance  2.420 0.051       0
4     Slope Variance  0.011 0.001       0
```

## Delayed Word Recall Unconditional ATL Model

```
          paramHeader   param    est    se  est_se    pval
1                 I.| WRD2004  1.000 0.000 999.000 999.000
2                 I.| WRD2006  1.000 0.000 999.000 999.000
3                 I.| WRD2008  1.000 0.000 999.000 999.000
4                 I.| WRD2010  1.000 0.000 999.000 999.000
5                 I.| WRD2012  1.000 0.000 999.000 999.000
6                 I.| WRD2014  1.000 0.000 999.000 999.000
7                 S.| WRD2004  0.000 0.000 999.000 999.000
8                 S.| WRD2006  2.000 0.000 999.000 999.000
9                 S.| WRD2008  4.000 0.000 999.000 999.000
10                S.| WRD2010  6.000 0.000 999.000 999.000
11                S.| WRD2012  8.000 0.000 999.000 999.000
12                S.| WRD2014 10.000 0.000 999.000 999.000
13         WRD2006.ON WRD2004  0.003 0.005   0.591   0.555
14         WRD2008.ON WRD2006  0.001 0.006   0.121   0.904
15         WRD2010.ON WRD2008 -0.037 0.008  -4.647   0.000
16         WRD2012.ON WRD2010 -0.058 0.011  -5.189   0.000
17         WRD2014.ON WRD2012 -0.060 0.015  -4.040   0.000
18             S.WITH       I -0.004 0.006  -0.730   0.466
19              Means       I  4.138 0.020 208.989   0.000
20              Means       S -0.051 0.006  -8.818   0.000
21         Intercepts WRD2004  0.000 0.000 999.000 999.000
22         Intercepts WRD2006  0.000 0.000 999.000 999.000
23         Intercepts WRD2008  0.000 0.000 999.000 999.000
24         Intercepts WRD2010  0.000 0.000 999.000 999.000
25         Intercepts WRD2012  0.000 0.000 999.000 999.000
26         Intercepts WRD2014  0.000 0.000 999.000 999.000
27          Variances       I  2.354 0.056  42.062   0.000
28          Variances       S  0.013 0.001  11.490   0.000
29 Residual.Variances WRD2004  1.761 0.044  39.873   0.000
30 Residual.Variances WRD2006  1.879 0.035  53.377   0.000
31 Residual.Variances WRD2008  1.885 0.034  55.546   0.000
32 Residual.Variances WRD2010  1.852 0.035  52.300   0.000
33 Residual.Variances WRD2012  1.822 0.044  41.193   0.000
34 Residual.Variances WRD2014  1.694 0.045  37.559   0.000
```

# Mental Status
## Mental Status Model Fit Indices

```
                                                                Title ChiSqM_Value ChiSqM_DF   CFI   TLI RMSEA_Estimate
1  ALT, bivariate, mental status quadratic, loneliness Unconditional       162.703        38 0.992 0.987          0.015
2  ALT, bivariate, mental status quadratic, loneliness Unconditional        72.186        32 0.998 0.995          0.009
3  ALT, bivariate, mental status quadratic, loneliness Unconditional            NA        NA    NA    NA             NA
4  ALT, bivariate, mental status quadratic, loneliness Unconditional        71.735        32 0.998 0.995          0.009
5  ALT, bivariate, mental status quadratic, loneliness Unconditional        82.863        32 0.997 0.994          0.010
6                                       ALT, quadratic Unconditional        50.405        10 0.996 0.994          0.016
7                                       ALT, quadratic Unconditional        30.410         7 0.998 0.995          0.015
8                                                 ALT, Unconditional        94.231        11 0.992 0.989          0.022
9                                       LCM, quadratic unconditional       317.710        12 0.970 0.962          0.041
10                                                LCM, Unconditional       367.455        16 0.965 0.968          0.038
    SRMR
1  0.082
2  0.065
3     NA
4  0.079
5  0.067
6  0.014
7  0.010
8  0.019
9  0.016
10 0.025
```

## Mental Status LCM Parameters

```
           Parameter    Est    SE p_value
1          Intercept  8.391 0.009       0
2              Slope -0.069 0.002       0
3 Intercept Variance  0.816 0.033       0
4     Slope Variance  0.014 0.001       0
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

# Loneliness
## Loneliness Model Fit Indices

```
                          Title ChiSqM_Value ChiSqM_DF   CFI   TLI RMSEA_Estimate  SRMR
1            ALT Unconditional        10.734        11 1.000 1.000          0.000 0.111
2 LCM quadratic, unconditional        28.864        12 0.995 0.993          0.011 0.181
3            LCM Unconditional        37.326        16 0.993 0.994          0.011 0.182
```

## Loneliness LCM Parameters

```
           Parameter   Est    SE p_value
1          Intercept 1.452 0.007   0.000
2              Slope 0.002 0.001   0.041
3 Intercept Variance 0.169 0.009   0.000
4     Slope Variance 0.001 0.000   0.001
```

## Loneliness Unconditional ATL Model

```
          paramHeader    param    est    se  est_se    pval
1                 I.| LONE2004  1.000 0.000 999.000 999.000
2                 I.| LONE2006  1.000 0.000 999.000 999.000
3                 I.| LONE2008  1.000 0.000 999.000 999.000
4                 I.| LONE2010  1.000 0.000 999.000 999.000
5                 I.| LONE2012  1.000 0.000 999.000 999.000
6                 I.| LONE2014  1.000 0.000 999.000 999.000
7                 S.| LONE2004  0.000 0.000 999.000 999.000
8                 S.| LONE2006  2.000 0.000 999.000 999.000
9                 S.| LONE2008  4.000 0.000 999.000 999.000
10                S.| LONE2010  6.000 0.000 999.000 999.000
11                S.| LONE2012  8.000 0.000 999.000 999.000
12                S.| LONE2014 10.000 0.000 999.000 999.000
13        LONE2006.ON LONE2004 -0.054 0.025  -2.198   0.028
14        LONE2008.ON LONE2006 -0.146 0.045  -3.232   0.001
15        LONE2010.ON LONE2008 -0.239 0.067  -3.570   0.000
16        LONE2012.ON LONE2010 -0.317 0.089  -3.551   0.000
17        LONE2014.ON LONE2012 -0.411 0.110  -3.728   0.000
18             S.WITH        I  0.003 0.002   1.221   0.222
19              Means        I  1.414 0.011 123.461   0.000
20              Means        S  0.065 0.016   4.031   0.000
21         Intercepts LONE2004  0.000 0.000 999.000 999.000
22         Intercepts LONE2006  0.000 0.000 999.000 999.000
23         Intercepts LONE2008  0.000 0.000 999.000 999.000
24         Intercepts LONE2010  0.000 0.000 999.000 999.000
25         Intercepts LONE2012  0.000 0.000 999.000 999.000
26         Intercepts LONE2014  0.000 0.000 999.000 999.000
27          Variances        I  0.169 0.011  15.543   0.000
28          Variances        S  0.001 0.000   2.565   0.010
29 Residual.Variances LONE2004  0.102 0.011   9.598   0.000
30 Residual.Variances LONE2006  0.116 0.006  17.879   0.000
31 Residual.Variances LONE2008  0.128 0.006  22.306   0.000
32 Residual.Variances LONE2010  0.123 0.006  21.357   0.000
33 Residual.Variances LONE2012  0.111 0.007  16.338   0.000
34 Residual.Variances LONE2014  0.108 0.007  15.335   0.000
```

# Social Contact
## Social Contact Model Fit Indices

```
                           Title ChiSqM_Value ChiSqM_DF   CFI   TLI RMSEA_Estimate  SRMR
1             ALT Unconditional        15.120        11 0.999 0.999          0.006 0.080
2 ALT, quadratic, Unconditional         6.723         7 1.000 1.000          0.000 0.068
3 ALT, quadratic, Unconditional        28.692        12 0.996 0.995          0.011 0.090
4  LCM quadratic, unconditional        28.692        12 0.996 0.995          0.011 0.090
5             LCM Unconditional        55.564        16 0.990 0.991          0.014 0.076
```

## Social Contact LCM Parameters

```
                    Parameter    Est    SE p_value
1                   Intercept 29.725 0.105       0
2                       Slope -0.106 0.015       0
3          Intercept Variance 46.478 2.279       0
4              Slope Variance  0.200 0.051       0
5  Residual.VariancesSOCO2004 34.998 2.769       0
6  Residual.VariancesSOCO2006 28.472 1.624       0
7  Residual.VariancesSOCO2008 31.436 1.394       0
8  Residual.VariancesSOCO2010 28.383 1.153       0
9  Residual.VariancesSOCO2012 27.826 1.693       0
10 Residual.VariancesSOCO2014 25.893 2.066       0
```

## Social Contact Unconditional ATL Model

```
          paramHeader    param    est    se  est_se    pval
1                 I.| SOCO2004  1.000 0.000 999.000 999.000
2                 I.| SOCO2006  1.000 0.000 999.000 999.000
3                 I.| SOCO2008  1.000 0.000 999.000 999.000
4                 I.| SOCO2010  1.000 0.000 999.000 999.000
5                 I.| SOCO2012  1.000 0.000 999.000 999.000
6                 I.| SOCO2014  1.000 0.000 999.000 999.000
7                 S.| SOCO2004  0.000 0.000 999.000 999.000
8                 S.| SOCO2006  2.000 0.000 999.000 999.000
9                 S.| SOCO2008  4.000 0.000 999.000 999.000
10                S.| SOCO2010  6.000 0.000 999.000 999.000
11                S.| SOCO2012  8.000 0.000 999.000 999.000
12                S.| SOCO2014 10.000 0.000 999.000 999.000
13        SOCO2006.ON SOCO2004 -0.108 0.019  -5.686   0.000
14        SOCO2008.ON SOCO2006 -0.193 0.038  -5.133   0.000
15        SOCO2010.ON SOCO2008 -0.279 0.055  -5.045   0.000
16        SOCO2012.ON SOCO2010 -0.380 0.073  -5.185   0.000
17        SOCO2014.ON SOCO2012 -0.490 0.093  -5.290   0.000
18             S.WITH        I  1.592 0.578   2.756   0.006
19              Means        I 29.915 0.189 158.190   0.000
20              Means        S  1.275 0.268   4.749   0.000
21         Intercepts SOCO2004  0.000 0.000 999.000 999.000
22         Intercepts SOCO2006  0.000 0.000 999.000 999.000
23         Intercepts SOCO2008  0.000 0.000 999.000 999.000
24         Intercepts SOCO2010  0.000 0.000 999.000 999.000
25         Intercepts SOCO2012  0.000 0.000 999.000 999.000
26         Intercepts SOCO2014  0.000 0.000 999.000 999.000
27          Variances        I 46.895 2.788  16.820   0.000
28          Variances        S  0.258 0.074   3.458   0.001
29 Residual.Variances SOCO2004 32.631 3.053  10.687   0.000
30 Residual.Variances SOCO2006 28.499 1.619  17.601   0.000
31 Residual.Variances SOCO2008 32.575 1.627  20.016   0.000
32 Residual.Variances SOCO2010 28.927 1.337  21.628   0.000
33 Residual.Variances SOCO2012 29.740 1.804  16.483   0.000
34 Residual.Variances SOCO2014 27.649 1.600  17.279   0.000
```

# Social Strain
## Social Strain Model Fit Indices

```
               Title ChiSqM_Value ChiSqM_DF   CFI   TLI RMSEA_Estimate  SRMR
1 ALT Unconditional            NA        NA    NA    NA             NA    NA
2 LCM Unconditional        40.811        10 0.989 0.989          0.016 0.425
```

## Social Strain LCM Parameters

```
                   Parameter    Est    SE p_value
1                  Intercept  5.212 0.023   0.000
2                      Slope -0.028 0.003   0.000
3         Intercept Variance  1.803 0.177   0.000
4             Slope Variance  0.011 0.004   0.002
5 Residual.VariancesSTRA2006  1.038 0.104   0.000
6 Residual.VariancesSTRA2008  1.206 0.084   0.000
7 Residual.VariancesSTRA2010  1.056 0.056   0.000
8 Residual.VariancesSTRA2012  1.141 0.085   0.000
9 Residual.VariancesSTRA2014  0.902 0.107   0.000
```

## Social Strain Unconditional ATL Model

```
          paramHeader    param    est    se  est_se    pval
1                 I.| STRA2006  1.000 0.000 999.000 999.000
2                 I.| STRA2008  1.000 0.000 999.000 999.000
3                 I.| STRA2010  1.000 0.000 999.000 999.000
4                 I.| STRA2012  1.000 0.000 999.000 999.000
5                 I.| STRA2014  1.000 0.000 999.000 999.000
6                 S.| STRA2006  2.000 0.000 999.000 999.000
7                 S.| STRA2008  4.000 0.000 999.000 999.000
8                 S.| STRA2010  6.000 0.000 999.000 999.000
9                 S.| STRA2012  8.000 0.000 999.000 999.000
10                S.| STRA2014 10.000 0.000 999.000 999.000
11        STRA2008.ON STRA2006 -0.175 0.029  -6.142   0.000
12        STRA2010.ON STRA2008 -0.350 0.057  -6.181   0.000
13        STRA2012.ON STRA2010 -0.517 0.086  -6.006   0.000
14        STRA2014.ON STRA2012 -0.695 0.114  -6.091   0.000
15             S.WITH        I  0.085 0.060   1.416   0.157
16              Means        I  4.369 0.145  30.193   0.000
17              Means        S  0.405 0.072   5.667   0.000
18         Intercepts STRA2006  0.000 0.000 999.000 999.000
19         Intercepts STRA2008  0.000 0.000 999.000 999.000
20         Intercepts STRA2010  0.000 0.000 999.000 999.000
21         Intercepts STRA2012  0.000 0.000 999.000 999.000
22         Intercepts STRA2014  0.000 0.000 999.000 999.000
23          Variances        I  1.254 0.350   3.585   0.000
24          Variances        S -0.004 0.013  -0.290   0.771
25 Residual.Variances STRA2006  1.066 0.178   5.978   0.000
26 Residual.Variances STRA2008  1.338 0.113  11.884   0.000
27 Residual.Variances STRA2010  1.221 0.103  11.801   0.000
28 Residual.Variances STRA2012  1.305 0.096  13.533   0.000
29 Residual.Variances STRA2014  0.941 0.103   9.128   0.000
```

# Social Support
## Social Support Model Fit Indices

```
                Title ChiSqM_Value ChiSqM_DF   CFI   TLI RMSEA_Estimate  SRMR
1  ALT Unconditional        33.206        11 0.994 0.992          0.013 0.142
2 LGCM Unconditional        67.774        12 0.985 0.981          0.020 0.153
3 LGCM Unconditional        78.666        16 0.983 0.984          0.018 0.159
```

## Social Support LGCM Parameters

```
                   Parameter    Est    SE p_value
1                  Intercept  9.566 0.020   0.000
2                      Slope -0.009 0.003   0.001
3         Intercept Variance  1.732 0.079   0.000
4             Slope Variance  0.008 0.002   0.000
5  Residual.VariancesSUP2004  0.913 0.094   0.000
6  Residual.VariancesSUP2006  0.892 0.053   0.000
7  Residual.VariancesSUP2008  1.031 0.048   0.000
8  Residual.VariancesSUP2010  0.998 0.041   0.000
9  Residual.VariancesSUP2012  0.906 0.060   0.000
10 Residual.VariancesSUP2014  0.880 0.076   0.000
```

## Social Support LGCM Parameters

```
          paramHeader   param    est    se  est_se    pval
1                 I.| SUP2004  1.000 0.000 999.000 999.000
2                 I.| SUP2006  1.000 0.000 999.000 999.000
3                 I.| SUP2008  1.000 0.000 999.000 999.000
4                 I.| SUP2010  1.000 0.000 999.000 999.000
5                 I.| SUP2012  1.000 0.000 999.000 999.000
6                 I.| SUP2014  1.000 0.000 999.000 999.000
7                 S.| SUP2004  0.000 0.000 999.000 999.000
8                 S.| SUP2006  2.000 0.000 999.000 999.000
9                 S.| SUP2008  4.000 0.000 999.000 999.000
10                S.| SUP2010  6.000 0.000 999.000 999.000
11                S.| SUP2012  8.000 0.000 999.000 999.000
12                S.| SUP2014 10.000 0.000 999.000 999.000
13         SUP2006.ON SUP2004 -0.059 0.022  -2.698   0.007
14         SUP2008.ON SUP2006 -0.098 0.045  -2.192   0.028
15         SUP2010.ON SUP2008 -0.137 0.067  -2.050   0.040
16         SUP2012.ON SUP2010 -0.169 0.089  -1.904   0.057
17         SUP2014.ON SUP2012 -0.212 0.110  -1.925   0.054
18             S.WITH       I -0.014 0.016  -0.860   0.390
19              Means       I  9.735 0.032 299.828   0.000
20              Means       S  0.176 0.105   1.679   0.093
21         Intercepts SUP2004  0.000 0.000 999.000 999.000
22         Intercepts SUP2006  0.000 0.000 999.000 999.000
23         Intercepts SUP2008  0.000 0.000 999.000 999.000
24         Intercepts SUP2010  0.000 0.000 999.000 999.000
25         Intercepts SUP2012  0.000 0.000 999.000 999.000
26         Intercepts SUP2014  0.000 0.000 999.000 999.000
27          Variances       I  1.827 0.090  20.275   0.000
28          Variances       S  0.010 0.002   4.164   0.000
29 Residual.Variances SUP2004  0.792 0.106   7.457   0.000
30 Residual.Variances SUP2006  0.884 0.052  16.928   0.000
31 Residual.Variances SUP2008  1.055 0.050  20.940   0.000
32 Residual.Variances SUP2010  1.004 0.041  24.602   0.000
33 Residual.Variances SUP2012  0.896 0.061  14.745   0.000
34 Residual.Variances SUP2014  0.897 0.070  12.859   0.000
```

# Depression
## Depression Model Fit Indices

```
                Title ChiSqM_Value ChiSqM_DF   CFI   TLI RMSEA_Estimate  SRMR
1  ALT Unconditional        19.138        11 0.999 0.999          0.007 0.006
2 LGCM Unconditional        47.093        16 0.997 0.997          0.011 0.009
```

## Depression LGCM Parameters

```
                   Parameter    Est    SE p_value
1                  Intercept  9.566 0.020   0.000
2                      Slope -0.009 0.003   0.001
3         Intercept Variance  1.732 0.079   0.000
4             Slope Variance  0.008 0.002   0.000
5  Residual.VariancesSUP2004  0.913 0.094   0.000
6  Residual.VariancesSUP2006  0.892 0.053   0.000
7  Residual.VariancesSUP2008  1.031 0.048   0.000
8  Residual.VariancesSUP2010  0.998 0.041   0.000
9  Residual.VariancesSUP2012  0.906 0.060   0.000
10 Residual.VariancesSUP2014  0.880 0.076   0.000
```

## Depression Unconditional ATL Model

```
          paramHeader   param    est    se  est_se    pval
1                 I.| DEP2004  1.000 0.000 999.000 999.000
2                 I.| DEP2006  1.000 0.000 999.000 999.000
3                 I.| DEP2008  1.000 0.000 999.000 999.000
4                 I.| DEP2010  1.000 0.000 999.000 999.000
5                 I.| DEP2012  1.000 0.000 999.000 999.000
6                 I.| DEP2014  1.000 0.000 999.000 999.000
7                 S.| DEP2004  0.000 0.000 999.000 999.000
8                 S.| DEP2006  2.000 0.000 999.000 999.000
9                 S.| DEP2008  4.000 0.000 999.000 999.000
10                S.| DEP2010  6.000 0.000 999.000 999.000
11                S.| DEP2012  8.000 0.000 999.000 999.000
12                S.| DEP2014 10.000 0.000 999.000 999.000
13         DEP2006.ON DEP2004  0.051 0.012   4.399   0.000
14         DEP2008.ON DEP2006  0.029 0.012   2.483   0.013
15         DEP2010.ON DEP2008  0.037 0.014   2.727   0.006
16         DEP2012.ON DEP2010  0.062 0.018   3.395   0.001
17         DEP2014.ON DEP2012  0.062 0.023   2.743   0.006
18             S.WITH       I -0.050 0.007  -6.931   0.000
19              Means       I  1.377 0.017  80.026   0.000
20              Means       S  0.006 0.004   1.493   0.136
21         Intercepts DEP2004  0.000 0.000 999.000 999.000
22         Intercepts DEP2006  0.000 0.000 999.000 999.000
23         Intercepts DEP2008  0.000 0.000 999.000 999.000
24         Intercepts DEP2010  0.000 0.000 999.000 999.000
25         Intercepts DEP2012  0.000 0.000 999.000 999.000
26         Intercepts DEP2014  0.000 0.000 999.000 999.000
27          Variances       I  2.098 0.069  30.489   0.000
28          Variances       S  0.010 0.001   8.373   0.000
29 Residual.Variances DEP2004  1.477 0.055  26.610   0.000
30 Residual.Variances DEP2006  1.525 0.045  34.060   0.000
31 Residual.Variances DEP2008  1.654 0.045  37.142   0.000
32 Residual.Variances DEP2010  1.431 0.044  32.186   0.000
33 Residual.Variances DEP2012  1.496 0.051  29.439   0.000
34 Residual.Variances DEP2014  1.430 0.059  24.263   0.000
```

# Bivariate ALT Models

## Bivariate ALT Immediate Word Recall and Social Contact
### Model Estimated

```r
knitr::include_graphics("ATL bivariate model immediate word recall social contact.pdf")
```

<embed src="ATL bivariate model immediate word recall social contact.pdf" width="5000px" type="application/pdf" />

<img src="ATL bivariate model immediate word recall social contact.pdf" alt="some text"  width="10000" height="5200">

Significant associations only
<img src="ATL bivariate model immediate word recall social contact sig.pdf" alt="some text"  width="6200" height="5200">

## Bivariate ALT Immediate Word Recall and Loneliness
<img src="ATL bivariate model immediate word recall loneliness sig.pdf" alt="some text"  width="6200" height="5200">

## Bivariate ALT Immediate Word Recall and Depressive Symptoms
<img src="ATL bivariate model immediate word recall depression sig.pdf" alt="some text"  width="6200" height="5200">

## Bivariate ALT Immediate Word Recall and Social Support
<img src="ATL bivariate model immediate word recall social support sig.pdf" alt="some text"  width="6200" height="5200">

## Bivariate ALT Immediate Word Recall and Social Network
<img src="ATL bivariate model immediate word recall social network sig.pdf" alt="some text"  width="6200" height="5200">

## Bivariate ALT Delayed Word Recall and Loneliness
<img src="ATL bivariate model delayed word recall loneliness sig.pdf" alt="some text"  width="6200" height="5200">

## Bivariate ALT Delayed Word Recall and Social Contact
<img src="ATL bivariate model delayed word recall social contact sig.pdf" alt="some text"  width="6200" height="5200">

## Bivariate ALT Delayed Word Recall and Social Network
<img src="ATL bivariate model delayed word recall social network sig.pdf" alt="some text"  width="6200" height="5200">

## Bivariate ALT Delayed Word Recall and Social Support
<img src="ATL bivariate model delayed word recall social support sig.pdf" alt="some text"  width="6200" height="5200">

## Bivariate ALT Delayed Word Recall and Depression
<img src="ATL bivariate model delayed word recall depression.pdf" alt="some text"  width="6200" height="5200">

## Bivariate ALT Mental Status and Loneliness
<img src="ALT bivariate quadratic mental status and loneliness sig.pdf" alt="some text"  width="6200" height="5200">

## Bivariate ALT Mental Status and Social Contact
<img src="ALT bivariate quadratic mental status and social contact sig.pdf" alt="some text"  width="6200" height="5200">

## Bivariate ALT Mental Status and social network
<img src="ALT bivariate quadratic mental status and social network sig.pdf" alt="some text"  width="6200" height="5200">

## Bivariate ALT Mental Status and Social Support
<img src="ALT bivariate quadratic mental status and social support sig.pdf" alt="some text"  width="6200" height="5200">

## Bivariate ALT Mental Status and Depressive Symptoms
<img src="ALT bivariate quadratic mental status and depression sig.pdf" alt="some text"  width="6200" height="5200">
