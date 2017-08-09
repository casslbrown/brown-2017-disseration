# knitr::stitch_rmd(script="./___/___.R", output="./___/___/___.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>% 
library(dplyr)
library(TabularManifest)
library(MplusAutomation)
library(semPlot)
# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
source("./scripts/common-functions.R") # used in multiple reports
source("./scripts/graphing/graph-presets.R") # fonts, colors, themes 
source("./scripts/graphing/graph-elemental.R") # graphs to be used in dipslays
source("./scripts/graphing/graph-complex.R") # info displays

# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2") # graphing
# requireNamespace("readr") # data input
requireNamespace("tidyr") # data manipulation
requireNamespace("dplyr") # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit")# For asserting conditions meet expected patterns.

compare_models_function <- function(
  summary_df
  ,cm_row
){
  # Compute the difference test scaling correction cd, where d0 is the degrees of freedom in the nested model, 
  # c0 is the scaling correction factor for the nested model, d1 is the degrees of freedom in the comparison model, 
  # and c1 is the scaling correction factor for the comparison model. Be sure to use the correction factor given in the output for the H0 model.
  # cd = (d0 * c0 - d1*c1)/(d0 - d1)
  
  d0 <- summary_df["ChiSqM_DF"]
  d1 <- summary_df[cm_row,"ChiSqM_DF"]
  
  # scaling correction factor for the nested model
  c0 <- summary_df["ChiSqM_ScalingCorrection"]
  
  # scaling correction factor for the comparison model
  c1 <- summary_df[cm_row,"ChiSqM_ScalingCorrection"]
  
  summary_df["cd"] <- (d0*c0 - d1*c1)/(d0-d1)
  # Compute the Satorra-Bentler scaled chi-square difference test TRd as follows:
  #   TRd = (T0*c0 - T1*c1)/cd
  # where T0 and T1 are the MLM, MLR, or WLSM chi-square values for the nested and comparison model, 
  # respectively. For MLM and MLR the products T0*c0 and T1*c1 are the same as the corresponding ML chi-square values.
  t1 <- summary_df[cm_row, "ChiSqM_Value"]
  t0 <- summary_df["ChiSqM_Value"]
  summary_df["TRd"] <- (t0*c0 - t1*c1)/summary_df["cd"]
  
  summary_df["df_diff"] <- summary_df["ChiSqM_DF"]-d1
  
  subset_model_summary <- summary_df %>% 
    dplyr::select(
      Title
      ,Observations
      ,ChiSqM_Value
      ,ChiSqM_DF
      ,ChiSqM_ScalingCorrection
      ,CM
      ,TRd
      ,df_diff
      ,CFI
      ,TLI
      ,RMSEA_Estimate
      ,SRMR
      ,Filename
    )
  return(subset_model_summary)
}

# ---- immediate-word-recall-model-summaries ---------------------------------------------------------------
# Extract the fit indices of relevant models
wrecti_fit_series1 <- extractModelSummaries("./output/univariate-models/wrectoti")

wrecti_fit_series1["CM"] <- 4
wrecti_fit_series1[1:3, "CM"] <- "-"

cm_row <- wrecti_fit_series1$CM

# Extract the fit indices of relevant models
wrecti_fit_series2 <- extractModelSummaries("./output/univariate-models-65plus/wrectoti")

wrecti_fit_series2["CM"] <- 4
wrecti_fit_series2[1:3, "CM"] <- "-"

cm_row <- wrecti_fit_series2$CM

# Extract the fit indices of relevant models
wrecti_fit_series3 <- extractModelSummaries("./output/univariate-models-nodem/wrectoti")

wrecti_fit_series3["CM"] <- 4
wrecti_fit_series3[1:3, "CM"] <- "-"

cm_row <- wrecti_fit_series3$CM

# Extract the fit indices of relevant models
wrecti_fit_series4 <- extractModelSummaries("./output/univariate-models-nodem-65plus/wrectoti")

wrecti_fit_series4["CM"] <- 4
wrecti_fit_series4[1:3, "CM"] <- "-"

cm_row <- wrecti_fit_series3$CM

# Show a summary table
wrecti_models_1 <- compare_models_function(wrecti_fit_series1, cm_row)
wrecti_models_2 <- compare_models_function(wrecti_fit_series2, cm_row)
wrecti_models_3 <- compare_models_function(wrecti_fit_series3, cm_row)
wrecti_models_4 <- compare_models_function(wrecti_fit_series4, cm_row)

write.csv(wrecti_models_4, file = "./output/univariate-models/wrecti_model_com.csv")

read.csv("./output/univariate-models/wrecti_model_com.csv")

# ---- immediate-word-recall-lgm ------------------------
# Create a table of relevant parameters
immed_word_recall_parameters <- extractModelParameters("./output/univariate-models/wrectoti/u02_nocov_wrectoti.out")

p <- as.data.frame(immed_word_recall_parameters)

p$Parameter <- paste0(p$unstandardized.paramHeader, p$unstandardized.param)

ps <- filter(p, grepl("Means", p[,"Parameter"]) | grepl("VariancesI", p[,"Parameter"])| grepl("VariancesS", p[,"Parameter"]))

ps <- ps %>% plyr::rename(c(
  "unstandardized.est" = "Est",
  "unstandardized.se"  = "SE",
  "unstandardized.pval" = "p_value"
))

ps$Parameter[ps$Parameter=="MeansI"] <- "Intercept"
ps$Parameter[ps$Parameter=="MeansS"] <- "Slope"
ps$Parameter[ps$Parameter=="VariancesI"] <- "Intercept Variance"
ps$Parameter[ps$Parameter=="VariancesS"] <- "Slope Variance"

ps %>%
  dplyr::select(Parameter, Est, SE, p_value)

# ---- immediate-word-recall-atl-------
# extract parameters from the models with memory disease ever excluded and only participants who were 65 or older at baseline.
immed_word_recall_ATLparameters <- extractModelParameters("./output/univariate-models-nodem-65plus/wrectoti/u04_nocov_wrectoti.out")

atl_unstandardized <- as.data.frame(immed_word_recall_ATLparameters$unstandardized)
print(atl_unstandardized)

# ---- delayed-word-recall-model-summaries ---------------------------------------------------------------
# Extract the fit indices of relevant models
del_word_recall_fit <- extractModelSummaries("./output/univariate-models/wrectotd")

del_word_recall_fit["CM"] <- 3
del_word_recall_fit[1:3, "CM"] <- "-"

cm_row <- del_word_recall_fit$CM

# Show a summary table
compare_models_function(del_word_recall_fit, cm_row)

# ---- delayed-word-recall-model-no-dementia-summaries ---------------------------------------------------------------
# Extract the fit indices of relevant models
del_word_recall_fit <- extractModelSummaries("./output/univariate-models-nodem/wrectotd")

del_word_recall_fit["CM"] <- 3
del_word_recall_fit[1:3, "CM"] <- "-"
del_word_recall_fit[1:3, "CM"] <- "-"

cm_row <- del_word_recall_fit$CM

# Show a summary table
compare_models_function(del_word_recall_fit, cm_row)

# ---- delayed-word-recall-lgm ------------------------
# Create a table of relevant parameters
delayed_word_recall_parameters <- extractModelParameters("./output/univariate-models/wrectotd/u02_nocov_wrectotd.out")

p <- as.data.frame(delayed_word_recall_parameters)

p$Parameter <- paste0(p$unstandardized.paramHeader, p$unstandardized.param)

ps <- filter(p, grepl("Means", p[,"Parameter"]) | grepl("VariancesI", p[,"Parameter"])| grepl("VariancesS", p[,"Parameter"]))

ps <- ps %>% plyr::rename(c(
  "unstandardized.est" = "Est",
  "unstandardized.se"  = "SE",
  "unstandardized.pval" = "p_value"
))

ps$Parameter[ps$Parameter=="MeansI"] <- "Intercept"
ps$Parameter[ps$Parameter=="MeansS"] <- "Slope"
ps$Parameter[ps$Parameter=="VariancesI"] <- "Intercept Variance"
ps$Parameter[ps$Parameter=="VariancesS"] <- "Slope Variance"

ps %>%
  dplyr::select(Parameter, Est, SE, p_value)

# ---- delayed-word-recall-atl-------
del_word_recall_ATLparameters <- extractModelParameters("./output/univariate-models/wrectotd/u03_nocov_wrectotd.out")

del_atl_unstandardized <- as.data.frame(del_word_recall_ATLparameters$unstandardized)
print(del_atl_unstandardized)

# ---- mental-status-model-summaries ---------------------------------------------------------------
# Extract the fit indices of relevant models
mental_status_fit <- extractModelSummaries("./output/univariate-models-nodem/mentalstatus_tot/")

mental_status_fit["CM"] <- 3
mental_status_fit[1:3, "CM"] <- "-"
mental_status_fit[5, "CM"] <- "2"
cm_row <- mental_status_fit$CM

# Show a summary table
compare_models_function(mental_status_fit, cm_row)

# ---- mental-status-lgm ------------------------
# Create a table of relevant parameters
mental_status_parameters <- extractModelParameters("./output/univariate-models/mentalstatus_tot/u03_nocov_mentalstatus_tot.out")

p <- as.data.frame(mental_status_parameters)

p$Parameter <- paste0(p$unstandardized.paramHeader, p$unstandardized.param)

ps <- filter(p, grepl("Means", p[,"Parameter"]) | grepl("VariancesI", p[,"Parameter"])| grepl("VariancesS", p[,"Parameter"]))

ps <- ps %>% plyr::rename(c(
  "unstandardized.est" = "Est",
  "unstandardized.se"  = "SE",
  "unstandardized.pval" = "p_value"
))

ps$Parameter[ps$Parameter=="MeansI"] <- "Intercept"
ps$Parameter[ps$Parameter=="MeansS"] <- "Slope"
ps$Parameter[ps$Parameter=="VariancesI"] <- "Intercept Variance"
ps$Parameter[ps$Parameter=="VariancesS"] <- "Slope Variance"

ps %>%
  dplyr::select(Parameter, Est, SE, p_value)

# ---- mental-status-atl-------
mental_status_ATLparameters <- extractModelParameters("./data-unshared/derived/Mental Status/ALT mental status.out")

mentalstatus_atl_unstandardized <- as.data.frame(mental_status_ATLparameters$unstandardized)
print(mentalstatus_atl_unstandardized)

