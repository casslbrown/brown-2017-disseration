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

# ---- common-functions --------
# Create a function that takes model summary data frames and a list of comparison model reference numbers
# and uses this information to compute the chi square and df difference for the models. 
# Returns a data frame with all relevant information for model comparisons.

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

# -----

# delayed word recall and loneliness three item scale
wrectotd_loneliness_summary <- extractModelSummaries("./output/bivariate-models/wrectotd-score_loneliness_3")

wrectotd_loneliness_summary["CM"] <- 3
wrectotd_loneliness_summary[1:3, "CM"] <- "-"
wrectotd_loneliness_summary[10:11, "CM"] <- "9"
wrectotd_loneliness_summary[12:14, "CM"] <- "11"
cm_row <- wrectotd_loneliness_summary$CM

compare_models_function(wrectotd_loneliness_summary, cm_row)
# ALT-11 fixed social on cognitive regressions over time is the final model add covariates.

# delayed word recall and social contact bivariate models
wrectotd_social_contact_summaries <- extractModelSummaries("./output/bivariate-models/wrectotd-social_contact_total")
wrectotd_social_contact_summaries["CM"] <- 3
wrectotd_social_contact_summaries[1:3, "CM"] <- "-"
wrectotd_social_contact_summaries[10:13, "CM"] <- 9
cm_row <- wrectotd_social_contact_summaries$CM
compare_models_function(wrectotd_social_contact_summaries, cm_row)
# ALT-11 fixed social on cognitive regressions over time is the final model add covariates.

# delayed word recall and social network
wrectotd_social_network_summaries <- extractModelSummaries("./output/bivariate-models/wrectotd-socialnetwork_total")
wrectotd_social_network_summaries["CM"] <- 3
wrectotd_social_network_summaries[1:3,"CM"] <- "-"
wrectotd_social_network_summaries[10:13,"CM"] <- 9
cm_row <- wrectotd_social_network_summaries$CM
compare_models_function(wrectotd_social_network_summaries, cm_row)

# delayed word recall and social support
wrectotd_social_support <- extractModelSummaries("./output/bivariate-models/wrectotd-social_support_mean")
wrectotd_social_support["CM"] <- 3
wrectotd_social_support[1:3,"CM"] <- "-"
wrectotd_social_support[10:13,"CM"] <- 9
wrectotd_social_support[12:13,"CM"] <- 11
cm_row <- wrectotd_social_support$CM
compare_models_function(wrectotd_social_support, cm_row)

# immediate word recall and loneliness
wrectoti_loneliness <- extractModelSummaries("./output/bivariate-models/wrectoti-score_loneliness_3")
wrectoti_loneliness["CM"] <- 3
wrectoti_loneliness[10:12,"CM"] <- 9
wrectoti_loneliness[13,"CM"] <- 12
wrectoti_loneliness[14,"CM"] <- 13
cm_row <- wrectoti_loneliness$CM
compare_models_function(wrectoti_loneliness, cm_row)

# immediate word recall and social contact
wrectoti_social_contact <- extractModelSummaries("./output/bivariate-models/wrectoti-social_contact_total")
wrectoti_social_contact["CM"] <- 3
cm_row <- wrectoti_social_contact$CM
compare_models_function(wrectoti_social_contact, cm_row)

# immediate word recall and social support
wrectoti_social_support <- extractModelSummaries("./output/bivariate-models/wrectoti-social_support_mean")
wrectoti_social_support["CM"]