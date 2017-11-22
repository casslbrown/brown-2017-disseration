# knitr::stitch_rmd(script="./___/___.R", output="./___/___/___.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>% 
library(dplyr)
#library(TabularManifest)
library(MplusAutomation)
library(papaja)
library(htmlTable)
library(xtable)
library(DiagrammeRsvg)
library(svglite)
library(rsvg)
library(png)
library(DiagrammeR)
#devtools::install_github("davidgohel/gdtools")
options(xtable.floating = FALSE)
options(xtable.timestamp = "")

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
# source("./scripts/common-functions.R") # used in multiple reports
# source("./scripts/graphing/graph-presets.R") # fonts, colors, themes
# source("./scripts/graphing/graph-elemental.R") # graphs to be used in dipslays
# source("./scripts/graphing/graph-complex.R") # info displays
source("./scripts/graphing/alt-path-diagram.R") # path diagrams for alt models
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
  summary_df["TRd"] <- round((t0*c0 - t1*c1)/summary_df["cd"], digits = 2)
  
  summary_df["df_diff"] <- summary_df["ChiSqM_DF"]-d1
  
  subset_model_summary <- summary_df %>% 
    dplyr::select(
      Title
      ,ChiSqM_Value
      ,ChiSqM_DF
      ,CM
      ,TRd
      ,df_diff
      ,CFI
      ,TLI
      ,RMSEA_Estimate
      ,SRMR
    )
  
  return(subset_model_summary)
}

parameter_extraction_function <- function(d){
  #d <- wrecti_ATL
  ALT_slope <- d[which(d[,"parameter"]=='Means SA'),"est"]
  ALT_slope_pval <- d[which(d[,"parameter"]=='Means SA'),"pval"]
  ALT_slope_variance <- d[which(d[,"parameter"]=='Variances SA'),"est"]
  ALT_slope_variance_pval <- d[which(d[,"parameter"]=='Variances SA'),"pval"]
  ALT_intercept <- d[which(d[,"parameter"]=='Means IA'),"est"]
  ALT_intercept_pval <- d[which(d[,"parameter"]=='Means IA'),"pval"]
  ALT_rho21 <- d[which(d[,"parameter"]=='A_02.ON A_01'),"est"]
  ALT_rho21_pval <- d[which(d[,"parameter"]=='A_02.ON A_01'),"pval"]
  ALT_rho32 <- d[which(d[,"parameter"]=='A_03.ON A_02'),"est"]
  ALT_rho32_pval <- d[which(d[,"parameter"]=='A_03.ON A_02'),"pval"]
  ALT_rho43 <- d[which(d[,"parameter"]=='A_04.ON A_03'),"est"]
  ALT_rho43_pval <- d[which(d[,"parameter"]=='A_04.ON A_03'),"pval"]
  ALT_rho54 <- d[which(d[,"parameter"]=='A_05.ON A_04'),"est"]
  ALT_rho54_pval <- d[which(d[,"parameter"]=='A_05.ON A_04'),"pval"]
  ALT_rho65 <- d[which(d[,"parameter"]=='A_06.ON A_05'),"est"]
  ALT_rho65_pval <- d[which(d[,"parameter"]=='A_06.ON A_05'),"pval"]
  par_list<- list()
  par_list[["ALT_slope"]] <-ALT_slope
  par_list[["ALT_slope_pval"]] <- ALT_slope_pval
  par_list[["ALT_slope_variance"]] <- ALT_slope_variance
  par_list[["ALT_slope_variance_pval"]] <- ALT_slope_variance_pval
  par_list[["ALT_intercept"]] <- ALT_intercept
  par_list[["ALT_intercept_pval"]] <- ALT_intercept_pval
  par_list[["ALT_rho21"]] <- ALT_rho21
  par_list[["ALT_rho21_pval"]] <- ALT_rho21_pval
  par_list[["ALT_rho32"]] <- ALT_rho32
  par_list[["ALT_rho32_pval"]] <- ALT_rho32_pval
  par_list[["ALT_rho43"]] <- ALT_rho43
  par_list[["ALT_rho43_pval"]] <- ALT_rho43_pval
  par_list[["ALT_rho54"]] <- ALT_rho54
  par_list[["ALT_rho54_pval"]] <- ALT_rho54_pval
  par_list[["ALT_rho65"]] <- ALT_rho65
  par_list[["ALT_rho65_pval"]] <- ALT_rho65_pval
  return(par_list)
}

bivariateALT_parameter_extraction_function <- function(d){
  #d <- wrectd_lonely_ATL
  par_list<- list()
  par_list[["cog_slope"]] <- d[which(d[,"parameter"]=='Means SA'),"est"]
  par_list[["cog_slope_pval"]] <- d[which(d[,"parameter"]=='Means SA'),"pval"]
  par_list[["cog_slope_variance"]] <- d[which(d[,"parameter"]=='Variances SA'),"est"]
  par_list[["cog_slope_variance_pval"]] <- d[which(d[,"parameter"]=='Variances SA'),"pval"]
  par_list[["cog_intercept"]] <- d[which(d[,"parameter"]=='Means IA'),"est"]
  par_list[["cog_intercept_pval"]]  <- d[which(d[,"parameter"]=='Means IA'),"pval"]
  
  par_list[["soc_slope"]] <- d[which(d[,"parameter"]=='Means SB'),"est"]
  par_list[["soc_slope_pval"]] <- d[which(d[,"parameter"]=='Means SB'),"pval"]
  par_list[["soc_slope_variance"]] <- d[which(d[,"parameter"]=='Variances SB'),"est"]
  par_list[["soc_slope_variance_pval"]] <- d[which(d[,"parameter"]=='Variances SB'),"pval"]
  par_list[["soc_intercept"]] <- d[which(d[,"parameter"]=='Means IB'),"est"]
  par_list[["soc_intercept_pval"]]  <- d[which(d[,"parameter"]=='Means IB'),"pval"]
  
  par_list[["A_01_WITH_B_01"]] <- d[which(d[,"parameter"]=='A_01.WITH B_01'),"est"]
  par_list[["A_01_WITH_B_01_pval"]] <- d[which(d[,"parameter"]=='A_01.WITH B_01'),"pval"]
  par_list[["IB_WITH_IA"]] <- d[which(d[,"parameter"]=='IB.WITH IA'),"est"]
  par_list[["IB_WITH_IA_pval"]] <- d[which(d[,"parameter"]=='IB.WITH IA'),"pval"]
  par_list[["SB_WITH_SA"]] <- d[which(d[,"parameter"]=='SB.WITH SA'),"est"]
  par_list[["SB_WITH_SA_pval"]] <- d[which(d[,"parameter"]=='SB.WITH SA'),"pval"]
  
  par_list[["cog_rho21"]] <- d[which(d[,"parameter"]=='A_02.ON A_01'),"est"]
  par_list[["cog_rho21_pval"]] <- d[which(d[,"parameter"]=='A_02.ON A_01'),"pval"]
  par_list[["cog_rho32"]] <- d[which(d[,"parameter"]=='A_03.ON A_02'),"est"]
  par_list[["cog_rho32_pval"]] <- d[which(d[,"parameter"]=='A_03.ON A_02'),"pval"]
  par_list[["cog_rho43"]] <- d[which(d[,"parameter"]=='A_04.ON A_03'),"est"]
  par_list[["cog_rho43_pval"]] <- d[which(d[,"parameter"]=='A_04.ON A_03'),"pval"]
  par_list[["cog_rho54"]] <- d[which(d[,"parameter"]=='A_05.ON A_04'),"est"]
  par_list[["cog_rho54_pval"]] <- d[which(d[,"parameter"]=='A_05.ON A_04'),"pval"]
  par_list[["cog_rho65"]] <- d[which(d[,"parameter"]=='A_06.ON A_05'),"est"]
  par_list[["cog_rho65_pval"]] <- d[which(d[,"parameter"]=='A_06.ON A_05'),"pval"]
  
  par_list[["soc_rho21"]] <- d[which(d[,"parameter"]=='B_02.ON B_01'),"est"]
  par_list[["soc_rho21_pval"]] <- d[which(d[,"parameter"]=='B_02.ON B_01'),"pval"]
  par_list[["soc_rho32"]] <- d[which(d[,"parameter"]=='B_03.ON B_02'),"est"]
  par_list[["soc_rho32_pval"]] <- d[which(d[,"parameter"]=='B_03.ON B_02'),"pval"]
  par_list[["soc_rho43"]] <- d[which(d[,"parameter"]=='B_04.ON B_03'),"est"]
  par_list[["soc_rho43_pval"]] <- d[which(d[,"parameter"]=='B_04.ON B_03'),"pval"]
  par_list[["soc_rho54"]] <- d[which(d[,"parameter"]=='B_05.ON B_04'),"est"]
  par_list[["soc_rho54_pval"]] <- d[which(d[,"parameter"]=='B_05.ON B_04'),"pval"]
  par_list[["soc_rho65"]] <- d[which(d[,"parameter"]=='B_06.ON B_05'),"est"]
  par_list[["soc_rho65_pval"]] <- d[which(d[,"parameter"]=='B_06.ON B_05'),"pval"]
  
  par_list[["A_02_B_01"]]       <- d[which(d[,"parameter"]=='A_02.ON B_01'),"est"]
  par_list[["A_02_B_01_pval"]]  <- d[which(d[,"parameter"]=='A_02.ON B_01'),"pval"]
  par_list[["A_03_B_02"]]       <- d[which(d[,"parameter"]=='A_03.ON B_02'),"est"]
  par_list[["A_03_B_02_pval"]]  <- d[which(d[,"parameter"]=='A_03.ON B_02'),"pval"]
  par_list[["A_04_B_03"]]       <- d[which(d[,"parameter"]=='A_04.ON B_03'),"est"]
  par_list[["A_04_B_03_pval"]]  <- d[which(d[,"parameter"]=='A_04.ON B_03'),"pval"]
  par_list[["A_05_B_04"]]       <- d[which(d[,"parameter"]=='A_05.ON B_04'),"est"]
  par_list[["A_05_B_04_pval"]]  <- d[which(d[,"parameter"]=='A_05.ON B_04'),"pval"]
  par_list[["A_06_B_05"]]       <- d[which(d[,"parameter"]=='A_06.ON B_05'),"est"]
  par_list[["A_06_B_05_pval"]]  <- d[which(d[,"parameter"]=='A_06.ON B_05'),"pval"]

  par_list[["B_02_A_01"]]       <- d[which(d[,"parameter"]=='B_02.ON A_01'),"est"]
  par_list[["B_02_A_01_pval"]]  <- d[which(d[,"parameter"]=='B_02.ON A_01'),"pval"]
  par_list[["B_03_A_02"]]       <- d[which(d[,"parameter"]=='B_03.ON A_02'),"est"]
  par_list[["B_03_A_02_pval"]]  <- d[which(d[,"parameter"]=='B_03.ON A_02'),"pval"]
  par_list[["B_04_A_03"]]       <- d[which(d[,"parameter"]=='B_04.ON A_03'),"est"]
  par_list[["B_04_A_03_pval"]]  <- d[which(d[,"parameter"]=='B_04.ON A_03'),"pval"]
  par_list[["B_05_A_04"]]       <- d[which(d[,"parameter"]=='B_05.ON A_04'),"est"]
  par_list[["B_05_A_04_pval"]]  <- d[which(d[,"parameter"]=='B_05.ON A_04'),"pval"]
  par_list[["B_06_A_05"]]       <- d[which(d[,"parameter"]=='B_06.ON A_05'),"est"]
  par_list[["B_06_A_05_pval"]]  <- d[which(d[,"parameter"]=='B_06.ON A_05'),"pval"]

  return(par_list)
}

bivariate_quad_ALT_parameter_extraction_function <- function(d){
  #d <- wrectd_lonely_ATL
  par_list<- list()
  par_list[["cog_slope"]] <- d[which(d[,"parameter"]=='Means SA'),"est"]
  par_list[["cog_slope_pval"]] <- d[which(d[,"parameter"]=='Means SA'),"pval"]
  par_list[["cog_slope_variance"]] <- d[which(d[,"parameter"]=='Variances SA'),"est"]
  par_list[["cog_slope_variance_pval"]] <- d[which(d[,"parameter"]=='Variances SA'),"pval"]
  par_list[["cog_intercept"]] <- d[which(d[,"parameter"]=='Means IA'),"est"]
  par_list[["cog_intercept_pval"]]  <- d[which(d[,"parameter"]=='Means IA'),"pval"]
  par_list[["cog_quad"]] <- d[which(d[,"parameter"]=="Means QA"), "est"]
  par_list[["cog_quad_pval"]] <- d[which(d[,"parameter"]=="Means QA"), "pval"]
  
  par_list[["soc_slope"]] <- d[which(d[,"parameter"]=='Means SB'),"est"]
  par_list[["soc_slope_pval"]] <- d[which(d[,"parameter"]=='Means SB'),"pval"]
  par_list[["soc_slope_variance"]] <- d[which(d[,"parameter"]=='Variances SB'),"est"]
  par_list[["soc_slope_variance_pval"]] <- d[which(d[,"parameter"]=='Variances SB'),"pval"]
  par_list[["soc_intercept"]] <- d[which(d[,"parameter"]=='Means IB'),"est"]
  par_list[["soc_intercept_pval"]]  <- d[which(d[,"parameter"]=='Means IB'),"pval"]
  
  par_list[["cog_rho21"]] <- d[which(d[,"parameter"]=='A_02.ON A_01'),"est"]
  par_list[["cog_rho21_pval"]] <- d[which(d[,"parameter"]=='A_02.ON A_01'),"pval"]
  par_list[["cog_rho32"]] <- d[which(d[,"parameter"]=='A_03.ON A_02'),"est"]
  par_list[["cog_rho32_pval"]] <- d[which(d[,"parameter"]=='A_03.ON A_02'),"pval"]
  par_list[["cog_rho43"]] <- d[which(d[,"parameter"]=='A_04.ON A_03'),"est"]
  par_list[["cog_rho43_pval"]] <- d[which(d[,"parameter"]=='A_04.ON A_03'),"pval"]
  par_list[["cog_rho54"]] <- d[which(d[,"parameter"]=='A_05.ON A_04'),"est"]
  par_list[["cog_rho54_pval"]] <- d[which(d[,"parameter"]=='A_05.ON A_04'),"pval"]
  par_list[["cog_rho65"]] <- d[which(d[,"parameter"]=='A_06.ON A_05'),"est"]
  par_list[["cog_rho65_pval"]] <- d[which(d[,"parameter"]=='A_06.ON A_05'),"pval"]
  
  par_list[["soc_rho21"]] <- d[which(d[,"parameter"]=='B_02.ON B_01'),"est"]
  par_list[["soc_rho21_pval"]] <- d[which(d[,"parameter"]=='B_02.ON B_01'),"pval"]
  par_list[["soc_rho32"]] <- d[which(d[,"parameter"]=='B_03.ON B_02'),"est"]
  par_list[["soc_rho32_pval"]] <- d[which(d[,"parameter"]=='B_03.ON B_02'),"pval"]
  par_list[["soc_rho43"]] <- d[which(d[,"parameter"]=='B_04.ON B_03'),"est"]
  par_list[["soc_rho43_pval"]] <- d[which(d[,"parameter"]=='B_04.ON B_03'),"pval"]
  par_list[["soc_rho54"]] <- d[which(d[,"parameter"]=='B_05.ON B_04'),"est"]
  par_list[["soc_rho54_pval"]] <- d[which(d[,"parameter"]=='B_05.ON B_04'),"pval"]
  par_list[["soc_rho65"]] <- d[which(d[,"parameter"]=='B_06.ON B_05'),"est"]
  par_list[["soc_rho65_pval"]] <- d[which(d[,"parameter"]=='B_06.ON B_05'),"pval"]
  
  par_list[["A_02_B_01"]]       <- d[which(d[,"parameter"]=='A_02.ON B_01'),"est"]
  par_list[["A_02_B_01_pval"]]  <- d[which(d[,"parameter"]=='A_02.ON B_01'),"pval"]
  par_list[["A_03_B_02"]]       <- d[which(d[,"parameter"]=='A_03.ON B_02'),"est"]
  par_list[["A_03_B_02_pval"]]  <- d[which(d[,"parameter"]=='A_03.ON B_02'),"pval"]
  par_list[["A_04_B_03"]]       <- d[which(d[,"parameter"]=='A_04.ON B_03'),"est"]
  par_list[["A_04_B_03_pval"]]  <- d[which(d[,"parameter"]=='A_04.ON B_03'),"pval"]
  par_list[["A_05_B_04"]]       <- d[which(d[,"parameter"]=='A_05.ON B_04'),"est"]
  par_list[["A_05_B_04_pval"]]  <- d[which(d[,"parameter"]=='A_05.ON B_04'),"pval"]
  par_list[["A_06_B_05"]]       <- d[which(d[,"parameter"]=='A_06.ON B_05'),"est"]
  par_list[["A_06_B_05_pval"]]  <- d[which(d[,"parameter"]=='A_06.ON B_05'),"pval"]
  
  par_list[["B_02_A_01"]]       <- d[which(d[,"parameter"]=='B_02.ON A_01'),"est"]
  par_list[["B_02_A_01_pval"]]  <- d[which(d[,"parameter"]=='B_02.ON A_01'),"pval"]
  par_list[["B_03_A_02"]]       <- d[which(d[,"parameter"]=='B_03.ON A_02'),"est"]
  par_list[["B_03_A_02_pval"]]  <- d[which(d[,"parameter"]=='B_03.ON A_02'),"pval"]
  par_list[["B_04_A_03"]]       <- d[which(d[,"parameter"]=='B_04.ON A_03'),"est"]
  par_list[["B_04_A_03_pval"]]  <- d[which(d[,"parameter"]=='B_04.ON A_03'),"pval"]
  par_list[["B_05_A_04"]]       <- d[which(d[,"parameter"]=='B_05.ON A_04'),"est"]
  par_list[["B_05_A_04_pval"]]  <- d[which(d[,"parameter"]=='B_05.ON A_04'),"pval"]
  par_list[["B_06_A_05"]]       <- d[which(d[,"parameter"]=='B_06.ON A_05'),"est"]
  par_list[["B_06_A_05_pval"]]  <- d[which(d[,"parameter"]=='B_06.ON A_05'),"pval"]
  return(par_list)
}

Covariates_ALT_parameter_extraction_function <- function(d){
  #d <- wrectd_lonely_ATL
  par_list<- list()
  par_list[["IA_age"]] <- d[which(d[,"parameter"]=='IA.ON AGE'),"est"]
  par_list[["IA_age_se"]] <- d[which(d[,"parameter"]=='IA.ON AGE'),"se"]
  par_list[["IA_educ"]] <- d[which(d[,"parameter"]=='IA.ON EDUC'),"est"]
  par_list[["IA_educ_se"]] <- d[which(d[,"parameter"]=='IA.ON EDUC'),"se"]
  par_list[["IA_coh"]] <- d[which(d[,"parameter"]=='IA.ON COH'),"est"]
  par_list[["IA_coh_se"]]  <- d[which(d[,"parameter"]=='IA.ON COH'),"se"]
  par_list[["IA_health"]] <- d[which(d[,"parameter"]=="IA.ON HEALTH"), "est"]
  par_list[["IA_health_se"]] <- d[which(d[,"parameter"]=="IA.ON HEALTH"), "se"]
  par_list[["IA_sex"]] <- d[which(d[,"parameter"]=="IA.ON SEX"), "est"]
  par_list[["IA_sex_se"]] <- d[which(d[,"parameter"]=="IA.ON SEX"), "se"]
  par_list[["SA_age"]] <- d[which(d[,"parameter"]=="SA.ON AGE"), "est"]
  par_list[["SA_age_se"]] <- d[which(d[,"parameter"]=="SA.ON AGE"), "se"]
  par_list[["SA_educ"]] <- d[which(d[,"parameter"]=="SA.ON EDUC"), "est"]
  par_list[["SA_educ_se"]] <- d[which(d[,"parameter"]=="SA.ON EDUC"), "se"]
  par_list[["SA_coh"]] <- d[which(d[,"parameter"]=="SA.ON COH"), "est"]
  par_list[["SA_coh_se"]] <- d[which(d[,"parameter"]=="SA.ON COH"), "se"]
  par_list[["SA_health"]] <- d[which(d[,"parameter"]=="SA.ON HEALTH"), "est"]
  par_list[["SA_health_se"]] <- d[which(d[,"parameter"]=="SA.ON HEALTH"), "se"]
  par_list[["SA_sex"]]  <-  d[which(d[,"parameter"]=="SA.ON SEX"), "est"]
  par_list[["SA_sex"]] <- d[which(d[,"parameter"]=="SA.ON SEX"), "se"]
  par_list[['A1_age']] <- d[which(d[,"parameter"]=="A_01.ON AGE"), "est"]
  par_list[['A1_age_se']] <- d[which(d[,"parameter"]=="A_01.ON AGE"), "se"]
  par_list[['A1_age_p']] <- d[which(d[,"parameter"]=="A_01.ON AGE"), "pval"]

  
  par_list[["IB_age"]] <- d[which(d[,"parameter"]=='IB.ON AGE'),"est"]
  par_list[["IB_age_se"]] <- d[which(d[,"parameter"]=='IB.ON AGE'),"se"]
  par_list[["IB_educ"]] <- d[which(d[,"parameter"]=='IB.ON EDUC'),"est"]
  par_list[["IB_educ_se"]] <- d[which(d[,"parameter"]=='IB.ON EDUC'),"se"]
  par_list[["IB_coh"]] <- d[which(d[,"parameter"]=='IB.ON COH'),"est"]
  par_list[["IB_coh_se"]]  <- d[which(d[,"parameter"]=='IB.ON COH'),"se"]
  par_list[["IB_health"]] <- d[which(d[,"parameter"]=="IB.ON HEALTH"), "est"]
  par_list[["IB_health_se"]] <- d[which(d[,"parameter"]=="IB.ON HEALTH"), "se"]
  par_list[["IB_sex"]] <- d[which(d[,"parameter"]=="IB.ON SEX"), "est"]
  par_list[["IB_sex_se"]] <- d[which(d[,"parameter"]=="IB.ON SEX"), "se"]
  par_list[["SB_age"]] <- d[which(d[,"parameter"]=="SB.ON AGE"), "est"]
  par_list[["SB_age_se"]] <- d[which(d[,"parameter"]=="SB.ON AGE"), "se"]
  par_list[["SB_educ"]] <- d[which(d[,"parameter"]=="SB.ON EDUC"), "est"]
  par_list[["SB_educ_se"]] <- d[which(d[,"parameter"]=="SB.ON EDUC"), "se"]
  par_list[["SB_coh"]] <- d[which(d[,"parameter"]=="SB.ON COH"), "est"]
  par_list[["SB_coh_se"]] <- d[which(d[,"parameter"]=="SB.ON COH"), "se"]
  par_list[["SB_health"]] <- d[which(d[,"parameter"]=="SB.ON HEALTH"), "est"]
  par_list[["SB_health_se"]] <- d[which(d[,"parameter"]=="SB.ON HEALTH"), "se"]
  par_list[["SB_sex"]]  <-  d[which(d[,"parameter"]=="SB.ON SEX"), "est"]
  par_list[["SB_sex"]] <- d[which(d[,"parameter"]=="SB.ON SEX"), "se"]
  
  return(par_list)
}
# This function produces a list of predicted values needed to produce a line plot of the predicted trajectory 
# given a mean initial value for univariate unconditional ALT models
#wrectd_lone_parameters <- bivariateALT_parameter_extraction_function(wrectd_lonely_ATL)

bALT_plot_function <- function(t1cog, t1soc, parameter_list){
  #t1cog <- mean(ds_wide$wrectotd_2004, na.rm = T)
  #t1soc <- mean(ds_wide$score_loneliness_3_2004, na.rm = T)
  #parameter_list <- wrectd_lone_parameters
  
  # This part enters the mplus produced parameter values into the equations of the ALT model.
  y1 <- t1cog # 2004, this value is within rounding error of the mplus output value
  x1 <- t1soc
  
  y2 <- parameter_list[["cog_intercept"]]+2*parameter_list[["cog_slope"]]+ parameter_list[["cog_rho21"]]*y1 + parameter_list[["A_02_B_01"]]*x1  #2006
  x2 <- parameter_list[["soc_intercept"]]+2*parameter_list[["soc_slope"]]+ parameter_list[["soc_rho21"]]*y1 + parameter_list[["B_02_A_01"]]*y1  #2006
  
  y3 <- parameter_list[["cog_intercept"]]+4*parameter_list[["cog_slope"]]+ parameter_list[["cog_rho32"]]*y2 + parameter_list[["A_03_B_02"]]*x2 #2008
  x3 <- parameter_list[["soc_intercept"]]+4*parameter_list[["soc_slope"]]+ parameter_list[["soc_rho32"]]*y2 + parameter_list[["B_03_A_02"]]*y2
  
  y4 <- parameter_list[["cog_intercept"]]+6*parameter_list[["cog_slope"]]+ parameter_list[["cog_rho43"]]*y3 + parameter_list[["A_04_B_03"]]*x3  #2010
  x4 <- parameter_list[["soc_intercept"]]+6*parameter_list[["soc_slope"]]+ parameter_list[["soc_rho43"]]*y3 + parameter_list[["B_04_A_03"]]*y3 
  
  y5 <- parameter_list[["cog_intercept"]]+8*parameter_list[["cog_slope"]]+ parameter_list[["cog_rho54"]]*y4 + parameter_list[["A_05_B_04"]]*x4  #2012
  x5 <- parameter_list[["soc_intercept"]]+8*parameter_list[["soc_slope"]]+ parameter_list[["soc_rho54"]]*y4 + parameter_list[["B_05_A_04"]]*y4
  
  y6 <- parameter_list[["cog_intercept"]]+10*parameter_list[["cog_slope"]]+parameter_list[["cog_rho65"]]*y5 + parameter_list[["A_06_B_05"]]*x5  #2014
  x6 <- parameter_list[["soc_intercept"]]+10*parameter_list[["soc_slope"]]+parameter_list[["soc_rho65"]]*y5 + parameter_list[["B_06_A_05"]]*y5  #2014
  
  
  # Create a list of predicted values, for initial value at the mean
  y_values <- c(y1,y2,y3,y4,y5,y6)
  x_values <- c(x1, x2, x3, x4, x5, x6)
  values <- list()
  values[["y_values"]] <- y_values
  values[["x_values"]] <- x_values

  return(values)
}


# This function produces a list of predicted values needed to produce a line plot of the predicted trajectory 
# given a mean initial value for univariate unconditional ALT models
# uses the parameters in the format created by parameter_extraction_function()
uALT_plot_function <- function(t1value, parameter_list){
  # This part enters the mplus produced parameter values into the equations of the ALT model.
  y1 <- mean(t1value, na.rm = T) # 2004, this value is within rounding error of the mplus output value
  y2 <- parameter_list[["ALT_intercept"]]+2*parameter_list[["ALT_slope"]]+ parameter_list[["ALT_rho21"]]*y1 #2006
  y3 <- parameter_list[["ALT_intercept"]]+4*parameter_list[["ALT_slope"]]+ parameter_list[["ALT_rho32"]]*y2 #2008
  y4 <- parameter_list[["ALT_intercept"]]+6*parameter_list[["ALT_slope"]]+ parameter_list[["ALT_rho43"]]*y3 #2010
  y5 <- parameter_list[["ALT_intercept"]]+8*parameter_list[["ALT_slope"]]+ parameter_list[["ALT_rho54"]]*y4 #2012
  y6 <- parameter_list[["ALT_intercept"]]+10*parameter_list[["ALT_slope"]]+parameter_list[["ALT_rho65"]]*y5 #2014
  # Create a list of predicted values, for initial value at the mean
  values <- c(y1,y2,y3,y4,y5,y6)
  return(values)
}

lgm_quadratic_function <- function(d){
  #d <- wrecti_ATL
  linear_slope <- d[which(d[,"parameter"]=='Means SA'),"est"]
  quadratic_slope <- d[which(d[,"parameter"]=='Means QA'),"pval"]
  intercept <- d[which(d[,"parameter"]=='Means IA'),"est"]
  
  return(par_list)
}

# ----- load-data -----------
path_input     <- "./data-unshared/derived/2-dto_b.rds"
path_input_wide <- "./data-unshared/derived/2-dto_wide_b.rds"

# load the long data product of 2-prepare-biomarker-data.R
ds_long <- readRDS(path_input)

# note that the exclusion criteria for dementia is implemented in mplus so this needs to be included here. 
ds_long <- dplyr::filter(ds_long, memory_disease_ever==0) 

ds_long <- dplyr::filter(ds_long, age_baseline > 64)

# load the wide data product (the one used for mplus analysis) of 2-prepare-biomarker-data.R
ds_wide <- readRDS(path_input_wide)

# convert NA and NaN to 9999 for Mplus.
ds_wide[ds_wide==9999] <- NA

# note that the exclusion criteria for dementia is implemented in mplus so this needs to be included here. 
ds_wide <- dplyr::filter(ds_wide, memory_disease_ever==0) 

ds_wide <- dplyr::filter(ds_wide, age_baseline > 64)


# ---- descriptive-statistics --------------------------
count <- ds_long %>%
  dplyr::select_("id","year", "intage_r") %>%
  na.omit() %>%
  # dplyr::mutate_(measure_name = as.numeric(measure_name)) %>%
  dplyr::group_by_("year") %>%
  dplyr::summarize_(lazyeval::interp(~ n()))

count_sn <- ds_long %>%
  dplyr::select_("id","year", "social_support_mean") %>%
  na.omit() %>%
  dplyr::group_by_("year") %>%
  dplyr::summarize_(lazyeval::interp(~ n()))

# # creates a data frame that gives the count of each gender
gender_count <- ds_long %>%
  dplyr::group_by(year) %>%
  dplyr::count(male)

# Create a vector for each year that will comprise the table. 
var_names <- c(" ", " ", "Women (%)", "Age", "Yrs Education", "Health Conditions", "Mental status", "Word recall immediate", "Word recall delayed",
               "Psychosocial Variables", "Loneliness", "Social contact", "Social support", "Depression", "Social network")
year_2004 <- c("M (SD)"
               ,paste0("n = ", count[1,2])
               ,round((gender_count[2,3]/(gender_count[1,3]+gender_count[2,3]))*100,2)
               ,paste0(round(mean(ds_wide$intage_r_2004, na.rm = T),2), " (", round(sd(ds_wide$intage_r_2004, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$raedyrs, na.rm = T),2)," (", round(sd(ds_wide$raedyrs, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$healthcond_2004, na.rm = T),2)," (", round(sd(ds_wide$healthcond_2004, na.rm = T),2),")") 
               ,paste0(round(mean(ds_wide$mentalstatus_tot_2004, na.rm = T), 2)," (", round(sd(ds_wide$mentalstatus_tot_2004, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$wrectoti_2004, na.rm = T), 2)," (", round(sd(ds_wide$wrectoti_2004, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$wrectotd_2004, na.rm = T), 2)," (", round(sd(ds_wide$wrectotd_2004, na.rm = T),2),")")
               ,paste0("n = ", count_sn[1,2])
               ,paste0(round(mean(ds_wide$score_loneliness_3_2004, na.rm = T), 2)," (", round(sd(ds_wide$score_loneliness_3_2004, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$social_contact_total_2004, na.rm = T), 2)," (", round(sd(ds_wide$social_contact_total_2004, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$social_support_mean_2004, na.rm = T), 2)," (", round(sd(ds_wide$social_support_mean_2004, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$dep_total_2004, na.rm = T), 2)," (", round(sd(ds_wide$dep_total_2004, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$socialnetwork_total_2004, na.rm = T), 2)," (", round(sd(ds_wide$socialnetwork_total_2004, na.rm = T),2),")")
)

year_2006 <- c("M (SD)"
                ,paste0("n = ", count[2,2])
               ,round((gender_count[3,3]/(gender_count[1,3]+gender_count[3,3]))*100,2)
               ,paste0(round(mean(ds_wide$intage_r_2006, na.rm = T),2), " (", round(sd(ds_wide$intage_r_2006, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$raedyrs, na.rm = T),2)," (", round(sd(ds_wide$raedyrs, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$healthcond_2006, na.rm = T),2)," (", round(sd(ds_wide$healthcond_2006, na.rm = T),2),")") 
               ,paste0(round(mean(ds_wide$mentalstatus_tot_2006, na.rm = T), 2)," (", round(sd(ds_wide$mentalstatus_tot_2006, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$wrectoti_2006, na.rm = T), 2)," (", round(sd(ds_wide$wrectoti_2006, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$wrectotd_2006, na.rm = T), 2)," (", round(sd(ds_wide$wrectotd_2006, na.rm = T),2),")")
               ,paste0("n = ", count_sn[2,2])
               ,paste0(round(mean(ds_wide$score_loneliness_3_2006, na.rm = T), 2)," (", round(sd(ds_wide$score_loneliness_3_2006, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$social_contact_total_2006, na.rm = T), 2)," (", round(sd(ds_wide$social_contact_total_2006, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$social_support_mean_2006, na.rm = T), 2)," (", round(sd(ds_wide$social_support_mean_2006, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$dep_total_2006, na.rm = T), 2)," (", round(sd(ds_wide$dep_total_2006, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$socialnetwork_total_2006, na.rm = T), 2)," (", round(sd(ds_wide$socialnetwork_total_2006, na.rm = T),2),")")
)

year_2008 <- c("M (SD)"
                ,paste0("n = ", count[3,2])
               ,round((gender_count[4,3]/(gender_count[4,3]+gender_count[4,3]))*100,2)
               ,paste0(round(mean(ds_wide$intage_r_2008, na.rm = T),2), " (", round(sd(ds_wide$intage_r_2008, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$raedyrs, na.rm = T),2)," (", round(sd(ds_wide$raedyrs, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$healthcond_2008, na.rm = T),2)," (", round(sd(ds_wide$healthcond_2008, na.rm = T),2),")") 
               ,paste0(round(mean(ds_wide$mentalstatus_tot_2008, na.rm = T), 2)," (", round(sd(ds_wide$mentalstatus_tot_2008, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$wrectoti_2008, na.rm = T), 2)," (", round(sd(ds_wide$wrectoti_2008, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$wrectotd_2008, na.rm = T), 2)," (", round(sd(ds_wide$wrectotd_2008, na.rm = T),2),")")
               ,paste0("n = ", count_sn[3,2])
               ,paste0(round(mean(ds_wide$score_loneliness_3_2008, na.rm = T), 2)," (", round(sd(ds_wide$score_loneliness_3_2008, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$social_contact_total_2008, na.rm = T), 2)," (", round(sd(ds_wide$social_contact_total_2008, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$social_support_mean_2008, na.rm = T), 2)," (", round(sd(ds_wide$social_support_mean_2008, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$dep_total_2008, na.rm = T), 2)," (", round(sd(ds_wide$dep_total_2008, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$socialnetwork_total_2008, na.rm = T), 2)," (", round(sd(ds_wide$socialnetwork_total_2008, na.rm = T),2),")")
)

year_2010 <- c("M (SD)"
               ,paste0("n = ", count[4,2])
               ,round((gender_count[5,3]/(gender_count[5,3]+gender_count[5,3]))*100,2)
               ,paste0(round(mean(ds_wide$intage_r_2010, na.rm = T),2), " (", round(sd(ds_wide$intage_r_2010, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$raedyrs, na.rm = T),2)," (", round(sd(ds_wide$raedyrs, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$healthcond_2010, na.rm = T),2)," (", round(sd(ds_wide$healthcond_2010, na.rm = T),2),")") 
               ,paste0(round(mean(ds_wide$mentalstatus_tot_2010, na.rm = T), 2)," (", round(sd(ds_wide$mentalstatus_tot_2010, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$wrectoti_2010, na.rm = T), 2)," (", round(sd(ds_wide$wrectoti_2010, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$wrectotd_2010, na.rm = T), 2)," (", round(sd(ds_wide$wrectotd_2010, na.rm = T),2),")")
               ,paste0("n = ", count_sn[4,2])
               ,paste0(round(mean(ds_wide$score_loneliness_3_2010, na.rm = T), 2)," (", round(sd(ds_wide$score_loneliness_3_2010, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$social_contact_total_2010, na.rm = T), 2)," (", round(sd(ds_wide$social_contact_total_2010, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$social_support_mean_2010, na.rm = T), 2)," (", round(sd(ds_wide$social_support_mean_2010, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$dep_total_2010, na.rm = T), 2)," (", round(sd(ds_wide$dep_total_2010, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$socialnetwork_total_2010, na.rm = T), 2)," (", round(sd(ds_wide$socialnetwork_total_2010, na.rm = T),2),")")
)

year_2012 <- c("M (SD)"
               ,paste0("n = ", count[5,2])
               ,round((gender_count[6,3]/(gender_count[6,3]+gender_count[6,3]))*100,2)
               ,paste0(round(mean(ds_wide$intage_r_2012, na.rm = T),2), " (", round(sd(ds_wide$intage_r_2012, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$raedyrs, na.rm = T),2)," (", round(sd(ds_wide$raedyrs, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$healthcond_2012, na.rm = T),2)," (", round(sd(ds_wide$healthcond_2012, na.rm = T),2),")") 
               ,paste0(round(mean(ds_wide$mentalstatus_tot_2012, na.rm = T), 2)," (", round(sd(ds_wide$mentalstatus_tot_2012, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$wrectoti_2012, na.rm = T), 2)," (", round(sd(ds_wide$wrectoti_2012, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$wrectotd_2012, na.rm = T), 2)," (", round(sd(ds_wide$wrectotd_2012, na.rm = T),2),")")
               ,paste0("n = ", count_sn[5,2])
               ,paste0(round(mean(ds_wide$score_loneliness_3_2012, na.rm = T), 2)," (", round(sd(ds_wide$score_loneliness_3_2012, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$social_contact_total_2012, na.rm = T), 2)," (", round(sd(ds_wide$social_contact_total_2012, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$social_support_mean_2012, na.rm = T), 2)," (", round(sd(ds_wide$social_support_mean_2012, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$dep_total_2012, na.rm = T), 2)," (", round(sd(ds_wide$dep_total_2012, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$socialnetwork_total_2012, na.rm = T), 2)," (", round(sd(ds_wide$socialnetwork_total_2012, na.rm = T),2),")")
)

year_2014 <- c("M (SD)"
               ,paste0("n = ", count[6,2])
               ,round((gender_count[7,3]/(gender_count[7,3]+gender_count[7,3]))*100,2)
               ,paste0(round(mean(ds_wide$intage_r_2014, na.rm = T),2), " (", round(sd(ds_wide$intage_r_2014, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$raedyrs, na.rm = T),2)," (", round(sd(ds_wide$raedyrs, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$healthcond_2014, na.rm = T),2)," (", round(sd(ds_wide$healthcond_2014, na.rm = T),2),")") 
               ,paste0(round(mean(ds_wide$mentalstatus_tot_2014, na.rm = T), 2)," (", round(sd(ds_wide$mentalstatus_tot_2014, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$wrectoti_2014, na.rm = T), 2)," (", round(sd(ds_wide$wrectoti_2014, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$wrectotd_2014, na.rm = T), 2)," (", round(sd(ds_wide$wrectotd_2014, na.rm = T),2),")")
               ,paste0("n = ", count_sn[6,2])
               ,paste0(round(mean(ds_wide$score_loneliness_3_2014, na.rm = T), 2)," (", round(sd(ds_wide$score_loneliness_3_2014, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$social_contact_total_2014, na.rm = T), 2)," (", round(sd(ds_wide$social_contact_total_2014, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$social_support_mean_2014, na.rm = T), 2)," (", round(sd(ds_wide$social_support_mean_2014, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$dep_total_2014, na.rm = T), 2)," (", round(sd(ds_wide$dep_total_2014, na.rm = T),2),")")
               ,paste0(round(mean(ds_wide$socialnetwork_total_2014, na.rm = T), 2)," (", round(sd(ds_wide$socialnetwork_total_2014, na.rm = T),2),")")
)

desc <- as.data.frame(cbind(var_names, year_2004, year_2006, year_2008, year_2010, year_2012, year_2014))

colnames(desc) <- c("", "2004", "2006", "2008", "2010", "2012", "2014")
rownames(desc) <- c()
apa_table(desc, landscape = TRUE, 
          caption = "Descriptive statistics by year")

# htmlTable(desc 
#           ,align="lccc"
#           ,header = paste(c(" ", "M (SD)", "M (SD)", "M (SD)", "M (SD)", "M (SD)", "M (SD)"))
#           ,cgroup = c("","2004", "2006", "2008", "2010", "2012", "2014")
#           ,n.cgroup = c(1,1,1,1,1,1)
#           ,css.tspanner = FALSE
#           ,ctable = TRUE
#           ,caption = "Table 1. Descriptive statistics by year"
# )


#---- immediate-word-recall-model-summaries --------------------------------------------------------------
# Extract the fit indices of relevant models
wrecti_fit_series4 <- extractModelSummaries("./output/univariate-models-nodem-65plus/wrectoti")

wrecti_fit_series4["CM"] <- 4
wrecti_fit_series4[1:3, "CM"] <- "-"

cm_row <- wrecti_fit_series4$CM
wrecti_fit_series4_table <- compare_models_function(wrecti_fit_series4, cm_row)


# print(xtable(wrecti_fit_series4_table, caption = "Model Fit Comparison for Immediate Word Recall",landscape = TRUE),
#       caption.placement="top", type = "latex")


colnames(wrecti_fit_series4_table) <- c("Model", "$\\chi^2$", "df", "CM", "$\\Delta\\chi^2$", "df$\\Delta$", "CFI", "TLI", "RMSEA", "SRMR")
# wrecti<- xtable(wrecti_fit_series4_table, caption = "Fit indices")
# print(wrecti, sanitize.colnames.function = function(x) {x})

apa_table.word(wrecti_fit_series4_table, caption = "Model Fit Indices for Immediate Word Recall")



#----immediate-word-recall-results-------
wrecti_ATLparameters <- extractModelParameters("./output/univariate-models-nodem-65plus/wrectoti/u04_nocov_wrectoti.out")

wrecti_ATL <- as.data.frame(wrecti_ATLparameters[[1]])

# columns to paste together
cols <- c( 'paramHeader' , 'param') 

# create a new column `parameter` with the two name columns paramHeader and param collapsed together
wrecti_ATL$parameter <- paste(wrecti_ATL$paramHeader,wrecti_ATL$param)

wrecti_ATL <- wrecti_ATL %>% dplyr::select(parameter, est, se, pval)
write.csv(wrecti_ATL, file = "./output/univariate-models/wrecti_model_parameters.csv")
read.csv(file = "./output/univariate-models/wrecti_model_parameters.csv")


# wrectiATL_slope <- wrecti_ATL[which(wrecti_ATL[,"parameter"]=='A_02.ON A_01'),2]
# which(d[,"parameter"]=='Means SA'), wrecti_ATLwrecti_ATL["est"]

ALT_slope <- wrecti_ATL[which(wrecti_ATL[,"parameter"]=='Means SA'),"est"]


#----lgm-quadratic-immediate-word-recall-plot
wrecti_LGM_quadratic_parameters <- extractModelParameters("./output/univariate-models-nodem-65plus/wrectoti/u03_nocov_wrectoti.out")

wrecti_LGM_quadratic <- as.data.frame(wrecti_LGM_quadratic_parameters[[1]])
# create a new column `parameter` with the two name columns paramHeader and param collapsed together

wrecti_LGM_quadratic$parameter <- paste(wrecti_LGM_quadratic$paramHeader,wrecti_LGM_quadratic$param)

wrecti_LGM_quadratic <- wrecti_LGM_quadratic %>% dplyr::select(parameter, est, se, pval)
d<-wrecti_LGM_quadratic
linear_slope <- d[which(d[,"parameter"]=='Means SA'),"est"]
quadratic_slope <- d[which(d[,"parameter"]=='Means QA'),"est"]
intercept <- d[which(d[,"parameter"]=='Means IA'),"est"]

z1 <- intercept+linear_slope*0+quadratic_slope*0^2
z2 <- intercept+linear_slope*2+quadratic_slope*2^2
z3 <- intercept+linear_slope*4+quadratic_slope*4^2
z4 <- intercept+linear_slope*6+quadratic_slope*6^2
z5 <- intercept+linear_slope*8+quadratic_slope*8^2
z6 <- intercept+linear_slope*10+quadratic_slope*10^2

# Create the data for the chart.
means_q <- c(z1,z2,z3,z4,z5,z6)

wrecti_lgm_quadratic_plot <- plot(means_q,type = "o", col = "BLACK", xlab = "Wave", ylab = "Predicted Mean",
                                  main = "Immediate Word Recall LGM Quadratic")

# Bivariate model results

#----immediate-word-recall-social-network-summary--------------------------
# TASKS TO COMPLETE: RENAME THE FILES SO THE TABLE PRODUCED CAN BE PUT IN MANUSCRIPT
wrectoti_social_network_summary<- extractModelSummaries("./output/bivariate-models-nodem-65plus/predetermined-models/wrectoti-socialnetwork_total/converged-models")
wrectoti_social_network_summary["CM"] <- 4
wrectoti_social_network_summary[1:3, "CM"] <- "-"
#wrectoti_social_network_summary[10:11, "CM"] <- "9"


cm_row <- wrectoti_social_network_summary$CM

wrectoti_social_network_table <- compare_models_function(wrectoti_social_network_summary, cm_row)

# Note that model 15 appears to be the most parsimonious best fitting model. 

colnames(wrectoti_social_network_table) <- c("Model", "$\\chi^2$", "df", "CM", "$\\Delta\\chi^2$", "df$\\Delta$", "CFI", "TLI", "RMSEA", "SRMR")

apa_table.word(wrectoti_social_network_table, caption = "Model Fit Indices for Immediate Word Recall and Social Network")

#----immediate-word-recall-social-network-results--------------------------
# model 11 the ALT model with no time specific correlations and fixed autoregressions for immediate word recall is the most supported model 
wrecti_sn_ATLparameters <- extractModelParameters("./output/bivariate-models-nodem-65plus/predetermined-models/wrectoti-socialnetwork_total/converged-models/m11b_nocov_wrectoti_socialnetwork_total.out")

wrecti_sn_ATLparameters <- as.data.frame(wrecti_sn_ATLparameters[[1]]) 

# # create a new column `parameter` with the two name columns paramHeader and param collapsed together
wrecti_sn_ATLparameters$parameter <- paste(wrecti_sn_ATLparameters$paramHeader,wrecti_sn_ATLparameters$param)

# select relevant parameters and then save file.
# this allows the parameters to be accessible to the R markdown file for writing results. 
wrecti_sn_ATL <- wrecti_sn_ATLparameters %>% dplyr::select(parameter, est, se, pval)
wrecti_sn_ATL$pval <- sprintf("%.3f", wrecti_sn_ATL$pval)
wrecti_sn_ATL$est <- sprintf("%.3f", wrecti_sn_ATL$est)
write.csv(wrecti_sn_ATL, file = "./output/bivariate-models-nodem-65plus/wrecti_social_network_model_parameters.csv")


#---- immediate-word-recall-social-support-summary --------------------------
# TASKS TO COMPLETE: RENAME THE FILES SO THE TABLE PRODUCED CAN BE PUT IN MANUSCRIPT
wrectoti_social_support_summary<- extractModelSummaries("./output/bivariate-models-nodem-65plus/predetermined-models/wrectoti-social_support_mean/converged-models")
wrectoti_social_support_summary["CM"] <- 3
wrectoti_social_support_summary[1:3, "CM"] <- "-"
wrectoti_social_support_summary[5, "CM"] <- "4"
wrectoti_social_support_summary[6, "CM"] <- "5"
wrectoti_social_support_summary[7, "CM"] <- "5"
wrectoti_social_support_summary[8, "CM"] <- "5"
wrectoti_social_support_summary[9, "CM"] <- "5"
wrectoti_social_support_summary[10, "CM"] <- "5"
wrectoti_social_support_summary[11, "CM"] <- "5"
cm_row <- wrectoti_social_support_summary$CM

wrectoti_social_support_table <- compare_models_function(wrectoti_social_support_summary, cm_row)
# Note that model 15 appears to be the most parsimonious best fitting model. 

colnames(wrectoti_social_support_table) <- c("Model", "$\\chi^2$", "df", "CM", "$\\Delta\\chi^2$", "df$\\Delta$", "CFI", "TLI", "RMSEA", "SRMR")

apa_table.word(wrectoti_social_support_table, caption = "Model Fit Indices for Immediate Word Recall and Social Network")

#----immediate-word-recall-social-support-results--------------------------
# model 11 the ALT model with no time specific correlations and fixed autoregressions for immediate word recall is the most supported model 
wrecti_socsup_ATLparameters <- extractModelParameters("./output/bivariate-models-nodem-65plus/predetermined-models/m13b_nocov_wrectoti_social_support_mean.inp")

wrecti_socsup_ATLparameters <- as.data.frame(wrecti_socsup_ATLparameters[[1]]) 

# # create a new column `parameter` with the two name columns paramHeader and param collapsed together
wrecti_socsup_ATLparameters$parameter <- paste(wrecti_socsup_ATLparameters$paramHeader,wrecti_socsup_ATLparameters$param)

# select relevant parameters and then save file.
# this allows the parameters to be accessible to the R markdown file for writing results. 
wrecti_socsup_ATL <- wrecti_socsup_ATLparameters %>% dplyr::select(parameter, est, se, pval)
write.csv(wrecti_socsup_ATL, file = "./output/bivariate-models-nodem-65plus/wrecti_social_support_model_parameters.csv")

# model 3 is the full ALT model save this as well for comparison
wrecti_socsup_fullALT_parameters <- extractModelParameters("./output/bivariate-models-nodem-65plus/wrectoti-social_support_mean/m03_nocov_wrectoti_social_support_mean.out")

wrecti_socsup_fullALT_parameters <- as.data.frame(wrecti_socsup_fullALT_parameters[[1]]) 

# # create a new column `parameter` with the two name columns paramHeader and param collapsed together
wrecti_socsup_fullALT_parameters$parameter <- paste(wrecti_socsup_fullALT_parameters$paramHeader,wrecti_socsup_fullALT_parameters$param)

# select relevant parameters and then save file.
# this allows the parameters to be accessible to the R markdown file for writing results. 
wrecti_socsup_fullALT <- wrecti_socsup_fullALT_parameters %>% dplyr::select(parameter, est, se, pval)
write.csv(wrecti_socsup_fullALT, file = "./output/bivariate-models-nodem-65plus/wrecti_social_suppport_fullmodel_parameters.csv")
bivariateALT_parameter_extraction_function(wrecti_socsup_fullALT)

#---- immediate-word-recall-social-contact-summary --------------------------
# TASKS TO COMPLETE: RENAME THE FILES SO THE TABLE PRODUCED CAN BE PUT IN MANUSCRIPT
wrectoti_social_contact_summary<- extractModelSummaries("./output/bivariate-models-nodem-65plus/predetermined-models/wrectoti-social_contact_total/converged-models")
wrectoti_social_contact_summary["CM"] <- 4
wrectoti_social_contact_summary[1:3, "CM"] <- "-"
wrectoti_social_contact_summary[4, "CM"] <- 3
wrectoti_social_contact_summary[6, "CM"] <- 5
wrectoti_social_contact_summary[7, "CM"] <- 4
wrectoti_social_contact_summary[8, "CM"] <- 5
wrectoti_social_contact_summary[9, "CM"] <- 6
wrectoti_social_contact_summary[10, "CM"] <- 5
cm_row <- wrectoti_social_contact_summary$CM

wrectoti_social_contact_table <- compare_models_function(wrectoti_social_contact_summary, cm_row)
# Note that model 15 appears to be the most parsimonious best fitting model. 

colnames(wrectoti_social_contact_table) <- c("Model", "$\\chi^2$", "df", "CM", "$\\Delta\\chi^2$", "df$\\Delta$", "CFI", "TLI", "RMSEA", "SRMR")

apa_table.word(wrectoti_social_contact_table, caption = "Model Fit Indices for Immediate Word Recall and Social Contact")

#---- immediate-word-recall-social-contact-results--------------------------
# model 11 the ALT model with no time specific correlations and fixed autoregressions for immediate word recall is the most supported model 
wrecti_social_contact_ATLparameters <- extractModelParameters("./output/bivariate-models-nodem-65plus/wrectoti-social_contact_total/converged-models/m11b_nocov_wrectoti_social_contact_total.out")

wrecti_social_contact_ATLparameters <- as.data.frame(wrecti_social_contact_ATLparameters[[1]]) 

# # create a new column `parameter` with the two name columns paramHeader and param collapsed together
wrecti_social_contact_ATLparameters$parameter <- paste(wrecti_social_contact_ATLparameters$paramHeader,wrecti_social_contact_ATLparameters$param)

# select relevant parameters and then save file.
# this allows the parameters to be accessible to the R markdown file for writing results. 
wrecti_social_contact_ATL <- wrecti_social_contact_ATLparameters %>% dplyr::select(parameter, est, se, pval)
write.csv(wrecti_social_contact_ATL, file = "./output/bivariate-models-nodem-65plus/wrecti_social_contact_model_parameters.csv")

# model 3 is the full ALT model save this as well for comparison
wrecti_social_contact_fullALT_parameters <- extractModelParameters("./output/bivariate-models-nodem-65plus/wrectoti-social_contact_total/m03_nocov_wrectoti_social_contact_total.out")

wrecti_social_contact_fullALT_parameters <- as.data.frame(wrecti_social_contact_fullALT_parameters[[1]]) 

# # create a new column `parameter` with the two name columns paramHeader and param collapsed together
wrecti_social_contact_fullALT_parameters$parameter <- paste(wrecti_social_contact_fullALT_parameters$paramHeader,wrecti_social_contact_fullALT_parameters$param)

# select relevant parameters and then save file.
# this allows the parameters to be accessible to the R markdown file for writing results. 
wrecti_social_contact_fullALT <- wrecti_social_contact_fullALT_parameters %>% dplyr::select(parameter, est, se, pval)
write.csv(wrecti_social_contact_fullALT, file = "./output/bivariate-models-nodem-65plus/wrecti_social_contact_fullmodel_parameters.csv")
bivariateALT_parameter_extraction_function(wrecti_social_contact_fullALT)

# Model 20 is the covariate model
wrecti_social_contact_covALT_parameters <- extractModelParameters("./output/bivariate-models-nodem-65plus/wrectoti-social_contact_total/m20_aechs_wrectoti_social_contact_total.out")

wrecti_social_contact_covALT_parameters <- as.data.frame(wrecti_social_contact_covALT_parameters[[1]]) 
#---- immediate-word-recall-loneliness-summary --------------------------
# TASKS TO COMPLETE: RENAME THE FILES SO THE TABLE PRODUCED CAN BE PUT IN MANUSCRIPT
wrectoti_loneliness_summary<- extractModelSummaries("./output/bivariate-models-nodem-65plus/predetermined-models/wrectoti-score_loneliness_3/converged-models")
wrectoti_loneliness_summary["CM"] <- 4
wrectoti_loneliness_summary[1:4, "CM"] <- "3"
wrectoti_loneliness_summary[4, "CM"] <- "3"
wrectoti_loneliness_summary[5, "CM"] <- "4"
wrectoti_loneliness_summary[6, "CM"] <- "5"
wrectoti_loneliness_summary[7, "CM"] <- "6"
wrectoti_loneliness_summary[8, "CM"] <- "7"
wrectoti_loneliness_summary[9, "CM"] <- "8"
wrectoti_loneliness_summary[10, "CM"] <- "9"
wrectoti_loneliness_summary[11, "CM"] <- "10"
cm_row <- wrectoti_loneliness_summary$CM

wrectoti_loneliness_table <- compare_models_function(wrectoti_loneliness_summary, cm_row)
# Note that model 15 appears to be the most parsimonious best fitting model. 

colnames(wrectoti_loneliness_table) <- c("Model", "$\\chi^2$", "df", "CM", "$\\Delta\\chi^2$", "df$\\Delta$", "CFI", "TLI", "RMSEA", "SRMR")

apa_table.word(wrectoti_loneliness_table, caption = "Model Fit Indices for Immediate Word Recall and Loneliness")

#---- immediate-word-recall-loneliness-results--------------------------
# model 14b the ALT model with no loneliness slope, no time specific correlations and fixed autoregressions for immediate word recall and loneliness is the most supported model 
wrecti_loneliness_ATLparameters <- extractModelParameters("./output/bivariate-models-nodem-65plus/predetermined-models/wrectoti-score_loneliness_3/converged-models/m13b_nocov_wrectoti_score_loneliness_3.out")

wrecti_loneliness_ATLparameters <- as.data.frame(wrecti_loneliness_ATLparameters[[1]]) 

# # create a new column `parameter` with the two name columns paramHeader and param collapsed together
wrecti_loneliness_ATLparameters$parameter <- paste(wrecti_loneliness_ATLparameters$paramHeader,wrecti_loneliness_ATLparameters$param)

# select relevant parameters and then save file.
# this allows the parameters to be accessible to the R markdown file for writing results. 
wrecti_loneliness_ATL <- wrecti_loneliness_ATLparameters %>% dplyr::select(parameter, est, se, pval)
write.csv(wrecti_loneliness_ATL, file = "./output/bivariate-models-nodem-65plus/predetermined-models/wrecti_loneliness_model_parameters.csv")

# model 3 is the full ALT model save this as well for comparison
wrecti_loneliness_fullALT_parameters <- extractModelParameters("./output/bivariate-models-nodem-65plus/wrectoti-score_loneliness_3/m03_nocov_wrectoti_score_loneliness_3.out")

wrecti_loneliness_fullALT_parameters <- as.data.frame(wrecti_loneliness_fullALT_parameters[[1]]) 

# # create a new column `parameter` with the two name columns paramHeader and param collapsed together
wrecti_loneliness_fullALT_parameters$parameter <- paste(wrecti_loneliness_fullALT_parameters$paramHeader,wrecti_loneliness_fullALT_parameters$param)

# select relevant parameters and then save file.
# this allows the parameters to be accessible to the R markdown file for writing results. 
wrecti_loneliness_fullALT <- wrecti_loneliness_fullALT_parameters %>% dplyr::select(parameter, est, se, pval)

write.csv(wrecti_loneliness_fullALT, file = "./output/bivariate-models-nodem-65plus/wrecti_loneliness_fullmodel_parameters.csv")
bivariateALT_parameter_extraction_function(wrecti_loneliness_fullALT)

# covariate model
wrecti_loneliness_covALT_parameters <- extractModelParameters("./output/bivariate-models-nodem-65plus/wrectoti-score_loneliness_3/m20_aechs_wrectoti_score_loneliness_3.out")


#----delayed-word-recall-loneliness-summary--------------------------
wrectotd_loneliness_summary<- extractModelSummaries("./output/bivariate-models-nodem-65plus/predetermined-models/wrectotd-score_loneliness_3/converged-models")
wrectotd_loneliness_summary["CM"] <- 5
wrectotd_loneliness_summary[1:3, "CM"] <- "-"
wrectotd_loneliness_summary[5, "CM"] <- "4"
wrectotd_loneliness_summary[8, "CM"] <- "6"
wrectotd_loneliness_summary[9, "CM"] <- "8"
wrectotd_loneliness_summary[11, "CM"] <- "9"
wrectotd_loneliness_summary[12, "CM"] <- "11"
wrectotd_loneliness_summary[13, "CM"] <- "11"
cm_row <- wrectotd_loneliness_summary$CM

wrectotd_loneliness_table <- compare_models_function(wrectotd_loneliness_summary, cm_row)

colnames(wrectotd_loneliness_table) <- c("Model", "$\\chi^2$", "df", "CM", "$\\Delta\\chi^2$", "df$\\Delta$", "CFI", "TLI", "RMSEA", "SRMR")

apa_table.word(wrectotd_loneliness_table, caption = "Model Fit Indices for Delayed Word Recall and Loneliness")

# ALT-12 model with fixed autoregressions for both cognitive and social. 
# Add covariates to this model. 

#---- delayed-word-recall-loneliness-results-------------
# model 12 the ALT model with no time specific correlations and fixed autoregressions is the final model
wrectd_lonely_ATLparameters <- extractModelParameters("./output/bivariate-models-nodem-65plus/predetermined-models/wrectotd-score_loneliness_3/m13b_nocov_wrectotd_score_loneliness_3.out")

wrectd_lonely_ATL <- as.data.frame(wrectd_lonely_ATLparameters[[1]]) 

# # create a new column `parameter` with the two name columns paramHeader and param collapsed together
wrectd_lonely_ATL$parameter <- paste(wrectd_lonely_ATL$paramHeader,wrectd_lonely_ATL$param)

# select relevant parameters and then save file.
# this allows the parameters to be accessible to the R markdown file for writing results. 
wrectd_lonely_ATL <- wrectd_lonely_ATL %>% dplyr::select(parameter, est, se, pval)
write.csv(wrectd_lonely_ATL, file = "./output/bivariate-models-nodem-65plus/wrectd_loneliness_model_parameters.csv")

# also create a csv with bivariate wrectd and loneliness full ALT model parameters for comparison
wrectd_lonely_fullALTparameters <- extractModelParameters("./output/bivariate-models-nodem-65plus/wrectotd-score_loneliness_3/m03_nocov_wrectotd_score_loneliness_3.out")

wrectd_lonely_fullALT <- as.data.frame(wrectd_lonely_fullALTparameters[[1]])

# # create a new column `parameter` with the two name columns paramHeader and param collapsed together
wrectd_lonely_fullALT$parameter <- paste(wrectd_lonely_fullALT$paramHeader,wrectd_lonely_fullALT$param)

# select relevant parameters and then save file.
wrectd_lonely_fullALT <- wrectd_lonely_fullALT %>% dplyr::select(parameter, est, se, pval)

write.csv(wrectd_lonely_fullALT, file = "./output/univariate-models-nodem-65plus/wrectd_loneliness_fullmodel_parameters.csv")

#----wrectd-lonely-covariate-model--------
wrectd_lonely_ALTcov <- readModels("./output/bivariate-models-nodem-65plus/wrectotd-score_loneliness_3/m20_aechs_wrectotd_score_loneliness_3.out")

wrectd_lonely_ALTcov_parameters <- wrectd_lonely_ALTcov$parameters$unstandardized

# # create a new column `parameter` with the two name columns paramHeader and param collapsed together
wrectd_lonely_ALTcov_parameters$parameter <- paste(wrectd_lonely_ALTcov_parameters$paramHeader,wrectd_lonely_ALTcov_parameters$param)

# define a vector of variables to keep for each parameter covariate regression
keepvars <- c('est', 'se', 'pval')
a1 <- wrectd_lonely_ALTcov_parameters[which(wrectd_lonely_ALTcov_parameters[,"parameter"]=="A_01.ON AGE"):which(wrectd_lonely_ALTcov_parameters[,"parameter"]=="A_01.ON SEX"),]
a1 <- a1[keepvars]

b1 <- wrectd_lonely_ALTcov_parameters[which(wrectd_lonely_ALTcov_parameters[,"parameter"]=="B_01.ON AGE"):which(wrectd_lonely_ALTcov_parameters[,"parameter"]=="B_01.ON SEX"),]
b1 <- b1[keepvars]

a1_int <- wrectd_lonely_ALTcov_parameters[which(wrectd_lonely_ALTcov_parameters[,"parameter"]=="IA.ON AGE"):which(wrectd_lonely_ALTcov_parameters[,"parameter"]=="IA.ON SEX"),]
a1_int <- a1_int[keepvars]

b1_int <- wrectd_lonely_ALTcov_parameters[which(wrectd_lonely_ALTcov_parameters[,"parameter"]=="IB.ON AGE"):which(wrectd_lonely_ALTcov_parameters[,"parameter"]=="IB.ON SEX"),]
b1_int <- b1_int[keepvars]

a1_s <- wrectd_lonely_ALTcov_parameters[which(wrectd_lonely_ALTcov_parameters[,"parameter"]=="SA.ON AGE"):which(wrectd_lonely_ALTcov_parameters[,"parameter"]=="SA.ON SEX"),]
a1_s <- a1_s[keepvars]

b1_s <- wrectd_lonely_ALTcov_parameters[which(wrectd_lonely_ALTcov_parameters[,"parameter"]=="SB.ON AGE"):which(wrectd_lonely_ALTcov_parameters[,"parameter"]=="SB.ON SEX"),]
b1_s <- b1_s[keepvars]

labels_column <- c('Age', 'Education', 'Cohort', 'Health', 'Sex')
wrectd_lonely_ALTcov_table <- cbind(labels_column, a1, a1_int, a1_s, b1, b1_int, b1_s)

#Covariates_ALT_parameter_extraction_function(wrectd_lonely_ALTcov_parameters)
# Covariate model effects table 
# Create a table from the extracted parameters that displays the covariate effects.

colnames(wrectd_lonely_ALTcov_table) <- c("", "_b_", "s.e", "_p_", "_b_", "s.e", "_p_", "_b_", "s.e", "_p_","_b_", "s.e", "_p_","_b_", "s.e", "_p_","_b_", "s.e", "_p_")
apa_table(wrectd_lonely_ALTcov_table, caption = "Results from the Regression of the Final Conditional ALT Parameters on the Predictors")

#----delayed-word-recall-social-contact-summary--------------------------
# TASKS TO COMPLETE: RENAME THE FILES SO THE TABLE PRODUCED CAN BE PUT IN MANUSCRIPT
wrectotd_social_contact_summary<- extractModelSummaries("./output/bivariate-models-nodem-65plus/predetermined-models/wrectotd-social_contact_total/converged-models")
wrectotd_social_contact_summary["CM"] <- 4
wrectotd_social_contact_summary[1:3, "CM"] <- "-"
wrectotd_social_contact_summary[6, "CM"] <- 5
wrectotd_social_contact_summary[7, "CM"] <- 6
wrectotd_social_contact_summary[8, "CM"] <- 6
cm_row <- wrectotd_social_contact_summary$CM

wrectotd_social_contact_table <- compare_models_function(wrectotd_social_contact_summary, cm_row)

colnames(wrectotd_social_contact_table) <- c("Model", "$\\chi^2$", "df", "CM", "$\\Delta\\chi^2$", "df$\\Delta$", "CFI", "TLI", "RMSEA", "SRMR")

apa_table.word(wrectotd_social_contact_table, caption = "Model Fit Indices for Delayed Word Recall and Loneliness")

#----delayed-word-recall-social-contact-results--------------------------
# model 12 the ALT model with no time specific correlations and fixed autoregressions is the final model
wrectd_sc_ATLparameters <- extractModelParameters("./output/bivariate-models-nodem-65plus/predetermined-models/wrectotd-social_contact_total/converged-models/m11b_nocov_wrectotd_social_contact_total.out")

wrectd_sc_ATLparameters <- as.data.frame(wrectd_sc_ATLparameters[[1]]) 

# # create a new column `parameter` with the two name columns paramHeader and param collapsed together
wrectd_sc_ATLparameters$parameter <- paste(wrectd_sc_ATLparameters$paramHeader,wrectd_sc_ATLparameters$param)

# select relevant parameters and then save file.
# this allows the parameters to be accessible to the R markdown file for writing results. 
wrectd_sc_ATL <- wrectd_sc_ATLparameters %>% dplyr::select(parameter, est, se, pval)
write.csv(wrectd_sc_ATL, file = "./output/bivariate-models-nodem-65plus/wrectd_social_contact_model_parameters.csv")

# The full model for comparison
wrectd_sc_ATLfullparameters <- extractModelParameters("./output/bivariate-models-nodem-65plus/wrectotd-social_contact_total/m03_nocov_wrectotd_social_contact_total.out")

wrectd_sc_ATLfullparameters <- as.data.frame(wrectd_sc_ATLfullparameters[[1]]) 

# # create a new column `parameter` with the two name columns paramHeader and param collapsed together
wrectd_sc_ATLfullparameters$parameter <- paste(wrectd_sc_ATLfullparameters$paramHeader,wrectd_sc_ATLfullparameters$param)

# select relevant parameters and then save file.
# this allows the parameters to be accessible to the R markdown file for writing results. 
wrectd_sc_ATLfull <- wrectd_sc_ATLfullparameters %>% dplyr::select(parameter, est, se, pval)
write.csv(wrectd_sc_ATLfull, file = "./output/bivariate-models-nodem-65plus/wrectd_social_contact_fullmodel_parameters.csv")

#----delayed-word-recall-social-support-summary--------------------------
# TASKS TO COMPLETE: RENAME THE FILES SO THE TABLE PRODUCED CAN BE PUT IN MANUSCRIPT
wrectotd_social_support_summary<- extractModelSummaries("./output/bivariate-models-nodem-65plus/wrectotd-social_support_mean")
wrectotd_social_support_summary["CM"] <- 3
wrectotd_social_support_summary[1:3, "CM"] <- "-"
wrectotd_social_support_summary[10:11, "CM"] <- "9"
wrectotd_social_support_summary[12, "CM"] <- "9"
wrectotd_social_support_summary[13, "CM"] <- "9"
wrectotd_social_support_summary[14, "CM"] <- "13"
wrectotd_social_support_summary[15, "CM"] <- "13"
wrectotd_social_support_summary[16, "CM"] <- "13"
cm_row <- wrectotd_social_support_summary$CM

wrectotd_social_support_table <- compare_models_function(wrectotd_social_support_summary, cm_row)

colnames(wrectotd_social_support_table) <- c("Model", "$\\chi^2$", "df", "CM", "$\\Delta\\chi^2$", "df$\\Delta$", "CFI", "TLI", "RMSEA", "SRMR")

apa_table.word(wrectotd_social_support_table, caption = "Model Fit Indices for Delayed Word Recall and Social Support")

#----delayed-word-recall-social-support-results--------------------------
# model 12 the ALT model with no time specific correlations and fixed autoregressions is the final model
wrectd_ss_ATLparameters <- extractModelParameters("./output/bivariate-models-nodem-65plus/wrectotd-social_support_mean/m15_nocov_wrectotd_social_support_mean.out")

wrectd_ss_ATLparameters <- as.data.frame(wrectd_ss_ATLparameters[[1]]) 

# # create a new column `parameter` with the two name columns paramHeader and param collapsed together
wrectd_ss_ATLparameters$parameter <- paste(wrectd_ss_ATLparameters$paramHeader,wrectd_ss_ATLparameters$param)

# select relevant parameters and then save file.
# this allows the parameters to be accessible to the R markdown file for writing results. 
wrectd_ss_ATL <- wrectd_ss_ATLparameters %>% dplyr::select(parameter, est, se, pval)
write.csv(wrectd_ss_ATL, file = "./output/bivariate-models-nodem-65plus/wrectd_social_support_model_parameters.csv")

# Model 3 is the full model read in these parameters for comparison
wrectd_ss_ATLfullparameters <- extractModelParameters("./output/bivariate-models-nodem-65plus/wrectotd-social_support_mean/m03_nocov_wrectotd_social_support_mean.out")

wrectd_ss_ATLfullparameters <- as.data.frame(wrectd_ss_ATLfullparameters[[1]]) 

# # create a new column `parameter` with the two name columns paramHeader and param collapsed together
wrectd_ss_ATLfullparameters$parameter <- paste(wrectd_ss_ATLfullparameters$paramHeader,wrectd_ss_ATLfullparameters$param)

# select relevant parameters and then save file.
# this allows the parameters to be accessible to the R markdown file for writing results. 
wrectd_ss_ALTfull <- wrectd_ss_ATLfullparameters %>% dplyr::select(parameter, est, se, pval)
write.csv(wrectd_ss_ALTfull, file = "./output/bivariate-models-nodem-65plus/wrectd_social_support_full_model_parameters.csv")

#----delayed-word-recall-social-network-summary--------------------------
# TASKS TO COMPLETE: RENAME THE FILES SO THE TABLE PRODUCED CAN BE PUT IN MANUSCRIPT
wrectotd_social_network_summary<- extractModelSummaries("./output/bivariate-models-nodem-65plus/wrectotd-socialnetwork_total")
wrectotd_social_network_summary["CM"] <- 3
wrectotd_social_network_summary[1:3, "CM"] <- "-"
wrectotd_social_network_summary[10:11, "CM"] <- "9"
wrectotd_social_network_summary[12, "CM"] <- "9"
wrectotd_social_network_summary[13, "CM"] <- "9"
wrectotd_social_network_summary[14, "CM"] <- "13"
wrectotd_social_network_summary[15, "CM"] <- "13"
wrectotd_social_network_summary[16, "CM"] <- "13"
cm_row <- wrectotd_social_network_summary$CM

wrectotd_social_network_table <- compare_models_function(wrectotd_social_network_summary, cm_row)
# Note that model 15 appears to be the most parsimonious best fitting model. 

colnames(wrectotd_social_contact_table) <- c("Model", "$\\chi^2$", "df", "CM", "$\\Delta\\chi^2$", "df$\\Delta$", "CFI", "TLI", "RMSEA", "SRMR")

apa_table.word(wrectotd_social_contact_table, caption = "Model Fit Indices for Delayed Word Recall and Loneliness")

#----delayed-word-recall-social-network-results-----
# model 12 the ALT model with no time specific correlations and fixed autoregressions is the final model
wrectd_sn_ATLparameters <- extractModelParameters("./output/bivariate-models-nodem-65plus/wrectotd-socialnetwork_total/m15_nocov_wrectotd_socialnetwork_total.out")

wrectd_sn_ATLparameters <- as.data.frame(wrectd_sn_ATLparameters[[1]]) 

# # create a new column `parameter` with the two name columns paramHeader and param collapsed together
wrectd_sn_ATLparameters$parameter <- paste(wrectd_sn_ATLparameters$paramHeader,wrectd_sn_ATLparameters$param)

# select relevant parameters and then save file.
# this allows the parameters to be accessible to the R markdown file for writing results. 
wrectd_sn_ATL <- wrectd_sn_ATLparameters %>% dplyr::select(parameter, est, se, pval)
write.csv(wrectd_sn_ATL, file = "./output/bivariate-models-nodem-65plus/wrectd_social_network_model_parameters.csv")

# Model 3 is the full model read in these parameters for comparison
wrectd_sn_ATLfullparameters <- extractModelParameters("./output/bivariate-models-nodem-65plus/wrectotd-socialnetwork_total/m03_nocov_wrectotd_socialnetwork_total.out")

wrectd_sn_ATLfullparameters <- as.data.frame(wrectd_sn_ATLfullparameters[[1]]) 

# # create a new column `parameter` with the two name columns paramHeader and param collapsed together
wrectd_sn_ATLfullparameters$parameter <- paste(wrectd_sn_ATLfullparameters$paramHeader,wrectd_sn_ATLfullparameters$param)

# select relevant parameters and then save file.
# this allows the parameters to be accessible to the R markdown file for writing results. 
wrectd_sn_ALTfull <- wrectd_sn_ATLfullparameters %>% dplyr::select(parameter, est, se, pval)
write.csv(wrectd_sn_ALTfull, file = "./output/bivariate-models-nodem-65plus/wrectd_social_network_full_model_parameters.csv")

#---- mental-status-social-network-summary --------------------------
# TASKS TO COMPLETE: RENAME THE FILES SO THE TABLE PRODUCED CAN BE PUT IN MANUSCRIPT
mental_socialnetwork_summary<- extractModelSummaries("./output/bivariate-models-nodem-65plus/mentalstatus_tot-socialnetwork_total")
mental_socialnetwork_summary["CM"] <- 3
mental_socialnetwork_summary[1:3, "CM"] <- "-"
mental_socialnetwork_summary[10:11, "CM"] <- "9"
mental_socialnetwork_summary[12, "CM"] <- "9"
mental_socialnetwork_summary[13, "CM"] <- "9"
mental_socialnetwork_summary[14, "CM"] <- "12"
mental_socialnetwork_summary[15, "CM"] <- "12"
#mental_socialnetwork_summary[16, "CM"] <- "13"
cm_row <- mental_socialnetwork_summary$CM

mental_socialnetwork_table <- compare_models_function(mental_socialnetwork_summary, cm_row)
# Note that model 15 appears to be the most parsimonious best fitting model. 

colnames(mental_socialnetwork_table) <- c("Model", "$\\chi^2$", "df", "CM", "$\\Delta\\chi^2$", "df$\\Delta$", "CFI", "TLI", "RMSEA", "SRMR")

apa_table.word(mental_socialnetwork_table, caption = "Model Fit Indices for Immediate Word Recall and Social Contact")

#---- mental-status-social-network-results--------------------------
# model 11 the ALT model with no time specific correlations and fixed autoregressions for immediate word recall is the most supported model 
mentalstatus_sn_ATLparameters <- extractModelParameters("./output/bivariate-models-nodem-65plus/mentalstatus_tot-socialnetwork_total/m16_nocov_mentalstatus_tot_socialnetwork_total.out")

mentalstatus_sn_ATLparameters <- as.data.frame(mentalstatus_sn_ATLparameters[[1]]) 

# # create a new column `parameter` with the two name columns paramHeader and param collapsed together
mentalstatus_sn_ATLparameters$parameter <- paste(mentalstatus_sn_ATLparameters$paramHeader,mentalstatus_sn_ATLparameters$param)

# select relevant parameters and then save file.
# this allows the parameters to be accessible to the R markdown file for writing results. 
mentalstatus_sn_ATL <- mentalstatus_sn_ATLparameters %>% dplyr::select(parameter, est, se, pval)
write.csv(mentalstatus_sn_ATL, file = "./output/bivariate-models-nodem-65plus/mentalstatus_socialnetwork_model_parameters.csv")
bivariateALT_parameter_extraction_function(mentalstatus_sn_ATL)
# model 3 is the full ALT model save this as well for comparison
mentalstatus_sn_fullATLparameters <- extractModelParameters("./output/bivariate-models-nodem-65plus/mentalstatus_tot-socialnetwork_total/m03_nocov_mentalstatus_tot_socialnetwork_total.out")

mentalstatus_sn_fullATLparameters <- as.data.frame(mentalstatus_sn_fullATLparameters[[1]]) 

# # create a new column `parameter` with the two name columns paramHeader and param collapsed together
mentalstatus_sn_fullATLparameters$parameter <- paste(mentalstatus_sn_fullATLparameters$paramHeader,mentalstatus_sn_fullATLparameters$param)

# select relevant parameters and then save file.
# this allows the parameters to be accessible to the R markdown file for writing results. 
mentalstatus_sn_fullATL <- mentalstatus_sn_fullATLparameters %>% dplyr::select(parameter, est, se, pval)
write.csv(mentalstatus_sn_fullATL, file = "./output/bivariate-models-nodem-65plus/mentalstatus_socialnetwork_fullmodel_parameters.csv")

#---- mental-status-social-support --------------------------
# TASKS TO COMPLETE: RENAME THE FILES SO THE TABLE PRODUCED CAN BE PUT IN MANUSCRIPT
mental_social_support_summary<- extractModelSummaries("./output/bivariate-models-nodem-65plus/mentalstatus_tot-social_support_mean")
mental_social_support_summary["CM"] <- 3
mental_social_support_summary[1:3, "CM"] <- "-"
mental_social_support_summary[10:11, "CM"] <- "9"
mental_social_support_summary[12, "CM"] <- "9"
mental_social_support_summary[13, "CM"] <- "9"
mental_social_support_summary[14, "CM"] <- "12"
mental_social_support_summary[15, "CM"] <- "12"
#mental_socialnetwork_summary[16, "CM"] <- "13"
cm_row <- mental_social_support_summary$CM

mental_social_support_table <- compare_models_function(mental_social_support_summary, cm_row)
# Note that model 15 appears to be the most parsimonious best fitting model. 

colnames(wrectoti_social_contact_table) <- c("Model", "$\\chi^2$", "df", "CM", "$\\Delta\\chi^2$", "df$\\Delta$", "CFI", "TLI", "RMSEA", "SRMR")

apa_table.word(wrectoti_social_contact_table, caption = "Model Fit Indices for Immediate Word Recall and Social Contact")


#---- mental-status-social-support-results---------------
# model 11 the ALT model with no time specific correlations and fixed autoregressions for immediate word recall is the most supported model 
mentalstatus_ss_ATLparameters <- extractModelParameters("./output/bivariate-models-nodem-65plus/mentalstatus_tot-social_support_mean/m14_nocov_mentalstatus_tot_social_support_mean.out")

mentalstatus_ss_ATLparameters <- as.data.frame(mentalstatus_ss_ATLparameters[[1]]) 

# # create a new column `parameter` with the two name columns paramHeader and param collapsed together
mentalstatus_ss_ATLparameters$parameter <- paste(mentalstatus_ss_ATLparameters$paramHeader,mentalstatus_ss_ATLparameters$param)

# select relevant parameters and then save file.
# this allows the parameters to be accessible to the R markdown file for writing results. 
mentalstatus_ss_ATL <- mentalstatus_ss_ATLparameters %>% dplyr::select(parameter, est, se, pval)
write.csv(mentalstatus_ss_ATL, file = "./output/bivariate-models-nodem-65plus/mentalstatus_social_support_model_parameters.csv")

# model 3 is the full ALT model save this as well for comparison
mentalstatus_ss_fullATLparameters <- extractModelParameters("./output/bivariate-models-nodem-65plus/mentalstatus_tot-social_support_mean/m03_nocov_mentalstatus_tot_social_support_mean.out")

mentalstatus_ss_fullATLparameters <- as.data.frame(mentalstatus_ss_fullATLparameters[[1]]) 

# # create a new column `parameter` with the two name columns paramHeader and param collapsed together
mentalstatus_ss_fullATLparameters$parameter <- paste(mentalstatus_ss_fullATLparameters$paramHeader,mentalstatus_ss_fullATLparameters$param)

# select relevant parameters and then save file.
# this allows the parameters to be accessible to the R markdown file for writing results. 
mentalstatus_ss_fullATL <- mentalstatus_ss_fullATLparameters %>% dplyr::select(parameter, est, se, pval)
write.csv(mentalstatus_ss_fullATL, file = "./output/bivariate-models-nodem-65plus/mentalstatus_social_support_fullmodel_parameters.csv")


#---- mental-status-social-contact --------------------------
# TASKS TO COMPLETE: RENAME THE FILES SO THE TABLE PRODUCED CAN BE PUT IN MANUSCRIPT
mental_social_support_contact<- extractModelSummaries("./output/bivariate-models-nodem-65plus/predetermined-models/mentalstatus_tot-social_contact_total")
mental_social_support_contact["CM"] <- 9
mental_social_support_contact[1:3, "CM"] <- "-"
mental_social_support_contact[5:7, "CM"] <- "-"
mental_social_support_contact[12, "CM"] <- "9"
mental_social_support_contact[13, "CM"] <- "9"
mental_social_support_contact[14, "CM"] <- "12"
mental_social_support_contact[15, "CM"] <- "12"
#mental_socialnetwork_summary[16, "CM"] <- "13"
cm_row <- mental_social_support_contact$CM

mental_social_contact_table <- compare_models_function(mental_social_support_contact, cm_row)
# Note that model 15 appears to be the most parsimonious best fitting model. 

colnames(mental_social_contact_table) <- c("Model", "$\\chi^2$", "df", "CM", "$\\Delta\\chi^2$", "df$\\Delta$", "CFI", "TLI", "RMSEA", "SRMR")

apa_table.word(mental_social_contact_table, caption = "Model Fit Indices for Immediate Word Recall and Social Contact")

#---- mental-status-social-contact-results---------------
# model 11 the ALT model with no time specific correlations and fixed autoregressions for immediate word recall is the most supported model 
mentalstatus_sc_ATLparameters <- extractModelParameters("./output/bivariate-models-nodem-65plus/predetermined-models/mentalstatus_tot-social_contact_total/m12_nocov_mentalstatus_tot_social_contact_total.out")

mentalstatus_sc_ATLparameters <- as.data.frame(mentalstatus_sc_ATLparameters[[1]]) 

# # create a new column `parameter` with the two name columns paramHeader and param collapsed together
mentalstatus_sc_ATLparameters$parameter <- paste(mentalstatus_sc_ATLparameters$paramHeader,mentalstatus_sc_ATLparameters$param)

# select relevant parameters and then save file.
# this allows the parameters to be accessible to the R markdown file for writing results. 
mentalstatus_sc_ATL <- mentalstatus_sc_ATLparameters %>% dplyr::select(parameter, est, se, pval)
write.csv(mentalstatus_sc_ATL, file = "./output/bivariate-models-nodem-65plus/mentalstatus_social_contact_model_parameters.csv")

# model 3 is the full ALT model save this as well for comparison
mentalstatus_sc_fullATLparameters <- extractModelParameters("./output/bivariate-models-nodem-65plus/mentalstatus_tot-social_contact_total/m03_nocov_mentalstatus_tot_social_contact_total.out")

mentalstatus_sc_fullATLparameters <- as.data.frame(mentalstatus_sc_fullATLparameters[[1]]) 

# # create a new column `parameter` with the two name columns paramHeader and param collapsed together
mentalstatus_sc_fullATLparameters$parameter <- paste(mentalstatus_sc_fullATLparameters$paramHeader,mentalstatus_sc_fullATLparameters$param)

# select relevant parameters and then save file.
# this allows the parameters to be accessible to the R markdown file for writing results. 
mentalstatus_sc_fullATL <- mentalstatus_sc_fullATLparameters %>% dplyr::select(parameter, est, se, pval)
write.csv(mentalstatus_sc_fullATL, file = "./output/bivariate-models-nodem-65plus/mentalstatus_social_contact_fullmodel_parameters.csv")

#---- mental-status-loneliness --------------------------
# TASKS TO COMPLETE: RENAME THE FILES SO THE TABLE PRODUCED CAN BE PUT IN MANUSCRIPT
mental_loneliness<- extractModelSummaries("./output/bivariate-models-nodem-65plus/predetermined-models/mentalstatus_tot-score_loneliness_3")
#mental_loneliness<- readModels("./output/bivariate-models-nodem-65plus/predetermined-models/mentalstatus_tot-score_loneliness_3")
#mental_loneliness_summary <- mental_loneliness$m01_nocov_mentalstatus_tot_score_loneliness_3.out$summaries
mental_loneliness["CM"] <- 3
mental_loneliness[1:3, "CM"] <- "-"
mental_loneliness[10:11, "CM"] <- "9"
mental_loneliness[12, "CM"] <- "9"
mental_loneliness[13, "CM"] <- "9"
mental_loneliness[14, "CM"] <- "12"
mental_loneliness[15, "CM"] <- "12"
#mental_socialnetwork_summary[16, "CM"] <- "13"
cm_row <- mental_loneliness$CM

mental_loneliness_table <- compare_models_function(mental_loneliness, cm_row)
# Note that model 15 appears to be the most parsimonious best fitting model. 

colnames(mental_loneliness_table) <- c("Model", "$\\chi^2$", "df", "CM", "$\\Delta\\chi^2$", "df$\\Delta$", "CFI", "TLI", "RMSEA", "SRMR")

apa_table.word(mental_loneliness_table, caption = "Model Fit Indices for Immediate Word Recall and Social Contact")

#---- mental-status-loneliness---------------
# model 11 the ALT model with no time specific correlations and fixed autoregressions for immediate word recall is the most supported model 
mentalstatus_lone_ATLparameters <- extractModelParameters("./output/bivariate-models-nodem-65plus/predetermined-models/mentalstatus_tot-score_loneliness_3/m12_nocov_mentalstatus_tot_score_loneliness_3.out")

mentalstatus_lone_ATLparameters <- as.data.frame(mentalstatus_lone_ATLparameters[[1]]) 

# # create a new column `parameter` with the two name columns paramHeader and param collapsed together
mentalstatus_lone_ATLparameters$parameter <- paste(mentalstatus_lone_ATLparameters$paramHeader,mentalstatus_lone_ATLparameters$param)

# select relevant parameters and then save file.
# this allows the parameters to be accessible to the R markdown file for writing results. 
mentalstatus_lone_ATL <- mentalstatus_lone_ATLparameters %>% dplyr::select(parameter, est, se, pval)
write.csv(mentalstatus_lone_ATL, file = "./output/bivariate-models-nodem-65plus/predetermined-models/mentalstatus_loneliness_model_parameters.csv")

# ---- mental-status-and-loneliness-plot ----
cog_labels <- c("Mental&#92;nStatus 1", "Mental&#92;nStatus 2","Mental&#92;nStatus 3","Mental&#92;nStatus 4","Mental&#92;nStatus 5","Mental&#92;nStatus 6")
soc_labels <- c("Loneliness 1", "Loneliness 2","Loneliness 3","Loneliness 4","Loneliness 5","Loneliness 6")
mental_lone <- read.csv("./output/bivariate-models-nodem-65plus/predetermined-models/mentalstatus_loneliness_model_parameters.csv")
diagram_parameters <- ALT_diagram_parameter_extraction_function(mental_lone)
mental_lone_path_diagram <- path_diagram_function()
# Create a PNG of this graph
tmp<-capture.output(rsvg_png(charToRaw(export_svg(wrecti_ss_path_diagram)),'./reports/wrecti_ss_path_diagram.png'))


# model 3 is the full ALT model save this as well for comparison
mentalstatus_lone_fullATLparameters <- extractModelParameters("./output/bivariate-models-nodem-65plus/mentalstatus_tot-score_loneliness_3/m03_nocov_mentalstatus_tot_score_loneliness_3.out")

mentalstatus_lone_fullATLparameters <- as.data.frame(mentalstatus_lone_fullATLparameters[[1]]) 

# # create a new column `parameter` with the two name columns paramHeader and param collapsed together
mentalstatus_lone_fullATLparameters$parameter <- paste(mentalstatus_lone_fullATLparameters$paramHeader,mentalstatus_lone_fullATLparameters$param)

# select relevant parameters and then save file.
# this allows the parameters to be accessible to the R markdown file for writing results. 
mentalstatus_lone_fullATLparameters <- mentalstatus_lone_fullATLparameters %>% dplyr::select(parameter, est, se, pval)
write.csv(mentalstatus_lone_fullATLparameters, file = "./output/bivariate-models-nodem-65plus/mentalstatus_loneliness_fullmodel_parameters.csv")

#---- delayed-word-recall-model-summaries --------------------------------------------------------------
# Extract the fit indices of relevant models
wrectd_fit_series4 <- extractModelSummaries("./output/univariate-models-nodem-65plus/wrectotd")

wrectd_fit_series4["CM"] <- 4
wrectd_fit_series4[1:3, "CM"] <- "-"

cm_row <- wrectd_fit_series4$CM
wrectd_fit_series4_table <- compare_models_function(wrectd_fit_series4, cm_row)

colnames(wrectd_fit_series4_table) <- c("Model", "$\\chi^2$", "df", "CM", "$\\Delta\\chi^2$", "df$\\Delta$", "CFI", "TLI", "RMSEA", "SRMR")

apa_table.word(wrectd_fit_series4_table, caption = "Model Fit Indices for Delayed Word Recall")

#----word-recall-delayed-----
wrectd_ATLparameters <- extractModelParameters("./output/univariate-models-nodem-65plus/wrectotd/u04_nocov_wrectotd.out")

wrectd_ATL <- as.data.frame(wrectd_ATLparameters[[1]])

# columns to paste together
cols <- c( 'paramHeader' , 'param') 

# create a new column `parameter` with the two name columns paramHeader and param collapsed together
wrectd_ATL$parameter <- paste(wrectd_ATL$paramHeader,wrectd_ATL$param)

# select relevant parameters and then save file.
wrectd_ATL <- wrectd_ATL %>% dplyr::select(parameter, est, se, pval)
write.csv(wrectd_ATL, file = "./output/univariate-models-nodem-65plus/wrectd_model_parameters.csv")

#----delayed-word-recall-plot------

wrectdALTp <- parameter_extraction_function(wrectd_ATL)

plotwrectd <- uALT_plot_function(ds_wide$wrectotd_2004, wrectdALTp)

# Plot the bar chart.
wrectd_plot <- plot(plotwrectd,type = "o", col = "BLACK", xlab = "Wave", ylab = "Predicted Mean",
                    main = "Delayed Word Recall")



#---- mental-status-model-summaries --------------------------------------------------------------
# Extract the fit indices of relevant models
mental_status_fit_series4 <- extractModelSummaries("./output/univariate-models-nodem-65plus/mentalstatus_tot")

mental_status_fit_series4["CM"] <- 4
mental_status_fit_series4[1:3, "CM"] <- "-"

cm_row <- mental_status_fit_series4$CM

mental_status_fit_series4_table <- compare_models_function(mental_status_fit_series4, cm_row)

colnames(mental_status_fit_series4_table) <- c("Model", "$\\chi^2$", "df", "CM", "$\\Delta\\chi^2$", "df$\\Delta$", "CFI", "TLI", "RMSEA", "SRMR")

apa_table.word(mental_status_fit_series4_table, caption = "Model Fit Indices for Mental Status")

#----mental-status-----
ms_ATLparameters <- extractModelParameters("./output/univariate-models-nodem-65plus/mentalstatus_tot/u04_nocov_mentalstatus_tot.out")

ms_ATL <- as.data.frame(ms_ATLparameters[[1]])

# columns to paste together
cols <- c( 'paramHeader' , 'param') 

# create a new column `parameter` with the two name columns paramHeader and param collapsed together
ms_ATL$parameter <- paste(ms_ATL$paramHeader,ms_ATL$param)

# select relevant parameters and then save file.
ms_ATL <- ms_ATL %>% dplyr::select(parameter, est, se, pval)

write.csv(ms_ATL, file = "./output/univariate-models-nodem-65plus/mental_status_model_parameters.csv")

#---- mental-status-plot ---------------
# mental status
ms_ALTparameters <- parameter_extraction_function(ms_ATL)

ms_plot_vals <- uALT_plot_function(8.517, ms_ALTparameters)

# Plot the bar chart.
ms_plot <- plot(ms_plot_vals,type = "o", col = "BLACK", xlab = "Wave", ylab = "Predicted Mean",
                    main = "Mental Status")


#---- loneliness-summaries --------------------------------------------------------------
# Extract the fit indices of relevant models
loneliness_series4 <- extractModelSummaries("./output/univariate-models-nodem-65plus/predetermined-models/score_loneliness_3")

loneliness_series4["CM"] <- 3
loneliness_series4[1:3, "CM"] <- "-"

cm_row <- loneliness_series4$CM
loneliness_series4_table <- compare_models_function(loneliness_series4, cm_row)

colnames(loneliness_series4_table) <- c("Model", "$\\chi^2$", "df", "CM", "$\\Delta\\chi^2$", "df$\\Delta$", "CFI", "TLI", "RMSEA", "SRMR")

apa_table.word(loneliness_series4_table, caption = "Model Fit Indices for Loneliness")

#----loneliness-----
loneliness_ATLparameters <- extractModelParameters("./output/univariate-models-nodem-65plus/predetermined-models/score_loneliness_3/u09_nocov_score_loneliness_3.out")

loneliness_ATL <- as.data.frame(loneliness_ATLparameters[[1]])

# columns to paste together
cols <- c( 'paramHeader' , 'param') 

# create a new column `parameter` with the two name columns paramHeader and param collapsed together
loneliness_ATL$parameter <- paste(loneliness_ATL$paramHeader,loneliness_ATL$param)

# select relevant parameters and then save file.
loneliness_ATL <- loneliness_ATL %>% dplyr::select(parameter, est, se, pval)
write.csv(loneliness_ATL, file = "./output/univariate-models-nodem-65plus/predetermined-models/loneliness_model_parameters.csv")

# Save full ALT model for comparison
loneliness_ATLparameters <- extractModelParameters("./output/univariate-models-nodem-65plus/predetermined-models/score_loneliness_3/u03_nocov_score_loneliness_3.out")

loneliness_ATL <- as.data.frame(loneliness_ATLparameters[[1]])

# columns to paste together
cols <- c( 'paramHeader' , 'param') 

# create a new column `parameter` with the two name columns paramHeader and param collapsed together
loneliness_ATL$parameter <- paste(loneliness_ATL$paramHeader,loneliness_ATL$param)

# select relevant parameters and then save file.
loneliness_ATL <- loneliness_ATL %>% dplyr::select(parameter, est, se, pval)
write.csv(loneliness_ATL, file = "./output/univariate-models-nodem-65plus/predetermined-models/loneliness_fullALTmodel_parameters.csv")

# ---- loneliness-plot ------

lone_ALTparameters <- parameter_extraction_function(loneliness_ATL)

# first value should be the 2004 mean, copy right from mplus output
loneliness_plot_vals <- uALT_plot_function(1.379, lone_ALTparameters)

# Plot the bar chart.
loneliness_plot <- plot(loneliness_plot_vals,type = "o", col = "BLACK", xlab = "Wave", ylab = "Predicted Mean",
                    main = "Loneliness")

#---- social-contact-summaries --------------------------------------------------------------
# Extract the fit indices of relevant models
social_contact_series4 <- extractModelSummaries("./output/univariate-models-nodem-65plus/social_contact_total")

social_contact_series4["CM"] <- 4
social_contact_series4[1:3, "CM"] <- "-"
cm_row <- social_contact_series4$CM
social_contact_series4_table <- compare_models_function(social_contact_series4, cm_row)

colnames(social_contact_series4_table) <- c("Model", "$\\chi^2$", "df", "CM", "$\\Delta\\chi^2$", "df$\\Delta$", "CFI", "TLI", "RMSEA", "SRMR")

apa_table.word(social_contact_series4_table , caption = "Model Fit Indices for Social Contact")

#----social-contact-----
social_contact_ATLparameters <- extractModelParameters("./output/univariate-models-nodem-65plus/social_contact_total/u04_nocov_social_contact_total.out")

social_contact_ATL <- as.data.frame(social_contact_ATLparameters[[1]])

# columns to paste together
cols <- c( 'paramHeader' , 'param') 

# create a new column `parameter` with the two name columns paramHeader and param collapsed together
social_contact_ATL$parameter <- paste(social_contact_ATL$paramHeader,social_contact_ATL$param)

# select relevant parameters and then save file.
social_contact_ATL <- social_contact_ATL %>% dplyr::select(parameter, est, se, pval)
write.csv(social_contact_ATL, file = "./output/univariate-models-nodem-65plus/social_contact_model_parameters.csv")

# ---- social-contact-plot ------
sc_ALTparameters <- parameter_extraction_function(social_contact_ATL)

# first value should be the 2004 mean, copy right from mplus output
sc_plot_vals <- uALT_plot_function(30.315, sc_ALTparameters)

# Plot the bar chart.
social_contact_plot <- plot(sc_plot_vals,type = "o", col = "BLACK", xlab = "Wave", ylab = "Predicted Mean",
                        main = "Social Contact")

#---- social-support-summaries --------------------------------------------------------------
# Extract the fit indices of relevant models
social_support_series4 <- extractModelSummaries("./output/univariate-models-nodem-65plus/social_support_mean")

social_support_series4["CM"] <- 4
social_support_series4[1:3, "CM"] <- "-"

social_support_series4_table <- compare_models_function(social_support_series4, cm_row)

colnames(social_support_series4_table) <- c("Model", "$\\chi^2$", "df", "CM", "$\\Delta\\chi^2$", "df$\\Delta$", "CFI", "TLI", "RMSEA", "SRMR")

apa_table.word(social_support_series4_table , caption = "Model Fit Indices for Social Support")


#----social-support-----
social_support_ATLparameters <- extractModelParameters("./output/univariate-models-nodem-65plus/social_support_mean/u08_nocov_social_support_mean.out")

social_support_ATL <- as.data.frame(social_support_ATLparameters[[1]])

# columns to paste together
cols <- c( 'paramHeader' , 'param') 

# create a new column `parameter` with the two name columns paramHeader and param collapsed together
social_support_ATL$parameter <- paste(social_support_ATL$paramHeader,social_support_ATL$param)

# select relevant parameters and then save file.
social_support_ATL <- social_support_ATL %>% dplyr::select(parameter, est, se, pval)
write.csv(social_support_ATL, file = "./output/univariate-models-nodem-65plus/social_support_model_parameters.csv")

# ---- social-support-plot ------
ss_ALTparameters <- parameter_extraction_function(social_support_ATL)

# first value should be the 2004 mean, copy right from mplus output
ss_plot_vals <- uALT_plot_function(9.781, ss_ALTparameters)

# Plot the bar chart.
social_support_plot <- plot(ss_plot_vals,type = "o", col = "BLACK", xlab = "Wave", ylab = "Predicted Mean",
                            main = "Social Support")

#---- social-network-summaries --------------------------------------------------------------
# Extract the fit indices of relevant models
social_network_series4 <- extractModelSummaries("./output/univariate-models-nodem-65plus/socialnetwork_total")

social_network_series4["CM"] <- 4
social_network_series4[1:3, "CM"] <- "-"

social_network_series4_table <- compare_models_function(social_network_series4, cm_row)

colnames(social_network_series4_table) <- c("Model", "$\\chi^2$", "df", "CM", "$\\Delta\\chi^2$", "df$\\Delta$", "CFI", "TLI", "RMSEA", "SRMR")

apa_table.word(social_network_series4_table, caption = "Model Fit Indices for Social Network")

#----social-network-----
social_network_ATLparameters <- extractModelParameters("./output/univariate-models-nodem-65plus/socialnetwork_total/u08_nocov_socialnetwork_total.out")

social_network_ATL <- as.data.frame(social_network_ATLparameters[[1]])

# columns to paste together
cols <- c( 'paramHeader' , 'param') 

# create a new column `parameter` with the two name columns paramHeader and param collapsed together
social_network_ATL$parameter <- paste(social_network_ATL$paramHeader,social_network_ATL$param)

# select relevant parameters and then save file.
social_network_ATL <- social_network_ATL %>% dplyr::select(parameter, est, se, pval)
write.csv(social_network_ATL, file = "./output/univariate-models-nodem-65plus/social_network_model_parameters.csv")

# ---- social-network-plot ------
sn_ALTparameters <- parameter_extraction_function(social_network_ATL)

# first value should be the 2004 mean, copy right from mplus output
sn_plot_vals <- uALT_plot_function(3.388, sn_ALTparameters)

# Plot the bar chart.
social_network_plot <- plot(sn_plot_vals,type = "o", col = "BLACK", xlab = "Wave", ylab = "Predicted Mean",
                            main = "Social Network")


# ---- delayed-word-recall-loneliness-plot ---------
wrectd_lone_value_list <- bivariateALT_parameter_extraction_function(wrectd_lonely_ATL)

wrectd_lonely_plot_vals <- bALT_plot_function(mean(ds_wide$wrectotd_2004, na.rm = T), mean(ds_wide$score_loneliness_3_2004, na.rm = T), wrectd_lone_value_list)


wrectd_lone_plot <- plot(wrectd_lonely_plot_vals$y_values,type = "o", col = "BLACK", xlab = "Wave", ylab = "Predicted Mean",
                         main = "Delayed Word Recall")

lone_wrectd_plot <- plot(wrectd_lonely_plot_vals$x_values,type = "o", col = "BLACK", xlab = "Wave", ylab = "Predicted Mean",
                         main = "Loneliness")

# ---- immediate-word-recall-loneliness-plot ---------
# immediate word recall and social network
wrecti_lone_ATL <- read.csv("./output/bivariate-models-nodem-65plus/wrecti_loneliness_model_parameters.csv")
diagram_parameters <- ALT_diagram_parameter_extraction_function(wrecti_lone_ATL)
cog_labels <- c("Immediate&#92;nWord&#92;nRecall 1", "Immediate&#92;nWord&#92;nRecall 2","Immediate&#92;nWord&#92;nRecall 3","Immediate&#92;nWord&#92;nRecall 4","Immediate&#92;nWord&#92;nRecall 5","Immediate&#92;nWord&#92;nRecall 6")
soc_labels <- c("Loneliness 1", "Loneliness 2","Loneliness 3","Loneliness 4","Loneliness 5","Loneliness 6")
wrecti_lone_path_diagram <- path_diagram_function()
# Create a PNG of this graph
tmp<-capture.output(rsvg_png(charToRaw(export_svg(wrecti_lone_path_diagram)),'./reports/wrecti_lone_path_diagram.png'))

# ---- immediate-word-recall-and-social-support-plot ----
cog_labels <- c("Immediate&#92;nWord&#92;nRecall 1", "Immediate&#92;nWord&#92;nRecall 2","Immediate&#92;nWord&#92;nRecall 3","Immediate&#92;nWord&#92;nRecall 4","Immediate&#92;nWord&#92;nRecall 5","Immediate&#92;nWord&#92;nRecall 6")
soc_labels <- c("Social&#92;nSupport 1", "Social&#92;nSupport 2","Social&#92;nSupport 3","Social&#92;nSupport 4","Social&#92;nSupport 5","Social&#92;nSupport 6")
wrecti_socsup <- read.csv("./output/bivariate-models-nodem-65plus/wrecti_social_support_model_parameters.csv")
diagram_parameters <- ALT_diagram_parameter_extraction_function(wrecti_socsup)
wrecti_ss_path_diagram <- path_diagram_function()
# Create a PNG of this graph
tmp<-capture.output(rsvg_png(charToRaw(export_svg(wrecti_ss_path_diagram)),'./reports/wrecti_ss_path_diagram.png'))

# ---- immediate-word-recall-and-social-network-plot ----
cog_labels <- c("Immediate&#92;nWord&#92;nRecall 1", "Immediate&#92;nWord&#92;nRecall 2","Immediate&#92;nWord&#92;nRecall 3","Immediate&#92;nWord&#92;nRecall 4","Immediate&#92;nWord&#92;nRecall 5","Immediate&#92;nWord&#92;nRecall 6")
soc_labels <- c("Social&#92;nNetwork 1", "Social&#92;nNetwork 2","Social&#92;nNetwork 3","Social&#92;nNetwork 4","Social&#92;nNetwork 5","Social&#92;nNetwork 6")
wrecti_sn <- read.csv("./output/bivariate-models-nodem-65plus/wrecti_social_network_model_parameters.csv")
diagram_parameters <- ALT_diagram_parameter_extraction_function(wrecti_sn)
wrecti_sn_path_diagram <- path_diagram_function()
# Create a PNG of this graph
tmp<-capture.output(rsvg_png(charToRaw(export_svg(wrecti_sn_path_diagram)),'./reports/wrecti_sn_path_diagram.png'))


cog_labels <- c("Immediate&#92;nWord&#92;nRecall 1", "Immediate&#92;nWord&#92;nRecall 2","Immediate&#92;nWord&#92;nRecall 3","Immediate&#92;nWord&#92;nRecall 4","Immediate&#92;nWord&#92;nRecall 5","Immediate&#92;nWord&#92;nRecall 6")
soc_labels <- c("Social&#92;nNetwork 1", "Social&#92;nNetwork 2","Social&#92;nNetwork 3","Social&#92;nNetwork 4","Social&#92;nNetwork 5","Social&#92;nNetwork 6")
wrecti_sn <- read.csv("./output/bivariate-models-nodem-65plus/wrecti_social_network_model_parameters.csv")
diagram_parameters <- ALT_diagram_parameter_extraction_function(wrecti_sn)
wrecti_sn_path_diagram <- graph_function_covariates()

