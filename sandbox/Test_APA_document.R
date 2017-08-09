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
library(papaja)
library(htmlTable)
library(xtable)
options(xtable.floating = FALSE)
options(xtable.timestamp = "")

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
# source("./scripts/common-functions.R") # used in multiple reports
# source("./scripts/graphing/graph-presets.R") # fonts, colors, themes
# source("./scripts/graphing/graph-elemental.R") # graphs to be used in dipslays
# source("./scripts/graphing/graph-complex.R") # info displays

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
apa_table(desc, 
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
