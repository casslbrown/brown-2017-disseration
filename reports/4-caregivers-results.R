# NOTE:
# R does not consume and data objects resulting from the execution of this script
# The function of this scripts is to prepare the data for modelingin MPlus

# knitr::stitch_rmd(script="./___/___.R", output="./___/___/___.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>% 
library(dplyr)
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



# ---- declare-globals --------------------------------------------------------
# connect to the data transfer object from the HRS repository
path_input  <- "./data-unshared/derived/caregiver-data-wide.rds" # product of ./3-prepare-for-caregivers-analysis.R
path_input2  <- "./data-unshared/derived/caregiver-data-long.rds"

# ---- load-data ---------------------------------------------------------------
# load the product of 3-prepare-for-caregivers-analysis.R
ds_wide <- readRDS(path_input)
ds_long <- readRDS(path_input2)


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
               ,paste0(round(mean(ds_wide$age_baseline, na.rm = T),2), " (", round(sd(ds_wide$age_baseline, na.rm = T),2),")")
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

# Look at caregivers only and investigate how they might be different demographically. 
