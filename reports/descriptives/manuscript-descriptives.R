# The function of this script is to take the data produced by the 2-prepare-biomarker-data.R script so that all final exclusion criteria
# have been implemented and produce descriptive statistics to be used in the paper. This script does not alter the data in any way.

#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>% 
library(TabularManifest)
library(dplyr)
library(htmlTable)
library(papaja)
library(Hmisc)
# ---- load-sources ------------------------------------------------------------
source("./scripts/common-functions.R") # used in multiple reports

# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2") # graphing
requireNamespace("tidyr") # data manipulation
requireNamespace("dplyr") # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("psych") # For descriptive functions
requireNamespace("Hmisc")


# ---- declare-globals --------------------------------------------------------
path_input     <- "./data-unshared/derived/2-dto_b.rds"
path_input_wide <- "./data-unshared/derived/2-dto_wide_b.rds"

# ---- load-data ---------------------------------------------------------------
# load the long data product of 2-prepare-biomarker-data.R
ds_long <- readRDS(path_input)

# load the wide data product (the one used for mplus analysis) of 2-prepare-biomarker-data.R
ds_wide <- readRDS(path_input_wide)

# note that the exclusion criteria for dementia is implemented in mplus so this needs to be included here. 
ds_wide <- dplyr::filter(ds_wide, memoryproblems_baseline ==0) 

# convert NA and NaN to 9999 for Mplus.
ds_wide[ds_wide==9999] <- NA

# ---- table ----------------
ds_long %>% summarize_over_time("year", "intage_r")
ds_long %>% summarize_over_time("year", "wrectoti") 
ds_long %>% summarize_over_time("year", "wrectotd")
ds_long %>% summarize_over_time("year", "mentalstatus_tot")
ds_long %>% summarize_over_time("year", "score_loneliness_3")
ds_long %>% summarize_over_time("year", "social_contact_total")
ds_long %>% summarize_over_time("year", "social_support_mean")
ds_long %>% summarize_over_time("year", "socialnetwork_total")
ds_long %>% summarize_over_time("year", "health_conditions_baseline")
ds_long %>% summarize_over_time("year", "healthcond_mean")

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

var_names <- c(" ", "Women (%)", "Age", "Yrs Education", "Health Conditions", "Mental status", "Word recall immediate", "Word recall delayed",
                "Psychosocial Variables", "Loneliness", "Social contact", "Social support", "Depression", "Social network")
year_2004 <- c(paste0("n = ", count[1,2])
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

year_2006 <- c(paste0("n = ", count[2,2])
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

year_2008 <- c(paste0("n = ", count[3,2])
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

year_2010 <- c(paste0("n = ", count[4,2])
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

year_2012 <- c(paste0("n = ", count[5,2])
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

year_2014 <- c(paste0("n = ", count[6,2])
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

desc <- cbind(var_names, year_2004, year_2006, year_2008, year_2010, year_2012, year_2014)

htmlTable(desc 
          ,align="lccc"
          ,header = paste(c(" ", "M (SD)", "M (SD)", "M (SD)", "M (SD)", "M (SD)", "M (SD)"))
          ,cgroup = c("","2004", "2006", "2008", "2010", "2012", "2014")
          ,n.cgroup = c(1,1,1,1,1,1)
          ,css.tspanner = FALSE
          ,ctable = TRUE
          ,caption = "Table 1. Descriptive statistics by year"
          )

#---- correlation-matrix ----
names(ds_wide)
cov_ds <- ds_wide %>%
  dplyr::select_(
    "mentalstatus_tot_2004"
    ,"mentalstatus_tot_2006"
    ,"mentalstatus_tot_2008"
    ,"mentalstatus_tot_2010"
    ,"mentalstatus_tot_2012"     
    ,"mentalstatus_tot_2014"
    ,"wrectotd_2004"             
    ,"wrectotd_2006"
    ,"wrectotd_2008"
    ,"wrectotd_2010"
    ,"wrectotd_2012"
    ,"wrectotd_2014"
    ,"wrectoti_2004"
    ,"wrectoti_2006"
    ,"wrectoti_2008"
    ,"wrectoti_2010"
    ,"wrectoti_2012"             
    ,"wrectoti_2014"
    ,"score_loneliness_3_2006"
    ,"score_loneliness_3_2008"
    ,"score_loneliness_3_2010"   
    ,"score_loneliness_3_2012"
    ,"score_loneliness_3_2014"
    ,"social_contact_total_2004"
    ,"social_contact_total_2006"
    ,"social_contact_total_2008" 
    ,"social_contact_total_2010"
    ,"social_contact_total_2012"
    ,"social_contact_total_2014"
    ,"social_support_mean_2004"  
    ,"social_support_mean_2006"
    ,"social_support_mean_2008"
    ,"social_support_mean_2010"
    ,"social_support_mean_2012"
    ,"social_support_mean_2014"
    ,"socialnetwork_total_2004"
    ,"socialnetwork_total_2006"
    ,"socialnetwork_total_2008"
    ,"socialnetwork_total_2010"
    ,"socialnetwork_total_2012"  
    ,"socialnetwork_total_2014"
    ,"wrectotd_2004"             
    ,"wrectotd_2006"
    ,"wrectotd_2008"
    ,"wrectotd_2010"
    ,"wrectotd_2012"
    ,"wrectotd_2014"
    ,"wrectoti_2004"
    ,"wrectoti_2006"
    ,"wrectoti_2008"
    ,"wrectoti_2010"
    ,"wrectoti_2012"             
    ,"wrectoti_2014"
    ,"male"
    ,"cohort"
    ,"raedyrs"
    )

glimpse(cov_ds)
cov_ds$male <- as.numeric(cov_ds$male)
corr_matrix <- rcorr(as.matrix(cov_ds))

corr_matrix <- cor(cov_ds)
round(corr_matrix, 2)
