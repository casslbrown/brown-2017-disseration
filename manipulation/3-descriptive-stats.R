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
library(papaja)

# ---- load-sources ------------------------------------------------------------
# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2") # graphing
requireNamespace("tidyr") # data manipulation
requireNamespace("dplyr") # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("psych") # For descriptive functions
requireNamespace("htmlTable")


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


names(ds_wide)
names(ds_long)

# -----
# creates a data frame that gives the count of each gender
gender_count <- ds_long %>%
  dplyr::group_by(year) %>%
  dplyr::count(male) 

dplyr::glimpse(ds_wide)

# Create a vector for each year that will comprise the table. 
var_names <- c("Women (%)", "Age", "Yrs Education", "Health Conditions", "Mental status", "Word recall immediate", "Word recall delayed",
                 "Psychosocial Variables", "Loneliness", "Social contact", "Social support", "Depression", "Social network")
vars_2004 <- c(round((gender_count[2,3]/(gender_count[1,3]+gender_count[2,3]))*100,2) 
                  ,paste0(round(mean(ds_wide$intage_r_2004, na.rm = T),2), " (", round(sd(ds_wide$intage_r_2004, na.rm = T),2),")")
                  ,round(mean(ds_wide$raedyrs, na.rm = T),2)
                  ,round(mean(ds_wide$healthcond_2004, na.rm = T),2)
                  ,round(mean(ds_wide$mentalstatus_tot_2004, na.rm = T), 2)
                  ,round(mean(ds_wide$wrectoti_2004, na.rm = T), 2)
                  ,round(mean(ds_wide$wrectotd_2004, na.rm = T), 2)
                  , " "
                  ,round(mean(ds_wide$score_loneliness_3_2004, na.rm = T), 2)
                  ,round(mean(ds_wide$social_contact_total_2004, na.rm = T), 2)
                  ,round(mean(ds_wide$social_support_mean_2004, na.rm = T), 2)
                  ,round(mean(ds_wide$dep_total_2004, na.rm = T), 2)
                  ,round(mean(ds_wide$socialnetwork_total_2004, na.rm = T), 2)
                 )

freq_gender <- c((gender_count[2,3]/(gender_count[1,3]+gender_count[2,3]))*100, (gender_count[4,3]/(gender_count[3,3]+gender_count[4,3])*100))
cesd <- c(paste0(round(mean(ds_wide$dep_total_2004, na.rm = T),2)," (", round(sd(ds_wide$dep_total_2004, na.rm = T),2),")"),
          paste0(round(mean(ds_wide$dep_total_2006, na.rm = T),2)," (", round(sd(ds_wide$dep_total_2006, na.rm = T),2),")"))

desc <- cbind(var_names, vars_2004)
# -----test-table-----
htmlTable::htmlTable(desc)


