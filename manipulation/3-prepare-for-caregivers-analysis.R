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
library(ggplot2)

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
path_input  <- "./data-unshared/derived/2-dto_c.rds" # product of ./manipulation/2-identify-caregivers.R
#path_output     <- "./data-unshared/derived/2-dto.rds"

# ---- load-data ---------------------------------------------------------------
# load the product of 2-identify-caregivers
ds <- readRDS(path_input)

# convert year to numeric for the wide to long conversion
class(ds$year)
ds$year <- as.numeric(as.character(ds$year))

# ---- exclusion criteria ------------------------------------------------------

# exclude caregivers who have dementia themselves
#ds <- ds %>% 
#  filter(dementia_ever != TRUE)

# exclude caregivers who have dementia themselves
#ds <- ds %>% 
#  filter(alzheimer_ever != TRUE)
length(unique(ds$id))

# exclude caregivers who have dementia themselves
ds <- ds %>% 
  filter(memory_disease_ever == FALSE)
length(unique(ds$id))

# exclude cases where there is no information for spouses
ds <- ds %>% 
  filter(is.na(spdem_pattern_assigned)==FALSE)
length(unique(ds$id))

# recode male to be 0 or 1
ds <- ds %>% 
  dplyr::mutate(
    male = male - 1
  )

# recode the hrs_tscore 
ds <- ds %>%
  dplyr::mutate(
        tscore = hrs_tscore+dplyr::lag(hrs_tscore))

# print cases for inspection
ds %>%
  dplyr::filter(id == 22860010|id==3010|id==10001010) %>%
  dplyr::select_("id","tscore","year") %>%
  print(n=nrow(.))

d_test <- ds %>% 
  filter(spdem_pattern_assigned=="000000")
length(unique(d_test$id))

table(ds$spdem_pattern_assigned)

ds$spdem_pattern_assigned
sum(is.na(ds$spouse_memory_disease_ever)==FALSE)


#ds <- ds %>% dplyr::filter(!spouse_memory_disease_pattern %in% list_suspect_memorydisease_patterns)
ds <- dplyr::filter(ds, proxy != 1) # exclude proxy interviews
length(unique(ds$id))

ds$timevar <- plyr::mapvalues(ds$year, from=c(2004, 2006, 2008, 2010, 2012, 2014), to=c(0, 2, 4, 6, 8, 10))

#-Select only relevant demographic variables and total scores for analysis----------

# list variables to keep separated for long to wide conversion
variables_static <- c("id", "male", "age_baseline", "birthyr_rand", "birthmo_rand", "race_rand", "hispanic_rand", "cohort", "raedyrs","raedegrm","spdem_pattern_assigned", "spdem_2004", "spdem_2006",
                      "spdem_2008", "spdem_2010", "spdem_2012", "spdem_2014","cspdem_2004", "cspdem_2006", "cspdem_2008", "cspdem_2010", "cspdem_2012", "cspdem_2014")

variables_longitudinal <- c(                    # Leave-behind wave
  "year"                      # Year
  ,"hrs_tscore" 
  ,"interview_date"            # Interview data year and   month
  ,"responded"                 # 
  ,"proxy"                     #
  ,"hhres"                     #
  ,"countb20r"                 #
  ,"shhidpnr"                  #
  ,"rmaritalst"                #
  ,"intage_r"                  #
  ,"rpartst"                   #
  ,"score_loneliness_3"        #
  ,"score_loneliness_11"       #
  ,"snspouse"                  #
  ,"snchild"                   #
  ,"snfamily"                  #
  ,"snfriends"                 #
  ,"socialnetwork_total"       #
  ,"close_social_network"      #
  ,"social_support_mean"       #
  ,"social_strain_mean"        #
  ,"social_contact_total"      #
  ,"activity_mean"             #
  ,"activity_sum"              #
  ,"srmemory"                  #
  ,"srmemoryp"                 #
  ,"wrectoti"                  #
  ,"wrectotd"                  #
  ,"listassi"
  ,"mentalstatus_tot"          #
  ,"vocab_total"               #
  ,"dep_total"                 #
  ,"healthcond"                #
  ,"exercise"                  #
  ,"memry"
  ,"smemry"
  ,"memrye"
  ,"smemrye"
  ,"alzhe"
  ,"salzhe"
  ,"alzhee"
  ,"salzhee"
  ,"demen"
  ,"sdemen"
  ,"sdemene" 
  ,"spouse_memory_disease"
  ,"spouse_memory_disease_pattern"
  ,"timevar"
)  # not static

variables_longitudinal_short <- c(
  "year"                      # Year
  ,"hrs_tscore"
  ,"intage_r"
  ,"interview_date"            # Interview data year and   month
  ,"responded"                 # 
  ,"proxy"                     #
  ,"hhres"                     #
  ,"shhidpnr"                  #
  ,"rmaritalst"                #
  ,"widow"
  ,"score_loneliness_3"        #
  ,"socialnetwork_total"       #
  ,"close_social_network"      #
  ,"social_support_mean"       #
  ,"social_strain_mean"        #
  ,"social_contact_total"      #
  ,"wrectoti"                  #
  ,"wrectotd"                  #
  ,"listassi"
  ,"mentalstatus_tot"          #
  ,"dep_total"                 #
  ,"healthcond"                #
  ,"timevar"
)  # not static


#ds$listassi <- plyr::mapvalues(ds$listassi, from=c(1, 11, 21, 31), to=c(1, 2, 3, 4))

# create a smaller dataset
#d <- ds %>%
# dplyr::select_(.dots = c(variables_static,  "year", variables_longitudinal)) 

# create a smaller dataset using the short version
d <- ds %>%
  dplyr::select_(.dots = c(variables_static,  "year", variables_longitudinal_short)) 

#table(d$spouse_memory_disease, d$year)
names(d)

table(d$widow)
table(d$rmaritalst)

# create a trajectory plot
wrecti_plot <- ggplot(data = d, aes(x = timevar, y = wrectoti, group = id))
wrecti_plot + geom_line()


# convert NA and NaN to 9999 for Mplus.
d[is.na(d)] <- 9999

# prepared for Mplus
write.table(d, "./data-unshared/derived/c-long-dataset.dat", row.names=F, col.names=F)
write(names(d), "./data-unshared/derived/c-long-variable-names.txt", sep=" ")
ds %>% glimpse()

# (variables_longitudinal <- variables_longitudinal[!variables_longitudinal=="year"]) # all except year
# # a year based wide data set
# d_wide <- ds %>%
#   dplyr::select_(.dots = c(variables_static,  "year", variables_longitudinal))  %>%
#   tidyr::gather_(key="variable", value="value", variables_longitudinal)  %>%
#   dplyr::mutate(year=as.character(year)) %>%
#   dplyr::mutate(male=as.character(male)) %>%
#   dplyr::arrange(id) %>% 
#   dplyr::mutate(
#     # variable = gsub("^v","",variable),
#     temp = paste0(variable,"_",year)) %>%
#   dplyr::select(-variable,-year) %>% 
#   tidyr::spread(temp, value)

(variables_longitudinal_short <- variables_longitudinal_short[!variables_longitudinal_short=="year"]) # all except year

# a year based wide data set
d_wide <- ds %>%
  dplyr::select_(.dots = c(variables_static,  "year", variables_longitudinal_short))  %>%
  tidyr::gather_(key="variable", value="value", variables_longitudinal_short)  %>%
  dplyr::mutate(year=as.character(year)) %>%
  dplyr::mutate(male=as.character(male)) %>%
  dplyr::arrange(id) %>% 
  dplyr::mutate(
    # variable = gsub("^v","",variable),
    temp = paste0(variable,"_",year)) %>%
  dplyr::select(-variable,-year) %>% 
  tidyr::spread(temp, value)


ds %>% glimpse()
ds_wide %>% glimpse()
d_wide %>% glimpse()
# Create a variable that is the interaction of social variables and caregiver status
# depression and caregiving
d_wide <- d_wide %>% 
  dplyr::mutate(
    depxc_2004 = spdem_2004*dep_total_2004,
    depxc_2006 = spdem_2006*dep_total_2006,
    depxc_2008 = spdem_2008*dep_total_2008,
    depxc_2010 = spdem_2010*dep_total_2010,
    depxc_2012 = spdem_2012*dep_total_2012,
    depxc_2014 = spdem_2014*dep_total_2014
  )

# social support and caregiving
d_wide <- d_wide %>% 
  dplyr::mutate(
    ssxc_2004 = spdem_2004*social_support_mean_2004,
    ssxc_2006 = spdem_2006*social_support_mean_2006,
    ssxc_2008 = spdem_2008*social_support_mean_2008,
    ssxc_2010 = spdem_2010*social_support_mean_2010,
    ssxc_2012 = spdem_2012*social_support_mean_2012,
    ssxc_2014 = spdem_2014*social_support_mean_2014
  )

# social contact and caregiving
d_wide <- d_wide %>% 
  dplyr::mutate(
    scxc_2004 = spdem_2004*social_contact_total_2004,
    scxc_2006 = spdem_2006*social_contact_total_2006,
    scxc_2008 = spdem_2008*social_contact_total_2008,
    scxc_2010 = spdem_2010*social_contact_total_2010,
    scxc_2012 = spdem_2012*social_contact_total_2012,
    scxc_2014 = spdem_2014*social_contact_total_2014
  )

# loneliness and caregiving
d_wide <- d_wide %>% 
  dplyr::mutate(
    lonexc_2004 = spdem_2004*score_loneliness_3_2004,
    lonexc_2006 = spdem_2006*score_loneliness_3_2006,
    lonexc_2008 = spdem_2008*score_loneliness_3_2008,
    lonexc_2010 = spdem_2010*score_loneliness_3_2010,
    lonexc_2012 = spdem_2012*score_loneliness_3_2012,
    lonexc_2014 = spdem_2014*score_loneliness_3_2014
  )

#hrs_tscore was calculated in 2-transformations as hrs_tscore = interview_date-dplyr::lag(interview_date)
#this has created no none missing values for the 2004 wave
# 

mean(d_wide$wrectoti_2004, na.rm = T)
mean(d_wide$wrectotd_2004, na.rm = T)
saveRDS(d_wide, file="./data-unshared/derived/caregiver-data-wide.rds")
saveRDS(ds, file="./data-unshared/derived/caregiver-data-long.rds")


range(d_wide$dep_total_2004, na.rm = T)
range(d_wide$mentalstatus_tot_2004, na.rm = T)
range(d_wide$hrs_tscore_2004, na.rm = T)
range(d_wide$hrs_tscore_2006, na.rm = T)

mean(d_wide$raedyrs, na.rm = T)

d_wide[is.na(d_wide)] <- 9999


# prepared for Mplus
write.table(d_wide, "./data-unshared/derived/c-wide-dataset.dat", row.names=F, col.names=F)
write(names(d_wide), "./data-unshared/derived/c-wide-variable-names.txt", sep=" ")

#ds <- ds %>% dplyr::filter(!spouse_memory_disease_pattern %in% list_suspect_memorydisease_patterns)
d_caregivers <- dplyr::filter(d_wide, spdem_pattern_assigned != "000000") # exclude proxy interviews
length(unique(d_caregivers$id))
table(d_caregivers$male)

#ds <- ds %>% dplyr::filter(!spouse_memory_disease_pattern %in% list_suspect_memorydisease_patterns)
d_noncaregivers <- dplyr::filter(d_wide, spdem_pattern_assigned == "000000") # exclude proxy interviews
length(unique(d_noncaregivers$id))
table(d_noncaregivers$male)

table(d_wide$spdem_2004)
table(d_wide$spdem_2006)
table(d_wide$spdem_2008)
table(d_wide$spdem_2010)
table(d_wide$spdem_2012)
table(d_wide$spdem_2014)
table(d_wide$hrs_tscore_2006)
table(d_wide$timevar_2014)


range(d_wide$social_contact_total_2004)
range(d_wide$social_support_mean_2004)
range(d_wide$social_support_mean_2006)
table(d_wide$rmaritalst)

mean(d_wide$age_baseline)
mean(d_wide$raedyrs)
