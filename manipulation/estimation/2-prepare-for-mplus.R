# NOTE:

# The function of this scripts is to impliment final exclusion criteria for the analysis
# and prepare the data for modeling in MPlus and for final reporting in dissertation.
# Use the data file produced by this file for descriptive statistics and analysis.

#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>% 
library(TabularManifest)
library(dplyr)

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
# source("./scripts/common-functions.R") # used in multiple reports
# source("./scripts/graph-presets.R") # fonts, colors, themes 
# source("./scripts/general-graphs.R") 
# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2") # graphing
# requireNamespace("readr") # data input
requireNamespace("tidyr") # data manipulation
requireNamespace("dplyr") # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit")# For asserting conditions meet expected patterns.
requireNamespace("psych") # For descriptive functions
requireNamespace("zoo")


# ---- declare-globals --------------------------------------------------------
path_input      <- "./data-unshared/derived/1-dto.rds" # product of 1-groom-augment.R
path_output     <- "./data-unshared/derived/2-dto.rds"

# ---- load-data ---------------------------------------------------------------
# load the product of 1-groom-augment
ds <- readRDS(path_input)

# ---- object-glossary ----------------------------------------------------
# list variables to keep separated for long to wide conversion
variables_static <- c(
  "id"                        #    
  ,"hhid"                    # Household id
  ,"male"                    # Gender 
  ,"birthyr_rand"            # Birth year from RAND longitudinal file
  ,"birthmo_rand"            # Month of birth
  ,"race_rand"               # Race
  ,"hispanic_rand"           # Whether Hispanic
  ,"cohort"                  # Cohort based on birth yr
  ,"raedyrs"                 # Years of Education
  ,"raedegrm"                # Highest Degree
  ,"memoryproblems_baseline" # Memory-related disease reported at the participant's first included wave
  ,"memory_disease_ever"     # Memory-disease, dementia, alzheimer disease, ever reported
  ,"age_baseline"            # Age at baseline is age at the first wave when the partipant was 65 or older
) # static

variables_longitudinal <- c(
  "lb_wave"                    # Leave-behind wave
  ,"year"                      # Year
  ,"lb_65_wave"                # Leave-behind wave at age 65 or older
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
)  # not static

# convert year to numeric for the wide to long conversion
class(ds$year)
ds$year <- as.numeric(as.character(ds$year))

# recode the listassi variable indicating which word list was given for word list learning for simplicity.
ds$listassi <- plyr::mapvalues(ds$listassi, from=c(1, 11, 21, 31), to=c(1, 2, 3, 4))
ds$listassi[is.na(ds$listassi)] <- 888

sum(is.na(ds$listassi))

# impliment exclusion criteria
# exclude proxy interviews
length(unique(ds$id))
ds <- dplyr::filter(ds, proxy != 1) 
length(unique(ds$id))

# exclude waves where the participant is younger than 65
ds <- dplyr::filter(ds, intage_r > 64) 
length(unique(ds$id))

# exclude those who are not in a cohort
ds <- dplyr::filter(ds, cohort!=0) 
length(unique(ds$id))

# exclude participants who reported having been diagnosed with a memory-related disease at baseline
# note that baseline is considered the first non NA response to the question about memory-related disease.
#ds <- dplyr::filter(ds, memoryproblems_baseline==0) 
sum(is.na(ds$memoryproblems_baseline))
# alternative exclusion criteria is to exclude those who ever reported any kind of memory disease (i.e., "memory-related disease, AD, dementia)
#ds <- dplyr::filter(ds, memory_disease_ever==F)
sum(is.na(ds$memory_disease_ever))

# exclude those who have no data for the variables of interest
ds <- ds %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(
    dep_total_missing = ifelse(all(is.na(dep_total)), T, F),
    mentalstatus_tot_missing = ifelse(all(is.na(mentalstatus_tot)), T, F),
    score_loneliness_3_missing = ifelse(all(is.na(score_loneliness_3)), T, F),
    social_contact_total_missing = ifelse(all(is.na(social_contact_total)), T, F),
    social_strain_mean_missing = ifelse(all(is.na(social_strain_mean)), T, F),
    social_support_mean_missing = ifelse(all(is.na(social_support_mean)), T, F),
    socialnetwork_total_missing = ifelse(all(is.na(socialnetwork_total)), T, F),
    wrectotd_missing = ifelse(all(is.na(wrectotd)), T, F),
    wrectoti_missing = ifelse(all(is.na(wrectoti)), T, F),
    missing_flag = sum(dep_total_missing, mentalstatus_tot_missing, score_loneliness_3_missing, social_contact_total_missing, 
                       social_strain_mean_missing,social_support_mean_missing, socialnetwork_total_missing, wrectotd_missing, wrectoti_missing)
  ) %>%
  dplyr::ungroup()


# sample to show missing flags.
ds %>%
  dplyr::filter(id==11323010|id==164888020|id == 211578010) %>%
  dplyr::select_("id","dep_total", "dep_total_missing","mentalstatus_tot", "mentalstatus_tot_missing", "score_loneliness_3","score_loneliness_3_missing", 
                 "social_contact_total_missing", "social_strain_mean_missing", "social_support_mean_missing", "socialnetwork_total_missing", 
                 "wrectotd_missing", "wrectoti_missing", "missing_flag") %>%
  print(n=nrow(.))

# Exclude those who have missing on all of one of the relevant variables.
ds <- dplyr::filter(ds, missing_flag==0)
length(unique(ds$id))

(variables_longitudinal <- variables_longitudinal[!variables_longitudinal=="year"]) # all except year
# a year based wide data set
d_wide <- ds %>%
  dplyr::select_(.dots = c(variables_static,  "year", variables_longitudinal))  %>%
  tidyr::gather_(key="variable", value="value", variables_longitudinal)  %>%
  dplyr::mutate(year=as.character(year)) %>%
  dplyr::mutate(male=as.character(male)) %>%
  dplyr::arrange(id) %>% 
  dplyr::mutate(
    # variable = gsub("^v","",variable),
    temp = paste0(variable,"_",year)) %>%
  dplyr::select(-variable,-year) %>% 
  tidyr::spread(temp, value)

# convert NA and NaN to 9999 for Mplus.
d_wide[is.na(d_wide)] <- 9999

# convert 9999 for TVC listassi to another number

d_wide$listassi_2004[d_wide$listassi_2004==9999] <- 0
d_wide$listassi_2006[d_wide$listassi_2006==9999] <- 0
d_wide$listassi_2008[d_wide$listassi_2008==9999] <- 0
d_wide$listassi_2010[d_wide$listassi_2010==9999] <- 0
d_wide$listassi_2012[d_wide$listassi_2012==9999] <- 0
d_wide$listassi_2014[d_wide$listassi_2014==9999] <- 0

mean(d_wide$raedyrs)
mean(d_wide$age_baseline)
range(d_wide$age_baseline)

sum(is.na(d_wide$hhid))

# ---- save-to-disk ----------------------------------------
saveRDS(ds, path_output)

# ---- save-mplus-data -------------------
write.table(d_wide, "./data-unshared/derived/wide-dataset.dat", row.names=F, col.names=F)
write(names(d_wide), "./data-unshared/derived/wide-variable-names.txt", sep=" ")

write.table(d_wide, "./data-unshared/derived/wide-dataset-ex.dat", row.names=F, col.names=F)
write(names(d_wide), "./data-unshared/derived/wide-variable-names-ex.txt", sep=" ")

write.table(d_wide, "./data-unshared/derived/wide-dataset-nodem.dat", row.names=F, col.names=F)
write(names(d_wide), "./data-unshared/derived/wide-variable-names-nodem.txt", sep=" ")
