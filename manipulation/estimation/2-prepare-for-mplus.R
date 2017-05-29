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
  ,"male"                      # Gender 
  ,"birth_year"                # Birth year from RAND longitudinal file
  ,"birth_month"               # Month of birth
  ,"race"                      # Race
  ,"hispanic"                  # Whether Hispanic
  ,"cohort"                    # Cohort based on birth yr
  ,"edu_years"                 # Years of Education
  ,"highest_degree"            # Highest Degree
  ,"memoryproblems_baseline"   # Memory-related disease reported at the participant's first included wave
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

# impliment exclusion criteria
# exclude proxy interviews
length(unique(ds$id))
ds <- dplyr::filter(ds, proxy != 1) 
length(unique(ds$id))
# exclude waves where the participant is younger than 65
ds <- dplyr::filter(ds, intage_r > 64) 
length(unique(ds$id))
# exclude participants who reported having been diagnosed with a memory-related disease at baseline
# note that baseline is considered the first non NA response to the question about memory-related disease.
ds <- dplyr::filter(ds, memoryproblems_baseline==0) 

# ---- save-to-disk ----------------------------------------
saveRDS(ds, path_output)

# ---- save-r-data -------------------


# ---- save-mplus-data -------------------


