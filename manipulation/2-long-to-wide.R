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
requireNamespace("mice")

# ---- declare-globals --------------------------------------------------------
# connect to the data transfer object from the HRS repository
path_input      <- "./data-unshared/derived/1-dto.rds" # product of 1-groom-augment.R
path_output     <- "./data-unshared/derived/2-dto.rds"

# ---- load-data ---------------------------------------------------------------
# load the product of 1-groom-augment
ds <- readRDS(path_input)

# convert year to numeric for the wide to long conversion
class(ds$year)
ds$year <- as.numeric(as.character(ds$year))

#-Select only relevant demographic variables and total scores for analysis----------

# list variables to keep separated for long to wide conversion
variables_static <- c("id", "male", "birthyr_rand", "birthmo_rand", "race_rand", "hispanic_rand", "cohort", "raedyrs","raedegrm")

variables_longitudinal <- c("lb_wave", "year","lb_65_wave","interview_date", "responded","proxy", "hhres","countb20r","shhidpnr","rmaritalst","intage_r","rpartst","score_loneliness_3", "score_loneliness_11",
                            "snspouse", "snchild", "snfamily", "snfriends","socialnetwork_total", "close_social_network",
                            "social_support_mean", "social_strain_mean","social_contact_total",
                            "activity_mean", "activity_sum","srmemory", "srmemoryp","listassi", "wrectoti", "wrectotd","mentalstatus_tot","vocab_total",
                            "dep_total","healthcond", "exercise")  # not static


ds$listassi <- plyr::mapvalues(ds$listassi, from=c(1, 11, 21, 31), to=c(1, 2, 3, 4))

# create a smaller dataset
#ds <- ds %>%
#  dplyr::select_(.dots = c(variables_static,  "year", variables_longitudinal)) 
names(ds)
ds %>% glimpse()

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

# select cases with at least one lb wave and less than 5.
ds_lb <- subset(ds, lb_wave>0 & lb_wave!=5)

# define variable properties for long-to-wide conversion
(variables_longitudinal <- variables_longitudinal[!variables_longitudinal=="lb_wave"]) # all except year

# an lb wave based wide data set
dlb_wide <- ds_lb %>%
  dplyr::select_(.dots = c(variables_static, "lb_wave", variables_longitudinal))  %>%
  tidyr::gather_(key="variable", value="value", variables_longitudinal)  %>%
  dplyr::mutate(lb_wave=as.character(lb_wave)) %>%
  dplyr::mutate(male=as.character(male)) %>%
  dplyr::arrange(id) %>% 
  dplyr::mutate(
    # variable = gsub("^v","",variable),
    temp = paste0(variable,"_",lb_wave)) %>%
  dplyr::select(-variable,-lb_wave) %>% 
  tidyr::spread(temp, value)

dplyr::glimpse(dlb_wide)

# Now that the data is in wide format it is easier to calculate a time score for lbwaves.
dlb_wide <- dlb_wide %>% 
  dplyr::mutate(
    lbtime_1 = 0,
    lbtime_2 = interview_date_2 - interview_date_1,
    lbtime_3 = interview_date_3 - interview_date_1,
    lbtime_4 = interview_date_4 - interview_date_1
  )

# ---- save-r-data -------------------
# tranformed data with supplementary variables
#saveRDS(ds,"./data-unshared/derived/data-long-select.rds")

saveRDS(d_wide, file="./data-unshared/derived/data-wide.rds")
# lb wave based wide file
saveRDS(dlb_wide, file="./data-unshared/derived/lb-data-wide.rds")


# convert NA and NaN to 9999 for Mplus.
dlb_wide[is.na(dlb_wide)] <- 9999

# prepared for Mplus
write.table(dlb_wide, "./data-unshared/derived/wide-dataset.dat", row.names=F, col.names=F)
write(names(dlb_wide), "./data-unshared/derived/wide-variable-names.txt", sep=" ")

# ----- Create-data-file --------
# a data set that contains only lb waves for those aged 65 or older. 
# select cases with at least one lb wave and less than 5.
ds_lb65 <- subset(ds, lb_65_wave>0 & lb_65_wave!=5)

# define variable properties for long-to-wide conversion
(variables_longitudinal <- variables_longitudinal[!variables_longitudinal=="lb_65_wave"]) # all except year

# an lb wave based wide data set
dlb65_wide <- ds_lb65 %>%
  dplyr::select_(.dots = c(variables_static, "lb_65_wave", variables_longitudinal))  %>%
  tidyr::gather_(key="variable", value="value", variables_longitudinal)  %>%
  dplyr::mutate(lb_65_wave=as.character(lb_65_wave)) %>%
  dplyr::mutate(male=as.character(male)) %>%
  dplyr::arrange(id) %>% 
  dplyr::mutate(
    # variable = gsub("^v","",variable),
    temp = paste0(variable,"_",lb_65_wave)) %>%
  dplyr::select(-variable,-lb_65_wave) %>% 
  tidyr::spread(temp, value)


dlb65_wide <- dlb65_wide %>% 
  dplyr::mutate(
    lbtime_1 = 0,
    lbtime_2 = interview_date_2 - interview_date_1,
    lbtime_3 = interview_date_3 - interview_date_1,
    lbtime_4 = interview_date_4 - interview_date_1
  )


# ---- save-r-data -------------------
# tranformed data with supplementary variables

saveRDS(ds_lb65,"./data-unshared/derived/data-long-lbwaves65.rds")

# lb wave based wide file
saveRDS(dlb65_wide, file="./data-unshared/derived/lb65-data-wide.rds")

# convert NA and NaN to 9999 for Mplus.
dlb65_wide[is.na(dlb65_wide)] <- 9999
# prepared for Mplus
write.table(dlb65_wide, "./data-unshared/derived/wide65-dataset.dat", row.names=F, col.names=F)
write(names(dlb65_wide), "./data-unshared/derived/wide65-variable-names.txt", sep=" ")
