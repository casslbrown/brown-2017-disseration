
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

# ---- declare-globals --------------------------------------------------------
# connect to the data transfer object from the HRS repository
path_input      <- "../HRS/data-unshared/derived/1-dto.rds" # product of 1-groom-augment.R
path_output     <- "./data-unshared/derived/2-dto.rds"

# convert year to numeric for the wide to long conversion
ds$year <- as.numeric(as.character(ds$year))

#-Select only relevant demographic variables and total scores for analysis----------

# list variables to keep separated for long to wide conversion
variables_static <- c("id", "male", "birthyr_rand", "birthmo_rand", "race_rand", "hispanic_rand", "cohort", "raedyrs","raedegrm")

variables_longitudinal <- c("year","lbwave","interview_date", "responded","proxy","countb20r","shhidpnr","rmaritalst","intage_r","rpartst","score_loneliness_3", "score_loneliness_11",
                            "snspouse", "snchild", "snfamily", "snfriends","socialnetwork_total", "close_social_network",
                            "support_spouse_total", "support_child_total", "support_fam_total", "support_friend_total",
                            "strain_spouse_total", "strain_child_total", "strain_family_total", "strain_friends_total",
                            "children_contact_mean", "family_contact_mean", "friend_contact_mean",
                            "activity_mean", "activity_sum","srmemory", "srmemoryp","wrectoti", "wrectotd","mentalstatus_tot","vocab_total",
                            "dep_total","healthcond", "exercise")  # not static

# create a smaller dataset
d_long <- ds_65 %>%
  dplyr::select_(.dots = c(variables_static,  "lbwave", variables_longitudinal)) 
names(d_long)

# a year based wide data set
d_wide <- ds %>%
  dplyr::select_(.dots = c(variables_static,  "year", variables_longitudinal))  %>%
  tidyr::gather_(key="variable", value="value", variables_longitudinal)  %>%
  dplyr::mutate(year=as.character(year)) %>%
  dplyr::mutate(male=as.character(male)) %>%
  dplyr::arrange(hhidpn) %>% 
  dplyr::mutate(
    # variable = gsub("^v","",variable),
    temp = paste0(variable,"_",year)) %>%
  dplyr::select(-variable,-year) %>% 
  tidyr::spread(temp, value)

ds_lb <- subset(d_long, lbwave>0 & lbwave!=5)
less65 <- subset(ds_lb, intage_r < 65)
length(unique(less65$id))
# define variable properties for long-to-wide conversion
(variables_longitudinal <- variables_longitudinal[!variables_longitudinal=="lbwave"]) # all except year

# an lb wave based wide data set
dlb_wide <- ds_lb %>%
  dplyr::select_(.dots = c(variables_static,  "lbwave", variables_longitudinal))  %>%
  tidyr::gather_(key="variable", value="value", variables_longitudinal)  %>%
  dplyr::mutate(lbwave=as.character(lbwave)) %>%
  dplyr::mutate(male=as.character(male)) %>%
  dplyr::arrange(id) %>% 
  dplyr::mutate(
    # variable = gsub("^v","",variable),
    temp = paste0(variable,"_",lbwave)) %>%
  dplyr::select(-variable,-lbwave) %>% 
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