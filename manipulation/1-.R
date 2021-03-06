
#############################################################
# 1-groom-augment
# This script is to be run following 0-ellis-island.R
# The purpose of this script is to perform data related cleaning and transformations
# it will filter observations, remove suspected typos, and compute various utility variables
# Data exploration and explanations of rational for data cleaning can be found (insert that here when available)

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
path_input      <- "../HRS/data-unshared/derived/1-dto.rds" # product of 1-assembly-line.R
path_input_list <- "../HRS/data-unshared/derived/1-dto-list.rds" # product of 1-assembly-line.R
path_output     <- "./data-unshared/derived/0-dto.rds"
# ---- load-data ---------------------------------------------------------------
# load the product of 1-scale-assembly.R a long data file
ds <- readRDS(path_input)
ls <- readRDS(path_input_list)

# ---- inspect-data -------------------------------------------------------------
names(ds)
names(ls)

# ---- create-lb_wave-counter ------------------------
# We will create a new variable that counts 
# the number of times a person provided a response on leave-behind qustnr. 

# The following items beling to the leave-behind questionnaire
# if any of these items show a non-NA value, we consider that person
# to be engaged with the leave-behind qstn in this year
leave_behind_items  <- c(
  "score_loneliness_3"
  ,"score_loneliness_11"
  ,"snspouse"
  ,"snchild"
  ,"snfamily"
  ,"snfriends"
  ,"support_spouse_total"
  ,"support_child_total"
  ,"support_fam_total"
  ,"support_friend_total"
  ,"strain_spouse_total"
  ,"strain_child_total"
  ,"strain_family_total"
  ,"strain_friends_total"
  ,"children_contact_mean"
  ,"family_contact_mean"
  ,"friend_contact_mean"
  ,"activity_mean"
  ,"activity_sum"
)
ds$leave_behind_tag <- ifelse(rowSums(!is.na(ds[leave_behind_items])) > 0 , TRUE, FALSE)
# inspect data for a few ids  
ds %>% 
  dplyr::filter(id %in% ids) %>% 
  dplyr::select(id, year, closechild, leave_behind_tag) %>% 
  print(n=nrow(.))
# create a temp object to store selected id-year values
d_temp <- ds %>% 
  dplyr::group_by(id) %>% 
  dplyr::filter(leave_behind_tag) %>%
  dplyr::mutate(
    n_lb_wave = sum(leave_behind_tag), # number of lb_waves for which data exists, auxillary 
    lb_wave   = seq(n())               # oder of LB response
  ) 
d_temp %>% glimpse()
# print a few cases for visual inspection
d_temp %>% 
  dplyr::filter(id %in% ids) %>%
  dplyr::select(id, year, closechild, leave_behind_tag, n_lb_wave, lb_wave) %>% 
  print(n=nrow(.))
# bring the lb_wave variabe into the larger file
ds <- dplyr::left_join(
  ds,  
  d_temp %>% dplyr::select(id, year, lb_wave ), 
  by = c("id","year")
)


# ---- correct-number-children ----------------------------------------------------
# Some of the values of closechild (number of children with which R stays close)
# are supsicious (e.g. 66, 44, 127) of bying typoes
# this section of the code demonstrates the issues and criteria for deletion
# of values suspect of being recording/handing errors

# create a target list of persons to test over
# create a list of ids, whose values on closechild include a repeating digit
ids_repeating_digit <- ds %>% 
  dplyr::filter(closechild %in% c(11,22,33,44,55,66,77,88,99)) %>% 
  dplyr::distinct(id) 
ids_repeating_digit <- as.integer(ids_repeating_digit$id)

ids_high_score <- ds %>% 
  dplyr::filter(closechild > 20) %>% 
  dplyr::distinct(id)
ids_high_score <- as.integer(ids_high_score$id)

target_ids <- unique( c(ids_repeating_digit, ids_high_score))   
target_ids %>% length()
# TODO : rexpress the above code as two new variables in dplyr::mutate() statement

ds %>% distinct(id) %>% count()
# Impliment Rule 1:	
# If the number of close children listed was a double digit (e.g., 22, 33, 44) the number of 
# children was made equal to the single digit. 
# [This solves the problem for the majority of cases with greater than 20 close children from 239 to 86]
set.seed(42)
ids <- sample(target_ids,5)
ids
ds %>% 
  dplyr::filter(id %in% ids) %>% 
  dplyr::select(id, year, closechild) %>% 
  print(n=nrow(.))

ds %>% 
  dplyr::filter(id %in% target_ids) %>% 
  dplyr::select(id, year, closechild) %>% 
  print(n=nrow(.))


for(i in ids){
  ds %>% 
    dplyr::filter(id %in% i) %>% 
    dplyr::select(id, year, closechild) %>% 
    print(n=nrow(.))
}


# create separate variables for each digit.
ds$digit1 <- substr(ds$closechild,1,1)
ds$digit2 <- substr(ds$closechild,2,3)


ds$digit1 <- plyr::mapvalues(ds$digit1, from=c("N"), to=c(NA))
ds$digit2 <- plyr::mapvalues(ds$digit2, from=c("aN"), to=c(NA))

# replace the double values with the single digit value.
ds$closechild <- as.numeric(ifelse(ds$digit1 == ds$digit2, ds$digit2, ds$closechild))

# Impliment Rule 2:
# Otherwise, recode closechild [number of children with whom one has a close relationship to NA if greater than]
ds$closechild <- ifelse(ds$closechild>20, NA, ds$closechild)


# ----- --------------------------
