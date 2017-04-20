
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
path_input      <- "./data-unshared/derived/0-dto.rds" # product of 0-ellis-island
path_output     <- "./data-unshared/derived/1-dto.rds"


loneliness <- c(
   "score_loneliness_3"  # score computed based on 3 items
  ,"score_loneliness_11" # score computed based on all 11 items
)

focal_variables <- c(loneliness)

# ---- load-data ---------------------------------------------------------------
# load the product of 1-scale-assembly.R a long data file
ds <- readRDS(path_input)

set.seed(41) # to set specific seed
# set.seed(NULL) # to disable specific seed
sample_size <- 3
ids <- sample(unique(ds$id), sample_size)
# ---- inspect-data -------------------------------------------------------------
names(ds)


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

# ---- create-lb_wave-age-65-or-older-counter ------------------------

ds$lb_65plus_tag <- ifelse(rowSums(!is.na(ds[leave_behind_items])) > 0 & ds[,"intage_r"] > 64, TRUE, FALSE)
# inspect data for a few ids  
ds %>% 
  dplyr::filter(id %in% ids) %>% 
  dplyr::select(id, year, intage_r, closechild, lb_65plus_tag) %>% 
  print(n=nrow(.))
# create a temp object to store selected id-year values
d_temp <- ds %>% 
  dplyr::group_by(id) %>% 
  dplyr::filter(lb_65plus_tag) %>%
  dplyr::mutate(
    n_lb65_wave = sum(lb_65plus_tag), # number of lb_waves for which data exists, auxillary 
    lb_65_wave   = seq(n())               # oder of LB response
  ) 
d_temp %>% glimpse()
# print a few cases for visual inspection
d_temp %>% 
  dplyr::filter(id %in% ids) %>%
  dplyr::select(id, year, closechild, lb_65plus_tag, n_lb65_wave, lb_65_wave, lb_wave) %>% 
  print(n=nrow(.))
# bring the lb_wave variabe into the larger file
ds <- dplyr::left_join(
  ds,  
  d_temp %>% dplyr::select(id, year, lb_65_wave ), 
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

ids_discordant_nchild <- ds %>% 
  dplyr::filter(closechild > nchild) %>% 
  dplyr::distinct(id)
ids_discordant_nchild <- as.integer(ids_discordant_nchild$id)

target_ids <- unique( c(ids_repeating_digit, ids_high_score))   
target_ids %>% length()
# TODO : rexpress the above code as two new variables in dplyr::mutate() statement

ds %>% distinct(id) %>% count()
# Impliment Rule 1:	
# If the number of close children listed was a double digit (e.g., 22, 33, 44) the number of 
# children was made equal to the single digit. 
# [This solves the problem for the majority of cases with greater than 20 close children from 239 to 86]

# print a few cases for visual inspection
set.seed(42)
target_sample <- sample(target_ids,5)

ds %>% 
  dplyr::filter(id %in% target_sample) %>% 
  dplyr::select(id, year, nchild, closechild, lb_65_wave, lb_wave) %>% 
  print(n=nrow(.))

# for(i in ids_repeating_digit){
#   ds %>%
#     dplyr::filter(id %in% i) %>%
#     dplyr::select(id, year, nchild, closechild, lb_65_wave, lb_wave) %>%
#     print(n=nrow(.))
# }

# create separate variables for each digit.
ds$digit1 <- substr(ds$closechild,1,1)
ds$digit2 <- substr(ds$closechild,2,3)


ds$digit1 <- plyr::mapvalues(ds$digit1, from=c("N"), to=c(NA))
ds$digit2 <- plyr::mapvalues(ds$digit2, from=c("aN"), to=c(NA))

# replace the double values with the single digit value.
ds$closechild <- as.numeric(ifelse(ds$digit1 == ds$digit2 & ds$closechild > ds$nchild, ds$digit2, ds$closechild))

# Impliment Rule 2:
# Recode closechild [number of children with whom one has a close relationship to NA if greater than 20 and this is discordant
# with the given number of living children]

# print a few cases for visual inspection
set.seed(42)
target_sample <- sample(ids_high_score,5)

ds %>% 
  dplyr::filter(id %in% target_sample) %>% 
  dplyr::select(id, year, nchild, closechild, lb_65_wave, lb_wave) %>% 
  print(n=nrow(.))

ds$closechild <- ifelse(ds$closechild>20 & ds$closechild > ds$nchild, NA, ds$closechild)

# ---- detect-outliers ----------------------------------------------------------
ids <- sample(size = 10, x = unique(ds$id) )
selected_variables <- c("id","year",'lb_wave', "closechild", "closefam", "closefri")

# remove the values we consider outliers
# Here is our rules for defining an outlier
# If 1)the number of close family members is greater than 4 standard deviations above the mean (21) 
# and 2)the change in number of close family members is greater than 4 standard deviations above the 
# mean change (21) then recode to NA. This is 112 cases. 
# identify cases in which the criteria for outliers is broken
d <- ds %>% 
  # dplyr::filter(id %in% ids) %>%
  # filter(id == 22860010 ) %>% 
  dplyr::filter(lb_wave > 0) %>%
  dplyr::select_(.dots = selected_variables) %>% 
  dplyr::group_by(id) %>%
  dplyr::mutate(
    closefam_lag  = abs(             closefam - dplyr::lag(closefam)),
    closefam_lead = abs(dplyr::lead(closefam) - closefam),
    closefri_lag  = abs(             closefri - dplyr::lag(closefri)),
    closefri_lead = abs(dplyr::lead(closefri) - closefri)
  ) %>% 
  dplyr::ungroup() %>%
  dplyr::mutate(
    closefam_mean     = mean(closefam, na.rm=T),
    closefam_sd       = sd(closefam, na.rm=T),
    closefam_lag_mean = mean(closefam_lag, na.rm=T),
    closefam_lag_sd   = sd(closefam_lag, na.rm=T),
    closefam_lead_m   = mean(closefam_lead, na.rm=T),
    closefam_lead_sd  = sd(closefam_lead, na.rm = T),
    closefam_flag     = ifelse(closefam     > closefam_mean + 4*closefam_sd, TRUE, FALSE),
    closefam_lag_flag = ifelse(closefam_lag > closefam_lag_mean + 4*closefam_lag_sd, TRUE, FALSE),
    closefam_lead_flag= ifelse(closefam_lead > closefam_lead_m + 4*closefam_lead_sd, TRUE, FALSE),
    closefam_out      = ifelse( (closefam_flag & closefam_lag_flag) | (closefam_flag & closefam_lead_flag), TRUE, FALSE),
    #closefri
    closefri_mean     = mean(closefri, na.rm=T),
    closefri_sd       = sd(closefri, na.rm=T),
    closefri_lag_mean = mean(closefri_lag, na.rm=T),
    closefri_lag_sd   = sd(closefri_lag, na.rm=T),
    closefri_lead_m   = mean(closefri_lead, na.rm=T),
    closefri_lead_sd  = sd(closefri_lead, na.rm = T),
    closefri_flag     = ifelse(closefri     > closefri_mean + 4*closefri_sd, TRUE, FALSE),
    closefri_lag_flag = ifelse(closefri_lag > closefri_lag_mean + 4*closefri_lag_sd, TRUE, FALSE),
    closefri_lead_flag= ifelse(closefri_lead > closefri_lead_m + 4*closefri_lead_sd, TRUE, FALSE),
    closefri_out      = ifelse( (closefri_flag & closefri_lag_flag) | (closefri_flag & closefri_lead_flag), TRUE, FALSE),
    # TODO : Cassandra, please finish for the other two variables
    # closechild_out    = ifelse( closefri_flag & closefri__lag_flag, TRUE, FALSE),
    # flag_out = ifelse(closefam_out | closefri_out | closechild_out, TRUE, FALSE)
    flag_out_obs = closefam_out | closefri_out # replace this when finished for all three  
  ) %>% 
  dplyr::group_by(id) %>% 
  dplyr::mutate(
    flag_out_id  = ifelse(sum(flag_out_obs)>0L, TRUE, FALSE)
  ) %>% 
  dplyr::ungroup()


# print a case for inspection
ds %>%
  dplyr::filter(id == 22860010) %>%
  dplyr::select_(.dots = selected_variables) %>%
  print(n=nrow(.))

# add the flag variables to the larger data set
ds <- dplyr::left_join(
  ds,  
  d %>% dplyr::select(id, year, closefam_out, closefri_out), 
  by = c("id","year")
)

# print a case for inspection
ds %>%
  dplyr::filter(id == 22860010) %>%
  dplyr::select_("id","year",'lb_wave', "closechild", "closefam", "closefri", "closefam_out", "closefri_out") %>%
  print(n=nrow(.))

# recode closefam values flagged as errors to NA
ds$closefam_clean <- ifelse(ds$closefam_out == TRUE, NA, ds$closefam)
# recode closefri values flagged as errors to NA
ds$closefri_clean <- ifelse(ds$closefri_out == TRUE, NA, ds$closefri)

networkvars <- c("snspouse", "snchild", "snfamily", "snfriends")
closevars <- c("closechild","closefam_clean","closefri_clean")

# Compute total scores with corrected data
compute_socialnetwork_scale_scores <- function(d){
  #d <- ds_long %>% dplyr::filter(hhidpn %in% c(3010,10281010))
  d[,"socialnetwork_total"] <- apply(d[networkvars],1,sum, na.rm = TRUE)
  d[,"close_social_network"] <- apply(d[closevars],1,sum, na.rm = TRUE)
  d$missing_count <- apply(d[networkvars], 1, function(z) sum(is.na(z)))
  d <- d %>%
    dplyr::mutate(
      socialnetwork_total = ifelse(missing_count<4,
                                   socialnetwork_total,NA))
  d$missing_count <- apply(d[closevars], 1, function(z) sum(is.na(z)))
  d <- d %>%
    dplyr::mutate(
      close_social_network = ifelse(missing_count<4,
                                    close_social_network,NA)
    )
  return(d)
}

ds <- ds %>% compute_socialnetwork_scale_scores()

# create a merge interview date variables for more precise time calculations and create a time variable for HRS data waves
ds <- ds %>%
  dplyr::mutate(
    interview_date = paste0(interview_mth,"/",interview_yr),
    interview_date = zoo::as.yearmon(interview_date, "%m/%Y"),
    hrs_tscore = interview_date-dplyr::lag(interview_date)
  )

# ---- save-to-disk ----------------------------------------
saveRDS(ds, path_output)

