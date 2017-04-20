#############################################################
# 0-ellis-is
# This script is to be run following 1-scale-assembly.R
# The purpose of this script is to take the long format data frame produced by that script and perform data cleaning operations.
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

# ---- merge-assembles-scales ----------------------------------
# you can recreate ds from ls by running the following merging script:
# merge multiple datasets that are stored as elements of a list
# merge_multiple_files <- function(list, by_columns){
#   Reduce(function( d_1, d_2 ) dplyr::full_join(d_1, d_2, by=by_columns), list)
# }
# 
# ds <- merge_multiple_files(ls, by_columns = c("year","hhidpn"))

# ---- tweak-data --------------------------------------------------------------
ds <- ds %>% dplyr::rename(id = hhidpn)
set.seed(41) # to set specific seed
# set.seed(NULL) # to disable specific seed
sample_size <- 3
ids <- sample(unique(ds$id), sample_size)
ids


# ---- save-to-disk ----------------------------------------
saveRDS(ds_long, path_output)



# ---- tweak-data --------------------------------------------------------------

# ---- basic-table --------------------------------------------------------------

# ---- basic-graph --------------------------------------------------------------
# get a small subset for illustration and development
set.seed(41)
ids <- sample(unique(ds$id),4)

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
  
ds %>% 
  dplyr::filter(id %in% ids) %>% 
  dplyr::select(id, year, closechild, leave_behind_tag) %>% 
  print(n=nrow(.))

d_temp <- ds %>% 
  dplyr::group_by(id) %>% 
  dplyr::filter(leave_behind_tag) %>%
  dplyr::mutate(
    n_lb_wave     = sum(leave_behind_tag), # number of lb_waves for which data exists
    lb_wave = seq(n())
  ) 


d_temp %>% 
  dplyr::filter(id %in% ids) %>% 
  dplyr::select(id, year, closechild, leave_behind_tag, n_lb_wave, order_lb_wave) %>% 
  print(n=nrow(.))

d

%>% 
  dplyr::filter(id %in% ids)

d <- ds %>% 
  dplyr::filter(leave_behind_tag) %>% 
  dplyr::group_by(id, leave_behind_tag) %>% 
  dplyr::mutate(
    n  = n()
    ,lbwavecount =seq(n())
    ) %>% 
  dplyr::ungroup()
d$lbwave <- ifelse(d$leave_behind_tag==1, d$lbwavecount, 0)

d %>% 
  dplyr::select(id,year, leave_behind_tag, n,lbwavecount, lbwave)

d %>% 
  dplyr::select(id, year, leave_behind_tag)
  dplyr::select(id, year,leave_behind_tag, lbwavecount, lbwave)
# create an indicator only of waves  
d$lbwave <- ifelse(d$leave_behind_tag==1, d$lbwavecount, 0)


# ---- detect-outliers ----------------------------------------------------------
ids <- sample(size = 200, x = unique(ds$hhidpn) )
selected_variables <- c("id","year",'lbwave', "closechild", "closefam", "closefri")

# remove the values we consider outliers
# Here is our rules for defining an outlier
# If 1)the number of close family members is greater than 4 standard deviations above the mean (21) 
# and 2)the change in number of close family members is greater than 4 standard deviations above the 
# mean change (21) then recode to NA. This is 112 cases. 
# identify cases in which the criteria for outliers is broken
d <- ds %>% 
  dplyr::rename(id = hhidpn) %>% 
  # dplyr::filter(id %in% ids) %>%
  # filter(id == 22860010 ) %>% 
  dplyr::filter(lbwave > 0) %>%
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

#d %>% dplyr::group_by(closefam_out) %>% summarize(n=n())

#rename id for merging
ds <- ds %>% 
  dplyr::rename(id = hhidpn)

#add the flag variables to the larger data set
ds2 <- merge(ds, d, by = selected_variables, all.x = TRUE)

#test <- ds2 %>% dplyr::filter(id == 22860010)
# recode closefam values flagged as errors to NA
ds2$closefam <- ifelse(ds2$closefam_out == TRUE, NA, ds2$closefam)
# recode closefri values flagged as errors to NA
ds2$closefri <- ifelse(ds2$closefri_out == TRUE, NA, ds2$closefri)

networkvars <- c("snspouse", "snchild", "snfamily", "snfriends")
closevars <- c("closechild","closefam","closefri")

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

ds2 <- ds2 %>% compute_socialnetwork_scale_scores()

# create a merge interview date variables for more precise time calculations and create a time variable for HRS data waves
ds2 <- ds2 %>%
  dplyr::mutate(
    interview_date = paste0(interview_mth,"/",interview_yr),
    interview_date = zoo::as.yearmon(interview_date, "%m/%Y"),
    hrs_tscore = interview_date-dplyr::lag(interview_date)
  )

# ---- save-to-disk ----------------------------------

saveRDS(ds2, "./data-unshared/derived/dto-ellis.rds")



range(ds2$intage_r, na.rm = T)

# select only those who are older than 65 for the analysis
#ds_65 <- subset(ds2, intage_r > 64)
ds_65 <- ds2
# convert year to numeric for the wide to long conversion
ds_65$year <- as.numeric(as.character(ds_65$year))

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
    lbtime_3 = interview_date_3 - interview_date_2,
    lbtime_4 = interview_date_4 - interview_date_3
  )

l
#mean(dlb_wide$close_social_network_1, na.rm = TRUE)
#sd(dlb_wide$close_social_network_1, na.rm = TRUE)
class(dlb_wide$lbtime_2)
mean(dlb_wide$lbtime_2)
table(dlb_wide$year_2)
table(dlb_wide$year_3)
table(dlb_wide$year_4)
# ---- save-to-disk ----------------------------------
# convert NA and NaN to 9999 for Mplus.
dlb_wide[is.na(dlb_wide)] <- 9999

# prepared for Mplus
write.table(dlb_wide, "./data-unshared/derived/wide-dataset.dat", row.names=F, col.names=F)
write(names(dlb_wide), "./data-unshared/derived/wide-variable-names.txt", sep=" ")

saveRDS(d, "./data-unshared/derived/dto-ellis.rds")


###################################################################
# developmental code after this point

psych::describeBy(ds_65$score_loneliness_3, group=ds_65$lbwave)

#-Select only relevant demographic variables and total scores for analysis----------

# list variables to keep separated for long to wide conversion
variables_static <- c("id", "male", "birthyr_rand", "birthmo_rand", "race_rand", "hispanic_rand", "cohort", "raedyrs","raedegrm")

variables_longitudinal <- c("year","lbwave","responded","proxy","countb20r","shhidpnr","rmaritalst","intage_r","rpartst","score_loneliness_3", "score_loneliness_11",
                            "snspouse", "snchild", "snfamily", "snfriends","socialnetwork_total", "close_social_network",
                            "support_spouse_total", "support_child_total", "support_fam_total", "support_friend_total",
                            "strain_spouse_total", "strain_child_total", "strain_family_total", "strain_friends_total",
                            "children_contact_mean", "family_contact_mean", "friend_contact_mean",
                            "activity_mean", "activity_sum","srmemory", "srmemoryp","wrectoti", "wrectotd","mentalstatus_tot","vocab_total",
                            "dep_total","healthcond", "exercise")  # not static



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


d_long <- ds_65 %>%
  dplyr::select_(.dots = c(variables_static,  "lbwave", variables_longitudinal)) 
names(d_long)
dplyr::glimpse(d_long)

ds_lb <- subset(d_long, lbwave>0 & lbwave!=5)

# define variable properties for long-to-wide conversion
(variables_longitudinal <- variables_longitudinal[!variables_longitudinal=="lbwave"]) # all except year

# an lb wave based wide data set
dlb_wide <- ds_lb %>%
  dplyr::select_(.dots = c(variables_static,  "lbwave", variables_longitudinal))  %>%
  tidyr::gather_(key="variable", value="value", variables_longitudinal)  %>%
  dplyr::mutate(lbwave=as.character(lbwave)) %>%
  dplyr::mutate(male=as.character(male)) %>%
  dplyr::arrange(hhidpn) %>% 
  dplyr::mutate(
    # variable = gsub("^v","",variable),
    temp = paste0(variable,"_",lbwave)) %>%
  dplyr::select(-variable,-lbwave) %>% 
  tidyr::spread(temp, value)

dplyr::glimpse(dlb_wide)

mean(dlb_wide$intage_r_1)
sd(dlb_wide$intage_r_1)
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

# ---- publisher ---------------------------------------
path_report_1 <- "./reports/report.Rmd"
allReports <- c(
  path_report_1
  # ,path_report_2
  # , ...
)
pathFilesToBuild <- c(allReports) ##########
testit::assert("The knitr Rmd files should exist.", base::file.exists(pathFilesToBuild))
# Build the reports
for( pathFile in pathFilesToBuild ) {
  
  rmarkdown::render(input = pathFile,
                    output_format=c(
                      "html_document" # set print_format <- "html" in seed-study.R
                      # "pdf_document"
                      # ,"md_document"
                      # "word_document" # set print_format <- "pandoc" in seed-study.R
                    ),
                    clean=TRUE)
}
