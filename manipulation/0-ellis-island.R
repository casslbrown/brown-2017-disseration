#############################################################
# 0-ellis-island
# This script is to be run following 1-scale-assembly.R
# The purpose of this script is to take the list of data frames from the HRS respository
# read them in and merge them to create a single long format longitudinal file. 

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
path_input      <- "../HRS/data-unshared/derived/1-dto.rds" # product of 1-scale-assembly.R
path_input_list <- "../HRS/data-unshared/derived/1-dto-list.rds" # product of 1-scale-assembly.R
path_input_biomarker <- "../HRS/data-unshared/derived/dto_b.rds" # product of 2-add-biomarkers-data.R
path_output     <- "./data-unshared/derived/0-dto.rds"
#path_output_bio <- "./data-unshared/derived/0-dto_b.rds"

# ---- load-data ---------------------------------------------------------------
# load the product of 1-scale-assembly.R a long data file
ds <- readRDS(path_input)
#ls <- readRDS(path_input_list)

# to add biomarker data load the product of 2-add-biomarker-data.R a long data file
#ds_bio <- readRDS(path_input_biomarker)

# ---- inspect-data -------------------------------------------------------------
names(ls)
names(ds)
# ---- merge-assembles-scales ----------------------------------
# you can recreate ds from ls by running the following merging script:
# merge multiple datasets that are stored as elements of a list
merge_multiple_files <- function(list, by_columns){
  Reduce(function( d_1, d_2 ) dplyr::full_join(d_1, d_2, by=by_columns), list)
}

# a ds without biomarker data
ds <- merge_multiple_files(ls, by_columns = c("year","hhidpn"))

# add biomarker data to the list
ls[["biomarkers"]] <- ds_bio

# then merge the list to create a long ds with biomarker data
ds_b <- merge_multiple_files(ls, by_columns = c("year","hhidpn"))
 

# ---- tweak-data --------------------------------------------------------------
ds <- ds %>% dplyr::rename(id = hhidpn)
set.seed(41) # to set specific seed
# set.seed(NULL) # to disable specific seed
sample_size <- 3
ids <- sample(unique(ds$id), sample_size)

ds_b <- ds_b %>% dplyr::rename(id = hhidpn)
set.seed(41) # to set specific seed
# set.seed(NULL) # to disable specific seed
sample_size <- 3
ids <- sample(unique(ds_b$id), sample_size)
# ---- save-to-disk ----------------------------------------
saveRDS(ds, path_output)
saveRDS(ds_b, path_output_bio)





# 
# 
# ### DEVELOPMENTAL SCRIPT BEYOND THIS POINT 
# 
# ### TODO:  Cassandra, continue going through script from here
# # placing the updated version of the code in the appropriate file
# 
# 
# 
# #-Select only relevant demographic variables and total scores for analysis----------
# 
# # list variables to keep separated for long to wide conversion
# variables_static <- c("id", "male", "birthyr_rand", "birthmo_rand", "race_rand", "hispanic_rand", "cohort", "raedyrs","raedegrm")
# 
# variables_longitudinal <- c("year","lbwave","interview_date", "responded","proxy","countb20r","shhidpnr","rmaritalst","intage_r","rpartst","score_loneliness_3", "score_loneliness_11",
#                             "snspouse", "snchild", "snfamily", "snfriends","socialnetwork_total", "close_social_network",
#                             "support_spouse_total", "support_child_total", "support_fam_total", "support_friend_total",
#                             "strain_spouse_total", "strain_child_total", "strain_family_total", "strain_friends_total",
#                             "children_contact_mean", "family_contact_mean", "friend_contact_mean",
#                             "activity_mean", "activity_sum","srmemory", "srmemoryp","wrectoti", "wrectotd","mentalstatus_tot","vocab_total",
#                             "dep_total","healthcond", "exercise")  # not static
# 
# # create a smaller dataset
# d_long <- ds_65 %>%
#   dplyr::select_(.dots = c(variables_static,  "lbwave", variables_longitudinal)) 
# names(d_long)
# 
# ds_lb <- subset(d_long, lbwave>0 & lbwave!=5)
# less65 <- subset(ds_lb, intage_r < 65)
# length(unique(less65$id))
# # define variable properties for long-to-wide conversion
# (variables_longitudinal <- variables_longitudinal[!variables_longitudinal=="lbwave"]) # all except year
# 
# # an lb wave based wide data set
# dlb_wide <- ds_lb %>%
#   dplyr::select_(.dots = c(variables_static,  "lbwave", variables_longitudinal))  %>%
#   tidyr::gather_(key="variable", value="value", variables_longitudinal)  %>%
#   dplyr::mutate(lbwave=as.character(lbwave)) %>%
#   dplyr::mutate(male=as.character(male)) %>%
#   dplyr::arrange(id) %>% 
#   dplyr::mutate(
#     # variable = gsub("^v","",variable),
#     temp = paste0(variable,"_",lbwave)) %>%
#   dplyr::select(-variable,-lbwave) %>% 
#   tidyr::spread(temp, value)
# 
# dplyr::glimpse(dlb_wide)
# 
# # Now that the data is in wide format it is easier to calculate a time score for lbwaves.
# 
# dlb_wide <- dlb_wide %>% 
#   dplyr::mutate(
#     lbtime_1 = 0,
#     lbtime_2 = interview_date_2 - interview_date_1,
#     lbtime_3 = interview_date_3 - interview_date_2,
#     lbtime_4 = interview_date_4 - interview_date_3
#   )
# 
# l
# #mean(dlb_wide$close_social_network_1, na.rm = TRUE)
# #sd(dlb_wide$close_social_network_1, na.rm = TRUE)
# class(dlb_wide$lbtime_2)
# mean(dlb_wide$lbtime_2)
# table(dlb_wide$year_2)
# table(dlb_wide$year_3)
# table(dlb_wide$year_4)
# # ---- save-to-disk ----------------------------------
# # convert NA and NaN to 9999 for Mplus.
# dlb_wide[is.na(dlb_wide)] <- 9999
# 
# # prepared for Mplus
# write.table(dlb_wide, "./data-unshared/derived/wide-dataset.dat", row.names=F, col.names=F)
# write(names(dlb_wide), "./data-unshared/derived/wide-variable-names.txt", sep=" ")
# 
# saveRDS(d, "./data-unshared/derived/dto-ellis.rds")
# 
# 
# ###################################################################
# # developmental code after this point
# 
# 
# 
# #-Select only relevant demographic variables and total scores for analysis----------
# 
# # list variables to keep separated for long to wide conversion
# variables_static <- c("id", "male", "birthyr_rand", "birthmo_rand", "race_rand", "hispanic_rand", "cohort", "raedyrs","raedegrm")
# 
# variables_longitudinal <- c("year","lbwave","responded","proxy","countb20r","shhidpnr","rmaritalst","intage_r","rpartst","score_loneliness_3", "score_loneliness_11",
#                             "snspouse", "snchild", "snfamily", "snfriends","socialnetwork_total", "close_social_network",
#                             "support_spouse_total", "support_child_total", "support_fam_total", "support_friend_total",
#                             "strain_spouse_total", "strain_child_total", "strain_family_total", "strain_friends_total",
#                             "children_contact_mean", "family_contact_mean", "friend_contact_mean",
#                             "activity_mean", "activity_sum","srmemory", "srmemoryp","wrectoti", "wrectotd","mentalstatus_tot","vocab_total",
#                             "dep_total","healthcond", "exercise")  # not static
# 
# 
# 
# # a year based wide data set
# d_wide <- ds %>%
#   dplyr::select_(.dots = c(variables_static,  "year", variables_longitudinal))  %>%
#   tidyr::gather_(key="variable", value="value", variables_longitudinal)  %>%
#   dplyr::mutate(year=as.character(year)) %>%
#   dplyr::mutate(male=as.character(male)) %>%
#   dplyr::arrange(hhidpn) %>% 
#   dplyr::mutate(
#     # variable = gsub("^v","",variable),
#     temp = paste0(variable,"_",year)) %>%
#   dplyr::select(-variable,-year) %>% 
#   tidyr::spread(temp, value)
# 
# 
# d_long <- ds_65 %>%
#   dplyr::select_(.dots = c(variables_static,  "lbwave", variables_longitudinal)) 
# names(d_long)
# dplyr::glimpse(d_long)
# 
# ds_lb <- subset(d_long, lbwave>0 & lbwave!=5)
# 
# # define variable properties for long-to-wide conversion
# (variables_longitudinal <- variables_longitudinal[!variables_longitudinal=="lbwave"]) # all except year
# 
# # an lb wave based wide data set
# dlb_wide <- ds_lb %>%
#   dplyr::select_(.dots = c(variables_static,  "lbwave", variables_longitudinal))  %>%
#   tidyr::gather_(key="variable", value="value", variables_longitudinal)  %>%
#   dplyr::mutate(lbwave=as.character(lbwave)) %>%
#   dplyr::mutate(male=as.character(male)) %>%
#   dplyr::arrange(hhidpn) %>% 
#   dplyr::mutate(
#     # variable = gsub("^v","",variable),
#     temp = paste0(variable,"_",lbwave)) %>%
#   dplyr::select(-variable,-lbwave) %>% 
#   tidyr::spread(temp, value)
# 
# dplyr::glimpse(dlb_wide)
# 
# mean(dlb_wide$intage_r_1)
# sd(dlb_wide$intage_r_1)
# # ---- save-r-data -------------------
# # tranformed data with supplementary variables
# #saveRDS(ds,"./data-unshared/derived/data-long-select.rds")
# 
# saveRDS(d_wide, file="./data-unshared/derived/data-wide.rds")
# # lb wave based wide file
# saveRDS(dlb_wide, file="./data-unshared/derived/lb-data-wide.rds")
# 
# # convert NA and NaN to 9999 for Mplus.
# dlb_wide[is.na(dlb_wide)] <- 9999
# 
# 
# # prepared for Mplus
# write.table(dlb_wide, "./data-unshared/derived/wide-dataset.dat", row.names=F, col.names=F)
# write(names(dlb_wide), "./data-unshared/derived/wide-variable-names.txt", sep=" ")

# # ---- publisher ---------------------------------------
# path_report_1 <- "./reports/report.Rmd"
# allReports <- c(
#   path_report_1
#   # ,path_report_2
#   # , ...
# )
# pathFilesToBuild <- c(allReports) ##########
# testit::assert("The knitr Rmd files should exist.", base::file.exists(pathFilesToBuild))
# # Build the reports
# for( pathFile in pathFilesToBuild ) {
#   
#   rmarkdown::render(input = pathFile,
#                     output_format=c(
#                       "html_document" # set print_format <- "html" in seed-study.R
#                       # "pdf_document"
#                       # ,"md_document"
#                       # "word_document" # set print_format <- "pandoc" in seed-study.R
#                     ),
#                     clean=TRUE)
# }
