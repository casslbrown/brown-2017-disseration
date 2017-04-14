#############################################################
# laundry room
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

# ---- load-data ---------------------------------------------------------------
# load the product of 1-scale-assembly.R a long data file
ds <- readRDS("./data-unshared/derived/dto-ellis.rds")

# ---
#############################################################
# laundry room
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

# ---- load-data ---------------------------------------------------------------
# load the product of 1-scale-assembly.R a long data file
ds <- readRDS("./data-unshared/derived/dto-ellis.rds")


ds <- ds %>% 
  subset(lbwave>0 & lbwave!=5) %>% 
  dplyr::group_by(lbwave) %>% 
  dplyr::mutate(
    activity_sum_mean = mean(activity_sum, na.rm = T),
    close_social_network_mean = mean(close_social_network, na.rm = T),
    score_loneliness11_mean = mean(score_loneliness_11, na.rm = T),
    score_loneliness3_mean = mean(score_loneliness_3, na.rm = T),
    children_contact_meanm = mean(children_contact_mean, na.rm = T),
    exercise_mean = mean(exercise, na.rm = T),
    wrectoti_mean = mean(wrectoti, na.rm=T),
    wrectotd_mean = mean(wrectotd, na.rm=T),
    mentalstat_mean = mean(mentalstatus_tot, na.rm = T)
  ) %>% 
  dplyr::ungroup()

ds$activity_sum_mean

psych::describeBy(ds$intage_r, group = ds$lbwave)
library(ggplot2)
ggplot(data=ds, mapping =aes(x=lbwave, y=activity_sum_mean, group = 1)) +
  geom_line()

ggplot(data=ds, mapping =aes(x=lbwave, y=close_social_network_mean, group = 1)) +
  geom_line()

ggplot(data=ds, mapping =aes(x=lbwave, y=wrectoti_mean, group = 1)) +
  geom_line()

ggplot(data=ds, mapping =aes(x=lbwave, y=wrectotd_mean, group = 1)) +
  geom_line()

ggplot(data=ds, mapping =aes(x=lbwave, y=score_loneliness11_mean, group = 1)) +
  geom_line()

ggplot(data=ds, mapping =aes(x=lbwave, y=score_loneliness3_mean, group = 1)) +
  geom_line()

ggplot(data=ds, mapping =aes(x=lbwave, y=children_contact_meanm, group = 1)) +
  geom_line()

ggplot(data=ds, mapping =aes(x=lbwave, y=exercise_mean, group = 1)) +
  geom_line()

ggplot(data=ds, mapping =aes(x=lbwave, y=mentalstat_mean, group = 1)) +
  geom_line()
