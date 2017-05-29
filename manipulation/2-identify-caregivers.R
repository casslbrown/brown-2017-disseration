
#############################################################
# 1-groom-augment
# This script is to be run following 1-groom-augment.R
# The purpose of this script is to...

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
path_input      <- "./data-unshared/derived/1-dto.rds" # use the product of 1-groom-augment.R
path_output     <- "./data-unshared/derived/2-dto_c.rds"

# ---- load-data ---------------------------------------------------------------
# load the product of 1-groom-augment
ds <- readRDS(path_input)

variables_memory_problems <- c(
  "memry",
  "memryq",
  "smemry",
  "smemryq",
  "memrye",
  "smemrye",
  "memryf",
  "smemryf",
  "alzhe",
  "salzhe",
  "alzheq",
  "salzheq",
  "alzhee",
  "salzhee",
  "alzhflag",
  "salzhflag",
  "demen",
  "sdemen",
  "demenq",
  "sdemenq",
  "demene",
  "sdemene", 
  "demenflag",
  "sdemenflag",
  "dementia_ever",
  "alzheimer_ever",
  "memoryproblems_ever", 
  "memory_disease_ever", 
  "spouse_dementia_ever",
  "spouse_alzheimer_ever",
  "spouse_memoryproblems_ever",
  "spouse_memory_disease_ever",
  "spouse_memory_disease"
)

variables_spouse_memory_problems <- c(
  "smemry",
  "salzhe",
  "sdemen",
  "spouse_memory_disease",
  "spouse_memory_disease_pattern"
)
  
# create variables indicating if the respondent and spouse has ever reported memory problems, dementia, or alzheimers
ds <- ds %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(
    #memoryproblem_baseline = ifelse(((year==2004) && (memry==1))==TRUE, 1, 0),
    dementia_ever = ifelse(any(demene==1, na.rm = T)==TRUE, TRUE, ifelse(all(is.na(demene)==T), NA, FALSE)),
    alzheimer_ever    = ifelse(any(alzhee==1, na.rm = T)==TRUE, TRUE, ifelse(all(is.na(alzhee)==T), NA, FALSE)),
    memoryproblems_ever    = ifelse(any(memrye==1, na.rm = T)==TRUE, TRUE, ifelse(all(is.na(memrye)==T), NA, FALSE)),
    memory_disease_ever = ifelse(dementia_ever==1 | alzheimer_ever==1 | memoryproblems_ever==1, T, F),
    spouse_dementia_ever = ifelse(any(sdemene==1, na.rm = T)==TRUE, TRUE, ifelse(all(is.na(sdemene)==T), NA, FALSE)),
    spouse_alzheimer_ever    = ifelse(any(salzhee==1, na.rm = T)==TRUE, TRUE, ifelse(all(is.na(salzhee)==T), NA, FALSE)),
    spouse_memoryproblems_ever    = ifelse(any(smemrye==1, na.rm = T)==TRUE, TRUE, ifelse(all(is.na(smemrye)==T), NA, FALSE)),
    spouse_memory_disease_ever = ifelse(spouse_dementia_ever==1 | spouse_alzheimer_ever==1 | spouse_memoryproblems_ever==1, T, F)
  ) %>%
  dplyr::ungroup()

# creates a 0 or 1 variable to indicate memory disease status.
ds <- ds %>%
  dplyr::group_by(id, year) %>%
  dplyr::mutate(
    spouse_memory_disease = ifelse(sum(c(sdemen,salzhe,smemry), na.rm=T)>0, 1, 
                                   ifelse(is.na(sdemen) & is.na(salzhe) & is.na(smemry), NA, 0))
  ) %>%
  dplyr::ungroup()

# view a sample of data for checking
set.seed(42)
# ids_1000 <- sample(unique(ds$id), 

d <- ds %>% 
  select_("id", "year", .dots = variables_memory_problems) %>% 
  filter(id %in% sample(unique(id),50)) 
d

#Cases where there is a diagnosis and then not are suspect
#Create a variable that indicates the pattern of the spouses diagnosis
ds <- ds %>% 
  group_by(id) %>% 
  dplyr::mutate(
    spouse_memory_disease_pattern = (paste0(spouse_memory_disease,lead(spouse_memory_disease),lead(spouse_memory_disease,2),lead(spouse_memory_disease,3),lead(spouse_memory_disease,4),lead(spouse_memory_disease,5))),
    spouse_memory_disease_pattern   = dplyr::first(spouse_memory_disease_pattern) # grabs the value for the first wave and forces it to all waves
  ) %>%
  dplyr::ungroup()

# view a sample of data for checking
set.seed(42)
# ids_1000 <- sample(unique(ds$id), 
d <- ds %>% 
  select_("id", "year", .dots = variables_spouse_memory_problems) %>% 
  filter(id %in% sample(unique(id),50)) 
d
ds1 <- ds %>% 
  filter(spouse_memory_disease_ever == TRUE)

# ---- save-to-disk ----------------------------------------
saveRDS(ds1, path_output)

######### Developmental script beyond this point #############
