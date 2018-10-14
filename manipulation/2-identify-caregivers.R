
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
  #"memryq",
  "smemry",
  #"smemryq",
  "memrye",
  "smemrye",
  #"memryf",
  #"smemryf",
  "alzhe",
  "salzhe",
  #"alzheq",
  #"salzheq",
  "alzhee",
  "salzhee",
  #"alzhflag",
  #"salzhflag",
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
  "spouse_memory_disease",
  "spdem_pattern_assigned"
)

variables_spouse_memory_problems <- c(
  "smemry",
  "salzhe",
  "sdemen",
  "spouse_memory_disease",
  "spouse_memory_disease_pattern",
  "spdem_2004",
  "spdem_2006",
  "spdem_2008",
  "spdem_2010",
  "spdem_2012",
  "spdem_2014"
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
# Need to find a way to create a variable that corrects for NA based on previous values. 
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

table(ds$spouse_memory_disease_pattern)
#Create a variable that uses the spouse_memory_disease_pattern to input NA's for a time varying covariate spouse_dementia
#WORKING HERE
# Step 1: Create six separate variables that ind
spouse_memory_disease_011111 <- c(
"011111",
"011NANANA",
"011110",      
"0111NANA",
"01NANANANA",
"01111NA",
"011NA11"
)

spouse_memory_disease_001111 <- c(
  "00111NA",
  "001NANANA",
  "0011NANA",
  "NA0111NA",
  "NA011NANA",
  "001NA11",
  "NA01NANANA",  
  "001111",
  "0NA1NANANA",
  "001NA1NA",
  "001NANA1"
)
# excluded, uncertain, 00NA11NA.
spouse_memory_disease_000111 <- c(
 "00011NA",
 "000111", 
 "NA0011NA",
 "0001NANA",
 "NANA011NA",   
 "NA00111",
 "NA001NANA",    
 "0NA01NANA",
  "0NA011NA"
)

spouse_memory_disease_000011 <- c(
 "00001NA",      
 "000011",      
 "NANANA011", 
 "NA00011",
  "0NA001NA",
  "00NA01NA",     
  "00NA011",
  "0NANA011"
)

spouse_memory_disease_000001 <- c(
  "00NANA01",
  "000001",      
  "NANANA001",      
  "NA00001" ,  
  "000NA01" ,
  "NANANANA01",   
  "00NA001",       
  "0NANANA01"
)

spouse_memory_disease_000000 <- c(
  "000000",     
  "0000NA0",
  "NANANANA00",
  "NANANA000",
  "NANANANANA0",  
  "00000NA",
  "000NANANA",
  "0000NANA",
  "00NANANANA",
  "0NANANANANANA",
  "NA000NANA",      
  "NANA0NA0NA",
  "NANA00NANA",
  "NANANA00NA",  
  "NANANA0NANA", 
  "NA00000",         
  "NANA0000"
)

spouse_memory_disease_111111 <- c(
  "1NANANANANA",     
  "1NA111NA",       
  "1NA11NANA",   
  "1NA1NANANA",
  "1NA1111",     
  "1111NANA",
  "111NANANA",   
  "1NANA111"
)

table(ds$spouse_memory_disease_pattern)
# Code separate variables for the first pattern
ds <- ds %>% 
  #group_by(id) %>% 
  dplyr::mutate(
    spdem_pattern_assigned = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_011111 == T, "011111", NA),
    spdem_2004 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_011111 == T, 0, NA),
    spdem_2006 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_011111 == T, 1, NA),
    spdem_2008 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_011111 == T, 1, NA),
    spdem_2010 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_011111 == T, 1, NA),
    spdem_2012 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_011111 == T, 1, NA),
    spdem_2014 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_011111 == T, 1, NA)
  ) #%>%
  #dplyr::ungroup()

table(ds$spouse_dem_cov1)


# Code variables for second pattern
ds <- ds %>% 
  group_by(id) %>% 
  dplyr::mutate(
    spdem_pattern_assigned = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_001111 == T, "001111", spdem_pattern_assigned),
    spdem_2004 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_001111 == T, 0, spdem_2004),
    spdem_2006 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_001111 == T, 0, spdem_2006),
    spdem_2008 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_001111 == T, 1, spdem_2008),
    spdem_2010 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_001111 == T, 1, spdem_2010),
    spdem_2012 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_001111 == T, 1, spdem_2012),
    spdem_2014 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_001111 == T, 1, spdem_2014)
  ) %>%
  dplyr::ungroup()



# Code variables for third pattern
ds <- ds %>% 
  group_by(id) %>% 
  dplyr::mutate(
    spdem_pattern_assigned = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000111 == T, "000111", spdem_pattern_assigned),
    spdem_2004 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000111 == T, 0, spdem_2004),
    spdem_2006 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000111 == T, 0, spdem_2006),
    spdem_2008 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000111 == T, 0, spdem_2008),
    spdem_2010 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000111 == T, 1, spdem_2010),
    spdem_2012 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000111 == T, 1, spdem_2012),
    spdem_2014 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000111 == T, 1, spdem_2014)
  ) %>%
  dplyr::ungroup()

# Code variables for fourth pattern
ds <- ds %>% 
  group_by(id) %>% 
  dplyr::mutate(
    spdem_pattern_assigned = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000111 == T, "000111", spdem_pattern_assigned),
    spdem_2004 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000111 == T, 0, spdem_2004),
    spdem_2006 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000111 == T, 0, spdem_2006),
    spdem_2008 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000111 == T, 0, spdem_2008),
    spdem_2010 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000111 == T, 1, spdem_2010),
    spdem_2012 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000111 == T, 1, spdem_2012),
    spdem_2014 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000111 == T, 1, spdem_2014)
  ) %>%
  dplyr::ungroup()

# Code variables for fourth pattern
ds <- ds %>% 
  group_by(id) %>% 
  dplyr::mutate(
    spdem_pattern_assigned = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000011 == T, "000011", spdem_pattern_assigned),
    spdem_2004 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000011 == T, 0, spdem_2004),
    spdem_2006 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000011 == T, 0, spdem_2006),
    spdem_2008 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000011 == T, 0, spdem_2008),
    spdem_2010 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000011 == T, 0, spdem_2010),
    spdem_2012 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000011 == T, 1, spdem_2012),
    spdem_2014 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000011 == T, 1, spdem_2014)
  ) %>%
  dplyr::ungroup()

# Code variables for fifth pattern
ds <- ds %>% 
  group_by(id) %>% 
  dplyr::mutate(
    spdem_pattern_assigned = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000001 == T, "000001", spdem_pattern_assigned),
    spdem_2004 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000001 == T, 0, spdem_2004),
    spdem_2006 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000001 == T, 0, spdem_2006),
    spdem_2008 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000001 == T, 0, spdem_2008),
    spdem_2010 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000001 == T, 0, spdem_2010),
    spdem_2012 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000001 == T, 0, spdem_2012),
    spdem_2014 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000001 == T, 1, spdem_2014)
  ) %>%
  dplyr::ungroup()

# Code variables for sixth pattern
ds <- ds %>% 
  group_by(id) %>% 
  dplyr::mutate(
    spdem_pattern_assigned = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000000 == T, "000000", spdem_pattern_assigned),
    spdem_2004 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000000 == T, 0, spdem_2004),
    spdem_2006 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000000 == T, 0, spdem_2006),
    spdem_2008 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000000 == T, 0, spdem_2008),
    spdem_2010 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000000 == T, 0, spdem_2010),
    spdem_2012 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000000 == T, 0, spdem_2012),
    spdem_2014 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000000 == T, 0, spdem_2014),
    spouse_memory_disease_000000 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000000 == T, 1, 0)
  ) %>%
  dplyr::ungroup()

# Code variables for seventh pattern
ds <- ds %>% 
  group_by(id) %>% 
  dplyr::mutate(
    spdem_pattern_assigned = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_111111 == T, "111111", spdem_pattern_assigned),
    spdem_2004 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_111111 == T, 1, spdem_2004),
    spdem_2006 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_111111 == T, 1, spdem_2006),
    spdem_2008 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_111111 == T, 1, spdem_2008),
    spdem_2010 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_111111 == T, 1, spdem_2010),
    spdem_2012 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_111111 == T, 1, spdem_2012),
    spdem_2014 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_111111 == T, 1, spdem_2014)
  ) %>%
  dplyr::ungroup()

table(ds$spdem_pattern_assigned)

# Checking the number of individuals with each assigned pattern. 
d_test <- ds %>% 
  filter(spdem_pattern_assigned=="000000")
length(unique(d_test$id))

table(ds$spdem_pattern_assigned)


# Code separate variables for the first pattern cumulative effect
ds <- ds %>% 
  #group_by(id) %>% 
  dplyr::mutate(
    cspdem_2004 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_011111 == T, 0, NA),
    cspdem_2006 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_011111 == T, 1, NA),
    cspdem_2008 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_011111 == T, 2, NA),
    cspdem_2010 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_011111 == T, 3, NA),
    cspdem_2012 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_011111 == T, 4, NA),
    cspdem_2014 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_011111 == T, 5, NA)
  ) #%>%
#dplyr::ungroup()




# Code variables for second pattern
ds <- ds %>% 
  dplyr::mutate(
    cspdem_2004 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_001111 == T, 0, cspdem_2004),
    cspdem_2006 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_001111 == T, 0, cspdem_2006),
    cspdem_2008 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_001111 == T, 1, cspdem_2008),
    cspdem_2010 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_001111 == T, 2, cspdem_2010),
    cspdem_2012 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_001111 == T, 3, cspdem_2012),
    cspdem_2014 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_001111 == T, 4, cspdem_2014)
  ) 



# Code variables for third pattern
ds <- ds %>% 
  dplyr::mutate(
    cspdem_2004 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000111 == T, 0, cspdem_2004),
    cspdem_2006 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000111 == T, 0, cspdem_2006),
    cspdem_2008 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000111 == T, 0, cspdem_2008),
    cspdem_2010 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000111 == T, 1, cspdem_2010),
    cspdem_2012 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000111 == T, 2, cspdem_2012),
    cspdem_2014 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000111 == T, 3, cspdem_2014)
  ) 

# Code variables for fourth pattern
ds <- ds %>% 
  dplyr::mutate(
    cspdem_2004 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000111 == T, 0, cspdem_2004),
    cspdem_2006 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000111 == T, 0, cspdem_2006),
    cspdem_2008 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000111 == T, 0, cspdem_2008),
    cspdem_2010 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000111 == T, 1, cspdem_2010),
    cspdem_2012 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000111 == T, 2, cspdem_2012),
    cspdem_2014 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000111 == T, 3, cspdem_2014)
  ) 

# Code variables for fourth pattern
ds <- ds %>% 
  dplyr::mutate(
    cspdem_2004 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000011 == T, 0, cspdem_2004),
    cspdem_2006 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000011 == T, 0, cspdem_2006),
    cspdem_2008 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000011 == T, 0, cspdem_2008),
    cspdem_2010 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000011 == T, 0, cspdem_2010),
    cspdem_2012 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000011 == T, 1, cspdem_2012),
    cspdem_2014 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000011 == T, 2, cspdem_2014)
  ) 

# Code variables for fifth pattern
ds <- ds %>% 
  dplyr::mutate(
    cspdem_2004 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000001 == T, 0, cspdem_2004),
    cspdem_2006 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000001 == T, 0, cspdem_2006),
    cspdem_2008 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000001 == T, 0, cspdem_2008),
    cspdem_2010 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000001 == T, 0, cspdem_2010),
    cspdem_2012 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000001 == T, 0, cspdem_2012),
    cspdem_2014 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000001 == T, 1, cspdem_2014)
  ) 

# Code variables for sixth pattern
ds <- ds %>% 
  dplyr::mutate(
    cspdem_2004 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000000 == T, 0, cspdem_2004),
    cspdem_2006 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000000 == T, 0, cspdem_2006),
    cspdem_2008 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000000 == T, 0, cspdem_2008),
    cspdem_2010 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000000 == T, 0, cspdem_2010),
    cspdem_2012 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000000 == T, 0, cspdem_2012),
    cspdem_2014 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_000000 == T, 0, cspdem_2014)
  ) 

# Code variables for seventh pattern
ds <- ds %>% 
  dplyr::mutate(
    cspdem_2004 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_111111 == T, 1, cspdem_2004),
    cspdem_2006 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_111111 == T, 2, cspdem_2006),
    cspdem_2008 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_111111 == T, 3, cspdem_2008),
    cspdem_2010 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_111111 == T, 4, cspdem_2010),
    cspdem_2012 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_111111 == T, 5, cspdem_2012),
    cspdem_2014 = ifelse(spouse_memory_disease_pattern %in% spouse_memory_disease_111111 == T, 6, cspdem_2014)
  ) 

# Create a variable to indicate widowhood
ds <- ds %>% 
  dplyr::mutate(
    widow = ifelse(ds$rmaritalst== 7, 1, 0)
)


# view a sample of data for checking
set.seed(32)
# ids_1000 <- sample(unique(ds$id), 
d <- ds %>% 
  select_("id", "year", .dots = variables_spouse_memory_problems) %>% 
  filter(id %in% sample(unique(id),50)) 
d

# view a sample of data for checking
set.seed(66)
# ids_1000 <- sample(unique(ds$id), 
d <- ds %>% 
  select_("id", "year", .dots = variables_spouse_memory_problems) %>% 
  filter(id %in% sample(unique(id),50)) 
d

# ide = 17031010 was the suspect case this no longer appears suspect. 
d <- ds %>%
  select_("id", "year", .dots = variables_memory_problems) %>%
  filter(id==17031010)
d

# To examine spouses with memory disease only.
ds1 <- ds %>% 
  filter(spouse_memory_disease_ever == TRUE)

# ---- save-to-disk ----------------------------------------
saveRDS(ds, path_output)


