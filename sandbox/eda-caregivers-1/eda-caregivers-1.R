# knitr::stitch_rmd(script="./___/___.R", output="./___/___/___.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>% 
library(dplyr)
library(TabularManifest)
# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
source("./scripts/common-functions.R") # used in multiple reports
source("./scripts/graphing/graph-presets.R") # fonts, colors, themes 
source("./scripts/graphing/graph-elemental.R") # graphs to be used in dipslays
source("./scripts/graphing/graph-complex.R") # info displays

# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2") # graphing
# requireNamespace("readr") # data input
requireNamespace("tidyr") # data manipulation
requireNamespace("dplyr") # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit")# For asserting conditions meet expected patterns.
# requireNamespace("car") # For it's `recode()` function.

# ---- declare-globals ---------------------------------------------------------
path_input  <- "./data-unshared/derived/2-dto_c.rds" # product of ./manipulation/2-identify-caregivers.R
# path_output <- ""

# ---- object-glossary ----------------------------------------------------
# list static variables
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
  ,"memry"
  ,"memryq"
  ,"smemry"
  ,"smemryq"
  ,"memrye"
  ,"smemrye"
  ,"memryf"
  ,"smemryf"
  ,"alzhe"
  ,"salzhe"
  ,"alzheq"
  ,"salzheq"
  ,"alzhee"
  ,"salzhee"
  ,"alzhflag"
  ,"salzhflag"
  ,"demen"
  ,"sdemen"
  ,"demenq"
  ,"sdemenq"
  ,"demene"
  ,"sdemene" 
  ,"demenflag"
  ,"sdemenflag"
  ,"dementia_ever"
  ,"alzheimer_ever"
  ,"memoryproblems_ever" 
  ,"memory_disease_ever" 
  ,"spouse_dementia_ever"
  ,"spouse_alzheimer_ever"
  ,"spouse_memoryproblems_ever"
  ,"spouse_memory_disease_ever"
  , "spouse_memory_disease"
  ,"spouse_memory_disease_pattern"
)  # not static

# ---- utility-functions ---------------------------------------------------------------

# ---- load-data ---------------------------------------------------------------
# load the product of 0-ellis-island.R,  a list object containing data and metadata
dto <- readRDS(path_input)

# ---- inspect-data -------------------------------------------------------------
# dto %>% glimpse()
class(dto)
#str(dto)

# ---- tweak-data --------------------------------------------------------------

# rename variables for graphing convenience, Cassandra, please move upstream when stable
ds <- dto %>% 
  dplyr::rename_(
      "id"             = "id"                         
    , "male"           = "male"                      
    , "birth_year"     = "birthyr_rand"              
    , "birth_month"    = "birthmo_rand"              
    , "race"           = "race_rand"                 
    , "hispanic"       = "hispanic_rand"             
    , "cohort"         = "cohort"                    
    , "edu_years"      = "raedyrs"                   
    , "highest_degree" = "raedegrm"                  
  ) 

# subset variables of relevance for this project
ds <- ds %>% 
  dplyr::select_(.dots = c(variables_static, variables_longitudinal)) %>% 
  as.data.frame() %>% 
  dplyr::mutate(
     male         = factor(male, levels = c(1,2), labels = c("Man", "Woman"))
    ,race         = factor(race, levels = c(1, 2, 3), labels = c("White","Black","Other") )
    ,cohort       = factor(cohort, levels = c(0, 1, 2, 3, 4, 5, 6), labels = c("Not in any cohort", "Ahead", "Coda", "Hrs", "WarBabies", "Early BabyBoomers", "Mid BabyBoomers") )
    ,age_at_visit = intage_r
    ,date_at_visit = interview_date
  ) %>% 
  tibble::as_tibble() 
  

ds %>% glimpse(width = 105)
ds %>% names_labels()

# ---- color-palettes ----------------------------
color_male <- c(
   "Men"   =  "#b3cde3" # blue
  ,"Women" =  "#fbb4ae" # salmon
)

color_race <- c(
  "White"  = "#66c2a5" # meak green
  ,"Black" = "#fc8d62" # burnt organe
  ,"Other" = "#8da0cb" # lily purpe
)


# ---- eda-a-1 -------------------------------------------------------------------
#How many individuals whose spouses have been diagnosed with memory disease are there?
ds %>% distinct(id) %>% count() # n = 1159, original n = 37495

#When were the spouses diagnosed?
#From 2004 to 2008 the question was whether a doctor had ever diagnosed a "memory problem"
table(ds$smemry, ds$year)
#2010 question was "Has a doctor ever told you that you have Alzheimer's Disease?"
#In follow up waves participants were asked to confirm the diagnosis if given at a previous wave.
table(ds$salzhe, ds$year)
#2010 question was:
#Has a doctor ever told you that you have dementia, senility or any other serious memory impairment?"
table(ds$sdemen, ds$year)

#Display the flag variable patterns
table(ds$smemryf,ds$year)
table(ds$salzhflag, ds$year)
table(ds$sdemenflag, ds$year)

table(ds$spouse_memory_disease, ds$year)


#Display patterns of spouses diagnosis of memory problems with frequency below
table(ds$spouse_memory_disease_pattern)

length(unique(ds$spouse_memory_disease_pattern))
unique(ds$spouse_memory_disease_pattern)


# some of the spouse memory disease patterns are suspect or look like they should not have been included.
spouse_memory_disease_patterns_to_examine <- c(
"000000"
,"NANANANANANA"
,"001000"
,"0000NANA"
,"NANA1000"
,"NANA1NANANA"
,"00NANANANA"
,"000100"
,"001NANANA"
,"00100NA"
,"NA1NANA0NA"
,"0010NANA"
,"00010NA"     
,"NA0000NA"
,"01NANA00"
,"NANA10NANA"
,"NANA100NA"   
,"001NA0NA"
,"0NANANANANA"
,"NANANANA1NA"
,"011110"
,"010000"
,"1NA100NA" 
,"00101NA"
,"101111" 
,"001001"
,"10NANANANA"
,"0000NA0"
,"NANANANA00"
,"NANANANANA0"
,"111011"
,"0NANANA10"   
,"NANA10NA1"
,"NA00011"
,"NA0100NA"
,"01100NA"
,"000NANANA"
,"0110NANA"
,"1NA0111"     
,"00000NA"
,"NA000NANA"
,"1NA1011"
,"000101"
,"NANA0NA0NA"
,"001011"
,"011000"
,"000010"
,"NANA00NANA"
,"1NANA000"
,"001110"
,"NANA1010"
,"001NA00"
,"011001"
,"NANANA00NA"
,"011100"
,"0NA0110"
,"1NA1100"     
,"1000NANA"
,"NANANA0NANA"
,"0NA100NA"
,"NA1101NA"
,"01000NA"     
,"010001"
,"NA00000"
,"100111"
,"001010"
,"0010NA0"
,"NANA1011"
,"000110"
,"1NA1101"
,"010NANANA"   
,"1NA10NANA"
,"1NANANA00"
,"00110NA"
,"NA0NANANANA" 
,"NANA0000"
,"1NA001NA"
,"0001NA0"
,"0NANA100"    
,"011NA0NA"
,"01NA000"
,"NA01000"
,"NA11000"     
,"1NA0000"
,"100000"
,"1NA1NA00"
,"111000"
,"NANANA110"
,"NANANA100"
,"NANANA101"
,"NANANA10NA"
)

d <- ds %>% 
 select(
   id, year,shhidpnr, age_at_visit, date_at_visit, spouse_memory_disease
   ,smemry, smemryq, smemrye, smemryf, salzhe, salzheq, salzhee, salzhflag
   ,sdemen, sdemenq,sdemene,sdemenflag
   ,spouse_dementia_ever
   ,spouse_alzheimer_ever
   ,spouse_memoryproblems_ever
   ,spouse_memory_disease_ever
   ,spouse_memory_disease
   ,spouse_memory_disease_pattern
  ) %>% 
   filter(spouse_memory_disease_pattern %in% spouse_memory_disease_patterns_to_examine) 

#Number of individuals with a suspect pattern of reported spouse memory disease
length(unique(d$id))
length(unique(d$shhidpnr))

# Exclude those with spouses who likely do not have dementia or AD
ds <- ds %>% dplyr::filter(!spouse_memory_disease_pattern %in% spouse_memory_disease_patterns_to_examine)

#Number of individuals once suspect patterns are excluded
length(unique(ds$id))
length(unique(ds$shhidpnr))

#Excluding those whose spouses likely do not have dementia (i.e., suspect memory disease patterns) what
# is the frequency of each pattern of diagnosis for the spouse?
table(ds$spouse_memory_disease_pattern)

#Display the flag variable patterns
table(ds$smemryf,ds$year)
table(ds$salzhflag, ds$year)
table(ds$sdemenflag, ds$year)

#Are there any individuals where the spouse has changed?
test <- ds %>% 
  dplyr::group_by(id) %>% 
  dplyr::mutate(
    firstspouse = dplyr::first(shhidpnr),
    flag = ifelse(all(shhidpnr== firstspouse | shhidpnr==0, na.rm = T), 0, 1)
  ) 
test %>% 
 select(id, year,shhidpnr,flag, spouse_memory_disease_pattern) %>% 
  dplyr::filter(flag==1)


#What does data look like for variables that do not change with time?
ds %>% select_(.dots = variables_static) 

#How many distinct values are there for each static variable?
set.seed(42)
ds %>%
  select_(.dots = variables_static) %>% 
  # filter(id %in% sample(unique(id),100)) %>%
  summarize_all(n_distinct)  %>% 
  t()

#How many distinct values are there for variables that change over time?
ds %>%
  select_(.dots = variables_longitudinal) %>% 
  summarize_all(n_distinct)  %>% 
  t()

# ---- id --------------------------------------------
# How many respondents are in the sample?
ds %>% distinct(id) %>% count()
ds %>% group_by(id) %>% summarize(n=n())


# ---- male ------------------------------------------
# what is the gender composion of the sample?
ds %>% group_by(male) %>% summarize(n=n())
ds %>% histogram_discrete("male")

# what is gender composition over time?
ds %>% count_over_time("year","male")
ds %>% count_over_time("lb_wave","male")

# ---- race ------------------------------
# what is the race compositon of the sample
ds %>% group_by(male) %>% summarize(n=n()) %>% neat("pandoc")
ds %>% histogram_discrete("race")

# what is race composition over time?
ds %>% count_over_time("year","race")
ds %>% count_over_time("lb_wave","race")

# there may not be enough sample size if split by race
ds %>% 
  dplyr::filter(lb_wave == 4) %>% 
  group_by(race) %>% 
  distinct(id ) %>% count()

# ---- hispanic ------------------------------
# what is the race compositon of the sample
ds %>% group_by(hispanic) %>% summarize(n=n()) %>% neat("pandoc")

# ---- cohort ------------------------------
# what is the race compositon of the sample
ds %>% group_by(cohort) %>% summarize(n=n()) %>% neat("pandoc")
ds %>% over_time("year", "cohort")

# ---- years-education ------------------------------
# what is the race compositon of the sample
ds %>% group_by(cohort) %>% summarize(n=n()) %>% neat("pandoc")
ds %>% over_time("year", "cohort")
edu_years

# ----- caregiver -------------------------------------
ds %>% over_time("year", "spouse_memory_disease")
ds %>% over_time("year", "spouse_memory_disease_ever")
ds %>% over_time("year", "spouse_memory_disease")
set.seed(42)
# ids_1000 <- sample(unique(ds$id), 

d <- ds %>% 
  mutate(
    age_at_visit  = intage_r,
    date_at_visit = interview_date
  ) %>% 
  select(
    id, year,  year, age_at_visit, date_at_visit, spouse_memory_disease
  ) %>% 
  filter(id %in% sample(unique(id),100)) 

# A single, elemental graph
d %>% elemental_line(
  variable_name  = "spouse_memory_disease", 
  time_metric    = "year", 
  color_name     = "black", 
  line_alpha     = .5, 
  line_size      = 1,
  smoothed       = T
)

view_temporal_pattern <- function(ds, measure, seed_value = 42){
  set.seed(seed_value)
  ds_long <- ds
  (ids <- sample(unique(ds_long$id),1))
  d <-ds_long %>%
    dplyr::filter(id %in% ids ) %>%
    dplyr::select_("id","year", measure)
  print(d)
}

ds %>% view_temporal_pattern("spouse_memory_disease", seed_value = 1)
ds %>% view_temporal_pattern("salzhee", seed_value = 1)
ds %>% view_temporal_pattern("sdemene", seed_value = 1)
ds %>% view_temporal_pattern("smemrye", seed_value = 1)
ds %>% view_temporal_pattern("smemry", seed_value = 1)
ds %>% view_temporal_pattern("salzhflag", seed_value = 1)
ds %>% view_temporal_pattern("sdemenflag", seed_value = 1)
ds %>% temporal_pattern("spouse_memory_disease")


psych::describe(ds_wide$lbtime_4)
table(ds_wide$lbtime_4)

# ----- proxy-interview -------------------------------
# examine proxy interview frequency over time
ds %>% over_time("year", "proxy")

# ----- mental-status ---------------------------------
# examine mental status over time
ds %>% over_time("year", "mentalstatus_tot")
ds %>% over_time("lb_wave", "mentalstatus_tot")

set.seed(42)
# ids_1000 <- sample(unique(ds$id), 

d <- ds %>% 
  mutate(
    age_at_visit  = intage_r,
    date_at_visit = interview_date
  ) %>% 
  select(
    id, year,  lb_wave, age_at_visit, date_at_visit, mentalstatus_tot
  ) %>% 
  filter(id %in% sample(unique(id),100)) 

# assemble various single graphs in a integrated information display
d %>% complex_line(
  variable_name  = "mentalstatus_tot", 
  line_size = 1, 
  line_alpha = .5 
)

# ---- word-list-recall --------------------------
# examine the assignment of word lists over time
ds %>% over_time("year", "listassi")
ds %>% over_time("lb_wave", "listassi")

set.seed(42)
# ids_1000 <- sample(unique(ds$id), 

d <- ds %>% 
  mutate(
    age_at_visit  = intage_r,
    date_at_visit = interview_date
  ) %>% 
  select(
    id, year,  lb_wave, age_at_visit, date_at_visit, wrectoti, wrectotd, listassi
  ) %>% 
  filter(id %in% sample(unique(id),100)) 

# A single, elemental graph
d %>% elemental_line(
  variable_name  = "wrectoti", 
  time_metric    = "age_at_visit", 
  color_name     = "black", 
  line_alpha     = .5, 
  line_size      = 1,
  smoothed       = T
)

# assemble various single graphs in a integrated information display
d %>% complex_line(
  variable_name  = "wrectoti", 
  line_size = 1, 
  line_alpha = .5 
)

# assemble various single graphs in a integrated information display
d %>% complex_line(
  variable_name  = "wrectotd", 
  line_size = 1, 
  line_alpha = .5 
)

# ---- social-suppport --------------------------

ds %>% summarize_over_time("year", "social_support_mean")
ds %>% summarize_over_time("lb_wave", "social_support_mean")
set.seed(42)
# ids_1000 <- sample(unique(ds$id), 

d <- ds %>% 
  mutate(
    age_at_visit  = intage_r,
    date_at_visit = interview_date
  ) %>% 
  select(
    id, year,  lb_wave, age_at_visit, date_at_visit, social_support_mean
  ) %>% 
  filter(id %in% sample(unique(id),100)) 

# A single, elemental graph
d %>% elemental_line(
  variable_name  = "social_support_mean", 
  time_metric    = "age_at_visit", 
  color_name     = "black", 
  line_alpha     = .5, 
  line_size      = 1,
  smoothed       = T
)
# assemble various sinle graphs in a integrated information display
d %>% complex_line(
  variable_name  = "social_support_mean", 
  line_size = 1, 
  line_alpha = .5 
)

# ---- social-strain --------------------------
ds %>% summarize_over_time("year", "social_strain_mean")
ds %>% summarize_over_time("lb_wave", "social_strain_mean")

set.seed(42)
# ids_1000 <- sample(unique(ds$id), 

d <- ds %>% 
  mutate(
    age_at_visit  = intage_r,
    date_at_visit = interview_date
  ) %>% 
  select(
    id, year,  lb_wave, age_at_visit, date_at_visit, social_strain_mean
  ) %>% 
  filter(id %in% sample(unique(id),100)) 

# A single, elemental graph
d %>% elemental_line(
  variable_name  = "social_strain_mean", 
  time_metric    = "age_at_visit", 
  color_name     = "black", 
  line_alpha     = .5, 
  line_size      = 1,
  smoothed       = T
)
# assemble various sinle graphs in a integrated information display
d %>% complex_line(
  variable_name  = "social_strain_mean", 
  line_size = 1, 
  line_alpha = .5 
)

# ---- loneliness-three --------------------------
ds %>% summarize_over_time("year", "score_loneliness_3")
ds %>% summarize_over_time("lb_wave", "score_loneliness_3")

set.seed(42)
# ids_1000 <- sample(unique(ds$id), 

d <- ds %>% 
  mutate(
    age_at_visit  = intage_r,
    date_at_visit = interview_date
  ) %>% 
  select(
    id, year,  lb_wave, age_at_visit, date_at_visit, score_loneliness_3
  ) %>% 
  filter(id %in% sample(unique(id),100)) 

# A single, elemental graph
d %>% elemental_line(
  variable_name  = "score_loneliness_3", 
  time_metric    = "age_at_visit", 
  color_name     = "black", 
  line_alpha     = .5, 
  line_size      = 1,
  smoothed       = T
)
# assemble various sinle graphs in a integrated information display
d %>% complex_line(
  variable_name  = "score_loneliness_3", 
  line_size = 1, 
  line_alpha = .5 
)

# ---- loneliness-eleven --------------------------
# examine the assignment of word lists over time
ds %>% summarize_over_time("year", "score_loneliness_11")
ds %>% summarize_over_time("lb_wave", "score_loneliness_11")
set.seed(42)
# ids_1000 <- sample(unique(ds$id), 

d <- ds %>% 
  mutate(
    age_at_visit  = intage_r,
    date_at_visit = interview_date
  ) %>% 
  select(
    id, year,  lb_wave, age_at_visit, date_at_visit, score_loneliness_11
  ) %>% 
  filter(id %in% sample(unique(id),100)) 

# assemble various sinle graphs in a integrated information display
d %>% complex_line(
  variable_name  = "score_loneliness_11", 
  line_size = 1, 
  line_alpha = .5 
)

# ---- socialnetwork_total ---------------------------
ds %>% summarize_over_time("year", "socialnetwork_total")
ds %>% summarize_over_time("lb_wave", "socialnetwork_total")

set.seed(42)
# ids_1000 <- sample(unique(ds$id), 

d <- ds %>% 
  mutate(
    age_at_visit  = intage_r,
    date_at_visit = interview_date
  ) %>% 
  select(
    id, year,  lb_wave, age_at_visit, date_at_visit, socialnetwork_total
  ) %>% 
  filter(id %in% sample(unique(id),100)) 

# assemble various sinle graphs in a integrated information display
d %>% complex_line(
  variable_name  = "socialnetwork_total", 
  line_size = 1, 
  line_alpha = .5 
)

# ---- close_social_network --------------------------
# examine the assignment of word lists over time
ds %>% summarize_over_time("year", "close_social_network")
ds %>% summarize_over_time("lb_wave", "close_social_network")
set.seed(42)
# ids_1000 <- sample(unique(ds$id), 

d <- ds %>% 
  mutate(
    age_at_visit  = intage_r,
    date_at_visit = interview_date
  ) %>% 
  select(
    id, year,  lb_wave, age_at_visit, date_at_visit, close_social_network
  ) %>% 
  filter(id %in% sample(unique(id),100)) 

# assemble various single graphs in a integrated information display
d %>% complex_line(
  variable_name  = "close_social_network", 
  line_size = 1, 
  line_alpha = .5 
)

# ---- activity --------------------------
# examine the assignment of activity over time
ds %>% summarize_over_time("year", "activity_sum")
ds %>% summarize_over_time("lb_wave", "activity_sum")
set.seed(42)
# ids_1000 <- sample(unique(ds$id), 

d <- ds %>% 
  mutate(
    age_at_visit  = intage_r,
    date_at_visit = interview_date
  ) %>% 
  select(
    id, year,  lb_wave, age_at_visit, date_at_visit, activity_sum
  ) %>% 
  filter(id %in% sample(unique(id),100)) 

# assemble various single graphs in a integrated information display
d %>% complex_line(
  variable_name  = "activity_sum", 
  line_size = 1, 
  line_alpha = .5 
)


# ---- eda-summaries ------------------------------------------------------------
# examine the pattern of measures over time for a given individual
ds %>%  temporal_pattern("year","srmemory", 42)

# examine the descriptives of a measure across time time points
ds %>% over_time("year", "srmemory")
ds %>% over_time("lb_wave", "srmemory")

# ---- basic-table --------------------------------------------------------------

# ---- basic-graph --------------------------------------------------------------

# ---- srmemory ----------------------------------------------
ds %>% over_time("year","srmemory")
ds %>% over_time("lb_wave", "srmemory")


# ---- publish ---------------------------------------
path_report_1 <- "./sandbox/eda-caregivers-1/eda-caregivers-1.Rmd"
# path_report_2 <- "./reports/*/report_2.Rmd"
# allReports <- c(path_report_1,path_report_2)
allReports <- c(path_report_1)

pathFilesToBuild <- c(allReports)
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

