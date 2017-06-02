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
path_input  <- "./data-unshared/derived/1-dto.rds" # product of ./manipulation/1-groom-augment.R
# path_output <- ""

# ---- object-glossary ----------------------------------------------------
# list variables to keep separated for long to wide conversion
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
)  # not static

# ---- utility-functions ---------------------------------------------------------------
# Some variables have different character codes for missing values
# Translate various character values into NA values
replace_with_na <- function(x){
  # x <- ds_location_map$facility_name
  na_tokens <- c(
    # "^NULL$"
    # ,"^-$"
    # ,"^NA$"
    # ,"^\\{blank\\}$"
    # ,"^n/a$"
    "^NaN$"
  )
  for(token in na_tokens){
    if(is.character(x)){
      x <- gsub(token,NA,x)
    }
  }
  return(x)
}
# Usage:
# ds_patient_profiles <- ds_patient_profiles %>% 
# dplyr::mutate_all(dplyr::funs(replace_with_na) )

# ---- load-data ---------------------------------------------------------------
# load the product of 0-ellis-island.R,  a list object containing data and metadata
dto <- readRDS(path_input)
ds_wide <- readRDS("./data-unshared/derived/lb65-data-wide.rds")
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
     male         = factor(male, levels = c(1,2), labels = c("Men", "Women"))
    ,race         = factor(race, levels = c(1, 2, 3), labels = c("White","Black","Other") )
    ,cohort       = factor(cohort, levels = c(0, 1, 2, 3, 4, 5, 6), labels = c("Not in any cohort", "Ahead", "Coda", "Hrs", "WarBabies", "Early BabyBoomers", "Mid BabyBoomers") )
    ,age_at_visit = intage_r
    ,date_at_visit = interview_date
  ) %>% 
  tibble::as_tibble() 
  

ds %>% glimpse(width = 105)
ds %>% names_labels()

# replace static variables with NA for those interview_dates that are NA
# this means that the person did not have any observation in that wave
# However, this is a temp fix. Address it upstream (possibly during elongation)
# these rows are the ones we've created, so we remove them now

ds %>% distinct(id) %>% count() # n = 37495
ds <- ds %>%  dplyr::filter(!is.na(interview_date))
ds %>% distinct(id) %>% count() # n = 28225


# # select a single case for inspection 
# ids <- sample(unique(ds$id),1)
# 

# ---- investigate -----------------------------
# # these targets have NA for gender in 2014. Cassandra, please investigate
# target_ids <- ds %>%
#   # dplyr::filter(id %in% ids)
#   dplyr::filter(is.na(male)) %>% 
#   dplyr::select(id) %>% 
#   as.list() %>% unlist()
# 
# d <- ds %>% dplyr::filter(id %in% target_ids)
# ds %>% mutate(id=as.character(id)) %>% View()


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
ds %>% histogram_discrete("male")

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

# ----- lb-wave ---------------------------------------
# examine hrs time
ds %>% over_time("year", "hrs_tscore")

# examine lb wave over time
ds %>% over_time("year", "lb_wave")

# examine lb wave time
psych::describe(ds_wide$lbtime_1)
table(ds_wide$lbtime_1)

psych::describe(ds_wide$lbtime_2)
table(ds_wide$lbtime_2)

psych::describe(ds_wide$lbtime_3)
table(ds_wide$lbtime_3)

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

# ---- vocabulary -------------------------------
ds %>% summarize_over_time("year", "vocab_total")

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
path_report_1 <- "./sandbox/eda-1/eda-1.Rmd"
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

