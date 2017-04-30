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
  "id"                         #
  ,"male"                      #
  ,"birthyr_rand"              #
  ,"birthmo_rand"              #
  ,"race_rand"                 #
  ,"hispanic_rand"             #
  ,"cohort"                    #
  ,"raedyrs"                   #
  ,"raedegrm"                  #
) # static

variables_longitudinal <- c(
  "lb_wave"                    #
  ,"year"                      #
  ,"lb_65_wave"                #
  ,"interview_date"            #
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
  ,"mentalstatus_tot"          #
  ,"vocab_total"               #
  ,"dep_total"                 #
  ,"healthcond"                #
  ,"exercise"                  #
)  # not static


# ---- load-data ---------------------------------------------------------------
# load the product of 0-ellis-island.R,  a list object containing data and metadata
dto <- readRDS(path_input)

# ---- inspect-data -------------------------------------------------------------
# dto %>% glimpse()
class(dto)
#str(dto)

# ---- tweak-data --------------------------------------------------------------
# subset variables of relevance for this project
ds <- dto %>% 
  dplyr::select_(.dots = c(variables_static, variables_longitudinal)) %>% 
  as.data.frame() %>% 
  tibble::as_tibble()

ds %>% glimpse(width = 105)
ds %>% names_labels()

ds <- ds %>% 
  dplyr::mutate(
    
  )

# ---- temporal-triangulation --------------------------
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

# ---- eda-a-1 -------------------------------------------------------------------
#What does data look like for variables that do not change with time?
ds %>%
  select_(.dots = variables_static)

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


# ---- eda-summaries ------------------------------------------------------------
# examine the pattern of measures over time for a given individual
ds %>%  temporal_pattern("year","srmemory", 42)

# examine the descriptives of a measure across time time points
ds %>% over_time("year", "srmemory")
ds %>% over_time("lb_wave", "srmemory")

# ---- basic-table --------------------------------------------------------------

# ---- basic-graph --------------------------------------------------------------


  


# ---- ----------------------------------------------

# ---- id --------------------------------------------
# How many persons are in the sample?
ds %>% distinct(id) %>% count()
ds %>% group_by(id) %>% summarize(n=n())

# ---- male ------------------------------------------
ds %>% group_by(male) %>% summarize(n=n())
ds %>% histogram_discrete("male")

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

