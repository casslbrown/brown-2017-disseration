# knitr::stitch_rmd(script="./___/___.R", output="./___/___/___.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>% 
library(dplyr)
# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
# source("./scripts/common-functions.R") # used in multiple reports
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
# path_input <- "./data-unshared/derived/data-long-lbwaves65+.rds"
# path_input <- "./data-unshared/derived/data-wide.rds"
# path_input <- "./data-unshared/derived/lb-data-wide.rds"
path_input <- "./data-unshared/derived/1-dto.rds"
# path_output <-
# ---- load-data ---------------------------------------------------------------
# load the product of 0-ellis-island.R,  a list object containing data and metadata
dto <- readRDS(path_input)

# ---- inspect-data -------------------------------------------------------------
dto %>% glimpse()

# ---- object-glossary ----------------------------------------------------
# list variables to keep separated for long to wide conversion
variables_static <- c(
  "id"
  ,"male"
  ,"birthyr_rand"
  ,"birthmo_rand"
  ,"race_rand"
  ,"hispanic_rand"
  ,"cohort"
  ,"raedyrs"
  ,"raedegrm"
) # static

variables_longitudinal <- c(
  "lb_wave"
  ,"year"
  ,"lb_65_wave"
  ,"interview_date"
  ,"responded"
  ,"proxy"
  ,"hhres"
  ,"countb20r"
  ,"shhidpnr"
  ,"rmaritalst"
  ,"intage_r"
  ,"rpartst"
  ,"score_loneliness_3"
  ,"score_loneliness_11"
  ,"snspouse"
  ,"snchild"
  ,"snfamily"
  ,"snfriends"
  ,"socialnetwork_total"
  ,"close_social_network"
  ,"social_support_mean"
  ,"social_strain_mean"
  ,"social_contact_total"
  ,"activity_mean"
  ,"activity_sum"
  ,"srmemory"
  ,"srmemoryp"
  ,"wrectoti"
  ,"wrectotd"
  ,"mentalstatus_tot"
  ,"vocab_total"
  ,"dep_total"
  ,"healthcond"
  ,"exercise"
)  # not static

# ---- tweak-data --------------------------------------------------------------

ds <- dto %>% 
  dplyr::select_(.dots = c(variables_static, variables_longitudinal))
                 
ds %>% glimpse()
# ---- basic-table --------------------------------------------------------------

# ---- basic-graph --------------------------------------------------------------
# this is how we can interact with the `dto` to call and graph data and metadata
dto[["metaData"]] %>% 
  dplyr::filter(type=="demographic") %>% 
  dplyr::select(name,name_new,label)

dto[["unitData"]]%>%
  histogram_continuous("age_death", bin_width=1)

dto[["unitData"]]%>%
  histogram_discrete("msex")

TabularManifest::histogram_continuous()

TabularManifest::histogram_discrete()

# ---- publish ---------------------------------------
path_report_1 <- "./reports/*/report_1.Rmd"
path_report_2 <- "./reports/*/report_2.Rmd"
allReports <- c(path_report_1,path_report_2)

pathFilesToBuild <- c(allReports)
testit::assert("The knitr Rmd files should exist.", base::file.exists(pathFilesToBuild))
# Build the reports
for( pathFile in pathFilesToBuild ) {
  
  rmarkdown::render(input = pathFile,
                    output_format=c(
                      # "html_document" # set print_format <- "html" in seed-study.R
                      # "pdf_document"
                      # ,"md_document"
                      "word_document" # set print_format <- "pandoc" in seed-study.R
                    ),
                    clean=TRUE)
}

