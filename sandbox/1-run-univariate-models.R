# knitr::stitch_rmd(script="./___/___.R", output="./___/stitched-output/___.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.

# load functions that generate scripts
source("./sandbox/functions-to-generate-Mplus-scripts.R")


# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>%
# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2") # graphing
requireNamespace("tidyr") # data manipulation
requireNamespace("dplyr") # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit")# For asserting conditions meet expected patterns.
# requireNamespace("car") # For it's `recode()` function.
requireNamespace("reshape2") # data transformations
requireNamespace("data.table") # data transformations
requireNamespace("MplusAutomation")
requireNamespace("stringr")
#requireNamespace("IalsaSynthesis")

# ---- declare-globals ---------------------------------------------------------
options(width=160)
#path_generic_data  <- "./data-unshared/derived/wide-dataset.dat"
path_generic_data  <- "./data-unshared/derived/wide-dataset-b.dat"
#path_generic_names <- "./data-unshared/derived/wide-variable-names.txt"
path_generic_names <- "./data-unshared/derived/wide-variable-names-b.txt"


varnames_cognitive <- c(
  "wrectoti"                  # Immediate word recall
  ,"wrectotd"                 # Delayed word recall
  ,"mentalstatus_tot"         # Mental status total score
)
varnames_social <- c(
   #"score_loneliness_3"        # Loneliness score for three item version
  "socialnetwork_total"       # Social network score 0-4
  #,"social_support_mean"       # Social support mean
  #,"social_strain_mean"        # Social strain mean
  #,"social_contact_total"      # Social contact total score
  #,"activity_sum"              # Activity sum
)

path_prototype_files <- list(
"Autoregressive-univariate.inp"
,"LGM-univariate.inp"
,"ALT-univariate-full.inp"
,"ALT-nested-LGM-univariate.inp"
,"ALT-no-slope-variance.inp"
,"ALT-no-slope.inp"
,"ALT-fixed-regressions.inp"
)

model_number <- c(
  "u01"
  ,"u02"
  ,"u03"
  ,"u04"
  ,"u05"
  ,"u06"
  ,"u07"
  #,"u08"
)

# ---- load-data ---------------------------------------------------------------
# ds_long <- readRDS("./data-unshared/derived/2-dto.rds") not sure if needed
ds_wide <- readRDS("./data-unshared/derived/2-dto.rds")


testit::assert("File does not exist",file.exists(path_generic_data))
testit::assert("File does not exist",file.exists(path_generic_names))

# file.copy(from=path_generic_names,to= "./sandbox/pipeline-demo-1/outputs/",overwrite = T)

# ---- inspect-data -------------------------------------------------------------

# ---- tweak-data --------------------------------------------------------------

# ---- basic-table --------------------------------------------------------------

# ---- basic-graph --------------------------------------------------------------

## Run the lines above to load the needed functions
## Execute script snippets for each pair individually below this
# ---- create-predictor-selector -----------------------------


#################################################################
## @knitr dummy_1
# Use the first example as the template for further pairs

subset_condition_1 = "memoryproblems_baseline EQ 0" # additional subgroup constraints
subset_condition_2 = "memory_disease_ever EQ 0" # exclude those who ever 
subset_group_1 = "age_baseline > 64 AND"
subset_group_2 = ""

folder_data        = "./data-unshared/derived/" # where data resides
#path_prototype    = "./manipulation/estimation/prototype-wide.inp" # Mplus stencil
folder_output     = "./output/univariate-models-nodem-65plus/predetermined-models" # where the output will go
#folder_output      = "./output/univariate-models-nodem/" # place output for models with dementia excluded.
#folder_output      = "./output/univariate-models-nodem-65plus/"
#folder_output       = "./output/univariate-models-nodem-65plus/"

# # single model univariate
# mplus_generator_univariate(
#   model_number        = "u01"
#   ,model_type         = "nocov"
#   ,covariates         = " "
#   ,process_a          = "wrectoti" # item name of process (A), goes into file name
#   ,subset_group_1     = subset_group_1
#   ,subset_condition_1 = subset_condition_2 # subset data to member of this group
#   ,data_file          = "wide-dataset-b.dat"
#   ,path_prototype     = paste0("./manipulation/estimation/univariate-models/","Autoregressive-univariate.inp")
#   ,folder_data        = folder_data
#   ,folder_output      = folder_output
#   ,run_models         = TRUE # If TRUE then Mplus runs estimation to produce .out, .gh5, and/or, other files
# )

#single model
    mplus_generator_univariate(
      model_number        = "u03"
      ,model_type         = "nocov"
      ,covariates         =  " "
      ,process_a          = "wrectotd" # item name of process (A), goes into file name
      ,subset_group_1     = subset_group_1
      ,subset_condition_1 = subset_condition_2 # subset data to member of this group
      ,data_file          = "wide-dataset-b.dat"
      ,path_prototype     = paste0("./manipulation/estimation/univariate-models/predetermined-univariate/","predetermined_univariate_ALT-full-Ou.inp")
      ,folder_data        = folder_data
      ,folder_output      = folder_output
      ,run_models         = TRUE # If TRUE then Mplus runs estimation to produce .out, .gh5, and/or, other files
    )


# loop over conditions
#for(cog_measure in varnames_cognitive){
for(soc_measure in varnames_social){
  for(i in 5:7)
    mplus_generator_univariate(
      model_number        = model_number[i]
      ,model_type         = "nocov"
      ,covariates         =  " "
      ,process_a          = soc_measure # item name of process (A), goes into file name
      ,subset_group_1     = subset_group_1
      ,subset_condition_1 = subset_condition_2 # subset data to member of this group
      ,data_file          = "wide-dataset-b.dat"
      ,path_prototype     = paste0("./manipulation/estimation/univariate-models/predetermined-univariate/",path_prototype_files[i])
      ,folder_data        = folder_data
      ,folder_output      = folder_output
      ,run_models         = TRUE # If TRUE then Mplus runs estimation to produce .out, .gh5, and/or, other files
    )
}

# loop over conditions
#for(cog_measure in varnames_cognitive){
  for(soc_measure in varnames_social){
    for(i in 5:7)
        mplus_generator_univariate(
          model_number        = model_number[i]
          ,model_type         = "nocov"
          ,covariates         =  " "
          ,process_a          = soc_measure # item name of process (A), goes into file name
          ,subset_condition_1 = subset_condition_1 # subset data to member of this group
          ,path_prototype     = paste0("./manipulation/estimation/univariate-models/",path_prototype_files[i])
          ,folder_data        = folder_data
          ,folder_output      = folder_output
          ,run_models         = TRUE # If TRUE then Mplus runs estimation to produce .out, .gh5, and/or, other files
       )
  }
#}



# ---- examine-created-output ----------------
source("./scripts/mplus/mplus.R") # downloaded from http://www.statmodel.com/mplus-R/mplus.R
path_gh5 <- "./output/studies/map/phys-cog/fev-wordlistim/b1_male_aehplus_fev_wordlistim.gh5"

# view options: https://www.statmodel.com/mplus-R/GH5_R.shtml

mplus.list.variables(path_gh5) # variables in the gh5 file
mplus.view.plots(path_gh5)  # available graphs for this type of gh5 file
# histograms
mplus.plot.histogram(path_gh5, "SA") # slope of process A
mplus.plot.histogram(path_gh5, "SB") # slope of process B
# scatterplots
mplus.plot.scatterplot(path_gh5, "IA", "IB") # intercepts
mplus.plot.scatterplot(path_gh5, "SA", "SB") # slopes
mplus.plot.scatterplot(path_gh5, "IA", "SA") # physical
mplus.plot.scatterplot(path_gh5, "IB", "SB") # cognitive

ds <- mplus.get.data(path_gh5, "SA")

summary(ds)
head(ds)





