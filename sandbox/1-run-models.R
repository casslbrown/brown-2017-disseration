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
   "score_loneliness_3"        # Loneliness score for three item version
  #,"socialnetwork_total"       # Social network score 0-4
  ,"social_support_mean"       # Social support mean
  ,"social_contact_total"      # Social contact total score

)

path_prototype_files <- list(
  "Model_1_LGM_bivariate.inp"
  ,"Model_2_autoregressive_bivariate.inp"
  ,"Model_3_ALT-full.inp"
  ,"Model_4_ALT-nestedLCM.inp"
  ,"Model_5_ALT-fixed-slope-cog.inp"
  ,"Model_6_ALT-no-slope-cog.inp"
  ,"Model_7_ALT-fixed-slope-soc.inp"
  ,"Model_8_ALT-no-slope-soc.inp"
  ,"Model_9_ALT-no-time-specific-correlations.inp"
  ,"Model_10_ALT-fixed-time-specific-correlations.inp"
  ,"Model_11_ALT-9-plus-fixed-autoregressions-cog.inp"
  ,"Model_12_ALT-9-plus-fixed-autoregressions-soc.inp"
  ,"ALT-fixed-soc-on-cog-regressions.inp"
  ,"ALT-fixed-cog-on-soc-regressions.inp"
)

model_number <- c(
  "m01"
  ,"m02"
  ,"m03"
  ,"m04"
  ,"m05"
  ,"m06"
  ,"m07"
  ,"m08"
  ,"m09"
  ,"m10"
  ,"m11"
  ,"m12"
  , "m13"
  ,"m14"
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
ls_model_number <- list(
  #"LGM_bivariate" = "m1"
  #,"autoregressive_bivariate" = "m2"
  "ALT_full_model" = "m3"
  ,"ALT_nested_LGM_model" = "m4"
  ,"ALT_fixed_slope_variance_cognitive_variable" = "m5"
  ,"ALT_no_slope_cognitive_variable" = "m6"
  ,"ALT_fixed_slope_variance_social_variable" = "m7"
  ,"ALT_no_slope_social_variable" = "m8"
  ,"ALT_no_time_specific_uniquenesses_correlations" = "m9"
  ,"ALT_fixed_time_specific_uniquenesses_correlations" = "m10"
  ,"ALT_fixed_autoregressions_cognitive_variable" = "m11"
  ,"ALT_fixed_autogregressions_social_variable" = "m12"
  ,"ALT_fixed_social_on_cognitive_regressions" = "m13"
  ,"ALT_fixed_cognitive_on_social_regressions" = "m14"
)



ls_covariates <- list(
 "a"       = c("age")
 ,"ae"      = c("age","educ")
 ,"cae"     = c("cohort","age","educ")
 ,"caeh"    = c("cohort", "age","educ","health")
)

ls_path_prototype <- list(
  #"LGM_bivariate" = "m1"
 # ,"autoregressive_bivariate" = "m2"
  "ALT_full_model" = "ALT-full.inp"
  ,"ALT_nested_LGM_model" = "ALT-nestedLCM.inp"
  ,"ALT_fixed_slope_variance_cognitive_variable" = "prototype-wide-ALT-fixed-slope-cog.inp"
  ,"ALT_no_slope_cognitive_variable" = "prototype-wide-ALT-no-slope-cog.inp"
  ,"ALT_fixed_slope_variance_social_variable" = "prototype-wide-ALT-fixed-slope-soc.inp"
  ,"ALT_no_slope_social_variable" = "prototype-wide-ALT-no-slope-soc.inp"
  ,"ALT_no_time_specific_uniquenesses_correlations" = "ALT-no-time-specific-correlations.inp"
  ,"ALT_fixed_time_specific_uniquenesses_correlations" = "ALT-fixed-time-specific-correlations.inp"
  ,"ALT_fixed_autoregressions_cognitive_variable" = "ALT-fixed-autoregression-cog.inp"
  ,"ALT_fixed_autogregressions_social_variable" = "ALT-fixed-autoregression-soc.inp"
  ,"ALT_fixed_social_on_cognitive_regressions" = "ALT_fixed_soc_on_cog_regressions.inp"
  ,"ALT_fixed_cognitive_on_social_regressions" = "ALT_fixed_cog_on_soc_regressions.inp"
)


#################################################################
## @knitr dummy_1

# Use the first example as the template for further pairs
subset_condition_1 = "memoryproblems_baseline EQ 0" # additional subgroup constraints
subset_condition_2 = "memory_disease_ever EQ 0" # exclude those who ever 
subset_group_1 = "age_baseline > 64 AND"
subset_group_2 = ""

folder_data        = "./data-unshared/derived/" # where data resides
#path_prototype     = "./manipulation/estimation/prototype-wide.inp" # Mplus stencil
folder_output      = "./output/bivariate-models-nodem-65plus/" # where the output will go

# # single model
mplus_generator_bivariate(
  model_number        = "m13"
  ,model_type         = "nocov"
  ,covariates         = ""
  ,process_a          = "wrectoti" # item name of process (A), goes into file name
  ,process_b          = "socialnetwork_total"# item name of process (B), goes into file name
  ,subset_group_1     = subset_group_1
  ,subset_condition_1 = subset_condition_2 # subset data to member of this group
  ,data_file          = "wide-dataset-b.dat"
  ,path_prototype     = paste0("./manipulation/estimation/bivariate-models/","Model_13_ALT-11-plus-fixed-cog-on-soc.inp")
  ,folder_data        = folder_data
  ,folder_output      = folder_output
  ,run_models         = TRUE # If TRUE then Mplus runs estimation to produce .out, .gh5, and/or, other files
)

# single model
mplus_generator_bivariate(
  model_number        = "m20"
  ,model_type         = "aechs"
  ,covariates         = c("age","educ","coh","health","sex")
  ,process_a          = "wrectoti" # item name of process (A), goes into file name
  ,process_b          = "socialnetwork_total"# item name of process (B), goes into file name
  ,subset_group_1     = subset_group_1
  ,subset_condition_1 = subset_condition_2 # subset data to member of this group
  ,data_file          = "wide-dataset-b.dat"
  ,path_prototype     = paste0("./manipulation/estimation/covariate-models/","ALT-fixed-auto-covariates.inp")
  ,folder_data        = folder_data
  ,folder_output      = folder_output
  ,run_models         = TRUE # If TRUE then Mplus runs estimation to produce .out, .gh5, and/or, other files
)

subset_condition_1 = "memoryproblems_baseline EQ 0" # additional subgroup constraints
folder_data        = "./data-unshared/derived/" # where data resides
#path_prototype     = "./manipulation/estimation/prototype-wide.inp" # Mplus stencil
folder_output      = "./output/bivariate-models/" # where the output will go
# folder_data        = "./data/unshared/derived/map"
# folder_output      = "./output/studies/map/phys-cog/pulmonary"


# loop over conditions
#for(phys_measure in "fev"){
#for(cog_measure in varnames_cognitive){
  #for(cog_measure in "wordlistim"){
  for(soc_measure in varnames_social){
    for(i in 1:14)
        mplus_generator_bivariate(
          model_number        = model_number[i]
          ,model_type         = "nocov"
          ,covariates         = ""
          ,process_a          = "mentalstatus_tot" # item name of process (A), goes into file name
          ,process_b          = soc_measure # item name of process (B), goes into file name
          ,subset_group_1     = subset_group_1
          ,subset_condition_1 = subset_condition_2 # subset data to member of this group
          ,data_file          = "wide-dataset-b.dat"
          ,path_prototype     = paste0("./manipulation/estimation/bivariate-models/",path_prototype_files[i])
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





