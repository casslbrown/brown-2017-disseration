# This script will read in model results from Mplus. 
# knitr::stitch_rmd(script="./___/___.R", output="./___/stitched-output/___.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.


# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>%
library(MplusAutomation)
# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2") # graphing
requireNamespace("tidyr") # data manipulation
requireNamespace("dplyr") # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit")# For asserting conditions meet expected patterns.
requireNamespace("reshape2") # data transformations
requireNamespace("data.table") # data transformations
requireNamespace("MplusAutomation")
requireNamespace("stringr")
requireNamespace("relimp")

# ---- load-data ---------------------------------------------------------------

# Immediate Word Recall
# Extract the fit indices of relevant models
immed_word_recall_fit <- extractModelSummaries("./data-unshared/derived/Immediate word recall")

# Extract relevant parameters
immed_word_recall_all <- MplusAutomation::readModels("./data-unshared/derived/LGM year based aged 65 plus/LGCM word recall immediate.out")

# Show a summary table
HTMLSummaryTable(immed_word_recall_fit, filename="./data-unshared/derived/Immediate word recall/ImmediateWordRecallSummary", keepCols = c("Title","ChiSqM_Value", "ChiSqM_DF", "CFI", "TLI","RMSEA_Estimate","SRMR"))

# Create a table of relevant parameters
immed_word_recall_parameters <- extractModelParameters("./data-unshared/derived/Immediate word recall/LGCM word recall immediate.out")


