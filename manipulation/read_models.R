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
# load the product of 1-scale-assembly.R a long data file
lgc_loneliness <- MplusAutomation::readModels("./data-unshared/derived/LGCM Loneliness.out")
lonelinessmodel <- extractModelSummaries("./data-unshared/derived/LGCM Loneliness.out")
lgc_loneliness_parameters <- as.data.frame(extractModelParameters("./data-unshared/derived/ATL immediate word recall by year.out"))
class(lonelinessmodel)
SummaryTable(lonelinessmodel)
SummaryTable(lgc_loneliness_parameters)

alt_model <- extractModelSummaries("./data-unshared/derived/ATL loneliness and mental status by year.out")
alt_model_parameters <- extractModelParameters("./data-unshared/derived/ATL loneliness and mental status by year.out")
