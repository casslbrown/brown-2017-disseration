# knitr::stitch_rmd(script="./___/___.R", output="./___/___/___.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>% 
library(dplyr)
library(TabularManifest)
library(MplusAutomation)
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

# ---- immediate-word-recall-model-summaries ---------------------------------------------------------------
# Extract the fit indices of relevant models
immed_word_recall_fit <- extractModelSummaries("./data-unshared/derived/Immediate word recall")
# Show a summary table
#HTMLSummaryTable(immed_word_recall_fit, filename="./data-unshared/derived/Immediate word recall/ImmediateWordRecallSummary", keepCols = c("Title","ChiSqM_Value", "ChiSqM_DF", "CFI", "TLI","RMSEA_Estimate","SRMR"))
ImmediateWordRecallSummary <- SummaryTable(immed_word_recall_fit, type = "markdown", keepCols = c("Title","ChiSqM_Value", "ChiSqM_DF", "CFI", "TLI","RMSEA_Estimate","SRMR"))
print(ImmediateWordRecallSummary) 


# ---- immediate-word-recall-lgm ------------------------
# Create a table of relevant parameters
immed_word_recall_parameters <- extractModelParameters("./data-unshared/derived/Immediate word recall/LGCM word recall immediate.out")

p <- as.data.frame(immed_word_recall_parameters)

p$Parameter <- paste0(p$unstandardized.paramHeader, p$unstandardized.param)

ps <- filter(p, grepl("Means", p[,"Parameter"]) | grepl("VariancesI", p[,"Parameter"])| grepl("VariancesS", p[,"Parameter"]))

ps <- ps %>% plyr::rename(c(
  "unstandardized.est" = "Est",
  "unstandardized.se"  = "SE",
  "unstandardized.pval" = "p_value"
))

ps$Parameter[ps$Parameter=="MeansI"] <- "Intercept"
ps$Parameter[ps$Parameter=="MeansS"] <- "Slope"
ps$Parameter[ps$Parameter=="VariancesI"] <- "Intercept Variance"
ps$Parameter[ps$Parameter=="VariancesS"] <- "Slope Variance"

ps %>%
  dplyr::select(Parameter, Est, SE, p_value)

# ---- immediate-word-recall-atl-------
immed_word_recall_ATLparameters <- extractModelParameters("./data-unshared/derived/Immediate word recall/ATL model immediate word recall.out")

atl_unstandardized <- as.data.frame(immed_word_recall_ATLparameters$unstandardized)
print(atl_unstandardized)

# ---- delayed-word-recall-model-summaries ---------------------------------------------------------------
# Extract the fit indices of relevant models
del_word_recall_fit <- extractModelSummaries("./data-unshared/derived/Delayed word recall")

# Show a summary table
delWordRecallSummary <- SummaryTable(del_word_recall_fit, type = "markdown", keepCols = c("Title","ChiSqM_Value", "ChiSqM_DF", "CFI", "TLI","RMSEA_Estimate","SRMR"))
print(delWordRecallSummary) 

# ---- delayed-word-recall-lgm ------------------------
# Create a table of relevant parameters
delayed_word_recall_parameters <- extractModelParameters("./data-unshared/derived/Delayed word recall/LGCM word recall delayed.out")

p <- as.data.frame(delayed_word_recall_parameters)

p$Parameter <- paste0(p$unstandardized.paramHeader, p$unstandardized.param)

ps <- filter(p, grepl("Means", p[,"Parameter"]) | grepl("VariancesI", p[,"Parameter"])| grepl("VariancesS", p[,"Parameter"]))

ps <- ps %>% plyr::rename(c(
  "unstandardized.est" = "Est",
  "unstandardized.se"  = "SE",
  "unstandardized.pval" = "p_value"
))

ps$Parameter[ps$Parameter=="MeansI"] <- "Intercept"
ps$Parameter[ps$Parameter=="MeansS"] <- "Slope"
ps$Parameter[ps$Parameter=="VariancesI"] <- "Intercept Variance"
ps$Parameter[ps$Parameter=="VariancesS"] <- "Slope Variance"

ps %>%
  dplyr::select(Parameter, Est, SE, p_value)

# ---- delayed-word-recall-atl-------
del_word_recall_ATLparameters <- extractModelParameters("./data-unshared/derived/Delayed word recall/ATL unconditional delayed word recall.out")

del_atl_unstandardized <- as.data.frame(del_word_recall_ATLparameters$unstandardized)
print(del_atl_unstandardized)

# ---- mental-status-model-summaries ---------------------------------------------------------------
# Extract the fit indices of relevant models
mental_status_fit <- extractModelSummaries("./data-unshared/derived/Mental Status")

# Show a summary table
mentalStatusSummary <- SummaryTable(mental_status_fit, type = "markdown", keepCols = c("Title","ChiSqM_Value", "ChiSqM_DF", "CFI", "TLI","RMSEA_Estimate","SRMR"))
print(mentalStatusSummary)

# ---- mental-status-lgm ------------------------
# Create a table of relevant parameters
mental_status_parameters <- extractModelParameters("./data-unshared/derived/Mental Status/LGCM mental status.out")

p <- as.data.frame(mental_status_parameters)

p$Parameter <- paste0(p$unstandardized.paramHeader, p$unstandardized.param)

ps <- filter(p, grepl("Means", p[,"Parameter"]) | grepl("VariancesI", p[,"Parameter"])| grepl("VariancesS", p[,"Parameter"]))

ps <- ps %>% plyr::rename(c(
  "unstandardized.est" = "Est",
  "unstandardized.se"  = "SE",
  "unstandardized.pval" = "p_value"
))

ps$Parameter[ps$Parameter=="MeansI"] <- "Intercept"
ps$Parameter[ps$Parameter=="MeansS"] <- "Slope"
ps$Parameter[ps$Parameter=="VariancesI"] <- "Intercept Variance"
ps$Parameter[ps$Parameter=="VariancesS"] <- "Slope Variance"

ps %>%
  dplyr::select(Parameter, Est, SE, p_value)

# ---- mental-status-atl-------
mental_status_ATLparameters <- extractModelParameters("./data-unshared/derived/Mental Status/ALT mental status.out")

mentalstatus_atl_unstandardized <- as.data.frame(mental_status_ATLparameters$unstandardized)
print(mentalstatus_atl_unstandardized)

# ---- loneliness-model-summaries ---------------------------------------------------------------
# Extract the fit indices of relevant models
loneliness_fit <- extractModelSummaries("./data-unshared/derived/Loneliness")

# Show a summary table
lonelinessSummary <- SummaryTable(loneliness_fit, keepCols = c("Title","ChiSqM_Value", "ChiSqM_DF", "CFI", "TLI","RMSEA_Estimate","SRMR"))

# ---- loneliness-lgm ------------------------
# Create a table of relevant parameters
loneliness_parameters <- extractModelParameters("./data-unshared/derived/Loneliness/LGCM loneliness.out")

p <- as.data.frame(loneliness_parameters)

p$Parameter <- paste0(p$unstandardized.paramHeader, p$unstandardized.param)

ps <- filter(p, grepl("Means", p[,"Parameter"]) | grepl("VariancesI", p[,"Parameter"])| grepl("VariancesS", p[,"Parameter"]))

ps <- ps %>% plyr::rename(c(
  "unstandardized.est" = "Est",
  "unstandardized.se"  = "SE",
  "unstandardized.pval" = "p_value"
))

ps$Parameter[ps$Parameter=="MeansI"] <- "Intercept"
ps$Parameter[ps$Parameter=="MeansS"] <- "Slope"
ps$Parameter[ps$Parameter=="VariancesI"] <- "Intercept Variance"
ps$Parameter[ps$Parameter=="VariancesS"] <- "Slope Variance"

ps %>%
  dplyr::select(Parameter, Est, SE, p_value)

# ----loneliness-atl-------
loneliness_ALTparameters <- extractModelParameters("./data-unshared/derived/Loneliness/ALT unconditional loneliness.out")

mentalstatus_atl_unstandardized <- as.data.frame(loneliness_ALTparameters$unstandardized)
print(mentalstatus_atl_unstandardized)

# ---- social-contact-model-summaries ---------------------------------------------------------------
# Extract the fit indices of relevant models
social_contact_fit <- extractModelSummaries("./data-unshared/derived/Social Contact")

# Show a summary table
socialcontactSummary <- SummaryTable(social_contact_fit, keepCols = c("Title","ChiSqM_Value", "ChiSqM_DF", "CFI", "TLI","RMSEA_Estimate","SRMR"))

# ---- social-contact-lgm ------------------------
# Create a table of relevant parameters
social_contact_parameters <- extractModelParameters("./data-unshared/derived/Social Contact/LGCM social contact.out")

p <- as.data.frame(social_contact_parameters)

p$Parameter <- paste0(p$unstandardized.paramHeader, p$unstandardized.param)

ps <- filter(p, grepl("Means", p[,"Parameter"]) | grepl("VariancesI", p[,"Parameter"])| grepl("VariancesS", p[,"Parameter"]))

ps <- ps %>% plyr::rename(c(
  "unstandardized.est" = "Est",
  "unstandardized.se"  = "SE",
  "unstandardized.pval" = "p_value"
))

ps$Parameter[ps$Parameter=="MeansI"] <- "Intercept"
ps$Parameter[ps$Parameter=="MeansS"] <- "Slope"
ps$Parameter[ps$Parameter=="VariancesI"] <- "Intercept Variance"
ps$Parameter[ps$Parameter=="VariancesS"] <- "Slope Variance"

ps %>%
  dplyr::select(Parameter, Est, SE, p_value)

# ----social-contact-atl-------
socialcontact_ALTparameters <- extractModelParameters("./data-unshared/derived/Social Contact/ALT social contact.out")

socialcontact_atl_unstandardized <- as.data.frame(socialcontact_ALTparameters$unstandardized)
print(socialcontact_atl_unstandardized)

# ---- social-strain-model-summaries ---------------------------------------------------------------
# Extract the fit indices of relevant models
social_strain_fit <- extractModelSummaries("./data-unshared/derived/Social strain")

# Show a summary table
socialstrainSummary <- SummaryTable(social_strain_fit, keepCols = c("Title","ChiSqM_Value", "ChiSqM_DF", "CFI", "TLI","RMSEA_Estimate","SRMR"))

# ---- social-strain-lgm ------------------------
# Create a table of relevant parameters
social_strain_parameters <- extractModelParameters("./data-unshared/derived/Social strain/LGCM social strain.out")

p <- as.data.frame(social_strain_parameters)

p$Parameter <- paste0(p$unstandardized.paramHeader, p$unstandardized.param)

ps <- filter(p, grepl("Means", p[,"Parameter"]) | grepl("VariancesI", p[,"Parameter"])| grepl("VariancesS", p[,"Parameter"]))

ps <- ps %>% plyr::rename(c(
  "unstandardized.est" = "Est",
  "unstandardized.se"  = "SE",
  "unstandardized.pval" = "p_value"
))

ps$Parameter[ps$Parameter=="MeansI"] <- "Intercept"
ps$Parameter[ps$Parameter=="MeansS"] <- "Slope"
ps$Parameter[ps$Parameter=="VariancesI"] <- "Intercept Variance"
ps$Parameter[ps$Parameter=="VariancesS"] <- "Slope Variance"

ps %>%
  dplyr::select(Parameter, Est, SE, p_value)

# ---- social-strain-atl-------
socialstrain_ALTparameters <- extractModelParameters("./data-unshared/derived/Social strain/ATL unconditional social strain.out")

socialstrain_atl_unstandardized <- as.data.frame(socialstrain_ALTparameters$unstandardized)
print(socialstrain_atl_unstandardized)

# ---- social-support-model-summaries ---------------------------------------------------------------
# Extract the fit indices of relevant models
social_support_fit <- extractModelSummaries("./data-unshared/derived/Social support")

# Show a summary table
socialupportSummary <- SummaryTable(social_support_fit, keepCols = c("Title","ChiSqM_Value", "ChiSqM_DF", "CFI", "TLI","RMSEA_Estimate","SRMR"))

# ---- social-support-lgm ------------------------
# Create a table of relevant parameters
social_support_parameters <- extractModelParameters("./data-unshared/derived/Social support/LCGM social support.out")

p <- as.data.frame(social_support_parameters)

p$Parameter <- paste0(p$unstandardized.paramHeader, p$unstandardized.param)

ps <- filter(p, grepl("Means", p[,"Parameter"]) | grepl("VariancesI", p[,"Parameter"])| grepl("VariancesS", p[,"Parameter"]))

ps <- ps %>% plyr::rename(c(
  "unstandardized.est" = "Est",
  "unstandardized.se"  = "SE",
  "unstandardized.pval" = "p_value"
))

ps$Parameter[ps$Parameter=="MeansI"] <- "Intercept"
ps$Parameter[ps$Parameter=="MeansS"] <- "Slope"
ps$Parameter[ps$Parameter=="VariancesI"] <- "Intercept Variance"
ps$Parameter[ps$Parameter=="VariancesS"] <- "Slope Variance"

ps %>%
  dplyr::select(Parameter, Est, SE, p_value)

# ---- social-support-atl-------
socialsupport_ALTparameters <- extractModelParameters("./data-unshared/derived/Social support/ATL unconditional social support.out")

socialsupport_atl_unstandardized <- as.data.frame(socialsupport_ALTparameters$unstandardized)
print(socialsupport_atl_unstandardized)

# ---- depression-model-summaries ------------------------
# Extract the fit indices of relevant models
depression_fit <- extractModelSummaries("./data-unshared/derived/depression")

# Show a summary table
socialstrainSummary <- SummaryTable(social_strain_fit, keepCols = c("Title","ChiSqM_Value", "ChiSqM_DF", "CFI", "TLI","RMSEA_Estimate","SRMR"))
