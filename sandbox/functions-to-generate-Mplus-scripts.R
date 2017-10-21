# ## This script declares the functions that generate Mplus .inp file used in model fitting.

mplus_generator_bivariate <- function(
  model_number
  ,model_type
  ,covariates
  ,process_a
  ,process_b
  ,subset_group_1
  ,subset_condition_1
  ,data_file
  ,path_prototype
  ,folder_data
  ,folder_output
  ,run_models         = FALSE # I
){
 # # Values for testing and development
  # model_number       = "m20"
  # model_type         = "aechs"
  # covariates         = c("age","educ","coh","health","sex")
  # process_a          = "wrectoti" # item name of process (A), goes into file name
  # process_b          = "score_loneliness_3" # item name of process (B), goes into file name
  # subset_group_1     = "age_baseline > 64 AND"
  # subset_condition_1 = "memory_disease_ever EQ 0" # subset data to member of this group
  # folder_data        = "./data-unshared/derived"
  # data_file          = "wide-dataset-b.dat"
  # path_prototype     = paste0("./manipulation/estimation/bivariate-models/","Model_13_ALT-11-plus-fixed-cog-on-soc.inp")
  # folder_output      = "./output/bivariate-models"
  # run_models         = TRUE # If TRUE then Mplus runs estimation to produce .out, .gh5, and/or, other files

   wave_set_modeled   =  c(1,2,3,4,5,6)
   slope_wave_set     = c(1,2,3,4,5,6)
   hrs_year           = c(2004,2006,2008,2010,2012,2014)
   time_slope         = c(0,2,4,6,8,10)  
  
  model_id  <- paste0(model_number,"_",model_type,"_",process_a,"_",process_b)
  
  #covariate_set <- ls_covariates[[model_type]]
  #covariate_set <- model_type
  covariate_set <- covariates
  
  sub_directory <- paste0(folder_output,"/",process_a,"-",process_b)
  dir.create(sub_directory, showWarnings = TRUE)
  path_generic_data <- file.path(folder_data,"wide-dataset-b.dat")
  path_local_data <- file.path(sub_directory,"wide-dataset-b.dat")
  path_generic_names <- file.path(folder_data,"wide-variable-names-b.txt")
  # path_generic_data <- file.path(folder_data,"wide-dataset.dat")
  # path_local_data <- file.path(sub_directory,"wide-dataset.dat")
  # path_generic_names <- file.path(folder_data,"wide-variable-names.txt")
  
  file.copy(
    from=path_generic_data,
    to  =path_local_data,
    overwrite = TRUE
  )
  
  # after modification .inp files will be saved as:
  input_file_name <- paste0(sub_directory,"/", model_id, ".inp")
  
  if(is.numeric(wave_set_modeled)){
    a <- as.character(wave_set_modeled)
    for(i in seq_along(a)){
      a[i] <- ifelse( a[i] %in% paste0(0:9), paste0("0",a[i]),a[i])
    }
    wave_set_modeled <- a
  }
# Added this can remove if slope_wave_set is removed  
  if(is.numeric(slope_wave_set)){
    a <- as.character(slope_wave_set)
    for(i in seq_along(a)){
      a[i] <- ifelse( a[i] %in% paste0(0:9), paste0("0",a[i]),a[i])
    }
    slope_wave_set <- a
  }
  
  
  # input the template to work with
  proto_input <- scan(path_prototype, what='character', sep='\n')
  #This makes it all one (big) element, if you need it in the future.
  # proto_input <- paste(proto_input, collapse="\n")
  
  
  
  names_are <- read.csv(path_generic_names, header = F, stringsAsFactors = F)[ ,1]
  
  # TITLE:
  # DATA:
  # File = wide_dataset.dat; # automatic object, created by `look-at-data.R`
  proto_input <- gsub(pattern = "%data_file%", replacement = data_file, x = proto_input)
  
  # VARIABLE:
  # NAMES are
  # define what variables exist in the dataset
  names_are <- paste(names_are, collapse="\n")  #Collapse all the variable names to one element (seperated by line breaks).
  names_are <- stringr::str_wrap(str = names_are, width  = 80, exdent = 4)
  proto_input <- gsub(pattern = "%names_are%", replacement = names_are, x = proto_input)
  
  
  # USEVARIABLES are
  # define what variables are used in estimation
  #(estimated_timepoints <- paste0("time","_",wave_set_modeled))
  #(estimated_timepoints <- paste(estimated_timepoints, collapse="\n"))
  #proto_input <- gsub(pattern ="%estimated_timepoints%", replacement = estimated_timepoints, x = proto_input)
  
  (process_a_timepoints <- paste0("a","_",wave_set_modeled))
  (process_a_timepoints <- paste(process_a_timepoints, collapse="\n"))
  proto_input <- gsub(pattern ="%process_a_timepoints%", replacement = process_a_timepoints, x = proto_input)
  
  (process_b_timepoints <- paste0("b","_",wave_set_modeled))
  (process_b_timepoints <- paste(process_b_timepoints, collapse="\n"))
  proto_input <- gsub(pattern ="%process_b_timepoints%", replacement = process_b_timepoints, x = proto_input)
  
  (covariate_set <- paste(covariate_set, collapse="\n"))
  proto_input <- gsub(pattern = "%covariate_set%", replacement = covariate_set, x = proto_input)
  
  
  # USEOBSERVATIONS are
  # select a subset of observation
  # TODO: allow for dynamic specification of the grouping variable (male) and values (0,1)
  proto_input <- gsub("%subset_group_1%", subset_group_1, proto_input)
  # subset
  proto_input <- gsub("%subset_condition_1%", subset_condition_1, proto_input)
  
  
  # DEFINE:
  (match_timepoints_process_a <- paste0("a","_",wave_set_modeled,"=",process_a,"_",hrs_year,";"))
  match_timepoints_process_a <- paste(match_timepoints_process_a, collapse="\n")
  proto_input <- gsub(pattern ="%match_timepoints_process_a%", replacement = match_timepoints_process_a, x = proto_input)
  
  (match_timepoints_process_b <- paste0("b","_",wave_set_modeled,"=",process_b,"_",hrs_year,";"))
  match_timepoints_process_b <- paste(match_timepoints_process_b, collapse="\n")
  proto_input <- gsub(pattern ="%match_timepoints_process_b%", replacement = match_timepoints_process_b, x = proto_input)
  
  
  # ANALYSIS:
  # MODEL:
  
  # define process (A) in time points
  (assigning_a_to_timepoints <- paste0("a","_",slope_wave_set,"@",time_slope))
  (assigning_a_to_timepoints <- paste(assigning_a_to_timepoints, collapse="\n"))
  proto_input <- gsub(pattern ="%assigning_a_to_timepoints%", replacement = assigning_a_to_timepoints, x = proto_input)
  
  # define process (B) in time points
  (assigning_b_to_timepoints <- paste0("b","_",slope_wave_set,"@",time_slope))
  (assigning_b_to_timepoints <- paste(assigning_b_to_timepoints, collapse="\n"))
  proto_input <- gsub(pattern ="%assigning_b_to_timepoints%", replacement = assigning_b_to_timepoints, x = proto_input)


  # browser()
  
  # MODEL CONSTRAINT:
  
  # SAVEDATA:
  # FILE is
  #proto_input <- gsub("%saved_analysis%", model_id, proto_input)
  
  # OUTPUT:
  # PLOT:
  
  writeLines(proto_input,input_file_name)
  
  # if(run_models){
  #   # run all models in the folder
  #   pathRoot <- getwd()
  #   saved_location_mplus <- paste0(pathRoot,"/",sub_directory)
  #   saved_location_mplus <- gsub("/./","/",saved_location_mplus)
  #   MplusAutomation::runModels(
  #     directory=saved_location_mplus,
  #     filefilter = paste0(model_id,".inp")
  #   )#, Mplus_command = Mplus_install_path)
  # }
  
  if(run_models){
    # run all models in the folder
    pathRoot <- getwd()
    saved_location_mplus <- paste0(pathRoot,"/",sub_directory)
    saved_location_mplus <- gsub("/./","/",saved_location_mplus)
    MplusAutomation::runModels(
      target =saved_location_mplus,
      filefilter = paste0(model_id,".inp")
    )#, Mplus_command = Mplus_install_path)
  }
  
  
  file.remove(path_local_data)
} # close function

# model generator for Ou paper models
mplus_generator_Ou_bivariate <- function(
  model_number
  ,model_type
  ,covariates
  ,process_a
  ,process_b
  ,subset_group_1
  ,subset_condition_1
  ,data_file
  ,path_prototype
  ,folder_data
  ,folder_output
  ,run_models         = FALSE # I
){
  # # Values for testing and development
  # model_number       = "m20"
  # model_type         = "aechs"
  # covariates         = c("age","educ","coh","health","sex")
  # process_a          = "wrectoti" # item name of process (A), goes into file name
  # process_b          = "score_loneliness_3" # item name of process (B), goes into file name
  # subset_group_1     = "age_baseline > 64 AND"
  # subset_condition_1 = "memory_disease_ever EQ 0" # subset data to member of this group
  # folder_data        = "./data-unshared/derived"
  # data_file          = "wide-dataset-b.dat"
  # path_prototype     = paste0("./manipulation/estimation/bivariate-models/","Model_13_ALT-11-plus-fixed-cog-on-soc.inp")
  # folder_output      = "./output/bivariate-models"
  # run_models         = TRUE # If TRUE then Mplus runs estimation to produce .out, .gh5, and/or, other files
  
  wave_set_modeled   =  c(1,2,3,4,5,6)
  slope_wave_set     = c(1,2,3,4,5,6)
  hrs_year           = c(2004,2006,2008,2010,2012,2014)
  time_slope         = c(0,2,4,6,8,10)  
  
  model_id  <- paste0(model_number,"_",model_type,"_",process_a,"_",process_b)
  
  #covariate_set <- ls_covariates[[model_type]]
  #covariate_set <- model_type
  covariate_set <- covariates
  
  sub_directory <- paste0(folder_output,"/",process_a,"-",process_b)
  dir.create(sub_directory, showWarnings = TRUE)
  path_generic_data <- file.path(folder_data,"wide-dataset-b.dat")
  path_local_data <- file.path(sub_directory,"wide-dataset-b.dat")
  path_generic_names <- file.path(folder_data,"wide-variable-names-b.txt")
  # path_generic_data <- file.path(folder_data,"wide-dataset.dat")
  # path_local_data <- file.path(sub_directory,"wide-dataset.dat")
  # path_generic_names <- file.path(folder_data,"wide-variable-names.txt")
  
  file.copy(
    from=path_generic_data,
    to  =path_local_data,
    overwrite = TRUE
  )
  
  # after modification .inp files will be saved as:
  input_file_name <- paste0(sub_directory,"/", model_id, ".inp")
  
  if(is.numeric(wave_set_modeled)){
    a <- as.character(wave_set_modeled)
    for(i in seq_along(a)){
      a[i] <- ifelse( a[i] %in% paste0(0:9), paste0("0",a[i]),a[i])
    }
    wave_set_modeled <- a
  }
  # Added this can remove if slope_wave_set is removed  
  if(is.numeric(slope_wave_set)){
    a <- as.character(slope_wave_set)
    for(i in seq_along(a)){
      a[i] <- ifelse( a[i] %in% paste0(0:9), paste0("0",a[i]),a[i])
    }
    slope_wave_set <- a
  }
  
  
  # input the template to work with
  proto_input <- scan(path_prototype, what='character', sep='\n')
  #This makes it all one (big) element, if you need it in the future.
  # proto_input <- paste(proto_input, collapse="\n")
  
  
  
  names_are <- read.csv(path_generic_names, header = F, stringsAsFactors = F)[ ,1]
  
  # TITLE:
  # DATA:
  # File = wide_dataset.dat; # automatic object, created by `look-at-data.R`
  proto_input <- gsub(pattern = "%data_file%", replacement = data_file, x = proto_input)
  
  # VARIABLE:
  # NAMES are
  # define what variables exist in the dataset
  names_are <- paste(names_are, collapse="\n")  #Collapse all the variable names to one element (seperated by line breaks).
  names_are <- stringr::str_wrap(str = names_are, width  = 80, exdent = 4)
  proto_input <- gsub(pattern = "%names_are%", replacement = names_are, x = proto_input)
  
  
  # USEVARIABLES are
  # define what variables are used in estimation
  #(estimated_timepoints <- paste0("time","_",wave_set_modeled))
  #(estimated_timepoints <- paste(estimated_timepoints, collapse="\n"))
  #proto_input <- gsub(pattern ="%estimated_timepoints%", replacement = estimated_timepoints, x = proto_input)
  
  (process_a_timepoints <- paste0("a","_",wave_set_modeled))
  (process_a_timepoints <- paste(process_a_timepoints, collapse="\n"))
  proto_input <- gsub(pattern ="%process_a_timepoints%", replacement = process_a_timepoints, x = proto_input)
  
  (process_b_timepoints <- paste0("b","_",wave_set_modeled))
  (process_b_timepoints <- paste(process_b_timepoints, collapse="\n"))
  proto_input <- gsub(pattern ="%process_b_timepoints%", replacement = process_b_timepoints, x = proto_input)
  
  (covariate_set <- paste(covariate_set, collapse="\n"))
  proto_input <- gsub(pattern = "%covariate_set%", replacement = covariate_set, x = proto_input)
  
  
  # USEOBSERVATIONS are
  # select a subset of observation
  # TODO: allow for dynamic specification of the grouping variable (male) and values (0,1)
  proto_input <- gsub("%subset_group_1%", subset_group_1, proto_input)
  # subset
  proto_input <- gsub("%subset_condition_1%", subset_condition_1, proto_input)
  
  
  # DEFINE:
  (match_timepoints_process_a <- paste0("a","_",wave_set_modeled,"=",process_a,"_",hrs_year,";"))
  match_timepoints_process_a <- paste(match_timepoints_process_a, collapse="\n")
  proto_input <- gsub(pattern ="%match_timepoints_process_a%", replacement = match_timepoints_process_a, x = proto_input)
  
  (match_timepoints_process_b <- paste0("b","_",wave_set_modeled,"=",process_b,"_",hrs_year,";"))
  match_timepoints_process_b <- paste(match_timepoints_process_b, collapse="\n")
  proto_input <- gsub(pattern ="%match_timepoints_process_b%", replacement = match_timepoints_process_b, x = proto_input)
  
  
  # ANALYSIS:
  # MODEL:
  
  # define process (A) in time points
  (assigning_a_to_timepoints <- paste0("a","_",slope_wave_set,"@",time_slope))
  (assigning_a_to_timepoints <- paste(assigning_a_to_timepoints, collapse="\n"))
  proto_input <- gsub(pattern ="%assigning_a_to_timepoints%", replacement = assigning_a_to_timepoints, x = proto_input)
  
  # define process (B) in time points
  (assigning_b_to_timepoints <- paste0("b","_",slope_wave_set,"@",time_slope))
  (assigning_b_to_timepoints <- paste(assigning_b_to_timepoints, collapse="\n"))
  proto_input <- gsub(pattern ="%assigning_b_to_timepoints%", replacement = assigning_b_to_timepoints, x = proto_input)
  
  
  # browser()
  
  # MODEL CONSTRAINT:
  
  # SAVEDATA:
  # FILE is
  #proto_input <- gsub("%saved_analysis%", model_id, proto_input)
  
  # OUTPUT:
  # PLOT:
  
  writeLines(proto_input,input_file_name)
  
  # if(run_models){
  #   # run all models in the folder
  #   pathRoot <- getwd()
  #   saved_location_mplus <- paste0(pathRoot,"/",sub_directory)
  #   saved_location_mplus <- gsub("/./","/",saved_location_mplus)
  #   MplusAutomation::runModels(
  #     directory=saved_location_mplus,
  #     filefilter = paste0(model_id,".inp")
  #   )#, Mplus_command = Mplus_install_path)
  # }
  
  if(run_models){
    # run all models in the folder
    pathRoot <- getwd()
    saved_location_mplus <- paste0(pathRoot,"/",sub_directory)
    saved_location_mplus <- gsub("/./","/",saved_location_mplus)
    MplusAutomation::runModels(
      target =saved_location_mplus,
      filefilter = paste0(model_id,".inp")
    )#, Mplus_command = Mplus_install_path)
  }
  
  
  file.remove(path_local_data)
} # close function

# ## This script declares the functions that generate Mplus .inp file used in model fitting.

mplus_generator_univariate <- function(
  model_number
  ,model_type
  ,covariates
  ,process_a
  ,subset_group_1
  ,subset_condition_1
  ,data_file
  ,path_prototype
  ,folder_data
  ,folder_output
  ,run_models         = FALSE # I
){
  # Values for testing and development
  # model_number       = "u1"
  # model_type         = "nocov"
  # covariates         = " "
  # process_a          = "wrectoti" # item name of process (A), goes into file name
  # subset_condition_1 = "memory_disease_ever EQ 0" # subset data to member of this group
  # subset_group_1
  # data_file          = "wide-dataset-b.dat"
  # folder_data        = "./data-unshared/derived"
  # path_prototype     = "./manipulation/estimation/univariate-models/Autoregressive-univariate.inp"
  # folder_output      = "./output/univariate-models"
  # run_models         = TRUE # If TRUE then Mplus runs estimation to produce .out, .gh5, and/or, other files
  # 
  wave_set_modeled   =  c(1,2,3,4,5,6)
  slope_wave_set     = c(2,3,4,5,6)
  hrs_year           = c(2004,2006,2008,2010,2012,2014)
  time_slope         = c(0,2,4,6,8,10)  
  
  model_id  <- paste0(model_number,"_",model_type,"_",process_a)
  
  #covariate_set <- ls_covariates[[model_type]]
  #covariate_set <- model_type
  covariate_set <- covariates
  
  sub_directory <- paste0(folder_output,"/",process_a)
  dir.create(sub_directory, showWarnings = TRUE)
  path_generic_data <- file.path(folder_data,"wide-dataset-b.dat")
  path_local_data <- file.path(sub_directory,"wide-dataset-b.dat")
  path_generic_names <- file.path(folder_data,"wide-variable-names-b.txt")
  # path_generic_data <- file.path(folder_data,"wide-dataset.dat")
  # path_local_data <- file.path(sub_directory,"wide-dataset.dat")
  # path_generic_names <- file.path(folder_data,"wide-variable-names.txt")
  
  file.copy(
    from=path_generic_data,
    to  =path_local_data,
    overwrite = TRUE
  )
  
  # after modification .inp files will be saved as:
  input_file_name <- paste0(sub_directory,"/", model_id, ".inp")
  
  if(is.numeric(wave_set_modeled)){
    a <- as.character(wave_set_modeled)
    for(i in seq_along(a)){
      a[i] <- ifelse( a[i] %in% paste0(0:9), paste0("0",a[i]),a[i])
    }
    wave_set_modeled <- a
  }
  
  
  # input the template to work with
  proto_input <- scan(path_prototype, what='character', sep='\n')
  #This makes it all one (big) element, if you need it in the future.
  # proto_input <- paste(proto_input, collapse="\n")
  
  names_are <- read.csv(path_generic_names, header = F, stringsAsFactors = F)[ ,1]
  
  # TITLE:
  # DATA:
  # File = wide_dataset.dat; # automatic object, created by `look-at-data.R`
  proto_input <- gsub(pattern = "%data_file%", replacement = data_file, x = proto_input)
  
  # VARIABLE:
  # NAMES are
  # define what variables exist in the dataset
  names_are <- paste(names_are, collapse="\n")  #Collapse all the variable names to one element (seperated by line breaks).
  names_are <- stringr::str_wrap(str = names_are, width  = 80, exdent = 4)
  proto_input <- gsub(pattern = "%names_are%", replacement = names_are, x = proto_input)
  
  
  # USEVARIABLES are
  # define what variables are used in estimation
  #(estimated_timepoints <- paste0("time","_",wave_set_modeled))
  #(estimated_timepoints <- paste(estimated_timepoints, collapse="\n"))
  #proto_input <- gsub(pattern ="%estimated_timepoints%", replacement = estimated_timepoints, x = proto_input)
  
  (process_a_timepoints <- paste0("a","_",wave_set_modeled))
  (process_a_timepoints <- paste(process_a_timepoints, collapse="\n"))
  proto_input <- gsub(pattern ="%process_a_timepoints%", replacement = process_a_timepoints, x = proto_input)
  
  (covariate_set <- paste(covariate_set, collapse="\n"))
  proto_input <- gsub(pattern = "%covariate_set%", replacement = covariate_set, x = proto_input)
  
  
  # USEOBSERVATIONS are
  # select a subset of observation
  # TODO: allow for dynamic specification of the grouping variable (male) and values (0,1)
  # subset
  proto_input <- gsub("%subset_group_1%", subset_group_1, proto_input)
  # subset
  proto_input <- gsub("%subset_condition_1%", subset_condition_1, proto_input)
  
  # DEFINE:
  (match_timepoints_process_a <- paste0("a","_",wave_set_modeled,"=",process_a,"_",hrs_year,";"))
  match_timepoints_process_a <- paste(match_timepoints_process_a, collapse="\n")
  proto_input <- gsub(pattern ="%match_timepoints_process_a%", replacement = match_timepoints_process_a, x = proto_input)
  
  
  # ANALYSIS:
  # MODEL:
  
  # define process (A) in time points
  (assigning_a_to_timepoints <- paste0("a","_",slope_wave_set,"@",time_slope))
  (assigning_a_to_timepoints <- paste(assigning_a_to_timepoints, collapse="\n"))
  proto_input <- gsub(pattern ="%assigning_a_to_timepoints%", replacement = assigning_a_to_timepoints, x = proto_input)
 
  # browser()
  
  # MODEL CONSTRAINT:
  
  # SAVEDATA:
  # FILE is
  #proto_input <- gsub("%saved_analysis%", model_id, proto_input)
  
  # OUTPUT:
  # PLOT:
  
  writeLines(proto_input,input_file_name)
  
  if(run_models){
    # run all models in the folder
    pathRoot <- getwd()
    saved_location_mplus <- paste0(pathRoot,"/",sub_directory)
    saved_location_mplus <- gsub("/./","/",saved_location_mplus)
    MplusAutomation::runModels(
      directory=saved_location_mplus,
      filefilter = paste0(model_id,".inp")
    )#, Mplus_command = Mplus_install_path)
  }
  file.remove(path_local_data)
} # close function