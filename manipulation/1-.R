# ---- correct-number-children ----------------------------------------------------
# Some of the values of closechild (number of children with which R stays close)
# are supsicious (e.g. 66, 44, 127) of bying typoes
# this section of the code demonstrates the issues and criteria for deletion
# of values suspect of being recording/handing errors

# create a target list of persons to test over
# create a list of ids, whose values on closechild include a repeating digit
ids_repeating_digit <- ds %>% 
  dplyr::filter(closechild %in% c(11,22,33,44,55,66,77,88,99)) %>% 
  dplyr::distinct(id) 
ids_repeating_digit <- as.integer(ids_repeating_digit$id)

ids_high_score <- ds %>% 
  dplyr::filter(closechild > 20) %>% 
  dplyr::distinct(id)
ids_high_score <- as.integer(ids_high_score$id)

target_ids <- unique( c(ids_repeating_digit, ids_high_score))   
target_ids %>% length()
# TODO : rexpress the above code as two new variables in dplyr::mutate() statement

ds %>% distinct(id) %>% count()
# Impliment Rule 1:	
# If the number of close children listed was a double digit (e.g., 22, 33, 44) the number of 
# children was made equal to the single digit. 
# [This solves the problem for the majority of cases with greater than 20 close children from 239 to 86]
set.seed(42)
ids <- sample(target_ids,5)
ids
ds %>% 
  dplyr::filter(id %in% ids) %>% 
  dplyr::select(id, year, closechild) %>% 
  print(n=nrow(.))

ds %>% 
  dplyr::filter(id %in% target_ids) %>% 
  dplyr::select(id, year, closechild) %>% 
  print(n=nrow(.))


for(i in ids){
  ds %>% 
    dplyr::filter(id %in% i) %>% 
    dplyr::select(id, year, closechild) %>% 
    print(n=nrow(.))
}


# create separate variables for each digit.
ds$digit1 <- substr(ds$closechild,1,1)
ds$digit2 <- substr(ds$closechild,2,3)


ds$digit1 <- plyr::mapvalues(ds$digit1, from=c("N"), to=c(NA))
ds$digit2 <- plyr::mapvalues(ds$digit2, from=c("aN"), to=c(NA))

# replace the double values with the single digit value.
ds$closechild <- as.numeric(ifelse(ds$digit1 == ds$digit2, ds$digit2, ds$closechild))

# Impliment Rule 2:
# Otherwise, recode closechild [number of children with whom one has a close relationship to NA if greater than]
ds$closechild <- ifelse(ds$closechild>20, NA, ds$closechild)


# ----- --------------------------
