# ---- functions-to-examine-temporal-patterns -------------------
# examine the pattern of measures over time for a given individual
temporal_pattern <- function(d,time, measure, seed_value = 42){
  set.seed(seed_value)
  d_long <- d
  (ids <- sample(unique(d_long$id),1))
  d1 <-d_long %>%
    dplyr::filter(id %in% ids ) %>%
    dplyr::select_(.dots = c("id",time, measure)) 
  print(d1)
}
# ds %>%  temporal_pattern("year","srmemory", 42)

# examine the descriptives of a measure across time time points
over_time <- function(ds,time, measure_name, exclude_values="") {
  ds <- as.data.frame(ds)
  testit::assert("No such measure in the dataset", measure_name %in% unique(names(ds)))
  # measure_name = "htval"; wave_name = "wave"; exclude_values = c(-99999, -1)
  cat("Measure : ", measure_name,"\n", sep="")
  t <- table( ds[,measure_name], ds[,time], useNA = "always"); t[t==0] <- ".";t
  print(t)
  cat("\n")
  ds[,measure_name] <- as.numeric(ds[,measure_name])
  
  d <- ds[!(ds[,measure_name] %in% exclude_values), ]
  a <- lazyeval::interp(~ round(mean(var),2) , var = as.name(measure_name))
  b <- lazyeval::interp(~ round(sd(var),3),   var = as.name(measure_name))
  c <- lazyeval::interp(~ n())
  dots <- list(a,b,c)
  t <- d %>%
    dplyr::select_("id",time, measure_name) %>%
    na.omit() %>%
    # dplyr::mutate_(measure_name = as.numeric(measure_name)) %>%
    dplyr::group_by_(time) %>%
    dplyr::summarize_(.dots = setNames(dots, c("mean","sd","count")))
  return(as.data.frame(t))
  
}
# ds %>% over_time("year", "srmemory")
# ds %>% over_time("lb_wave", "srmemory")
