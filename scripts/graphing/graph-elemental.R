
#########################################################
### Declare functions to build elemental graphs
#########################################################
# plots value counts of a measure over a metric of time
count_over_time <- function(
  d              # dataset
  ,time_name     # goes on x
  ,measure_name  # mapped to fill
  ,color_palette = "auto" # custom colors for fill
){
  # use these assignments for testing and development
  # d  <- ds
  # time_name     = "year"
  # measure_name  = "male"
  # color_palette = color_male
  # create dataset to be used for graphing
  d1 <- d %>% 
    dplyr::group_by_(time_name, measure_name) %>% 
    dplyr::summarize(n=n()) %>% 
    dplyr::group_by_(time_name) %>% 
    dplyr::mutate(
      time_sum = sum(n)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      pct = scales::percent( n / time_sum )
    )
  # create the graph
  g <- d1 %>% 
    ggplot(aes_string(x=time_name, y="n", fill = measure_name)) +
    geom_bar(stat = "identity")+
    geom_text( inherit.aes = TRUE,
               aes_string(x = time_name, y = "n", label = "pct") 
               # ,nudge_y = 1000 # see recipe
               # ,colour   = "white"
               # ,position = position_dodge(.9)
               # ,size=3
    )+
    theme_minimal()
  if(!color_palette=="auto"){
    g <- g + scale_fill_manual(values=color_palette)
  }
  return(g)
}
# usage:
# ds %>% count_over_time(time_name = "year", measure_name = "male",color_palette = color_male)
# ds %>% count_over_time("year","race",color_race)


# builds a spaghetti plot from raw data
elemental_line <- function(
  d_observed,
  variable_name,
  time_metric, 
  color_name="black",
  line_alpha=1,
  line_size =.5, 
  smoothed = FALSE,
  main_title     = variable_name,
  x_title        = paste0("Time metric: ", time_metric),
  y_title        = variable_name,
  rounded_digits = 0L
) {
  
  d_observed <- as.data.frame(d_observed) #Hack so dplyr datasets don't mess up things
  d_observed <- d_observed[!base::is.na(d_observed[, variable_name]), ]
  
  g <- ggplot(d_observed, aes_string(x=time_metric, y = variable_name)) 
  if(!smoothed){
    g <- g + geom_line(aes_string(group="id"), size=line_size, color=scales::alpha(color_name,line_alpha), na.rm=T)   
  } else{
    g <- g + geom_smooth(aes_string(group="id"),size=line_size,  method="lm",color=scales::alpha(color_name,line_alpha), na.rm=T, se=F )
    g <- g + geom_smooth(method="loess", color="blue", size=1, fill="gray80", alpha=.3, na.rm=T)
    
  }  
  
  g <- g + 
    # scale_x_continuous(labels=scales::comma_format()) +
    scale_y_continuous(labels=scales::comma_format()) +
    # labs(title=main_title, x=x_title, y=y_title) +
    theme_light() +
    theme(axis.ticks.length = grid::unit(0, "cm"))
  return( g )
}
