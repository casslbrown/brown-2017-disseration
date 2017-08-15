

wrectd_lone_value_list <- bivariate_quad_ALT_parameter_extraction_function(wrectd_lonely_ATL)

y1 <- t1cog # 2004, this value is within rounding error of the mplus output value
x1 <- t1soc
parameter_list <- wrectd_lone_value_list
y2 <- parameter_list[["cog_intercept"]]+2*parameter_list[["cog_slope"]] + parameter_list[["cog_quad"]]*2^2 + parameter_list[["cog_rho21"]]*y1 + parameter_list[["A_02_B_01"]]*x1  #2006
x2 <- parameter_list[["soc_intercept"]]+2*parameter_list[["soc_slope"]] + parameter_list[["soc_rho21"]]*y1 + parameter_list[["B_02_A_01"]]*y1  #2006

y3 <- parameter_list[["cog_intercept"]]+4*parameter_list[["cog_slope"]] + parameter_list[["cog_quad"]]*4^2 + parameter_list[["cog_rho32"]]*y2 + parameter_list[["A_03_B_02"]]*x2 #2008
x3 <- parameter_list[["soc_intercept"]]+4*parameter_list[["soc_slope"]] + parameter_list[["soc_rho32"]]*y2 + parameter_list[["B_03_A_02"]]*y2

y4 <- parameter_list[["cog_intercept"]]+6*parameter_list[["cog_slope"]] + parameter_list[["cog_quad"]]*6^2 + parameter_list[["cog_rho43"]]*y3 + parameter_list[["A_04_B_03"]]*x3  #2010
x4 <- parameter_list[["soc_intercept"]]+6*parameter_list[["soc_slope"]] + parameter_list[["soc_rho43"]]*y3 + parameter_list[["B_04_A_03"]]*y3 

y5 <- parameter_list[["cog_intercept"]]+8*parameter_list[["cog_slope"]] + parameter_list[["cog_quad"]]*8^2 + parameter_list[["cog_rho54"]]*y4 + parameter_list[["A_05_B_04"]]*x4  #2012
x5 <- parameter_list[["soc_intercept"]]+8*parameter_list[["soc_slope"]] + parameter_list[["soc_rho54"]]*y4 + parameter_list[["B_05_A_04"]]*y4

y6 <- parameter_list[["cog_intercept"]]+10*parameter_list[["cog_slope"]]+ parameter_list[["cog_quad"]]*10^2 +parameter_list[["cog_rho65"]]*y5 + parameter_list[["A_06_B_05"]]*x5  #2014
x6 <- parameter_list[["soc_intercept"]]+10*parameter_list[["soc_slope"]]+ parameter_list[["soc_rho65"]]*y5 + parameter_list[["B_06_A_05"]]*y5  #2014

# ---- mental-status -----
# Create a list of predicted values, for initial value at the mean
y_values <- c(y1,y2,y3,y4,y5,y6)
x_values <- c(x1, x2, x3, x4, x5, x6)
wrectd_lonely_plot_vals <- bALT_plot_function(mean(ds_wide$wrectotd_2004, na.rm = T), mean(ds_wide$score_loneliness_3_2004, na.rm = T), wrectd_lone_value_list)

ms_LGM_quadratic_parameters <- extractModelParameters("./output/univariate-models-nodem-65plus/mentalstatus_tot/u03_nocov_mentalstatus_tot.out")

ms_LGM_quadratic <- as.data.frame(ms_LGM_quadratic_parameters[[1]])
# create a new column `parameter` with the two name columns paramHeader and param collapsed together

ms_LGM_quadratic$parameter <- paste(ms_LGM_quadratic$paramHeader,ms_LGM_quadratic$param)

ms_LGM_quadratic <- ms_LGM_quadratic %>% dplyr::select(parameter, est, se, pval)
d<-ms_LGM_quadratic
linear_slope <- d[which(d[,"parameter"]=='Means SA'),"est"]
quadratic_slope <- d[which(d[,"parameter"]=='Means QA'),"est"]
intercept <- d[which(d[,"parameter"]=='Means IA'),"est"]

z1 <- intercept+linear_slope*0+quadratic_slope*0^2
z2 <- intercept+linear_slope*2+quadratic_slope*2^2
z3 <- intercept+linear_slope*4+quadratic_slope*4^2
z4 <- intercept+linear_slope*6+quadratic_slope*6^2
z5 <- intercept+linear_slope*8+quadratic_slope*8^2
z6 <- intercept+linear_slope*10+quadratic_slope*10^2

# Create the data for the chart.
means_q <- c(z1,z2,z3,z4,z5,z6)

mental_status_quadratic_plot <- plot(means_q,type = "o", col = "BLACK", xlab = "Wave", ylab = "Predicted Mean",
                                     main = "Mental Status LGM Quadratic")

wrectd_lone_plot <- plot(y_values,type = "o", col = "BLACK", xlab = "Wave", ylab = "Predicted Mean",
                         main = "Delayed Word Recall")

lone_wrectd_plot <- plot(x_values,type = "o", col = "BLACK", xlab = "Wave", ylab = "Predicted Mean",
                         main = "Loneliness")


wrectd_LGM_quadratic_parameters <- extractModelParameters("./output/univariate-models-nodem-65plus/wrectotd/u03_nocov_wrectotd.out")

wrectd_LGM_quadratic <- as.data.frame(wrectd_LGM_quadratic_parameters[[1]])
# create a new column `parameter` with the two name columns paramHeader and param collapsed together

wrectd_LGM_quadratic$parameter <- paste(wrectd_LGM_quadratic$paramHeader,wrectd_LGM_quadratic$param)

wrectd_LGM_quadratic <- wrectd_LGM_quadratic %>% dplyr::select(parameter, est, se, pval)
d<-wrectd_LGM_quadratic
linear_slope <- d[which(d[,"parameter"]=='Means SA'),"est"]
quadratic_slope <- d[which(d[,"parameter"]=='Means QA'),"est"]
intercept <- d[which(d[,"parameter"]=='Means IA'),"est"]

z1 <- intercept+linear_slope*0+quadratic_slope*0^2
z2 <- intercept+linear_slope*2+quadratic_slope*2^2
z3 <- intercept+linear_slope*4+quadratic_slope*4^2
z4 <- intercept+linear_slope*6+quadratic_slope*6^2
z5 <- intercept+linear_slope*8+quadratic_slope*8^2
z6 <- intercept+linear_slope*10+quadratic_slope*10^2

# Create the data for the chart.
means_q <- c(z1,z2,z3,z4,z5,z6)

wrectd_lgm_quadratic_plot <- plot(means_q,type = "o", col = "BLACK", xlab = "Wave", ylab = "Predicted Mean",
                                  main = "Delayed Word Recall LGM Quadratic")

