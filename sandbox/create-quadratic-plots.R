

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


# Create a list of predicted values, for initial value at the mean
y_values <- c(y1,y2,y3,y4,y5,y6)
x_values <- c(x1, x2, x3, x4, x5, x6)
wrectd_lonely_plot_vals <- bALT_plot_function(mean(ds_wide$wrectotd_2004, na.rm = T), mean(ds_wide$score_loneliness_3_2004, na.rm = T), wrectd_lone_value_list)


wrectd_lone_plot <- plot(y_values,type = "o", col = "BLACK", xlab = "Wave", ylab = "Predicted Mean",
                         main = "Delayed Word Recall")

lone_wrectd_plot <- plot(x_values,type = "o", col = "BLACK", xlab = "Wave", ylab = "Predicted Mean",
                         main = "Loneliness")
