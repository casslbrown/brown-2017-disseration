
library('DiagrammeR')

ALT_diagram_parameter_extraction_function <- function(d){
  #d <- wrectd_lonely_ATL
  par_list<- list()
  par_list[["cog_slope"]] <- ifelse(d[which(d[,"parameter"]=='Means SA'),"pval"] < 0.05, paste0(d[which(d[,"parameter"]=='Means SA'),"est"],"*"), d[which(d[,"parameter"]=='Means SA'), "est"])
  par_list[["cog_slope_variance"]] <- ifelse(d[which(d[,"parameter"]=='Variances SA'),"est"] < 0.05, paste0(d[which(d[,"parameter"]=='Variances SA'),"est"],"*"), d[which(d[,"parameter"]=='Variances SA'), "est"])

  par_list[["cog_intercept"]] <- ifelse(d[which(d[,"parameter"]=='Means IA'),"pval"] < 0.05, paste0(d[which(d[,"parameter"]=='Means IA'),"est"], "*"),d[which(d[,"parameter"]=='Means IA'),"est"])
  par_list[["cog_intercept_variance"]] <- ifelse(d[which(d[,"parameter"]=='Variances IA'),"pval"] < 0.05, paste0(d[which(d[,"parameter"]=='Variances IA'),"est"], "*"), d[which(d[,"parameter"]=='Variances IA'),"est"])
  par_list[["soc_slope"]] <- ifelse(d[which(d[,"parameter"]=='Means SB'),"pval"] < 0.05, paste0(d[which(d[,"parameter"]=='Means SB'),"est"], "*"), d[which(d[,"parameter"]=='Means SB'),"est"])
  par_list[["soc_slope_variance"]] <- ifelse(d[which(d[,"parameter"]=='Variances SB'),"pval"] < 0.05, paste0(d[which(d[,"parameter"]=='Variances SB'),"est"], "*"), d[which(d[,"parameter"]=='Variances SB'),"est"])
  par_list[["soc_intercept"]] <- ifelse(d[which(d[,"parameter"]=='Means IB'),"pval"] < 0.05, paste0(d[which(d[,"parameter"]=='Means IB'),"est"],"*"), d[which(d[,"parameter"]=='Means IB'),"est"])
  par_list[["soc_intercept_variance"]] <- ifelse(d[which(d[,"parameter"]=='Variances IB'),"pval"] < 0.05, paste0(d[which(d[,"parameter"]=='Variances IB'),"est"], "*"), d[which(d[,"parameter"]=='Variances IB'),"est"])
  par_list[["cog_rho21"]] <- ifelse(d[which(d[,"parameter"]=='A_02.ON A_01'),"pval"] < 0.05, paste0(d[which(d[,"parameter"]=='A_02.ON A_01'),"est"],"*"), d[which(d[,"parameter"]=='A_02.ON A_01'),"est"])
  par_list[["cog_rho32"]] <- ifelse(d[which(d[,"parameter"]=='A_03.ON A_02'),"pval"] < 0.05, paste0(d[which(d[,"parameter"]=='A_03.ON A_02'),"est"],"*"), d[which(d[,"parameter"]=='A_03.ON A_02'),"est"])
  par_list[["cog_rho43"]] <- ifelse(d[which(d[,"parameter"]=='A_04.ON A_03'),"pval"] < 0.05, paste0(d[which(d[,"parameter"]=='A_04.ON A_03'),"est"],"*"), d[which(d[,"parameter"]=='A_04.ON A_03'),"est"])
  par_list[["cog_rho54"]] <- ifelse(d[which(d[,"parameter"]=='A_05.ON A_04'),"pval"] < 0.05, paste0(d[which(d[,"parameter"]=='A_05.ON A_04'),"est"],"*"), d[which(d[,"parameter"]=='A_05.ON A_04'),"est"])
  par_list[["cog_rho65"]] <- ifelse(d[which(d[,"parameter"]=='A_06.ON A_05'),"pval"] < 0.05, paste0(d[which(d[,"parameter"]=='A_06.ON A_05'),"est"],"*"), d[which(d[,"parameter"]=='A_06.ON A_05'),"est"])
  par_list[["soc_rho21"]] <- ifelse(d[which(d[,"parameter"]=='B_02.ON B_01'),"pval"] < 0.05, paste0(d[which(d[,"parameter"]=='B_02.ON B_01'),"est"],"*"), d[which(d[,"parameter"]=='B_02.ON B_01'),"est"])
  par_list[["soc_rho32"]] <- ifelse(d[which(d[,"parameter"]=='B_03.ON B_02'),"pval"] < 0.05, paste0(d[which(d[,"parameter"]=='B_03.ON B_02'),"est"],"*"), d[which(d[,"parameter"]=='B_03.ON B_02'),"est"])
  par_list[["soc_rho43"]] <- ifelse(d[which(d[,"parameter"]=='B_04.ON B_03'),"pval"] < 0.05, paste0(d[which(d[,"parameter"]=='B_04.ON B_03'),"est"],"*"), d[which(d[,"parameter"]=='B_04.ON B_03'),"est"])
  par_list[["soc_rho54"]] <- ifelse(d[which(d[,"parameter"]=='B_05.ON B_04'),"pval"] < 0.05, paste0(d[which(d[,"parameter"]=='B_05.ON B_04'),"est"],"*"), d[which(d[,"parameter"]=='B_05.ON B_04'),"est"])
  par_list[["soc_rho65"]] <- ifelse(d[which(d[,"parameter"]=='B_06.ON B_05'),"pval"] < 0.05, paste0(d[which(d[,"parameter"]=='B_06.ON B_05'),"est"],"*"), d[which(d[,"parameter"]=='B_06.ON B_05'),"est"])

  par_list[["A_02_B_01"]] <- ifelse(d[which(d[,"parameter"]=='A_02.ON B_01'),"pval"] < 0.05, paste0(d[which(d[,"parameter"]=='A_02.ON B_01'),"est"],"*"), d[which(d[,"parameter"]=='A_02.ON B_01'),"est"])
  par_list[["A_03_B_02"]] <- ifelse(d[which(d[,"parameter"]=='A_03.ON B_02'),"pval"] < 0.05, paste0(d[which(d[,"parameter"]=='A_03.ON B_02'),"est"],"*"), d[which(d[,"parameter"]=='A_03.ON B_02'),"est"])
  par_list[["A_04_B_03"]] <- ifelse(d[which(d[,"parameter"]=='A_04.ON B_03'),"pval"] < 0.05, paste0(d[which(d[,"parameter"]=='A_04.ON B_03'),"est"],"*"), d[which(d[,"parameter"]=='A_04.ON B_03'),"est"])
  par_list[["A_05_B_04"]] <- ifelse(d[which(d[,"parameter"]=='A_05.ON B_04'),"pval"] < 0.05, paste0(d[which(d[,"parameter"]=='A_05.ON B_04'),"est"],"*"), d[which(d[,"parameter"]=='A_05.ON B_04'),"est"])
  par_list[["A_06_B_05"]] <- ifelse(d[which(d[,"parameter"]=='A_06.ON B_05'),"pval"] < 0.05, paste0(d[which(d[,"parameter"]=='A_06.ON B_05'),"est"],"*"), d[which(d[,"parameter"]=='A_06.ON B_05'),"est"])

  par_list[["B_02_A_01"]] <- ifelse(d[which(d[,"parameter"]=='B_02.ON A_01'),"pval"] < 0.05, paste0(d[which(d[,"parameter"]=='B_02.ON A_01'),"est"],"*"), d[which(d[,"parameter"]=='B_02.ON A_01'),"est"])
  par_list[["B_03_A_02"]] <- ifelse(d[which(d[,"parameter"]=='B_03.ON A_02'),"pval"] < 0.05, paste0(d[which(d[,"parameter"]=='B_03.ON A_02'),"est"],"*"), d[which(d[,"parameter"]=='B_03.ON A_02'),"est"])
  par_list[["B_04_A_03"]] <- ifelse(d[which(d[,"parameter"]=='B_04.ON A_03'),"pval"] < 0.05, paste0(d[which(d[,"parameter"]=='B_04.ON A_03'),"est"],"*"), d[which(d[,"parameter"]=='B_04.ON A_03'),"est"])
  par_list[["B_05_A_04"]] <- ifelse(d[which(d[,"parameter"]=='B_05.ON A_04'),"pval"] < 0.05, paste0(d[which(d[,"parameter"]=='B_05.ON A_04'),"est"],"*"), d[which(d[,"parameter"]=='B_05.ON A_04'),"est"])
  par_list[["B_06_A_05"]] <- ifelse(d[which(d[,"parameter"]=='B_06.ON A_05'),"pval"] < 0.05, paste0(d[which(d[,"parameter"]=='B_06.ON A_05'),"est"],"*"), d[which(d[,"parameter"]=='B_06.ON A_05'),"est"])
  
  return(par_list)
  }

# immediate word recall and social network
wrecti_sn_ATL <- read.csv("./output/bivariate-models-nodem-65plus/wrecti_social_network_model_parameters.csv")
diagram_parameters <- ALT_diagram_parameter_extraction_function(wrecti_sn_ATL)

# immediate word recall and social network
wrecti_socsup <- read.csv("./output/bivariate-models-nodem-65plus/wrecti_social_support_model_parameters.csv")
wrecti_socsup_parameters <- ALT_diagram_parameter_extraction_function(wrecti_socsup)

wrecti_labels <- c("Immediate&#92;nWord&#92;nRecall 1", "Immediate&#92;nWord&#92;nRecall 2","Immediate&#92;nWord&#92;nRecall 3","Immediate&#92;nWord&#92;nRecall 4","Immediate&#92;nWord&#92;nRecall 5","Immediate&#92;nWord&#92;nRecall 6")
diagram_parameters <- wrecti_socsup_parameters
cog_labels <- wrecti_labels
graph_function()

#---------
graph_function_covariates <- function(){
alt_plot <- grViz("
digraph SEM {
      
            graph [layout = neato,
            overlap = false,
            outputorder = edgesfirst]
            
            node [shape = rectangle]
            
            cog1 [pos = '-6,1!', label = '@@6-1']
            cog2 [pos = '-4,1!', label = '@@6-2']
            cog3 [pos = '-2,1!', label = '@@6-3']
            cog4 [pos = '0,1!', label = '@@6-4']
            cog5 [pos = '2,1!', label = '@@6-5']
            cog6 [pos = '4,1!', label = '@@6-6']
            age [pos = '-10,2!', label = 'Age']
            sex [pos ='-10,4!', label = 'Sex']
            cohort [pos = '-10,0!', label = 'Cohort']
            health [pos = '-10,-2!' label = 'Health&#92;nConditions']
            

            intercepta [pos = '-3,-1!', label = '@@1', shape = circle]
            slopea [pos = '-2,-2!', label = '@@2', shape = circle]
            
            soc1 [pos = '-6,4!', label = 'soc 1']
            soc2 [pos = '-4,4!', label = 'soc 2']
            soc3 [pos = '-2,4!', label = 'soc 3']
            soc4 [pos = '0,4!', label = 'soc 4']
            soc5 [pos = '2,4!', label = 'soc 5']
            soc6 [pos = '4,4!', label = 'soc 6']

            interceptb [pos = '-3,6!', label = '@@3', shape = circle]
            slopeb [pos = '-2,7!', label = '@@4', shape = circle]
            age->intercepta
            age->slopea
            age->interceptb
            age->slopeb
            sex->intercepta
            sex->slopea
            sex->interceptb
            sex->slopeb
            cohort->intercepta
            cohort->slopea
            cohort->interceptb
            cohort->slopeb
            health->intercepta
            health->slopea
            health->interceptb
            health->slopeb
            cog1->cog2 [label = '@@5-9']
            cog2->cog3 [label = '@@5-10']
            cog3->cog4 [label = '@@5-11']
            cog4->cog5 [label = '@@5-12']
            cog5->cog6 [label = '@@5-13']
            intercepta->cog1 [label = '1']
            intercepta->cog2 [label = '1']
            intercepta->cog3 [label = '1']
            intercepta->cog4 [label = '1']
            intercepta->cog5 [label = '1']
            intercepta->cog6 [label = '1']

            slopea->cog1 [label = '0']
            slopea->cog2 [label = '2']
            slopea->cog3 [label = '4']
            slopea->cog4 [label = '6']
            slopea->cog5 [label = '8']
            slopea->cog6 [label = '10']
            
            soc1->soc2 [label = '@@5-14']
            soc2->soc3 [label = '@@5-15']
            soc3->soc4 [label = '@@5-16']
            soc4->soc5 [label = '@@5-17']
            soc5->soc6 [label = '@@5-18']

            interceptb->soc1 [label = '1']
            interceptb->soc2 [label = '1']
            interceptb->soc3 [label = '1']
            interceptb->soc4 [label = '1']
            interceptb->soc5 [label = '1']
            interceptb->soc6 [label = '1']
            
            slopeb->soc1 [label = '0']
            slopeb->soc2 [label = '2']
            slopeb->soc3 [label = '4']
            slopeb->soc4 [label = '6']
            slopeb->soc5 [label = '8']
            slopeb->soc6 [label = '10']
            soc1->cog2 [label = '@@5-24']
            soc2->cog3 [label = '@@5-25']
            soc3->cog4 [label = '@@5-26']
            soc4->cog5 [label = '@@5-27']
            soc5->cog6 [label = '@@5-28']
            cog1->soc2 [label = '@@5-19'] 
            cog2->soc3 [label = '@@5-20']
            cog3->soc4 [label = '@@5-21']
            cog4->soc5 [label = '@@5-22']
            cog5->soc6 [label = '@@5-23']


      }
[1]: paste0('i = ',diagram_parameters[['cog_intercept']],'&#92;n','var = ', diagram_parameters[['cog_intercept_variance']])
[2]: paste0('s = ',diagram_parameters[['cog_slope']],'&#92;n','var = ', diagram_parameters[['cog_slope_variance']])
[3]: paste0('i = ',diagram_parameters[['soc_intercept']],'&#92;n','var = ', diagram_parameters[['soc_intercept_variance']])
[4]: paste0('s = ',diagram_parameters[['soc_slope']],'&#92;n','var = ', diagram_parameters[['soc_slope_variance']])
[5]: diagram_parameters
[6]: cog_labels
            
             ")
return(alt_plot)
}


#---------
graph_function <- function(){
  alt_plot <- grViz("
                    digraph SEM {
                    
                    graph [layout = neato,
                    overlap = false,
                    outputorder = edgesfirst]
                    
                    node [shape = rectangle]
                    
                    cog1 [pos = '-6,1!', label = '@@6-1']
                    cog2 [pos = '-4,1!', label = '@@6-2']
                    cog3 [pos = '-2,1!', label = '@@6-3']
                    cog4 [pos = '0,1!', label = '@@6-4']
                    cog5 [pos = '2,1!', label = '@@6-5']
                    cog6 [pos = '4,1!', label = '@@6-6']
                    
                    
                    intercepta [pos = '-3,-1!', label = '@@1', shape = circle]
                    slopea [pos = '-2,-2!', label = '@@2', shape = circle]
                    
                    soc1 [pos = '-6,4!', label = 'soc 1']
                    soc2 [pos = '-4,4!', label = 'soc 2']
                    soc3 [pos = '-2,4!', label = 'soc 3']
                    soc4 [pos = '0,4!', label = 'soc 4']
                    soc5 [pos = '2,4!', label = 'soc 5']
                    soc6 [pos = '4,4!', label = 'soc 6']
                    
                    interceptb [pos = '-3,6!', label = '@@3', shape = circle]
                    slopeb [pos = '-2,7!', label = '@@4', shape = circle]

                    cog1->cog2 [label = '@@5-9']
                    cog2->cog3 [label = '@@5-10']
                    cog3->cog4 [label = '@@5-11']
                    cog4->cog5 [label = '@@5-12']
                    cog5->cog6 [label = '@@5-13']
                    intercepta->cog1 [label = '1']
                    intercepta->cog2 [label = '1']
                    intercepta->cog3 [label = '1']
                    intercepta->cog4 [label = '1']
                    intercepta->cog5 [label = '1']
                    intercepta->cog6 [label = '1']
                    
                    slopea->cog1 [label = '0']
                    slopea->cog2 [label = '2']
                    slopea->cog3 [label = '4']
                    slopea->cog4 [label = '6']
                    slopea->cog5 [label = '8']
                    slopea->cog6 [label = '10']
                    
                    soc1->soc2 [label = '@@5-14']
                    soc2->soc3 [label = '@@5-15']
                    soc3->soc4 [label = '@@5-16']
                    soc4->soc5 [label = '@@5-17']
                    soc5->soc6 [label = '@@5-18']
                    
                    interceptb->soc1 [label = '1']
                    interceptb->soc2 [label = '1']
                    interceptb->soc3 [label = '1']
                    interceptb->soc4 [label = '1']
                    interceptb->soc5 [label = '1']
                    interceptb->soc6 [label = '1']
                    
                    slopeb->soc1 [label = '0']
                    slopeb->soc2 [label = '2']
                    slopeb->soc3 [label = '4']
                    slopeb->soc4 [label = '6']
                    slopeb->soc5 [label = '8']
                    slopeb->soc6 [label = '10']
                    soc1->cog2 [label = '@@5-24']
                    soc2->cog3 [label = '@@5-25']
                    soc3->cog4 [label = '@@5-26']
                    soc4->cog5 [label = '@@5-27']
                    soc5->cog6 [label = '@@5-28']
                    cog1->soc2 [label = '@@5-19'] 
                    cog2->soc3 [label = '@@5-20']
                    cog3->soc4 [label = '@@5-21']
                    cog4->soc5 [label = '@@5-22']
                    cog5->soc6 [label = '@@5-23']
                    
                    
                    }
                    [1]: paste0('i = ',diagram_parameters[['cog_intercept']],'&#92;n','var = ', diagram_parameters[['cog_intercept_variance']])
                    [2]: paste0('s = ',diagram_parameters[['cog_slope']],'&#92;n','var = ', diagram_parameters[['cog_slope_variance']])
                    [3]: paste0('i = ',diagram_parameters[['soc_intercept']],'&#92;n','var = ', diagram_parameters[['soc_intercept_variance']])
                    [4]: paste0('s = ',diagram_parameters[['soc_slope']],'&#92;n','var = ', diagram_parameters[['soc_slope_variance']])
                    [5]: diagram_parameters
                    [6]: cog_labels
                    
                    ")
  return(alt_plot)
}