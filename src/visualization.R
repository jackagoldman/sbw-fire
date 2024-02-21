# visualization
#'@author Jack A. Goldman


#' prep data for visualization
#'
#' @param severity data frame with burn severity data, must have cols: id, fire_name, rbr_median, rbr_extreme, rbr_cv
#' @param defoliation data frame with defoliation data. must have cols: id, defoliation, tsd, cumltve_yrs
#' @param slope data frame with slope data, must have cols id, sens10, sens1, sens2
#'
#' @return combined df joined by id
#' @export
#'
#' @examples
vis_prep <- function(severity, defoliation, slope){
  
  defol <- dplyr::select(defoliation, c("id", "defoliated", "tsd", "cumltve_yrs"))
  slope <- dplyr::select(slope, -c("...1", "fire_name"))
  df <- dplyr::left_join(severity,defol, by ="id") 
  df <- dplyr::left_join(df,slope, by ="id" )
  df <- dplyr::mutate(df, defoliated = case_when(defoliated == 1 ~ "Defoliated",
                                                 defoliated == 0 ~ "Non-Defoliated"))
  return(df)
}



#' Clean results from ttest
#'
#' @param results dataframe. Ttest results from hypothesis testing. cols: test, t-value, df, p-value
#' @param nrowSub row to subset. must be one of All. 1 (slope 1-5), 2(slope6-10), Median, Extreme or Variability.
#'
#' @return tibble for with column .y., group 1, group 2, n1, n2, df, statistic, p, p.signif
#' 
#'
#' @examples
createTib <- function(results, nrowSub){
  
  y <- "len"
  group1 <- "Defoliated"
  group2 <- "Non-Defoliated"
  n1 = 33
  n2 = 33
  df = 65
  
  if(nrowSub == "all"){
    nrow = 1
  } else if(nrowSub == "1"){
    nrow = 2
  } else if(nrowSub == "2"){
    nrow = 3
  } else if(nrowSub == "Median"){
    nrow = 1
  }else if(nrowSub == "Extreme"){
    nrow = 1
  } else if (nrowSub == "Variability"){
    nrow = 3
  }
  
 
    x <- dplyr::slice(results, nrow)
    title <- dplyr::select(x, c(Test))
    statistic <-  x$`T-value`
    p <- x$`P-value`
    p <- stringr::str_remove(p, "<")
    p.signif <- if(p <= 0.0001){
      p.signif = "***"
    }else if(p <= 0.01 & p >= 0.001){
      p.signif = "**" 
    }else if(p <= 0.05 & p >=0.01){
      p.signif = "*"
    }
    
    tibRes <- tibble(y, group1, group2, n1, n2, statistic, df, p, p.signif)
    
    tibRes <- dplyr::rename(tibRes, ".y." = "y")
    
    return(tibRes)
  }
  


#' Boxplots comparing t-tests
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
recovery_visBox <- function(df, resSlope, resSev){
  
  
  # defol plot med
  sigVals_med <- createTib(resSev, "Median")
  defolPlot_med <- ggplot2::ggplot(df, aes(x = defolaited, y = rbr_median))+
    geom_boxplot() +
    labs(x = "Median Severity", y = "Presence/Absence", title = "Defoliation Presence/Absence") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) 
  
  # get max mean, lwr and upper ci from boxplot to get the location for where to put the annotation
  limits <- layer_scales(defolPlot_senmed)$y$get_limits()[[2]] + 2
  
  # set location for comparison
  sigVals_med <- dplyr::mutate(sigVals_med, y.position = c(limits)) # this needs a position on graph
  
  # annotate plot
  defolPlot_med <- defolPlot_med + ggpubr::stat_pvalue_manual(sigVals_med, label = "p")
  
  
  # defol plot ext
  sigVals_ext <- createTib(resSev, "Extreme")
  defolPlot_ext <- ggplot2::ggplot(df, aes(x = defoliated, y = rbr_extreme))+
    geom_boxplot() +
    labs(x = "Extreme Severity", y = "Presence/Absence", title = "Defoliation Presence/Absence") +
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5)) 
  
  # get max mean, lwr and upper ci from boxplot to get the location for where to put the annotation
  limits <- layer_scales(defolPlot_senext)$y$get_limits()[[2]] + 2
  
  # set location for comparison
  sigVals_ext <- dplyr::mutate(sigVals_ext, y.position = c(limits)) # this needs a position on graph
  
  # annotate plot
  defolPlot_ext <- defolPlot_ext + ggpubr::stat_pvalue_manual(sigVals_ext, label = "p")
  
  
  # defol plot variability
  sigVals_cv <- createTib(resSev, "Variability")
  defolPlot_cv <-  ggplot2::ggplot(df, aes(x = defoliated, y = rbr_cv))+
    geom_boxplot() +
    labs(x = "Variability in burn Severity", y = "Presence/Absence", title = "Defoliation Presence/Absence") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) 
  
  # get max mean, lwr and upper ci from boxplot to get the location for where to put the annotation
  limits <- layer_scales(defolPlot_sencv)$y$get_limits()[[2]] + 2
  
  # set location for comparison
  sigVals_cv <- dplyr::mutate(sigVals_cv, y.position = c(limits)) # this needs a position on graph
  
  # annotate plot
  defolPlot_cv <- defolPlot_cv + ggpubr::stat_pvalue_manual(sigVals_cv, label = "p")
  
  
  # defol plot sens 10
  sigVals_s10 <- createTib(resSlope, "All")
  defolPlot_sens10 <-  ggplot2::ggplot(df, aes(x =defoliated, y = sens10))+
    geom_boxplot() +
    labs(x = "10yr slope of recovery", y = "Presence/Absence", title = "Defoliation Presence/Absence") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) 
  
  # get max mean, lwr and upper ci from boxplot to get the location for where to put the annotation
  limits <- layer_scales(defolPlot_sens10)$y$get_limits()[[2]] + 2
  
  # set location for comparison
  sigVals_s10 <- dplyr::mutate(sigVals_s10, y.position = c(limits)) # this needs a position on graph
  
  # annotate plot
  defolPlot_sens10 <- defolPlot_sens10 + ggpubr::stat_pvalue_manual(sigVals_s10, label = "p")
  
  
  # defol plot sens 1
  sigVals_s1 <- createTib(resSlope, "1")
  defolPlot_sens1 <- ggplot2::ggplot(df, aes(x = defoliated, y = sens1))+
    geom_boxplot() +
    labs(x = "1-5yr slope of recovery", y = "Presence/Absence", title = "Defoliation Presence/Absence") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) 
  # get max mean, lwr and upper ci from boxplot to get the location for where to put the annotation
  limits <- layer_scales(defolPlot_sens1)$y$get_limits()[[2]] + 2
  
  # set location for comparison
  sigVals_s1 <- dplyr::mutate(sigVals_s1, y.position = c(limits)) # this needs a position on graph
  
  # annotate plot
  defolPlot_sens1 <- defolPlot_sens1 + ggpubr::stat_pvalue_manual(sigVals_s1, label = "p")
  
  
  # defol plot sens 2
  sigVals_s2 <- createTib(resSlope, "2") 

  defolPlot_sens2 <- ggplot2::ggplot(df, aes(x = defoliated, y = sens2))+
    geom_boxplot() +
    labs(x = "6-10yr slope of recovery", y = "Presence/Absence", title = "Defoliation Presence/Absence") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) 
  
  # get max mean, lwr and upper ci from boxplot to get the location for where to put the annotation
  limits <- layer_scales(defolPlot_sens2)$y$get_limits()[[2]] + 2
  
  # set location for comparison
  sigVals_s2 <- dplyr::mutate(sigVals_s2, y.position = c(limits)) # this needs a position on graph
  
  # annotate plot
  defolPlot_sens2 <- defolPlot_sens2 + ggpubr::stat_pvalue_manual(sigVals_s2, label = "p")
  
  
  # combine plots to 6 panel figure
  plot <- ggpubr::ggarrange(defolPlot_med, defolPlot_ext, defolPlot_cv, 
                    defolPlot_sens10, defolPlot_sens1, defolPlot_sens2, ncol = 3, nrow =3)
  
  return(plot)
  
}




#' trend visualization data preparation
#'
#' @param recovery_data 
#' @param defol_data 
#'
#' @return
#' @export
#'
#' @examples
trendPrep <- function(recovery_data, defol_data){
  
  # clean defoliated data and join to recovery data
  defol_data <- dplyr::select(defol_data, c("id", "defoliated", "tsd", "cumltve_yrs"))
  recData <- dplyr::left_join(recovery_data, defol_data, by = "id")
  
  #set time order for variables
  time_order <- c('preNBR', 'nbr1','nbr2', 'nbr3', 'nbr4', 'nbr5','nbr6', 'nbr7', 'nbr8', 'nbr9', 'nbr10')

  recTs <-  dplyr::select(recData, c('fire_name', 'defoliated', 'preNBR', 'nbr1','nbr2', 'nbr3', 'nbr4', 'nbr5','nbr6', 'nbr7', 'nbr8', 'nbr9', 'nbr10'))
  
 return(recTs)
  
}

#' Trend plots
#'
#' @param trendPrep_data 
#'
#' @return
#' @export
#'
#' @examples
trend_plot <-  function(ts_data){
  
  
  recTs_1 <- recTs |> 
    tidyr::pivot_longer(-c(defoliated, fire_name)) |> 
    dplyr::filter(defoliated == "1") 
  
  
  recTs_0 <- recTs |>
    tidyr::pivot_longer(-c(defoliated, fire_name)) |> 
    dplyr::filter(defoliated == "0")
 
  
  sum1 <- summarySE(recTs_1, measurevar="value", groupvars=c("name")) |> 
    dplyr::mutate(defoliated = 1)
  sum0 <- summarySE(recTs_0, measurevar="value", groupvars=c("name")) |> 
    dplyr::mutate(defoliated = 0)
  
  
  sum_nbr <- rbind(sum1, sum0)
  pd <- position_dodge(0.1) # move them .05 to the left and right
  
  
  sum_nbr <- rbind(sum1, sum0)
  pd <- ggplot2::position_dodge(0.1) # move them .05 to the left and right
  
  plot <- ggplot2::ggplot(data = sum_nbr, aes(factor(name, level = time_order),y = value, colour = as.factor(defoliated))) +
    ggplot2::geom_line(aes(factor(name, level = time_order), 
                  y = value, colour = as.factor(defoliated)), position=pd) +
   ggplot2::geom_point(position=pd) +
    ggplot2::geom_errorbar(aes(ymin=value-ci, ymax=value+ci), width=.1, position=pd) +
    ggplot2::labs(y= " NBR Recovery Rate", x = "Time Period (pre-10yrs post)")  + 
    ggplot2::theme(axis.text.x=element_text(angle=60,hjust=1))+ 
    ggplot2::ggtitle("NBR recovery for 10 Years following fire") + 
    ggplot2::guides(colour=guide_legend(title="Defoliation Presence/Absent")) +
    ggplot2::theme_classic() 
  
  return(plot)
}





# Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

