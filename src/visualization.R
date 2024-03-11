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
  require(dplyr)
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
#' @param df dataframe returned from vis_prep function
#' @param resSlope
#' @param resSev
#'
#' @return
#' @export
#'
#' @examples
recovery_visBox <- function(df, resSlope, resSev){
  require(ggplot2)
  
  # defol plot med
  sigVals_med <- createTib(resSev, "Median")
  defolPlot_med <- ggplot(df, aes(x = defoliated, y = rbr_median))+
    geom_boxplot() +
    labs(x = "Median Severity", y = "Presence/Absence", title = "Defoliation Presence/Absence") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) 
  
  # get max mean, lwr and upper ci from boxplot to get the location for where to put the annotation
  limits <- layer_scales(defolPlot_med)$y$get_limits()[[2]] + 2
  
  # set location for comparison
  sigVals_med <- dplyr::mutate(sigVals_med, y.position = c(limits)) # this needs a position on graph
  
  # annotate plot
  defolPlot_med <- defolPlot_med + ggpubr::stat_pvalue_manual(sigVals_med, label = "p")
  
  
  # defol plot ext
  sigVals_ext <- createTib(resSev, "Extreme")
  defolPlot_ext <- ggplot(df, aes(x = defoliated, y = rbr_extreme))+
    geom_boxplot() +
    labs(x = "Extreme Severity", y = "Presence/Absence", title = "Defoliation Presence/Absence") +
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5)) 
  
  # get max mean, lwr and upper ci from boxplot to get the location for where to put the annotation
  limits <- layer_scales(defolPlot_ext)$y$get_limits()[[2]] + 2
  
  # set location for comparison
  sigVals_ext <- dplyr::mutate(sigVals_ext, y.position = c(limits)) # this needs a position on graph
  
  # annotate plot
  defolPlot_ext <- defolPlot_ext + ggpubr::stat_pvalue_manual(sigVals_ext, label = "p")
  
  
  # defol plot variability
  sigVals_cv <- createTib(resSev, "Variability")
  defolPlot_cv <- ggplot(df, aes(x = defoliated, y = rbr_cv))+
    geom_boxplot() +
    labs(x = "Variability in burn Severity", y = "Presence/Absence", title = "Defoliation Presence/Absence") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) 
  
  # get max mean, lwr and upper ci from boxplot to get the location for where to put the annotation
  limits <- layer_scales(defolPlot_cv)$y$get_limits()[[2]] + 2
  
  # set location for comparison
  sigVals_cv <- dplyr::mutate(sigVals_cv, y.position = c(limits)) # this needs a position on graph
  
  # annotate plot
  defolPlot_cv <- defolPlot_cv + ggpubr::stat_pvalue_manual(sigVals_cv, label = "p")
  
  
  # defol plot sens 10
  sigVals_s10 <- createTib(resSlope, "all")
  defolPlot_sens10 <- ggplot(df, aes(x =defoliated, y = sens10))+
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
  defolPlot_sens1 <- ggplot(df, aes(x = defoliated, y = sens1))+
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

  defolPlot_sens2 <- ggplot(df, aes(x = defoliated, y = sens2))+
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
trend_plot <- function(ts_data){
  
  
  recTs_1 <- ts_data |> 
    tidyr::pivot_longer(-c(defoliated, fire_name)) |> 
    dplyr::filter(defoliated == "1") 
  
  
  recTs_0 <- ts_data |>
    tidyr::pivot_longer(-c(defoliated, fire_name)) |> 
    dplyr::filter(defoliated == "0")
 
  
  sum1 <- summarySE(recTs_1, measurevar="value", groupvars=c("name")) |> 
    dplyr::mutate(defoliated = 1) |> 
    dplyr::mutate(ordr = dplyr::case_when(name == "nbr1" ~ 2,
                                    name == "preNBR" ~ 1,
                                    name == "nbr2" ~ 3, 
                                    name == "nbr3" ~ 4,
                                    name == "nbr4" ~ 5,
                                    name == "nbr5" ~ 6,
                                    name == "nbr6" ~ 7, 
                                    name == "nbr7" ~ 8,
                                    name == "nbr8" ~ 9,
                                    name == "nbr9" ~ 10, 
                                    name == "nbr10" ~ 11)) |> 
    arrange(ordr) |> 
    dplyr::mutate(name, levels = name) |> 
    dplyr::select(-c(ordr, levels))

  sum0 <- summarySE(recTs_0, measurevar="value", groupvars=c("name")) |> 
    dplyr::mutate(defoliated = 0)|> 
    dplyr::mutate(ordr = dplyr::case_when(name == "nbr1" ~ 2,
                                          name == "preNBR" ~ 1,
                                          name == "nbr2" ~ 3, 
                                          name == "nbr3" ~ 4,
                                          name == "nbr4" ~ 5,
                                          name == "nbr5" ~ 6,
                                          name == "nbr6" ~ 7, 
                                          name == "nbr7" ~ 8,
                                          name == "nbr8" ~ 9,
                                          name == "nbr9" ~ 10, 
                                          name == "nbr10" ~ 11)) |> 
    arrange(ordr) |> 
    dplyr::mutate(name, levels = name) |> 
    dplyr::select(-c(ordr, levels))

  
  sum_nbr <- rbind(sum1, sum0)
  pd <- ggplot2::position_dodge(0.1) # move them .05 to the left and right
  

require(ggplot2)
  
  plot <- ggplot(sum_nbr,aes(factor(name, level = time_order), 
        y = value, colour = as.factor(defoliated))) + 
    geom_point(position=pd) +
    geom_errorbar(aes(ymin=value-ci, ymax=value+ci), width=.1, position=pd) +
    labs(y= " NBR Recovery Rate", x = "Time Period (pre-10yrs post)")  + 
    theme(axis.text.x=element_text(angle=60,hjust=1))+ ggtitle("NBR recovery for 10 Years following fire") + 
    guides(colour=guide_legend(title="Defoliation Presence/Absent")) +
    theme_classic()
  
  return(plot)
}


#' lollipop charts
#'
#' @param data dataframe returned from vis_prep function
#' @param responseType
#' @return
#' @export
#'
#' @examples
lollidot <- function(data, responseType, slopeType){
  
  require(ggplot2)
  data <- dplyr::arrange(data, fire_name)
  
  # move geom_point to occur aftetr geom_line so the line is covered by the point
  if(slopeType == "lm"){
    data1 <- change_direction(data, "lm" )
  } else if(slopeType == "sens"){
    data1 <- change_direction(data, "sens" )
    
  }
  
  
  
  # calculate the n for increases and decreases for each response
  n_dir <- n_direction(data1)
    
    
  # arrange data by fire name
  data1 <- dplyr::arrange(data1, fire_name)
  
  # level order
  level_order <- c('Non-Defoliated', 'Defoliated') 
  
  
  # median
 med_plot <-  ggplot(data1, aes(x = factor(defoliated, level = level_order), y = rbr_median)) + 
    geom_line(aes(group = fire_name, color = med_diff_fct)) +
    geom_point(aes(color = defoliated),size = 3, show.legend = FALSE)+
    scale_color_manual(breaks = c("Increased", "Decreased"), values = c("Defoliated" = "#E69F00", 
                                  "Non-Defoliated" = "#D55E00", 
                                  "Decreased" = "#CC79A7",
                                  "Increased" = "#009E73"))+
    guides(color=guide_legend(title="Change in\nBurn Severity"))+
    ylab("Median Severity") +
    xlab("Paired Defoliated/Non-Defolaited Fires") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    theme_bw()
 
 tgrob <- txtgrob_annttion(n_dir, "med")
 
 
 med_plot1 <-  med_plot + annotation_custom(tgrob, xmin = 0.1, xmax =1.5,  ymin = 350, ymax = 400)
  
 med_plot1
 
  # extreme
  ext_plot <- ggplot(data1, aes(x = factor(defoliated, level = level_order), y = rbr_extreme)) + 
    geom_line(aes(group = fire_name, color = ext_diff_fct)) +
    geom_point(aes(color = defoliated),size = 3, show.legend = FALSE)+
    scale_color_manual(breaks = c("Increased", "Decreased"), values = c("Defoliated" = "#E69F00", 
                                                                                          "Non-Defoliated" = "#D55E00", 
                                                                                          "Decreased" = "#CC79A7",
                                                                                          "Increased" = "#009E73"))+
    guides(color=guide_legend(title="Change in\nBurn Severity"))+
    ylab("Severity Extremes") +
    xlab("Paired Defoliated/Non-Defolaited Fires") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10),  limits = c(55,650)) +
    theme_bw()
  
  tgrob <- txtgrob_annttion(n_dir, "ext")
  
  
  ext_plot1 <-  ext_plot + annotation_custom(tgrob, xmin = 0.1, xmax =1.5,  ymin = 600, ymax = 650)
  
  ext_plot1
  
  # cv
  cv_plot <- ggplot(data1, aes(x = factor(defoliated, level = level_order), y = rbr_cv)) + 
    geom_line(aes(group = fire_name, color = cv_diff_fct)) +
    geom_point(aes(color = defoliated),size = 3, show.legend = FALSE)+
    scale_color_manual(breaks = c("Increased", "Decreased"), values = c("Defoliated" = "#E69F00", 
                                                                                          "Non-Defoliated" = "#D55E00", 
                                                                                          "Decreased" = "#CC79A7",
                                                                                          "Increased" = "#009E73"))+
    guides(color=guide_legend(title="Change in\nBurn Severity"))+
    ylab("Variability in Severity") +
    xlab("Paired Defoliated/Non-Defolaited Fires") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(-3.5, 2.5)) +
    theme_bw()
  
  tgrob <- txtgrob_annttion(n_dir, "cv")
  
  
  cv_plot1 <-  cv_plot + annotation_custom(tgrob, xmin = 0.1, xmax =1.5,  ymin = 2, ymax = 2.5)
  
  cv_plot1 

  
  # slope
  if(slopeType == "lm"){
    s10 <- "slope10"
    s1 <- "slope1"
    s2 <- "slope2"
  }else if(slopeType == "sens"){
    s10 <- "sens10"
    s1 <- "sens1"
    s2 <- "sens2"
  }
  s10_plot <- ggplot(data1, aes(x = factor(defoliated, level = level_order), y = s10)) + 
    geom_line(aes(group = fire_name, color = s10_diff_fct)) +
    geom_point(aes(color = defoliated),size = 3, show.legend = FALSE)+
    scale_color_manual(breaks = c("Increased", "Decreased"), values = c("Defoliated" = "#E69F00", 
                                                                        "Non-Defoliated" = "#D55E00", 
                                                                        "Decreased" = "#CC79A7",
                                                                        "Increased" = "#009E73"))+
    guides(color=guide_legend(title="Change in\nBurn Severity"))+
    ylab("Slope of Recovery 10yr post-fire") +
    xlab("Paired Defoliated/Non-Defolaited Fires") +
    ggtitle("Slope of Recovery Over 10 Years Post-Fire")+
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(-10, 20)) +
    theme_bw()
  
  
  tgrob <- txtgrob_annttion(n_dir, "sens10")
  
  
  s10_plot1 <-  s10_plot + annotation_custom(tgrob, xmin = 0.1, xmax =1.5,  ymin = 16, ymax = 19)
  
  s10_plot1
  
  # sens 1-5
  s1_plot <- ggplot(data1, aes(x = factor(defoliated, level = level_order), y = s1)) + 
    geom_line(aes(group = fire_name, color = s1_diff_fct)) +
    geom_point(aes(color = defoliated),size = 3, show.legend = FALSE)+
    scale_color_manual(breaks = c("Increased", "Decreased"), values = c("Defoliated" = "#E69F00", 
                                                                        "Non-Defoliated" = "#D55E00", 
                                                                        "Decreased" = "#CC79A7",
                                                                        "Increased" = "#009E73"))+
    guides(color=guide_legend(title="Change in\nBurn Severity"))+
    ylab("Slope of Recovery 1-5yr post-fire") +
    xlab("Paired Defoliated/Non-Defolaited Fires") +
    ggtitle("Slope of Recovery Over 5 Years Post-Fire")+
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(-25, 30)) +
    theme_bw()
  
  tgrob <- txtgrob_annttion(n_dir, "sens1")
  
  
  s1_plot1 <-  s1_plot + annotation_custom(tgrob, xmin = 0.1, xmax =1.5,  ymin = 25, ymax = 29.5)
  
  s1_plot1
  
  
  # sens 5-10
  s2_plot <- ggplot(data1, aes(x = factor(defoliated, level = level_order), y = s2)) + 
    geom_line(aes(group = fire_name, color = s1_diff_fct)) +
    geom_point(aes(color = defoliated),size = 3, show.legend = FALSE)+
    scale_color_manual(breaks = c("Increased", "Decreased"), values = c("Defoliated" = "#E69F00", 
                                                                        "Non-Defoliated" = "#D55E00", 
                                                                        "Decreased" = "#CC79A7",
                                                                        "Increased" = "#009E73"))+
    guides(color=guide_legend(title="Change in\nBurn Severity"))+
    ylab("Slope of Recovery 6-10yr post-fire") +
    xlab("Paired Defoliated/Non-Defolaited Fires") +
    ggtitle("Slope of Recovery between 5-10 Years Post-Fire")+
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(-35, 50)) +
    theme_bw()
  
  tgrob <- txtgrob_annttion(n_dir, "sens1")
  
  
  s2_plot1 <-  s2_plot + annotation_custom(tgrob, xmin = 0.1, xmax =1.5,  ymin = 42, ymax = 44)
  
  s2_plot1
  
  # arrange plots 
  if(responseType == "severity"){
    plot <- ggpubr::ggarrange(med_plot1, ext_plot1, cv_plot1,
                              ncol = 1, nrow =3, common.legend = TRUE, legend = "bottom")    
   

    return(plot)
    
  }else if(responseType == "slope"){
    
    plot <- ggpubr::ggarrange(s10_plot1, s1_plot1, s2_plot1,
                              ncol = 1, nrow =3, common.legend = TRUE, legend = "bottom")
    return(plot)
    
  }
  
  
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



#' Summarizes sample size.
#' gets the sample size for the direction of the change in non-defoliated vs. defolaited
#'
#' @param data 
#'
#' @return dataframe with columns for direction, sample size (n) and response variable.
#' @export
#'
#' @examples
n_direction <- function(data){
  
  
  n_s10 <-  data |> 
    group_by(s10_diff_fct) |> 
    filter(defoliated == "Defoliated") |> 
    mutate(pos = sum(s10_diff > 0),
           neg = sum(s10_diff <0)) |> 
    select(c(pos, neg)) |> 
    distinct() |> 
    rowwise() |> 
    mutate(dir = case_when(s10_diff_fct == "Decreased"~ sum(c_across(pos:neg), na.rm = TRUE),
                           s10_diff_fct == "Increased"~ sum(c_across(pos:neg), na.rm = TRUE))) |> 
    select(-c(pos, neg)) |> 
    rename(n = dir) |> 
    rename(direction = s10_diff_fct) |> 
    mutate(var = rep("sens10"))
  
  n_s1 <-  data |> 
    group_by(s1_diff_fct) |> 
    filter(defoliated == "Defoliated") |> 
    mutate(pos = sum(s1_diff > 0),
           neg = sum(s1_diff <0)) |> 
    select(c(pos, neg)) |> 
    distinct() |> 
    rowwise() |> 
    mutate(dir = case_when(s1_diff_fct == "Decreased"~ sum(c_across(pos:neg), na.rm = TRUE),
                           s1_diff_fct == "Increased"~ sum(c_across(pos:neg), na.rm = TRUE))) |> 
    select(-c(pos, neg)) |> 
    rename(n = dir) |> 
    rename(direction = s1_diff_fct)|> 
    mutate(var = rep("sens1"))
  
  n_s2 <-  data |> 
    group_by(s2_diff_fct) |> 
    filter(defoliated == "Defoliated") |> 
    mutate(pos = sum(s2_diff > 0),
           neg = sum(s2_diff <0)) |> 
    select(c(pos, neg)) |> 
    distinct() |> 
    rowwise() |> 
    mutate(dir = case_when(s2_diff_fct == "Decreased"~ sum(c_across(pos:neg), na.rm = TRUE),
                           s2_diff_fct == "Increased"~ sum(c_across(pos:neg), na.rm = TRUE))) |> 
    select(-c(pos, neg)) |> 
    rename(n = dir) |> 
    rename(direction = s2_diff_fct)|> 
    mutate(var = rep("sens2"))
  
  n_med <-  data |> 
    group_by(med_diff_fct) |> 
    filter(defoliated == "Defoliated") |> 
    mutate(pos = sum(med_diff > 0),
           neg = sum(med_diff <0)) |> 
    select(c(pos, neg)) |> 
    distinct() |> 
    rowwise() |> 
    mutate(dir = case_when(med_diff_fct == "Decreased"~ sum(c_across(pos:neg), na.rm = TRUE),
                           med_diff_fct == "Increased"~ sum(c_across(pos:neg), na.rm = TRUE))) |> 
    select(-c(pos, neg)) |> 
    rename(n = dir) |> 
    rename(direction = med_diff_fct)|> 
    mutate(var = rep("med"))
  
  n_ext <-  data |> 
    group_by(ext_diff_fct) |> 
    filter(defoliated == "Defoliated") |> 
    mutate(pos = sum(ext_diff > 0),
           neg = sum(ext_diff <0)) |> 
    select(c(pos, neg)) |> 
    distinct() |> 
    rowwise() |> 
    mutate(dir = case_when(ext_diff_fct == "Decreased"~ sum(c_across(pos:neg), na.rm = TRUE),
                           ext_diff_fct == "Increased"~ sum(c_across(pos:neg), na.rm = TRUE))) |> 
    select(-c(pos, neg)) |> 
    rename(n = dir) |> 
    rename(direction = ext_diff_fct)|> 
    mutate(var = rep("ext"))
  
  n_cv <-  data |> 
    group_by(cv_diff_fct) |> 
    filter(defoliated == "Defoliated") |> 
    mutate(pos = sum(cv_diff > 0),
           neg = sum(cv_diff <0)) |> 
    select(c(pos, neg)) |> 
    distinct() |> 
    rowwise() |> 
    mutate(dir = case_when(cv_diff_fct == "Decreased"~ sum(c_across(pos:neg), na.rm = TRUE),
                           cv_diff_fct == "Increased"~ sum(c_across(pos:neg), na.rm = TRUE))) |> 
    select(-c(pos, neg)) |> 
    rename(n = dir) |> 
    rename(direction = cv_diff_fct)|> 
    mutate(var = rep("cv"))
  
  df <- rbind(n_med, n_ext, n_cv, n_s10, n_s1, n_s2)
  
  return(df)
  
}


#' Prepare textgrob annotation
#'
#' @param n_dir 
#' @param var 
#'
#' @return
#' @export
#'
#' @examples
txtgrob_annttion <- function(n_dir, var) {
  
  if(var == "med"){
  
  n_med <- n_dir[n_dir$var == "med",] 
  
  n_med <- n_med |> select(-c(var))
  
  text1 <- paste(n_med[1,1], n_med[1,2], sep = " = ")
  text2 <- paste(n_med[2,1], n_med[2,2], sep = " = ")
  text <- paste(text1, text2, sep = "\n")
  
  tgrob <- ggpubr::text_grob(text, size = 8, color = "black")
  
  return(tgrob)
  } else if(var == "ext"){
    
    n_med <- n_dir[n_dir$var == "ext",] 
    
    n_med <- n_med |> select(-c(var))
    
    text1 <- paste(n_med[1,1], n_med[1,2], sep = " = ")
    text2 <- paste(n_med[2,1], n_med[2,2], sep = " = ")
    text <- paste(text1, text2, sep = "\n")
    
    tgrob <- ggpubr::text_grob(text, size = 8, color = "black")
    
    return(tgrob)
    
  }else if(var == "cv"){
    n_med <- n_dir[n_dir$var == "cv",] 
    
    n_med <- n_med |> select(-c(var))
    
    text1 <- paste(n_med[1,1], n_med[1,2], sep = " = ")
    text2 <- paste(n_med[2,1], n_med[2,2], sep = " = ")
    text <- paste(text1, text2, sep = "\n")
    
    tgrob <- ggpubr::text_grob(text, size = 8,color = "black")
    return(tgrob)
    
    
  }else if(var == "sens10"){
    n_med <- n_dir[n_dir$var == "sens10",] 
    
    n_med <- n_med |> select(-c(var))
    
    text1 <- paste(n_med[1,1], n_med[1,2], sep = " = ")
    text2 <- paste(n_med[2,1], n_med[2,2], sep = " = ")
    text <- paste(text1, text2, sep = "\n")
    
    tgrob <- ggpubr::text_grob(text,size = 8, color = "black")
    return(tgrob)
    
    
  }else if(var == "sens1"){
    n_med <- n_dir[n_dir$var == "sens1",] 
    
    n_med <- n_med |> select(-c(var))
    
    text1 <- paste(n_med[1,1], n_med[1,2], sep = " = ")
    text2 <- paste(n_med[2,1], n_med[2,2], sep = " = ")
    text <- paste(text1, text2, sep = "\n")
    
    tgrob <- ggpubr::text_grob(text,size = 8, color = "black")
    return(tgrob)
    
    
  }else if(var == "sens2"){
    n_med <- n_dir[n_dir$var == "sens2",] 
    
    n_med <- n_med |> select(-c(var))
    
    text1 <- paste(n_med[1,1], n_med[1,2], sep = " = ")
    text2 <- paste(n_med[2,1], n_med[2,2], sep = " = ")
    text <- paste(text1, text2, sep = "\n")
    
    tgrob <- ggpubr::text_grob(text,size = 8, color = "black")
    return(tgrob)
    
  }else if(var == "slope10"){
    n_med <- n_dir[n_dir$var == "slope10",] 
    
    n_med <- n_med |> select(-c(var))
    
    text1 <- paste(n_med[1,1], n_med[1,2], sep = " = ")
    text2 <- paste(n_med[2,1], n_med[2,2], sep = " = ")
    text <- paste(text1, text2, sep = "\n")
    
    tgrob <- ggpubr::text_grob(text,size = 8, color = "black")
    return(tgrob)
    
    
  }else if(var == "slope1"){
    n_med <- n_dir[n_dir$var == "slope1",] 
    
    n_med <- n_med |> select(-c(var))
    
    text1 <- paste(n_med[1,1], n_med[1,2], sep = " = ")
    text2 <- paste(n_med[2,1], n_med[2,2], sep = " = ")
    text <- paste(text1, text2, sep = "\n")
    
    tgrob <- ggpubr::text_grob(text,size = 8, color = "black")
    return(tgrob)
    
    
  }else if(var == "slope2"){
    n_med <- n_dir[n_dir$var == "slope2",] 
    
    n_med <- n_med |> select(-c(var))
    
    text1 <- paste(n_med[1,1], n_med[1,2], sep = " = ")
    text2 <- paste(n_med[2,1], n_med[2,2], sep = " = ")
    text <- paste(text1, text2, sep = "\n")
    
    tgrob <- ggpubr::text_grob(text,size = 8, color = "black")
    return(tgrob)
    
  }
  
  return(tgrob)
  
  
  
}
