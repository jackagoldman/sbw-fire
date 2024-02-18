# visualization


#' prep data for visualization
#'
#' @param severity 
#' @param defoliation 
#' @param slope 
#'
#' @return
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
#' @param results 
#' @param nrowSub
#'
#' @return
#' @export
#'
#' @examples
createTib <- function(results, nrowSub){
  
  y <- "len"
  group1 <- "Defoliated"
  group2 <- "Non-Defoliated"
  n1 = 33
  n2 = 33
  df = 65
  
  if(nrowSub == "10yr"){
    nrow = 1
  } else if(nrowSub == "1-5yr"){
    nrow = 2
  } else if(nrowSub == "6-10yr"){
    nrow = 3
  } else if(nrowSub == "Median"){
    nrow = 1
  }else if(nrowSub == "Extreme"){
    nrow = 1
  } else if (nrowSub == "Variability"){
    nrow = 3
  }
  
 
    x <- dplyr::slice(res, nrow)
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
recovery_visBox <- function(df){
  
  
  defolPlot_med <-  ggplot2::ggplot(df, aes(x = defolaited, y = rbr_median))+
    geom_boxplot() +
    labs(x = "Median Severity", y = "Presence/Absence", title = "Defoliation Presence/Absence") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_bw()
  
  defolPlot_ext <-  ggplot2::ggplot(df, aes(x = defoliated, y = rbr_extreme))+
    geom_boxplot() +
    labs(x = "Extreme Severity", y = "Presence/Absence", title = "Defoliation Presence/Absence") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_bw()
  
  defolPlot_cv <-  ggplot2::ggplot(df, aes(x = defoliated, y = rbr_cv))+
    geom_boxplot() +
    labs(x = "Variability in burn Severity", y = "Presence/Absence", title = "Defoliation Presence/Absence") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_bw()
  
  defolPlot_sens10 <-  ggplot2::ggplot(df, aes(x =defoliated, y = sens10))+
    geom_boxplot() +
    labs(x = "10yr slope of recovery", y = "Presence/Absence", title = "Defoliation Presence/Absence") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_bw()
  
  defolPlot_sens1 <- ggplot2::ggplot(df, aes(x = defoliated, y = sens1))+
    geom_boxplot() +
    labs(x = "1-5yr slope of recovery", y = "Presence/Absence", title = "Defoliation Presence/Absence") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_bw()
  
  defolPlot_sens2 <- ggplot2::ggplot(df, aes(x = defoliated, y = sens2))+
    geom_boxplot() +
    labs(x = "6-10yr slope of recovery", y = "Presence/Absence", title = "Defoliation Presence/Absence") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_bw()
  
  
  
  plot <- ggpubr::ggarrange(defolPlot_med, defolPlot_ext, defolPlot_cv, 
                    defolPlot_sens10, defolPlot_sens1, defolPlot_sens2, ncol = 3, nrow =3)
  
  return(plot)
  
}