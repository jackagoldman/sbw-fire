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
#' @param resSev 
#' @param resSlope 
#'
#' @return
#' @export
#'
#' @examples
vis_ttestRes <- function(resSev, resSlope){
  
  emptyTib <- tibble(
    .y. = character(),
    group1 = character(),
    group2 = character(),
    n1 = integer(),
    n2 = integer(),
    statistic = double(),
    df = double(),
    p = double(),
    p.signif = character()
  )
  
  tibRes <- 
  
  
  
  
  
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