## hypothesis testing


#' Join defoliation data and order for t-test
#'
#' @param df1 
#' @param df2 
#'
#' @return
#' @export
#'
#' @examples
order_data <- function(df1, df2){
  defol <- dplyr::select(df2, c("id", "defoliated"))
  df <- dplyr::left_join(df1,defol, by ="id") 
  df <- df[order(df$fire_name),]
  return(df)
}



#' Burn severity t-test function
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
severity_ttest <-  function(df){
  
  # make sure function does not use scientific notation 
  op <- options(scipen=999)
  on.exit(options(op), add = TRUE)
  # run paired t-test
  res_median <- t.test(df$defoliated, df$rbr_median, paired = TRUE) 
  res_extreme <- t.test(df$defoliated, df$rbr_extreme, paired  = TRUE)
  res_cv <- t.test(df$defoliated, df$rbr_cv, paired = TRUE)
  
  # extract values
  median_t <- res_median$statistic[[1]]
  median_p <- res_median$p.value
  median_df <- res_median$parameter[[1]]
  
  extreme_t <- res_extreme$statistic[[1]]
  extreme_p <- res_extreme$p.value
  extreme_df <- res_extreme$parameter[[1]]
  
  cv_t <- res_cv$statistic[[1]]
  cv_p <- res_cv$p.value
  cv_df <- res_cv$parameter[[1]]
  
  #make df
  cols <- c("Test", "t-value", "df", "P-value")
  median <- cbind("Median Severity", median_t, median_df, median_p)
  median <- as.data.frame(median)
  colnames(median) <- cols
  
  
  extreme <- cbind("Extreme Severity", extreme_t, extreme_df, extreme_p)
  extreme <- as.data.frame(extreme)
  colnames(extreme) <- cols
  
  
  cv <- cbind("Variability in Severity", cv_t, cv_df, cv_p)
  cv <- as.data.frame(cv)
  colnames(cv) <- cols
  
  # rbind
  results <- rbind(median, extreme, cv)
  
  # round the results
  results <- dplyr::mutate(results, `P-value` = as.numeric(`P-value`))
  results <- dplyr::mutate(results, `P-value` = formatC(as.numeric(`P-value`),
                                                        format = "f", digits = 4))
  
  results <- dplyr::mutate(results, `P-value` = case_when(`P-value` < 0.0001 ~ "<0.0001",
                                                          TRUE ~ formatC(as.numeric(`P-value`),
                                                                         format = "f", digits = 3)))
  return(results)
}


#' Sens slope t-test function
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
recovery_ttest <- function(df){
  
  # make sure function does not use scientific notation 
  op <- options(scipen=999)
  on.exit(options(op), add = TRUE)
  
  # run paired t-test
  sens10 <- t.test(df$defoliated, df$sens10, paired = TRUE) 
  sens1 <- t.test(df$defoliated, df$sens1, paired = TRUE) 
  sens2 <- t.test(df$defoliated, df$sens2, paired = TRUE) 
  
  # extract values
  s10_t <- sens10$statistic[[1]]
  s10_p <- sens10$p.value
  s10_df <- sens10$parameter[[1]]
  
  s1_t <- sens1$statistic[[1]]
  s1_p <- sens1$p.value
  s1_df <- sens1$parameter[[1]]
  
  s2_t <- sens2$statistic[[1]]
  s2_p <- sens2$p.value
  s2_df <- sens2$parameter[[1]]
  
  #make df
  cols <- c("Test", "t-value", "df", "P-value")
  sn10 <- cbind("10yr post-fire", s10_t, s10_df, as.numeric(s10_p))
  sn10 <- as.data.frame(sn10)
  colnames(sn10) <- cols
  
  
  sn1 <- cbind("1-5yr post-fire", s1_t, s1_df, as.numeric(s1_p))
  sn1 <- as.data.frame(sn1)
  colnames(sn1) <- cols
  
  
  sn2 <- cbind("6-10yr post-fire", s2_t, s2_df, as.numeric(s2_p))
  sn2 <- as.data.frame(sn2)
  colnames(sn2) <- cols
  
  # rbind
  results <- rbind(sn10, sn1, sn2)
  
  # round the results
  results <- dplyr::mutate(results, `P-value` = as.numeric(`P-value`))
  results <- dplyr::mutate(results, `P-value` = formatC(as.numeric(`P-value`),
                                                       format = "f", digits = 4))
  
  results <- dplyr::mutate(results, `P-value` = case_when(`P-value` < 0.0001 ~ "<0.0001",
                                                          TRUE ~ formatC(as.numeric(`P-value`),
                                                                         format = "f", digits = 3)))
  
  return(results)
  
  
}
