## hypothesis testing


order_data = function(df1, df2){
  
  defol <- dplyr::select(df2, c("id", "defoliated"))
  df <- dplyr::left_join(df1,defol, by ="id") 
  df <- df[order(df$fire_name),]
  return(df)
}


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
  results <- dplyr::mutate(results, `P-value` = case_when(`P-value` < 0.0001 ~ 0.0001,
                                                                   TRUE ~ round(as.numeric(results$`P-value`), digits = 3)))

  return(results)
}

recovery_ttest <- function(data){
  
  
  
  
  
  
}
