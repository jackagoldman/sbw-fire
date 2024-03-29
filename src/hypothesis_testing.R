## hypothesis testing



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
  res_median <- t.test(df$rbr_median ~ df$defoliated, paired = TRUE) 
  res_extreme <- t.test(df$rbr_extreme ~ df$defoliated, paired  = TRUE)
  res_cv <- t.test(df$rbr_cv ~ df$defoliated, paired = TRUE)
  
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
recovery_ttest <- function(df, slopeType){
  
  
  # make sure function does not use scientific notation 
  op <- options(scipen=999)
  on.exit(options(op), add = TRUE)
  
  if(slopeType == "sens"){
  
  # run paired t-test
  sens10 <- t.test(df$sens10 ~ df$defoliated, paired = TRUE) 
  sens1 <- t.test(df$sens1 ~ df$defoliated, paired = TRUE) 
  sens2 <- t.test(df$sens2 ~ df$defoliated, paired = TRUE) 
  
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
  
  }else if(slopeType == "lm"){
    
    # run paired t-test
    sens10 <- t.test(df$slope10 ~ df$defoliated, paired = TRUE) 
    sens1 <- t.test(df$slope1 ~ df$defoliated, paired = TRUE) 
    sens2 <- t.test(df$slope2 ~ df$defoliated, paired = TRUE) 
    
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
    
  }
  
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



#' chisq test for increase and decrease
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
chisq <- function(data, slopeType){
  
  if(slopeType == "lm"){
    # clean data for change_direction
    data2 <- change_direction(data, "lm")
  } else {
    # clean data for change_direction
    data2 <- change_direction(data, "sens")
  }
  
  
  
  # chisq
  med.chisq <- chisq.test(x = table(data2$med_diff_fct))
  ext.chisq <- chisq.test(x = table(data2$ext_diff_fct))
  cv.chisq <- chisq.test(x = table(data2$cv_diff_fct))
  s10.chisq <- chisq.test(x = table(data2$s10_diff_fct))
  s1.chisq <- chisq.test(x = table(data2$s1_diff_fct))
  s2.chisq <- chisq.test(x = table(data2$s2_diff_fct))
  
 # indiv table
  med <- chisq_table(med.chisq, "Median")
  ext <- chisq_table(ext.chisq, "Extreme")
  cv <- chisq_table(cv.chisq, "CV")
  s10 <- chisq_table(s10.chisq, "s10")
  s1 <- chisq_table(s1.chisq, "s1")
  s2 <- chisq_table(s2.chisq, "s2")
  
  # combine tables
  res.table <- rbind(med, ext, cv, s10, s1, s2)
  
  return(res.table)
}




#' Repeated Measures HLM
#' This function preforms are repeated measures hierachical linear model using the lme4 package
#' 
#'
#' @param data raw data frame over severity, slopes and defoliation
#'
#' @return list of dataframes 1 = median severity, 2 = extreme severity, 3 = variability in severity
#'          4 = slope 10 years, 5 = slope 1-5 years, 6 = slope 6-10 years
#' @export
#'
#' @examples
rm_hlm <- function(data){
  require(lme4)
  
  # clean data
  data <- rmHlm_prep(data)
  
  # models
  med.mod <- lmer(formula = rbr_median ~ defoliated*tsd*cumltve_yrs + (1 | id_nest),
                       data = data)
  ext.mod <- lmer(formula = rbr_extreme ~ defoliated*tsd*cumltve_yrs + (1 | id_nest),
                        data = data)
  cv.mod <- lmer(formula = rbr_cv ~ defoliated*tsd*cumltve_yrs + (1 | id_nest),
                        data = data)
  s10.mod <- lmer(formula = sens10 ~ defoliated*tsd*cumltve_yrs + (1 | id_nest),
                  data = data)
  s1.mod <- lmer(formula = sens1 ~ defoliated*tsd*cumltve_yrs + (1 | id_nest),
                  data = data)
  s2.mod <- lmer(formula = sens2 ~ defoliated*tsd*cumltve_yrs + (1 | id_nest),
                 data = data)
  # results
  med.res <- rmHlm_results(med.mod, "Median", "lme4")
  ext.res <- rmHlm_results(ext.mod, "Extreme", "lme4")
  cv.res <- rmHlm_results(cv.mod, "CV", "lme4")
  s10.res <- rmHlm_results(s10.mod, "s10", "lme4")
  s1.res <- rmHlm_results(s1.mod, "s1", "lme4")
  s2.res <- rmHlm_results(s2.mod, "s2", "lme4")
  
  df.list = list(med.res, ext.res, cv.res, s10.res, s1.res, s2.res)
  
  return(df.list)
  
}


#' Title
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
rmHlm_lme <- function(data, slopeType){
  require(nlme)
  # clean data
  data <- rmHlm_prep(data)
  
  if(slopeType == "sens"){
  # rbr median
  mod.med <- nlme::lme(rbr_median ~ tsd*cumltve_yrs  , random = ~1| id_nest, data = data)
  # rbr extreme
  mod.ext <- nlme::lme(rbr_extreme ~ tsd*cumltve_yrs  , random = ~ 1 | id_nest, data = data)
  #rbr cv 
  mod.cv <- nlme::lme(rbr_cv ~ tsd*cumltve_yrs  , random = ~ 1 | id_nest, data = data)
  #sens 10
  mod.s10 <- nlme::lme(sens10 ~ tsd*cumltve_yrs  , random = ~ 1 | id_nest, data = data)
  #sens 1
  mod.s1 <- nlme::lme(sens1 ~ tsd*cumltve_yrs  , random = ~ 1 | id_nest, data = data)
  #sens 2
  mod.s2 <- nlme::lme(sens2 ~ tsd*cumltve_yrs  , random = ~ 1 | id_nest, data = data)

  # results
  med.res <- rmHlm_results(mod.med, "Median", "nlme")
  ext.res <- rmHlm_results(mod.ext, "Extreme", "nlme")
  cv.res <- rmHlm_results(mod.cv, "CV", "nlme")
  s10.res <- rmHlm_results(mod.s10, "s10", "nlme")
  s1.res <- rmHlm_results(mod.s1, "s1", "nlme")
  s2.res <- rmHlm_results(mod.s2, "s2", "nlme")
  
  df.list = list(med.res, ext.res, cv.res, s10.res, s1.res, s2.res)
  
  }else if(slopeType == "lm"){
    
    # rbr median
    mod.med <- nlme::lme(rbr_median ~ tsd*cumltve_yrs  , random = ~1| id_nest, data = data)
    # rbr extreme
    mod.ext <- nlme::lme(rbr_extreme ~ tsd*cumltve_yrs  , random = ~ 1 | id_nest, data = data)
    #rbr cv 
    mod.cv <- nlme::lme(rbr_cv ~ tsd*cumltve_yrs  , random = ~ 1 | id_nest, data = data)
    #sens 10
    mod.s10 <- nlme::lme(slope10 ~ tsd*cumltve_yrs  , random = ~ 1 | id_nest, data = data)
    #sens 1
    mod.s1 <- nlme::lme(slope1 ~ tsd*cumltve_yrs  , random = ~ 1 | id_nest, data = data)
    #sens 2
    mod.s2 <- nlme::lme(slope2 ~ tsd*cumltve_yrs  , random = ~ 1 | id_nest, data = data)
    
    # results
    med.res <- rmHlm_results(mod.med, "Median", "nlme")
    ext.res <- rmHlm_results(mod.ext, "Extreme", "nlme")
    cv.res <- rmHlm_results(mod.cv, "CV", "nlme")
    s10.res <- rmHlm_results(mod.s10, "s10", "nlme")
    s1.res <- rmHlm_results(mod.s1, "s1", "nlme")
    s2.res <- rmHlm_results(mod.s2, "s2", "nlme")
    
    df.list = list(med.res, ext.res, cv.res, s10.res, s1.res, s2.res)
    
    
  }
  
  return(df.list)
}




#' Piecewise sem models
#'
#' @param data dataframe. from vis_prep function
#' @param responseType one of Median, Extreme, CV
#' @param slopeClass one of s1, s2, s10
#'
#' @return piecewise sem results
#' @export
#'
#' @examples
sem_mod <- function(data, responseType, slopeClass){
  
  require(piecewiseSEM)
  
  if(slopeClass == "s10"){
  
  if(responseType == "Median"){
  
    data$composite <- synthetic_index(data, "Median")
    
  sem <- piecewiseSEM::psem(
    lm(rbr_median ~ composite, data = data),
    lm(slope10 ~ rbr_median, data = data))
  }else if(responseType == "Extreme"){
    
    data$composite <- synthetic_index(data, "Extreme")
    
    
    sem <- piecewiseSEM::psem(
      lm(rbr_extreme ~ composite, data = data),
      lm(slope10 ~ rbr_extreme, data = data))
    
    
  }else if(responseType == "CV"){
    
    data$composite <- synthetic_index(data, "CV")
    
    
   sem <- piecewiseSEM::psem(
      lm(rbr_cv ~ composite, data = data),
      lm(slope10 ~ rbr_cv, data = data))
    
    
  }}else if(slopeClass == "s1"){
    
    if(responseType == "Median"){
      
      data$composite <- synthetic_index(data, "Median")
      
      sem <- piecewiseSEM::psem(
        lm(rbr_median ~ composite, data = data),
        lm(slope1 ~ rbr_median, data = data))
    }else if(responseType == "Extreme"){
      
      data$composite <- synthetic_index(data, "Extreme")
      
      
      sem <- piecewiseSEM::psem(
        lm(rbr_extreme ~ composite, data = data),
        lm(slope1 ~ rbr_extreme, data = data))
      
      
    }else if(responseType == "CV"){
      
      data$composite <- synthetic_index(data, "CV")
      
      
      sem <- piecewiseSEM::psem(
        lm(rbr_cv ~ composite, data = data),
        lm(slope1 ~ rbr_cv, data = data))
      
      
    }
    
    
    
  }else if(slopeClass == "s2"){
    
    if(responseType == "Median"){
      
      data$composite <- synthetic_index(data, "Median")
      
      sem <- piecewiseSEM::psem(
        lm(rbr_median ~ composite, data = data),
        lm(slope2 ~ rbr_median, data = data))
    }else if(responseType == "Extreme"){
      
      data$composite <- synthetic_index(data, "Extreme")
      
      
      sem <- piecewiseSEM::psem(
        lm(rbr_extreme ~ composite, data = data),
        lm(slope2 ~ rbr_extreme, data = data))
      
      
    }else if(responseType == "CV"){
      
      data$composite <- synthetic_index(data, "CV")
      
      
      sem <- piecewiseSEM::psem(
        lm(rbr_cv ~ composite, data = data),
        lm(slope2 ~ rbr_cv, data = data))
      
      
    }
    
    
    
  }
 
  return(sem)
  
  
  
}
