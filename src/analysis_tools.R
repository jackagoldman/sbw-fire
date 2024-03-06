# various tools for analysis



# calculate difference between severity defoliated to non-defoliated
#' This function computes the difference between burn severity and slope for paired fires and returns columns with 
#' amout difference and whether the overall trend was increasing or decreasing
#' @param data vis prep data frame 
#'
#' @return
#' @export
#'
#' @examples
change_direction <- function(data){
data1 <- data |> 
  arrange(fire_name, defoliated) |>  
  group_by(fire_name) |> 
  mutate(med_diff = (diff(rbr_median))) |> 
  mutate(ext_diff = (diff(rbr_extreme))) |> 
  mutate(cv_diff = (diff(rbr_cv))) |> 
  mutate(med_diff = (med_diff * -1)) |> 
  mutate(ext_diff = (ext_diff * -1)) |> 
  mutate(cv_diff = (cv_diff * -1)) |> 
  mutate(med_diff_fct = case_when(med_diff <0 ~ "Decreased",
                                  med_diff >0 ~ "Increased")) |> 
  mutate(ext_diff_fct = case_when(ext_diff <0 ~ "Decreased",
                                  ext_diff >0 ~ "Increased")) |> 
  mutate(cv_diff_fct = case_when(cv_diff <0 ~ "Decreased",
                                 cv_diff >0 ~ "Increased")) |> 
  mutate(s10_diff = (diff(sens10))) |> 
  mutate(s1_diff = (diff(sens1))) |> 
  mutate(s2_diff = (diff(sens2))) |> 
  mutate(s10_diff = (s10_diff * -1)) |> 
  mutate(s1_diff = (s1_diff * -1)) |> 
  mutate(s2_diff = (s2_diff * -1)) |>
  mutate(s10_diff_fct = case_when(s10_diff <0 ~ "Decreased",
                                  s10_diff >0 ~ "Increased")) |> 
  mutate(s1_diff_fct = case_when(s1_diff <0 ~ "Decreased",
                                 s1_diff >0 ~ "Increased")) |> 
  mutate(s2_diff_fct = case_when(s2_diff <0 ~ "Decreased",
                                 s2_diff >0 ~ "Increased"))

return(data1)
}


#' Creates chisq table
#' This function is used in the chisq analysis and creates a table of all chisq test results
#'
#' @param chisqRes results from chisq analysis
#' @param responseGroup response group, one of median, extreme, cv, s10, s1, s2
#'
#' @return
#' @export
#'
#' @examples
chisq_table <- function(chisqRes, responseGroup){
 
  x <- chisqRes[[1]]
  df <- chisqRes[[2]]
  p <- chisqRes[[3]]
  
  if(responseGroup == "Median"){
    name <- "Median Severity"
  }else if(responseGroup == "Extreme"){
    name <- "Extreme Severity"
  }else if(responseGroup == "CV"){
    name <- "Varibility in Severity"
  }else if(responseGroup == "s10"){
    name <- "10 yr slope of recovery"
  }else if(responseGroup == "s1"){
    name <- "1-5 yr slope of recovery"
  }else{
    name <- "6-10 yr slope of recovery"
    }
  
  
  #make df
  cols <- c("Independent Variable of Interest", "x\u00B2", "df", "P-value")
  table <- cbind(name, x, df, p)
  table <- as.data.frame(table)
  colnames(table) <- cols
  rownames(table) <- NULL
  table$`x²` <- as.numeric(table$`x²`)
  table$df <- as.numeric(table$df)
  table$`P-value` <- as.numeric(table$`P-value`)
  table <- dplyr::mutate(table, `x²` = round(`x²`, 3))
  table <- dplyr::mutate(table, `P-value` = round(`P-value`, 3))
  
  
  
  
  return(table)
  
  
}


#' ouput results from chisq test
#'
#' @param chisqTable 
#' @param RES_DIR 
#'
#' @return
#' @export
#'
#' @examples
output_chisq <- function(chisqTable, RES_DIR){
  
  # build outputpath
  pathChisq <- paste0(RES_DIR, "chisq_results.csv")

  # output dataframes
  write.csv(chisqTable, pathChisq)
  
  
}

#' output results from ttest
#'
#' @param sevTtest 
#' @param slopeTtest 
#' @param RES_DIR 
#'
#' @return
#' @export
#'
#' @examples
output_ttest <- function(sevTtest, slopeTtest, RES_DIR){
  
  # build outputpath
  pathTtest_sev <- paste0(RES_DIR, "severity_ttest_results.csv")
  pathTtest_slope <- paste0(RES_DIR, "slope_ttest_results.csv")
  
  
  # output dataframes
  write.csv(sevTtest, pathTtest_sev)
  write.csv(slopeTtest, pathTtest_slope)
  
  
}



#' Prep data for repeated measures hierachical linear model
#'
#' @param data dataframe. from the vis_prep function. 
#'
#' @return cleaned data frame with unique id for each fire_name combo and all NA's = 0.
#'
#' @examples
rmHlm_prep <- function(data){
  
  data2 <- data |> 
    group_by(fire_name) |> 
    mutate(id_nest =cur_group_id()) 
  
  data2[(is.na(data2))] <- 0
  
 
  
  return(data2)
  
  
}



#' Get results from repeated measures hierarchical linear model
#' This function creates tables from repeated measures hierarchical linear model
#'
#' @param mod lme4 dataframe: lmer model
#' @param responseType character. Response variable of interest
#'
#' @return dataframe consisting of results (terms, F, DF, df.res, p-value)
#'
#' @examples
rmHlm_results <- function(mod, responseType, modelType){
  require(car)
  require(broom.mixed)


  if(modelType == "lme4"){
  
  if(responseType == "Median"){
    mod <- Anova(mod, test = "F", type = "III")
  }else if(responseType == "Extreme"){
    mod <- Anova(mod, test = "F", type = "III")
  }else if(responseType == "CV"){
    mod <- Anova(mod, test = "F", type = "III")
  }else if(responseType == "s10"){
     mod <- Anova(mod, test = "F", type = "III")
  }else if(responseType == "s1"){
    mod <- Anova(mod, test = "F", type = "III")
  }else if(responseType == "s2"){
    mod <- Anova(mod, test = "F", type = "III")
  }
  
  # row names to column
  mod <- tibble::rownames_to_column(mod, "terms")
  
  # rename columns
  mod <- dplyr::rename(mod, `p-value` = `Pr(>F)`)
  
  # format p-value column
  mod$`p-value` <- format(mod$`p-value`,scientific = FALSE)
  
  # character
  mod$`p-value` <- as.numeric(mod$`p-value`)
  
  # round p value, f and df
  mod <- dplyr::mutate(mod, `p-value` = round(`p-value`, 3))
  mod <- dplyr::mutate(mod, `F` = round(`F`, 3))
  mod <- dplyr::mutate(mod, Df.res = round(Df.res, 3))

  }else if(modelType == "nlme"){
    
    if(responseType == "Median"){
      mod <- broom.mixed::tidy(mod)
      
    }else if(responseType == "Extreme"){
      mod <- broom.mixed::tidy(mod)
    }else if(responseType == "CV"){
      mod <- broom.mixed::tidy(mod)
    }else if(responseType == "s10"){
      mod <- broom.mixed::tidy(mod)
    }else if(responseType == "s1"){
      mod <- broom.mixed::tidy(mod)
    }else if(responseType == "s2"){
      mod <- broom.mixed::tidy(mod)
    }
    
    mod <- mod |> 
      select(-c(effect, group)) |> 
      slice(1:4)
    
    mod$p.value <- format(mod$p.value,scientific = FALSE)
    mod$p.value <- as.numeric(mod$p.value)
    mod<- dplyr::mutate(mod, p.value = round(p.value, 3))
  }

return(mod)  
  
}

#' save results from repeated measures hlm 
#'
#' @param modList list. results from rh_hlm function
#' @param RES_DIR path to file directory
#' 
#' @return csv written to results fold
#'
#' @examples
output_rmHlm <- function(modList, RES_DIR){
  
  # get results as dataframes
  med <- as.data.frame(modList[1])
  ext <- as.data.frame(modList[2])
  cv <- as.data.frame(modList[3])
  s10 <- as.data.frame(modList[4])
  s1 <- as.data.frame(modList[5])
  s2 <- as.data.frame(modList[6])
  
  # build outputpath
  pathRmHlm_med <- paste0(RES_DIR, "rmhlm_med_results.csv")
  pathRmHlm_ext <- paste0(RES_DIR, "rmhlm_ext_results.csv")
  pathRmHlm_cv <- paste0(RES_DIR, "rmhlm_cv_results.csv")
  pathRmHlm_s10 <- paste0(RES_DIR, "rmhlm_s10_results.csv")
  pathRmHlm_s1 <- paste0(RES_DIR, "rmhlm_s1_results.csv")
  pathRmHlm_s2 <- paste0(RES_DIR, "rmhlm_s2_results.csv")
  
  # output dataframes
  write.csv(med, pathRmHlm_med)
  write.csv(ext, pathRmHlm_ext)
  write.csv(cv, pathRmHlm_cv)
  write.csv(s10, pathRmHlm_s10)
  write.csv(s1, pathRmHlm_s1)
  write.csv(s2, pathRmHlm_s2)
}




#' Calculates synthetic of defoliation severity
#'
#' @param data 
#' @param responseType 
#'
#' @return
#' @export
#'
#' @examples
synthetic_index <- function(data, responseType){
  
  
  if(responseType == "Median"){
  
  comp_model <- lm(rbr_median ~ tsd + cumltve_yrs, data = data)
  
  beta_td <- summary(comp_model)$coefficients[2, 1]

  beta_years <- summary(comp_model)$coefficients[3, 1]
  
  composite <- beta_td * data$tsd + beta_years * data$cumltve_yrs
  
  return(composite)
  
  }else if(responseType == "Extreme"){
    
    comp_model <- lm(rbr_extreme ~ tsd + cumltve_yrs, data = data)
    
    beta_td <- summary(comp_model)$coefficients[2, 1]
    
    beta_years <- summary(comp_model)$coefficients[3, 1]
    
    composite <- beta_td * data$tsd + beta_years * data$cumltve_yrs
    
    return(composite)
    
  }else if(responseType == "CV"){
    
    comp_model <- lm(rbr_cv ~ tsd + cumltve_yrs, data = data)
    
    beta_td <- summary(comp_model)$coefficients[2, 1]
    
    beta_years <- summary(comp_model)$coefficients[3, 1]
    
    composite <- beta_td * data$tsd + beta_years * data$cumltve_yrs
    
    return(composite)
    
  }else if(responseType == "s10"){
    
    comp_model <- lm(sens10 ~ tsd + cumltve_yrs, data = data)
    
    beta_td <- summary(comp_model)$coefficients[2, 1]
    
    beta_years <- summary(comp_model)$coefficients[3, 1]
    
    composite <- beta_td * data$tsd + beta_years * data$cumltve_yrs
    
    return(composite)
    
  }else if(responseType == "s1"){
    
    comp_model <- lm(sens1 ~ tsd + cumltve_yrs, data = data)
    
    beta_td <- summary(comp_model)$coefficients[2, 1]
    
    beta_years <- summary(comp_model)$coefficients[3, 1]
    
    composite <- beta_td * data$tsd + beta_years * data$cumltve_yrs
    
    return(composite)
    
  }else if(responseType == "s2"){
    
    comp_model <- lm(sens2 ~ tsd + cumltve_yrs, data = data)
    
    beta_td <- summary(comp_model)$coefficients[2, 1]
    
    beta_years <- summary(comp_model)$coefficients[3, 1]
    
    composite <- beta_td * data$tsd + beta_years * data$cumltve_yrs
    
    return(composite)
    
  }
  
  
}
