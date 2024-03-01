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
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
rmHlm_prep <- function(data){
  
  data2 <- data |> 
    group_by(fire_name) |> 
    mutate(id_nest =cur_group_id()) 
  
  data2[(is.na(data2))] <- 0
  
 
  
  return(data2)
  
  
}



rmHlm_results <- function(mod, responseType){
  require(car)

  
  if(responseType == "Median"){
    mod <- Anova(mod, test = "F", type = "III")
  }else if(responseGroup == "Extreme"){
    mod <- Anova(mod, test = "F", type = "III")
  }else if(responseGroup == "CV"){
    mod <- Anova(mod, test = "F", type = "III")
  }else if(responseGroup == "s10"){
     mod <- Anova(mod, test = "F", type = "III")
  }else if(responseGroup == "s1"){
    mod <- Anova(mod, test = "F", type = "III")
  }else{
    mod <- Anova(mod, test = "F", type = "III")
  }
  
  # row names to column
  mod <- tibble::rownames_to_column(mod, "terms")
  
  # rename columns
  mod <- dplyr::rename(mod, `p-value` = `Pr(>F)`)
  
  # format p-value column
  mod$`p-value` <- format(mod$`p-value`,scientific = FALSE)


return(mod)  
  
}


