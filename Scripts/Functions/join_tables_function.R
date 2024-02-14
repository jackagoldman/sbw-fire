#' Join tables function takes up to 5 tables based on the schema of the paired defoliation x fire project
#' and return a complete table that is structured to fit a given analysis.
#'
#' @param key dataframe must contain ID column 
#' @param df2 dataframe must contain ID column 
#' @param df3 dataframe must contain ID column 
#' @param df4 dataframe must contain ID column 
#' @param df5 dataframe must contain ID column 
#' @param method One of pairwise or regression. Structures dataframe according to analysis type
#'
#' @return large data table joined based on ID column
#' @export
#'
#' @examples join_tables(df_key, defol_table, severity_table, method = "pairwise")
#' 
join_tables <- function(key, df2, df3, df4, df5, method) {
  
  #load required packages
  require(dplyr)
  require(tidyverse)
    
    # check to see if the key is provided, if not return error
    if(missing(key)){
      stop("Please provide a key")
    }
    
    # check to see if a table is provided to join to the key. if not return error
    if(missing(df2)){
      stop("Missing table to join to key")
    }
    
    # check to see if there is an X column in the table. Sometimes this happens using csv. If so, remove and 
    # join to key. If no X column, join to key.
    if("X" %in% colnames(df2)){
      
      #remove X column
      df2 <- dplyr::select(-c(X))
      
      #join table table based on ID and fire name
      data_table <- dplyr::left_join(key, df2 , by = c("id", "fire_name"))
      
      #return data table
      return(data_table)
      
    }else{
      #join table table based on ID and fire name
      data_table <- dplyr::left_join(key, df2 , by = c("id", "fire_name"))
      
      #return data table
      return(data_table)
    }
    
    # check to see if there is an second table supplied. if missing, print message
    if(missing(df3)){
      message("Only one table supplied, joining key to one table")
    
      # check to see if x column.
    }else if("X" %in% colnames(df3)){
      
      df3 <- dplyr::select(-c(X))
      
      data_table <-  left_join(data_table, df3, by = c("id", "fire_name"))
      
      return(data_table)
      
      message("joined two tables to key")
    
      }else{
      
      data_table <-  left_join(data_table, df3, by = c("id", "fire_name"))
      
      return(data_table)
      
      message("joined two tables to key")
    }
    
    
    # check to see if there is an third table supplied. 
    if(missing(df4)){

      # check to see if x column.
    }else if("X" %in% colnames(df4)){
      
      df4 <- dplyr::select(-c(X))
      
      data_table <-  left_join(data_table, df4, by = c("id", "fire_name"))
      
      return(data_table)
      
      message("joined three tables to key")
      
    }else{
      
      data_table <-  left_join(data_table, df4, by = c("id", "fire_name"))
      
      return(data_table)
      
      message("joined three tables to key")
    }
    
    # check to see if there is an fourth table supplied. 
    if(missing(df5)){
      
      # check to see if x column.
    }else if("X" %in% colnames(df5)){
      
      df5 <- dplyr::select(-c(X))
      
      data_table <-  left_join(data_table, df5, by = c("id", "fire_name"))
      
      return(data_table)
      
      message("joined four tables to key")
      
    }else{
      
      data_table <-  left_join(data_table, df5, by = c("id", "fire_name"))
      
      return(data_table)
      
      message("joined four tables to key")
    }
    return(data_table)
    
    
  #specify the method of analysis
  #if(method == "pairwise"){
    
    #pivot wider by defoliation
    #data_table <-  tidyr::pivot_wider()
    
    
    #}
    
    
 # }else if(method == "regression"){
    
    
    
    
    
    
    
  #}
  
}
