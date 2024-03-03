# report tools

#' make clean gt table 
#'
#' @param table results table from model
#' @param reponseType character. Response variable of model
#'
#' @return gt table.
#' 
#'
#' @examples
rmHlm_table <- function(table, reponseType){
  require(gt)
  # make sure dataframe is a tibble
  table <- as.tibble(table)
  
  
  
  if(responseType == "Median"){
    # get value to colour
    val <- colour_cell(table)
    
    #as gt
    gt.table <- gt(table)
    
    # add header
    gt.table <-  gt.table |> 
      tab_header(
        title = md("**Median Burn Severity**"),
        subtitle = "Results from repeated measures hierachical linear mixed model fit in nlme"
      ) |> 
        tab_style_body(
          style = cell_fill(color = "orange"),
          values = c(val)
        )
    
    
  }else if(responseType == "Extreme"){
    # get value to colour
    val <- colour_cell(table)
    
    #as gt
    gt.table <- gt(table)
    
    # add header
    gt.table <-  gt.table |> 
      tab_header(
        title = md("**Burn Severity Extremes**"),
        subtitle = "Results from repeated measures hierachical linear mixed model fit in nlme"
      )|> 
      tab_style_body(
        style = cell_fill(color = "orange"),
        values = c(val)
      )
    
  }else if(responseType == "CV"){
    # get value to colour
    val <- colour_cell(table)
    
    #as gt
    gt.table <- gt(table)
    
    # add header
    gt.table <-  gt.table |> 
      tab_header(
        title = md("**Variability in Burn Severity**"),
        subtitle = "Results from repeated measures hierachical linear mixed model fit in nlme"
      )|> 
      tab_style_body(
        style = cell_fill(color = "orange"),
        values = c(val)
      )
  }else if(responseType == "s10"){
    # get value to colour
    val <- colour_cell(table)
    
    #as gt
    gt.table <- gt(table)
    
    # add header
    gt.table <-  gt.table |> 
      tab_header(
        title = md("**Slope of Recovery 1-10 Years Post-Fire**"),
        subtitle = "Results from repeated measures hierachical linear mixed model fit in nlme"
      )|> 
      tab_style_body(
        style = cell_fill(color = "orange"),
        values = c(val)
      )
  }else if(responseType == "s1"){
    # get value to colour
    val <- colour_cell(table)
    
    #as gt
    gt.table <- gt(table)
    
    # add header
    gt.table <-  gt.table |> 
      tab_header(
        title = md("**Slope of Recovery 1-5 Years Post-Fire**"),
        subtitle = "Results from repeated measures hierachical linear mixed model fit in nlme"
      )|> 
      tab_style_body(
        style = cell_fill(color = "orange"),
        values = c(val)
      )
  }else if(responseType == "s2"){
    # get value to colour
    val <- colour_cell(table)
    
    #as gt
    gt.table <- gt(table)
    
    # add header
    gt.table <-  gt.table |> 
      tab_header(
        title = md("**Slope of Recovery 6-10 Years Post-Fire**"),
        subtitle = "Results from repeated measures hierachical linear mixed model fit in nlme"
      )|> 
      tab_style_body(
        style = cell_fill(color = "orange"),
        values = c(val)
      )
  }
  
  
  
  return(gt.table)
  
  
}



severity_table <- function(table){
  require(gt)
  
  #as gt
  gt.table <- gt(table)
  
  # add header
  gt.table <-  gt.table |> 
    tab_header(
      title = md("**Results from Pairwise T-test on severity between defoliated and non-deoliated**"),
    ) 
  return(gt.table)
}

slope_table <- function(table){
  require(gt)
  
  #as gt
  gt.table <- gt(table)
  
  # add header
  gt.table <-  gt.table |> 
    tab_header(
      title = md("**Results from Pairwise T-test on Slope of Recovery Between Defoliated and Non-Deoliated**"),
    ) 
  return(gt.table)
}


#' get value of cell to colour in gt tabe
#'
#' @param table rm hlm results table
#'
#' @return
#' @export
#'
#' @examples
colour_cell <- function(table){
  
  
    table1 <- table[table$p.value <0.05, ]
    
    table1 <- table1 |> 
      filter(!(term == "(Intercept)"))
    
    val <-table1$p.value
 
  return(val)
}
