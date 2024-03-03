# report tools

rmHlm_table <- function(table, reponseType){
  require(gt)
  # make sure dataframe is a tibble
  table <- as.tibble(table)
  
  #as gt
  gt.table <- gt(table)
  
  if(responseType == "Median"){
    # add header
    gt.table <-  gt.table |> 
      tab_header(
        title = md("**Median Burn Severity**"),
        subtitle = "Results from repeated measures hierachical linear mixed model fit in nlme"
      )
    
    if(table$term == tsd & table$p.value < 0.05){
      
      val <-table[[2,6]]
      
      gt.table <- gt.table |> 
        tab_style_body(
          style = cell_fill(color = "orange"),
          values = c(val)
        )
      
      
    }
    
    if(table$term == "cumltve_yrs" & table$p.value < 0.05){
      
      val <-table[[3,6]]
      
      gt.table <- gt.table |> 
        tab_style_body(
          style = cell_fill(color = "orange"),
          values = c(val)
        )
      
      
    }
    
    if(table$term == "tsd:cumltve_yrs" & table$p.value < 0.05){
      
      val <-table[[4,6]]
      
      gt.table <- gt.table |> 
        tab_style_body(
          style = cell_fill(color = "orange"),
          values = c(val)
        )
      
      
    }
    
    
  }else if(responseType == "Extreme"){
    # add header
    gt.table <-  gt.table |> 
      tab_header(
        title = md("**Burn Severity Extremes**"),
        subtitle = "Results from repeated measures hierachical linear mixed model fit in nlme"
      )
  }else if(responseType == "CV"){
    # add header
    gt.table <-  gt.table |> 
      tab_header(
        title = md("**Variability in Burn Severity**"),
        subtitle = "Results from repeated measures hierachical linear mixed model fit in nlme"
      )
  }else if(responseType == "s10"){
    # add header
    gt.table <-  gt.table |> 
      tab_header(
        title = md("**Slope of Recovery 1-10 Years Post-Fire**"),
        subtitle = "Results from repeated measures hierachical linear mixed model fit in nlme"
      )
  }else if(responseType == "s1"){
    # add header
    gt.table <-  gt.table |> 
      tab_header(
        title = md("**Slope of Recovery 1-5 Years Post-Fire**"),
        subtitle = "Results from repeated measures hierachical linear mixed model fit in nlme"
      )
  }else if(responseType == "s2"){
    # add header
    gt.table <-  gt.table |> 
      tab_header(
        title = md("**Slope of Recovery 6-10 Years Post-Fire**"),
        subtitle = "Results from repeated measures hierachical linear mixed model fit in nlme"
      )
  }
  
  
  
  return(gt.table)
  
  
}