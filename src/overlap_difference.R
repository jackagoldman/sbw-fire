#' OVERLAP DIFFERENCE FUNCTION
#' 
#' This function gets the difference between the union for each fire (all area defoliated) and the full fire perimeter
#'
#' @param fr original fire perimeters. Dataframe must have a valid geometry column and a Fire_ID column
#' @param d_fr defoliated portion of the fire perimeters. Dataframe must have a valid geometry column and a Fire_ID column that matches fr Fire_ID column
#'
#' @return dataframe consisting only of fires that were partially defoliated. Dataframes have a fire_id and geometry column
#' 
#'
#' @examples func_test <- diff_to_defoliated_fire(fire, int_union)
#' 
#' 
#' 
overlap_difference <- function(fr, d_fr){
  require(sf)
  require(tidyverse)
  # create empty list
  non_defol_vec <- list()
  
  # loop through each fire in defoliated fires
  for (i in 1:nrow(d_fr)){
    
    #subset data
    row <- d_fr[i,]
    #get fire id
    fId <- row |> pull(Fire_ID)
    
    # filter fire
    defol_fr <- d_fr[d_fr$Fire_ID ==fId,]
    
    # filter original fire dataframe by fire ID
    og_fr <- fr[fr$Fire_ID ==fId,]
    
    #select only the rows required from each dataframe
    # original
    og_fr <- og_fr |> 
      select(c("Fire_ID", "geometry"))
    
    # transform geometry to planar from geodesic
    og_fr <- st_transform(og_fr, "+proj=eqc")
    defol_fr <- st_transform(defol_fr, "+proj=eqc")
    
    # calculate the difference between og_fr and defolaited area fr
    difference <- st_difference(og_fr, defol_fr) |> select(c(Fire_ID)) 
    
    # If the row in difference is not empty, put difference output into the list
    # if the row is empty, skip it and continue with loop
    if (dim(difference)[1] != 0) {
      # put into vector 
      non_defol_vec[[i]] <- difference
    }else if(dim(difference)[1] == 0){
      next
    }
    
  } 
  # rbind ouput list to dataframe
  vec <- do.call(rbind, non_defol_vec)
  # return dataframe
  return(vec)
}
