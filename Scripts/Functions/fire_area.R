#' fFire area calculation
#'
#' @param x spatial dataframe (sf object)
#'
#' @return spatial dataframe 
#' @export
#'
#' @examples fire_shape_data <- fire_area(fires)
#' 
#' 
#' 
fire_area <- function(x){
  
  require(sf)
  require(tidyverse)
  
  
  # make sure it is an sf object
  if(!is(x, "sf")) {
    warning("Y has to be a spatial object - converting to sf ")
  x <- sf::st_as_sf(x)
  }
  
  # make sure function does not use scientific notation 
  op <- options(scipen=999)
  on.exit(options(op), add = TRUE)
  
 ######################## calculate area 
  x <-  x |> 
    
    # area
    mutate(area = st_area(geometry)) |> 
    
    # remove [m^2] from area string
    mutate(area = str_remove(area, "[m^2]")) |> 
    
    # calculate in hectares
    mutate(area_ha = (as.numeric(area) / 10000)) |> 
    
    # rename are column to meters squared
    rename(area_m2 = area) |> 
    
    # move geo column to last column
    relocate(geometry, .after = area_ha)
  
  return(x)
}


