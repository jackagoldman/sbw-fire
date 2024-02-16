#' OVERLAP INTERSECTION FUNCTION
#'
#' @param fire spatial dataframe containing fire perimeter. Dataframe must have columns for fire_year and fire_id (unqiue identifiers)
#' @param insect spatial dataframe containing defoliated perimeters. Dataframe must have a single observation per year and columns for year.
#' @param max_year numeric, maximum year to filter data. Both fire and insect data will be passed to argument.
#' @param min_year numeric, minimum year to filter data. Both fire and insect data will be passed to argument.
#' @param year_gap numeric, maximum number of years between the occurence of insect and fire events.
#' 
#' 
#' @return spatial dataframe with columns for fire_year, tsd, and, cumltve_years
#' @export
#'
#' @examples data <-overlap_intersection(fire, defols, 2012, 1970, 15)
#' 
#' 
overlap_intersection <- function(fire,insect,max_year,min_year,year_gap){
  
  require(sf)
  require(tidyverse)
  
  max_year <- as.numeric(max_year)
  min_year <- as.numeric(min_year)
  
  
  # make sure all columns are lowercase
  names(fire) <- tolower(names(fire))
  names(insect) <- tolower(names(insect))


  # sort fires by max year if specified
  if(!is.null(max_year)){
    fire <- fire |> 
      dplyr::filter(fire_year <= max_year)
    insect <- insect |> 
      dplyr::filter(year <= max_year)
  } else if(is.null(max_year)){
  next
  }
  
  # sort insect by min year if specified
  if(!is.null(min_year)){
    insect <- insect |> 
      dplyr::filter(year >= min_year)
  } else if(is.null(min_year)){
    next
  }
  
  # create gap year variable based on numeric value passed to gap year argument
  if(is.null(year_gap)){
    stop("missing argument for gap year - number of years between insect and fire")
  } else{
    #make sure it is numeric
    gap_year = as.numeric(year_gap)
  }
  
  
  # assume planar
  sf_use_s2(FALSE) 
  
  # make insect st object valie
  insect <- st_make_valid(insect)
  
  
  # required column messages
  if(!"fire_year" %in% colnames(fire))
  {
    stop("missing column 'fire_year' in fire perimeter dataframe - please add column")
  }
  
  if(!"year" %in% colnames(insect))
  {
    stop("missing column 'year' in insect dataframe - please add column")
  }
  
  if(!"fire_id" %in% colnames(fire))
  {
    stop("missing column 'fire_id' in fire dataframe - please add column")
  }
  
  
  # get the intersection of fire and insect
  intersect <- st_intersection(fire, insect) |> 
    
    # add row with the interval year (insect defol - fire year)
    mutate(int_year = paste0(year, "-", fire_year)) |> 
    
    # get the difference between fire year and insect disturbance year
    mutate(diff = (as.numeric(fire_year) - as.numeric(year))) |> 
    
    #turn year column into insect year
    mutate(insect_year = year) |> 
    
    # filter the difference between insect and fire occurrence to meet timeframe specified in gap_year argument
    filter(diff <= gap_year) |> 
    
    # make sure difference between fire and insect disturbance is greater that 0 years
    filter(diff >= 0) |> 
    
    #select required columns
    select(c("fire_id", "fire_year", "insect_year", "diff", "int_year","geometry")) |> 
   
    # group by fire id
    group_by(fire_id) |> 
    
    # calculate the cumulative years that a fire was defoliated
    mutate(cumltve_yrs= n()) |> 
    
    # calculate time since insect disturbance
    mutate(tsd = max(insect_year)) |> 
    
    # caculate difference between time since disturbance and fire year
    mutate(tsd = (fire_year - as.numeric(tsd)))
 
  
  
  # Merge all geometries to get the entire area defoliated
  # previous intersection returns a shape for each year the fire was defoliated.
  # we want to merge this together
  
  
  # get the data we want from intersect
  insect_stats <- intersect |> 
    select(c("fire_id", "fire_year", "tsd", "cumltve_yrs")) |> 
   
     # group dataframe by fire id
    group_by(fire_id) |> 
    
    # dissolve all geometries present for a given fire using the union function
    summarise(geometry = st_union(geometry),
              tsd = max(tsd),
              cumltve_yrs = max(cumltve_yrs),
              fire_year = max(fire_year)) 
    
  
  
  # return processed data
  return(insect_stats)
}
