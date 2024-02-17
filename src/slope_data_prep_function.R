# Data prep for slope function


getData <- function(file){
  readr::read_csv(file)
}



#' slope data prep
#'
#' @param data dataframe with unique id, fire_name and nbr1-10 variables. One row per fire.
#'
#' @return returns dataframe with row for id, fire_name and nbr. recovery_time = nbr year (1-10), recovery_interval = 1 for 1-5 yrs and 2 for 6-10 yrs.
#' @export
#'
#' @examples data <- slopePrep(data)

slopePrep <- function(x){
  
    x <- dplyr::select(x, c(id, fire_name, nbr1, nbr2, nbr3, nbr4, 
                               nbr5, nbr6, nbr7, nbr8, nbr9,nbr10))
    
    x <- tidyr::pivot_longer(x, !c(fire_name, id), names_to = "Recovery_Time", values_to = "nbr" )  
    
    
    x <- dplyr::mutate(x, recovery_interval = case_when(Recovery_Time == "nbr1" ~ 1, 
                                         Recovery_Time == "nbr2" ~ 1,
                                         Recovery_Time == "nbr3" ~ 1, 
                                         Recovery_Time == "nbr4" ~ 1, 
                                         Recovery_Time == "nbr5" ~ 1, 
                                         Recovery_Time == "nbr6" ~ 2,
                                         Recovery_Time == "nbr7" ~ 2, 
                                         Recovery_Time == "nbr8" ~ 2,
                                         Recovery_Time == "nbr9" ~ 2,
                                         Recovery_Time == "nbr10" ~ 2))
  
  #scale nbr
  x <- dplyr::mutate(x, nbr = (nbr*10^3))
  
  # make sure all columns are lower case
  names(x) <- tolower(names(x))
    
  
  return(x)
}

