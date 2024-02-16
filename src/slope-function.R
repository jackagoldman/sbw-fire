# slope functions

#' slope function using linear model
#'
#' @param data 
#'
#' @return dataframe consisting of slopes from lm
#' @export
#'
#' @examples
recoverySlope_lm <- function(data){
  
  require(trend)
  require(tidyverse)
  
  x <- data |> 
    select(c(Fire_ID, nbr10, nbr1, nbr2, nbr3, nbr4, nbr5, nbr6, nbr7, nbr8, nbr9)) |> 
    pivot_longer(!Fire_ID, names_to = "Recovery_Time", values_to = "nbr" ) |> 
    mutate(recovery_interval = case_when(Recovery_Time == "nbr1" ~ 1, 
                                         Recovery_Time == "nbr2" ~ 1,
                                         Recovery_Time == "nbr3" ~ 1, 
                                         Recovery_Time == "nbr4" ~ 1, 
                                         Recovery_Time == "nbr5" ~ 1, 
                                         Recovery_Time == "nbr6" ~ 2,
                                         Recovery_Time == "nbr7" ~ 2, 
                                         Recovery_Time == "nbr8" ~ 2,
                                         Recovery_Time == "nbr9" ~ 2,
                                         Recovery_Time == "nbr10" ~ 2,))
  
 
  
  mylist <- list()
  


  #loop
  for(i in 1:nrow(x)){
     #subset data
    row <- x[i,]
    #get fire id
    fId <- row |> select(c(Fire_ID)) |> slice(1)
    # subset data for fire id
    row2 <- x |> filter(Fire_ID == c(fId))
    #run first model
    mod1 <- lm(nbr ~ (recovery_interval == 1), data = row2)
    #run second model
    mod2 <- lm(nbr ~ (recovery_interval == 2), data = row2)
    summary(mod1)
    #get coefficients
    cf1 <- coef(mod1)
    cf2 <- coef(mod2)
    
    #get slope
    slope1 <- cf1[2]
    slope2 <- cf2[2]
    
    #get se
    se1 <- sqrt(diag(vcov(mod1)))[2]
    se2 <- sqrt(diag(vcov(mod2)))[2]
    

    vec <- c(fId, slope1, slope2, se1, se2, sens1, sens2, uppr1, uppr2, lwr1, lwr2, sens10, uppr10, lwr10)
    names(vec) <- c("fId", "slope1", "slope2", "se1", "se2", "sens1", "sens2", "uppr1", "uppr2", "lwr1", "lwr2", "sens10", "uppr10", "lwr10")
    
    vec <-  vec[1:14]
    
    mylist[[i]] <- vec
    i+1
  }
  res <- do.call(rbind, mylist)
  res <- res |> as.tibble(.name_repair = 'unique') |>  
    group_by(fId) %>% 
    slice_head(n = 1) |> 
    unnest( )
 
}


#' slope function using theil-sen regression
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
recoverySlope_sens <- function(data){
  
  
  require(trend)
  require(tidyverse)
  
  x <- data |> 
    select(c(Fire_ID, nbr10, nbr1, nbr2, nbr3, nbr4, nbr5, nbr6, nbr7, nbr8, nbr9)) |> 
    pivot_longer(!Fire_ID, names_to = "Recovery_Time", values_to = "nbr" ) |> 
    mutate(recovery_interval = case_when(Recovery_Time == "nbr1" ~ 1, 
                                         Recovery_Time == "nbr2" ~ 1,
                                         Recovery_Time == "nbr3" ~ 1, 
                                         Recovery_Time == "nbr4" ~ 1, 
                                         Recovery_Time == "nbr5" ~ 1, 
                                         Recovery_Time == "nbr6" ~ 2,
                                         Recovery_Time == "nbr7" ~ 2, 
                                         Recovery_Time == "nbr8" ~ 2,
                                         Recovery_Time == "nbr9" ~ 2,
                                         Recovery_Time == "nbr10" ~ 2,))
  
  mylist <- list()
  
  
  
  #loop
  for(i in 1:nrow(x)){
    #subset data
    row <- x[i,]
    #get fire id
    fId <- row |> select(c(Fire_ID)) |> slice(1)
    # subset data for fire id
    row2 <- x |> filter(Fire_ID == c(fId))
  
  
  # sens slope 1-5, 6-10
  
  data1 <- row2 |> filter(recovery_interval == 1) |> select(c(nbr))
  data2 <- row2 |> filter(recovery_interval == 2) |> select(c(nbr))
  
  ts1 <- ts(data1, start =1, frequency = 10) 
  ts2 <- ts(data2, start =1, frequency = 10)
  
  
  
  sslope1 <- sens.slope(ts1, conf.level = 0.95) 
  sslope2 <- sens.slope(ts2, conf.level = 0.95)
  
  
  sens1 <- sslope1$estimates
  sens2 <- sslope2$estimates
  
  
  
  uppr1 <- sslope1$conf.int[2]
  uppr2 <- sslope2$conf.int[2]
  lwr1 <- sslope1$conf.int[1]
  lwr2 <- sslope2$conf.int[1]
  
  #sens slope 1-10
  
  data10 <- row2 |>  select(c(nbr))
  
  
  ts10 <- ts(data10, start =1, frequency = 10) 
  
  
  sslope10 <- sens.slope(ts10, conf.level = 0.95) 
  
  
  sens10 <- sslope10$estimates
  
  uppr10 <- sslope10$conf.int[2]
  lwr10 <- sslope10$conf.int[1]
  vec <- c(fId, slope1, slope2, se1, se2, sens1, sens2, uppr1, uppr2, lwr1, lwr2, sens10, uppr10, lwr10)
  names(vec) <- c("fId", "slope1", "slope2", "se1", "se2", "sens1", "sens2", "uppr1", "uppr2", "lwr1", "lwr2", "sens10", "uppr10", "lwr10")
  
  vec <-  vec[1:14]
  
  mylist[[i]] <- vec
  i+1
}
res <- do.call(rbind, mylist)
res <- res |> as.tibble(.name_repair = 'unique') |>  
  group_by(fId) %>% 
  slice_head(n = 1) |> 
  unnest( )
}
