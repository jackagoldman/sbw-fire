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
  require(tidyr)
  # create empty list
  mylist <- list()
  
  # calculate slopes
  for(i in 1:nrow(data)){
     #subset data
    row <- data[i,]
    #get fire id
    fId <- row |> select(c(id)) |> slice(1)
    # subset data for fire id
    row2 <- data |> filter(id == c(fId))
    #run first model
    mod1 <- lm(nbr ~ (recovery_interval == 1), data = row2)
    #run second model
    mod2 <- lm(nbr ~ (recovery_interval == 2), data = row2)
    #run third model
    mod3 <- lm(nbr ~ recovery_interval, data = row2)
    
    #get coefficients
    cf1 <- coef(mod1)
    cf2 <- coef(mod2)
    cf10 <- coef(mod3)
    #get slope
    slope1 <- cf1[2]
    slope2 <- cf2[2]
    slope10 <- cf10[2]
    #get se
    se1 <- sqrt(diag(vcov(mod1)))[2]
    se2 <- sqrt(diag(vcov(mod2)))[2]
    se10 <- sqrt(diag(vcov(mod3)))[2]

    vec <- c(fId, slope10, slope1, slope2,se10, se1, se2)
    names(vec) <- c("fId", "slope10", "slope1", "slope2", "se10", "se1", "se2")
    
    vec <-  vec[1:7]
    
    mylist[[i]] <- vec
    i+1
  }
  res <- do.call(rbind, mylist)
  res <- res |> tidyr::as_tibble(.name_repair = 'unique') |>  
    group_by(fId) |> 
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
  
  
  mylist <- list()
  
  # calculate sens slope
  for(i in 1:nrow(data)){
    #subset data
    row <- data[i,]
    #get fire id
    fId <- row |> select(c(id)) |> slice(1)
    # subset data for fire id
    row2 <- data |> filter(id == c(fId))
  
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
  vec <- c(fId, sens1, sens2, uppr1, uppr2, lwr1, lwr2, sens10, uppr10, lwr10)
  names(vec) <- c("fId", "sens1", "sens2", "uppr1", "uppr2", "lwr1", "lwr2", "sens10", "uppr10", "lwr10")
  
  vec <-  vec[1:10]
  
  mylist[[i]] <- vec
  i+1
}
res <- do.call(rbind, mylist)
res <- res |> as.tibble(.name_repair = 'unique') |>  
  group_by(fId) %>% 
  slice_head(n = 1) |> 
  unnest( )
}




matchIds <- function(data){
  
  data <- dplyr::select(data, c("id", "fire_name"))
  
  return(data)
}


slopePostProcess <- function(dataLm, dataSens, ids, DATA_DIR){
  
  # rename fId column to id
  dataLm2 <- dplyr::rename(dataLm, id = fId )
  dataSens2 <- dplyr::rename(dataSens, id = fId)
  
  # add fire_name to columns
  processedLm <- dplyr::left_join(dataLm2, ids, by = "id")
  processedSens <- dplyr::left_join(dataSens2, ids, by ="id")
  
  # move columns
  processedLm <- dplyr::relocate(processedLm, fire_name, .after ="id")
  processedSens <- dplyr::relocate(processedSens, fire_name, .after ="id")
  
  # build outputpath
  pathLm <- paste0(DATA_DIR, "recovery_lm.csv")
  pathSens <- paste0(DATA_DIR, "recovery_sensSlope.csv")
  
  # output dataframes
  write.csv(processedLm, pathLm)
  write.csv(processedSens, pathSens)

}
