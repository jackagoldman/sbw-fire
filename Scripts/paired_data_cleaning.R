######### severity and nbr clearning

# load libraries
library(tidyverse)
library(sf)

########################################
######## SEVERITY DATA CLEANING ########
########################################

# read in burn severity
# d = defoliation
# nd = non-defoliated
bs_d <- read_csv("/Users/jgoldman/Library/CloudStorage/GoogleDrive-jandrewgoldman@gmail.com/My Drive/chapter-3-paired-severity/chapter-3-severity-paired.csv")
bs_nd <- read_csv("/Users/jgoldman/Library/CloudStorage/GoogleDrive-jandrewgoldman@gmail.com/My Drive/chapter-3-paired-severity/chapter-3-severity-non_defol-paired.csv")

# remove .geom and system:index column
# change fire_id to fire_name

bs_d <- bs_d |> 
  select(-c(.geo, 'system:index')) |> 
  rename(fire_year = Fire_Yr) |> 
  rename(defoliated = defoltd) |> 
  rename(cumltve_yrs = cmltv_y) |> 
  rename(rbr_cv = rbrCV) |> 
  rename(rbr_extreme = rbrExtreme) |> 
  rename(rbr_median = rbrMedian) |> 
  rename(fire_name = Fire_ID)

bs_nd <- bs_nd |> 
  select(-c(.geo, 'system:index')) |> 
  rename(fire_year = Fire_Year) |> 
  rename(rbr_cv = rbrCV) |> 
  rename(rbr_extreme = rbrExtreme) |> 
  rename(rbr_median = rbrMedian) |> 
  rename(fire_name = Fire_ID)

# add ID columns
# 1-33 for defoliated
# 34-66 for non-defoliated

bs_d <- bs_d |> 
  mutate(id = rep(1:33, 1)) |> 
  relocate(id, .before = fire_name)

bs_nd <- bs_nd |> 
  mutate(id = rep(34:66, 1)) |> 
  relocate(id, .before = fire_name)


  