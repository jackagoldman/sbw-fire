######### severity and nbr clearning

# load libraries
library(tidyverse)

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

# Format area columns
bs_d$area_ha <- formatC(bs_d$area_ha, format = "f")
bs_d$area_m2 <- formatC(bs_d$area_m2, format = "f")

bs_nd$area_ha <- formatC(bs_nd$area_ha, format = "f")
bs_nd$area_m2 <- formatC(bs_nd$area_m2, format = "f")

#######################################
######### NBR DATA CLEANING ###########
#######################################

#read in files
nbr_nd <- read_csv("/Users/jgoldman/Library/CloudStorage/GoogleDrive-jandrewgoldman@gmail.com/My Drive/paired_nbr non-defol/non_defol_recovery.csv")
nbr_d <- read_csv("/Users/jgoldman/Library/CloudStorage/GoogleDrive-jandrewgoldman@gmail.com/My Drive/paired_nbr defol/defol_recovery.csv")

# remove .geom and system:index column
# change fire_id to fire_name
nbr_d <- nbr_d |> 
  select(-c(.geo, 'system:index', tsd, cmltv_y, Fire_Yr, area_ha, area_m2)) |>
  rename(defoliated = defoltd) |> 
  rename(fire_name = Fire_ID) |> 
  relocate(preNBR, .before =nbr1) |> 
  relocate(nbr10, .after=nbr9)

nbr_nd <- nbr_nd |> 
  select(-c(.geo, 'system:index', Fire_Year, area_ha, area_m2)) |>
  rename(fire_name = Fire_ID) |> 
  relocate(preNBR, .before =nbr1) |> 
  relocate(nbr10, .after=nbr9)
  
# add ID columns from bs_d
bs_ids_d <- bs_d |> 
  select(c(fire_name, id))
nbr_d <- nbr_d |> 
  left_join(bs_ids_d, by = "fire_name") |> 
  relocate(id, .before = "fire_name")

bs_ids_nd <- bs_nd |> 
  select(c(fire_name, id))

nbr_nd <- nbr_nd |> 
  left_join(bs_ids_nd, by = "fire_name") |> 
  relocate(id, .before = "fire_name")

########################################
########### FIRE INFO DATA #############
########################################

d_info <- bs_d |> 
  select(c(id, fire_name, fire_year, area_ha, area_m2))

nd_info <- bs_nd |> 
  select(c(id, fire_name, fire_year, area_ha, area_m2))

paired_info <- rbind(d_info, nd_info)

write_csv(paired_info, "data/paired_fires/fire_info.csv")
  
#########################################
############# DEFOL DATA ################
#########################################
d_defol <- bs_d |> 
  select(c(id, fire_name, tsd, defoliated, cumltve_yrs))

nd_defol <- bs_nd |> 
  select(c(id, fire_name, defoliated)) |> 
  mutate(tsd = rep(NA, 33)) |> 
  mutate(cumltve_yrs = rep(NA, 33))

paired_defol <- rbind(d_defol, nd_defol)

paired_defol <- paired_defol |> 
  relocate(defoliated, .before = tsd)

write_csv(paired_defol, "data/paired_fires/fire_defol.csv")

#######################################
########### SEVERITY DATA #############
#######################################

sev_d <- bs_d |> 
  select(c(id, fire_name, rbr_cv, rbr_extreme, rbr_median))

sev_nd <- bs_nd |> 
  select(c(id, fire_name, rbr_cv, rbr_extreme, rbr_median))

paired_severity <- rbind(sev_d, sev_nd)

write_csv(paired_severity, "data/paired_fires/fire_severity.csv")

##################################
########### NBR DATA #############
##################################

nbr_d <- nbr_d |> 
  select(-c(defoliated))
nbr_nd <- nbr_nd |> 
  select(-c(defoliated))

paired_nbr <- rbind(nbr_d, nbr_nd)

write_csv(paired_nbr, "data/paired_fires/fire_recovery.csv")

