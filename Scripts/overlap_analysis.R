# overlap analysis and exploration / comaprison to samuel

library(sf)
library(tidyverse)

# source functions
source("/Users/jgoldman/Work/PhD/rbr-project/RBR_bias_project/code/functions/overlap_intersection.R")
source("/Users/jgoldman/Work/PhD/rbr-project/RBR_bias_project/code/functions/overlap_difference.R")
source("/Users/jgoldman/Work/PhD/rbr-project/RBR_bias_project/code/functions/fire_area.R")


# read in fire and defoliation data
defol <- st_read("/Users/jgoldman/Library/CloudStorage/GoogleDrive-jandrewgoldman@gmail.com/My Drive/Fire-SBW project/defoliation-data/sbw_merge/clean/sbw_60_20_v2.shp")
fire <- st_read("/Users/jgoldman/Library/CloudStorage/GoogleDrive-jandrewgoldman@gmail.com/My Drive/Fire-SBW project/fire-perimeters/ON_FirePerimeters_85to2020_v00.shp")


#calculate fire intersection
data_intersection <-overlap_intersection(fire, defol, 2012, 1970, 15)


#calculate difference
data_difference <- overlap_difference(fire, data_intersection)


#calculate fire area
data_intersection_v1 <- fire_area(data_intersection)
data_difference_v1 <- fire_area(data_difference)

#filter data difference to be greater than 1
data_nd <- data_difference_v1 |> 
  filter(area_ha >=1)

# get paired fire names
non_defol_names <- data_nd |> 
  pull(fire_id)

#filter defolaited perims
data_d <- data_intersection_v1 |> 
  filter(fire_id %in% non_defol_names)

############## compare results vs samuels ###########################
fire_key <-  read_csv("/Users/jgoldman/Library/CloudStorage/OneDrive-UniversityofToronto/Data/fire-only-sbw-only-perimeters/database_files/fire_key.csv")
defol_table <- read_csv("/Users/jgoldman/Library/CloudStorage/OneDrive-UniversityofToronto/Data/fire-only-sbw-only-perimeters/database_files/defol_table.csv")



# fire_key subset 1-34

fire_key_sub <- fire_key |> 
  filter(id <= 34)

data_d_check <-  data_d |> 
  st_drop_geometry() |> 
  rename(fire_name = fire_id) |> 
  left_join(fire_key_sub, by = "fire_name") |> 
  relocate(id, .before = fire_name)


# fire id 13 and 17 are missing from sam's analysis

fk_13 <-  fire_key |> # KEN36_2012_1917
  filter(id == "13")


fk_17 <-  fire_key |> # RED84_2011_1823
  filter(id == "17")

# are missing fires in the are filtered db?

data_difference_v1 |> 
  filter(fire_id == "KEN36_2012_1917") # not found in difference

data_intersection_v1 |> 
  filter(fire_id == "KEN36_2012_1917") # not found in intersection

data_difference_v1 |> 
  filter(fire_id == "RED84_2011_1823") # not found in difference

data_intersection_v1 |> 
  filter(fire_id == "RED84_2011_1823") # not found in intersection

# are these in the fire db?

fire |> 
  filter(Fire_ID == "RED84_2011_1823") |> plot()

fire |> 
  filter(Fire_ID == "KEN36_2012_1917") |> plot()

########################## run overlap on missing fires #######################
missing_fires <- fire |> 
  filter(Fire_ID == "KEN36_2012_1917" | Fire_ID == "RED84_2011_1823")

#calculate fire intersection
defol_gt <- defol |> filter(year >= 1970 | year <=2012)
# get the intersection of fire and insect
names(missing_fires) <- tolower(names(missing_fires))

mf_int <- st_intersection(missing_fires, defol_gt) |> 
  mutate(int_year = paste0(year, "-", fire_year)) |> 
  mutate(diff = (as.numeric(fire_year) - as.numeric(year))) |> 
  mutate(insect_year = year) 


# these fire were remove because the 
