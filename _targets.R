library(targets)
library(tarchetypes) 

# Set target options:
tar_option_set(
  packages = c("tibble", "dplyr", "trend", "readr", "tidyr") 
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source("src/slope_data_prep_function.R")
tar_source("src/slope-function.R")


DATA_DIR <- "data/paired_fires/"


# Replace the target list below with your own:
list(
  tar_target(name = file, "data/paired_fires/fire_recovery.csv", format = "file"),
  tar_target(name = data, getData(file)),
  tar_target(name = cleanData, slopePrep(data)),
  tar_target(name = lm_recovery, recoverySlope_lm(cleanData)),
  tar_target(name = sens_recovery, recoverySlope_sens(cleanData)),
  tar_target(name = ids, matchIds(data)),
  tar_target(name = output, slopePostProcess(lm_recovery, sens_recovery, ids, DATA_DIR))
)