library(targets)
library(tarchetypes) 

# Set target options:
tar_option_set(
  packages = c("tibble", "dplyr", "trend", "readr", "tidyr", "quarto") 
)

# R functions
tar_source("src/slope_data_prep_function.R")
tar_source("src/slope-function.R")
tar_source("src/hypothesis_testing.R")
tar_source("src/visualization.R")


DATA_DIR <- "data/paired_fires/"


# target list
list(
  # set file path
  tar_target(name = file, "data/paired_fires/fire_recovery.csv", format = "file"),
  # read in file
  tar_target(name = data, getData(file)),
  # clean data for slope analysis
  tar_target(name = cleanData, slopePrep(data)),
  # get slopes using lm
  tar_target(name = lm_recovery, recoverySlope_lm(cleanData)),
  # get slopes using theil-sen regression
  tar_target(name = sens_recovery, recoverySlope_sens(cleanData)),
  # match ids
  tar_target(name = ids, matchIds(data)),
  # post process recovery analysis data
  tar_target(name = output, slopePostProcess(lm_recovery, sens_recovery, ids, DATA_DIR)),
  # set file path to severity data
  tar_target(name = severity_data, "data/paired_fires/fire_severity.csv", format = "file"),
  # set file path to defol data
  tar_target(name = defol_data, "data/paired_fires/fire_defol.csv", format = "file"),
  #  read in severity data
  tar_target(name = sev_df, getData(severity_data)),
  # read in defol data
  tar_target(name = defol_df, getData(defol_data)),
  # order data for severity
  tar_target(name = sev_order, order_data(sev_df, defol_df)),
  # severity ttest
  tar_target(name = sev_ttest, severity_ttest(sev_order)),
  # set path to slope results 
  tar_target(name = slope_data, "data/paired_fires/recovery_sensSlope.csv", format = "file"),
  # read in slope data
  tar_target(name = slope_df, getData(slope_data)),
  # join slope data
  tar_target(name = slope_order, order_data(slope_df, defol_df)),
  tar_target(name = slo_ttest, recovery_ttest(slope_order)),
  # prep data for visualiation
  tar_target(name = vis_data, vis_prep(sev_df, defol_df, slope_df)),
  # make boxplots
  tar_target(name = boxplot, recovery_visBox(vis_data, slo_ttest, sev_ttest)),
  # trend prep
  tar_target(name = ts_data , trendPrep(data, defol_df)),
  # trend plot
  tar_target(name = trendPlot, trend_plot(ts_data)),
  #render report
  tarchetypes::tar_render(project_report, "project_report.Rmd") 
  
)