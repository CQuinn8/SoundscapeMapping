library(terra)
library(stringr)
library(readr)
library(dplyr)
library(tibble)
library(tidyr)
library(data.table)

args <- commandArgs(TRUE)
slurm_id <- args[1]
slurm_id <- as.integer(slurm_id)
year <- "2021"
wd <- ""

# list all prediction surfaces 
surface_files <- list.files(paste0(wd, 'results/rf_vif3/prediction_surfaces/', year, '/'), full.names = TRUE, pattern = ".tif$")
modeling_params <- read_csv(paste0(wd, "data/model_params_n290.csv"))

# focus on response and period
period_resp_df <- modeling_params %>% 
  select(response, period) %>% 
  distinct()
job_params <- period_resp_df[slurm_id,]

# current response and periof
temp_resp <- job_params$response[1]
temp_period <- job_params$period[1]
print(temp_resp)
print(temp_period)

# gather current response period surface names
temp_files <- surface_files[grepl(temp_period, surface_files) & grepl(temp_resp, surface_files)]

# read in surface files 
temp_rast <- rast(temp_files)

# IQR
temp_q <- quantile(temp_rast, probs = c(0.25,0.5,0.75))
temp_iqr <- temp_q$q0.75 - temp_q$q0.25

# median map
temp_med <- temp_q$q0.5

# write IQR and median maps
writeRaster(temp_med, paste0(wd,
                             "results/rf_vif3/processed/prediction_surfaces/", 
                             year, "-",
                             temp_resp, "_",
                             temp_period, "_med.tif"),
            overwrite = TRUE)
writeRaster(temp_iqr, paste0(wd, 
                             "results/rf_vif3/processed/prediction_surfaces/", 
                             year, "-",
                             temp_resp, "_",
                             temp_period, "_iqr.tif"),
            overwrite = TRUE)

# clean up
rm(temp_q, temp_rast, temp_iqr, temp_med)
