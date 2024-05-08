
library(terra)
library(raster)
library(tidyverse)

job_params <- read_csv("code/2_spatial_modeling/model_params_n290.csv") %>% 
  distinct(response, period)
# periods <- c("dawn", "midday", "dusk", "night")
responses <- tibble(responses = unique(job_params$response))
raster_files <- list.files("results/rf_vif3/processed/prediction_surfaces/", pattern = "^2021.*.tif$", full.names = TRUE)

# Response correlation maps
# create table of unique pairs
pairs <- responses %>%
  expand(resp1 = responses, resp2 = responses) %>%
    filter(resp1 != resp2) %>%
    filter(!duplicated(paste0(pmax(resp1, resp2), pmin(resp1, resp2))))

temp_period <- "dawn"
# period specific median maps
temp_raster_files <- raster_files[grepl(temp_period, raster_files) & grepl("med", raster_files)]

for(i in 1:(nrow(pairs))){
  temp_pair <- pairs[i,]
  print(i)
  print(temp_pair)
  
  temp_1 <- raster(temp_raster_files[grepl(temp_pair$resp1, temp_raster_files)])
  temp_2 <- raster(temp_raster_files[grepl(temp_pair$resp2, temp_raster_files)])
  
  corSpear <- corLocal(temp_1, temp_2, method = 'spearman')
  #plot(corSpear)
  raster::writeRaster(corSpear, filename = paste0('results/rf_vif3/processed/surface_correlations/', 
                                                  temp_pair$resp1,'_',
                                                  temp_pair$resp2,'_',
                                                  temp_period,
                                                  '_corr.tif'),  overwrite = TRUE)
}