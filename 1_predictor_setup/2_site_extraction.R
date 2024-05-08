
# load libraries
# library(raster)
# library(sf)
library(terra)
library(exactextractr)

# ABG data
site_hr_abg <- read_csv("data/acoustic_data/site_by_hour_ABGQI_2017-2021.csv")

# Sites
sites <- vect("data/spatial_data/sites/sonoma_s2l_locations_220620_nodup_nobad_utm_wgs84.shp")
sites$YY <- paste0("20", substr(sites$SiteID, start = 10, stop = 11))

# Predictor stack
date <- "20230223"
preds <- rast(paste0("data/spatial_data/predictors/predictors_",date,".tif"))
preds_annual <- rast(paste0("data/spatial_data/predictors/predictors_annual_",date,".tif"))

# extract cell values for non-annual predictors
extracted <- values(terra::extract(preds, sites, method = "simple", bind = TRUE))

# force NA cols to Zero
na_count <- extracted %>%
  summarise_all(list(~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "preds", values_to = "Count_na") %>%
  filter(Count_na > 0)
non_na <- c("pct_ag_450m_mean", "pct_ag_450m_sd", "pct_ag_90m", 
            "pct_ag_990m_mean", "pct_ag_990m_sd")
extracted <- extracted %>%
  mutate_at(non_na, ~replace_na(., 0))

years <- c("2017","2018","2019","2020","2021")
extracted_annual = list()
for(i in years){
  print(i)
  temp_subset <- preds_annual[i]
  
  # simplify names and make consistent
  names(temp_subset) <- gsub(sprintf("pred_%1$s-06-15_|_%1$s|wy%1$s_|%1$s_", i), "", names(temp_subset))
  
  # subset sites
  temp_sites <- subset(sites, grepl(i, sites$YY))
  
  # extract current year's site predictor data
  temp_extract <- terra::extract(temp_subset, temp_sites, method = "simple", bind = TRUE)
  
  extracted_annual[[i]] <- values(temp_extract)
}

# combine
extracted_all <- extracted_annual %>%
  bind_rows() %>%
  full_join(., extracted) %>%
  dplyr::select(-c(Property, Protective, UniqueID, Number_of_, YY)) %>%
  drop_na() # n = -15

# Save file
date_stamp <- format(Sys.Date(), '%Y%m%d')
write.csv(extracted_all, paste0("data/spatial_data/predictors/extracted_predictors_", date_stamp, ".csv"), row.names=FALSE)
