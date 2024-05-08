library(raster)
library(exactextractr)
library(sf)
library(tidyverse)
library(terra)

setwd("")
out_dir = ""

periods <- c("dawn","midday","dusk","night")
responses <- c("Anthropophony", "Biophony", "Geophony", "Quiet", "ACI", "NDSI", "H", "bird_richness")

pred_ras_dir <- "prediction_surfaces/"
pred_ras_files <- list.files(pred_ras_dir, pattern = "*med.tif$", full.names = TRUE)
temp_ras <- raster(pred_ras_files[1])
ref_crs <- crs(temp_ras)

lulc_file <- "data/spatial_data/anthropogenic/LULC.shp"
lulc_shp <- st_read(lulc_file)
lulc_shp <- st_transform(lulc_shp, ref_crs)
lulc_temp <- st_area(lulc_shp)
lulc_classes <- lulc_shp$LCLU

# Generate precomputed fires by year
fire_file <- "data/spatial_data/fire_severity/fire21_2.gdb"
fire_shp <- vect(fire_file)
fire_shp <- project(fire_shp, ref_crs)
for(i in c(2017, 2019, 2020)){
  print(i)
  if(i == 2017){
    fire_names <- c("TUBBS", "NUNS", "POCKET")
  } else if(i == 2019){
    fire_names <- c("KINCADE")
  } else if(i == 2020){
    fire_names <- c("GLASS", "WALBRIDGE", "MEYERS", "HENNESSEY")
  }
  fire_shp_temp <- fire_shp[fire_shp$FIRE_NAME %in% fire_names, ]
  fire_shp_temp <- fire_shp_temp[fire_shp_temp$YEAR_ == i, ]
  print(fire_shp_temp$FIRE_NAME)

  outfile <- paste0("data/spatial_data/fire_severity/fireyear_",i,".shp")
  writeVector(fire_shp_temp, outfile, overwrite = TRUE)
  rm(fire_shp_temp)
}

# intesect lulc classes
for(i in pred_ras_files){
  temp_r <- raster(i)
  components <- strsplit(basename(i), "_")[[1]]
  year <- substr(components[1], 1, 4)
  response <- substr(components[1], 6, nchar(components[1]))
  period <- components[2]
  print(basename(i))
  if(response == "bird"){
    period = "allday"
    response = "birdrichness"
  }
  print(paste0(year, ": ", response, ": ", period))
  outfile <- paste0(out_dir, year, "_", response, "_", period, "_lulc_coverage.csv")

  if(file.exists(outfile)){
    print("skipping file...")
  } else {
    # inverse mask previous fire years
    year_vec <- seq(2017, year, 1)
    for(j in year_vec){
      fire_file <- paste0("data/spatial_data/fire_severity/fireyear_", j, ".shp")
      if(file.exists(fire_file)){
        temp_fire_shp <- st_read(fire_file, quiet = TRUE)
        temp_r <- mask(temp_r, temp_fire_shp, inverse = TRUE)
      }
    }
      intersection <- exactextractr::exact_extract(temp_r,
                                                   lulc_shp,
                                                   progress = TRUE,
                                                   force_df = TRUE)
      names(intersection) <- lulc_classes
      intersection <- lapply(intersection, na.omit)
      intersection_df <- intersection %>%
        bind_rows(.id = "list_name") %>%
        filter(coverage_fraction == 1) %>%
        mutate("response" = response,
               "year" = year,
               "period" = period)

      write_csv(intersection_df, outfile)
  }
}