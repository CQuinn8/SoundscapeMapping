# Goal: General pre-processing spatial data with QGIS
# - generate 90m reference grid
# - resample predictors to 90m for stacking 
# - generate focal window stats @450m and @990m diameters
# - save original resolution data stack for easier site extraction


library(raster)
library(terra)
library(exactextractr)
library(sp)
library(tidyverse)
source("code/utils.R")
sonoma_shp_file <- "data/spatial_data/shapefiles/sonoma_county_utm_wgs84.shp"
sonoma <- vect(sonoma_shp_file)

##### Reference layer #####
# Definitive raster defining minimum extent and each resolution
# ID minimum extent dataset
# Climate
climate_files <- list.files('data/spatial_data/climate/wy_2020/', pattern = "*.tif$", full.names = TRUE)

# Pheno
pheno_files <- list.files('data/spatial_data/phenology/', pattern = "*.tif$", full.names = TRUE)
pheno_tiles <- lapply(pheno_files, function(x) rast(x))
pheno_ras <- terra::mosaic(sprc(pheno_tiles))

# Geomorphology
dist_files <- list.files('data/spatial_data/geomorphology', pattern = "*.tif$", full.names = TRUE)
dist_ras <- rast(dist_files[1]) # processed identically - extents match

# Structure
struct_files <- list.files('data/spatial_data/structure/', pattern = "*.tif$", full.names = TRUE)
struct_ras <- rast(struct_files[1]) # processed identically - extents match


# Extents of a single layer in each
ext(rast(climate_files[1]))
ext(pheno_ras)
ext(dist_ras)
ext(struct_ras)
min_extent <- c(453697,           # dist_to_buildings ; 453716.410686283, # xmin (dist and terrain)
                556885.625824712, # xmax (dist and terrain)
                4218366,          # dist to buildings ; 4218428.64475636, # ymin (dist and terrain)
                4303770.24390267) # ymax (climate)

# Definitive minimum extent
reference_crs <- crs(pheno_ras)
(reference_ras_90 <- rast(extent = min_extent, crs = reference_crs, res = 90, vals = -9999))
plot(ext(reference_ras))
plot(sonoma, add = TRUE)

# save reference rasters
writeRaster(reference_ras_90, filename = 'data/spatial_data/reference_grids/reference_90m.tif', overwrite = TRUE)

##### Anthropogenic: Buildings #####
# Building density using QGIS 90m density
# create a focal window that sums all values in the circle
dens_90m <- rast('data/spatial_data/anthropogenic/building_density_90m.tif')
raster_90m <- rast('data/spatial_data/reference_grids/reference_90m.tif')

# 990m 
fw_coarse <- focalWeight(dens_90m, 495, "circle")
fw_coarse[fw_coarse > 0 ] <- 1 # set to weights of 1 for sum
dens_coarse <- focal(dens_90m, w = fw_coarse, fun = sum, na.rm = TRUE)
writeRaster(dens_coarse_mask, filename = 'data/spatial_data/anthropogenic/building_density_990m.tif', overwrite = TRUE)

# 450m 
fw_mid <- focalWeight(dens_90m, 225, "circle")
fw_mid[fw_mid > 0 ] <- 1 # set to weights of 1 for sum
dens_mid <- focal(dens_90m, w = fw_mid, fun = sum, na.rm = TRUE)
writeRaster(dens_mid, filename = 'data/spatial_data/anthropogenic/building_density_450m.tif', overwrite = TRUE)

##### Anthropogenic: LULC #####
ag_shp <- sf::read_sf("data/spatial_data/anthropogenic/LULC_ag_smoothed.shp")
raster_90m <- rast("data/spatial_data/anthropogenic/building_density_90m.tif")
values(raster_90m) <- 1

# extract shapefile overlay
ag_extract <- exact_extract(raster_90m, ag_shp, include_xy = TRUE)
ag_extract <- data.frame(ag_extract[[1]]) %>%
  dplyr::select(-c(value)) %>%
  rename("pct_ag" = "coverage_fraction")

# generate raster from extracted values
ag_ras <- rasterFromXYZ(ag_extract)

# stack ag rasters
lulc_ras <- resample(rast(ag_ras), raster_90m)
writeRaster(lulc_ras, filename = 'data/spatial_data/anthropogenic/lulc_90m.tif', overwrite = TRUE)

# focal windows
# 990m
lulc_coarse_u <- focal_window(ras_stack = lulc_ras, res = 495, func = "mean")
lulc_coarse_u <- resample(lulc_coarse_u,raster_90m)
writeRaster(lulc_coarse_u, filename = 'data/spatial_data/anthropogenic/lulc_990m_mean.tif', overwrite = TRUE)

lulc_coarse_sd <- focal_window(ras_stack = lulc_ras, res = 495, func = "sd")
lulc_coarse_sd <- resample(lulc_coarse_sd,raster_90m)
writeRaster(lulc_coarse_sd, filename = 'data/spatial_data/anthropogenic/lulc_990m_sd.tif', overwrite = TRUE)


# 450m 
lulc_mid_u <- focal_window(ras_stack = lulc_ras, res = 225, func = "mean")
lulc_mid_u <- resample(lulc_mid_u,raster_90m)
writeRaster(lulc_mid_u, filename = 'data/spatial_data/anthropogenic/lulc_450m_mean.tif', overwrite = TRUE)

lulc_mid_sd <- focal_window(ras_stack = lulc_ras, res = 225, func = "sd")
lulc_mid_sd <- resample(lulc_mid_sd,raster_90m)
writeRaster(lulc_mid_sd, filename = 'data/spatial_data/anthropogenic/lulc_450m_sd.tif', overwrite = TRUE)

# convert NA ag values to 0
lulc_files <- list.files('data/spatial_data/anthropogenic/', pattern = '*.tif$', full.names = TRUE)
lulc_files <- lulc_files[grepl("lulc", lulc_files)]
lulc_ras <- rast(lulc_files)
lulc_ras[is.na(lulc_ras)] <- 0
names(lulc_ras) <- c("pct_ag_450m_mean",
                     "pct_ag_450m_sd",
                     "pct_ag_90m" ,
                     "pct_ag_990m_mean",
                     "pct_ag_990m_sd")
writeRaster(lulc_ras, filename = 'data/spatial_data/anthropogenic/ag_all_noNA.tif', overwrite = TRUE)

##### Anthropogenic: All #####
anthro_files <- list.files('data/spatial_data/anthropogenic/', pattern = '*.tif$', full.names = TRUE)
anthro_files <- anthro_files[!grepl("*30m*|*raster*|*anthropogenic_|viirs|lulc", anthro_files)]
anthro_ras <- rast(anthro_files)
names(anthro_ras) <- c("pct_ag_450m_mean",
                       "pct_ag_450m_sd",
                       "pct_ag_90m",
                       "pct_ag_990m_mean",
                       "pct_ag_990m_sd",
                       "building_density_450m",
                       "building_density_90m",
                       "building_density_990m" ,
                       "distance_to_buildings_90m",
                       "distance_to_streets_90m")


writeRaster(anthro_ras, "data/spatial_data/anthropogenic/anthropogenic_90m.tif", overwrite = TRUE)

##### Structure #####
struct_files <- list.files('data/spatial_data/structure/', 
                           pattern = "*.tif$", 
                           full.names = TRUE)
struct_ras <- rast(struct_files[1:15])
writeRaster(struct_ras, filename = 'data/spatial_data/structure/structure_30m_stack.tif', overwrite = TRUE)

# resample to 90m for predictors 
ref_ras <- rast('data/spatial_data/reference_grids/reference_90m.tif')
struct_90m <- resample(struct_ras, ref_ras, method = "bilinear", progress = TRUE)
writeRaster(struct_90m, filename = 'data/spatial_data/structure/structure_90m.tif', overwrite = TRUE)

# 990m 
struct_coarse_u <- focal_window(struct_ras, res = 495, func = "mean")
struct_coarse_u <- resample(struct_coarse_u, ref_ras, method = "bilinear", progress = TRUE)
writeRaster(struct_coarse_u, filename = 'data/spatial_data/structure/structure_990m_mean.tif', overwrite = TRUE)

struct_coarse_sd <- focal_window(struct_ras, res = 495, func = "sd")
struct_coarse_sd <- resample(struct_coarse_sd, ref_ras, method = "bilinear", progress = TRUE)
writeRaster(struct_coarse_sd, filename = 'data/spatial_data/structure/structure_990m_sd.tif', overwrite = TRUE)


# 450m 
struct_mid_u <- focal_window(struct_ras,  res = 225, func = "mean")
struct_mid_u <- resample(struct_mid_u, ref_ras, method = "bilinear", progress = TRUE)
writeRaster(struct_mid_u, filename = 'data/spatial_data/structure/structure_450m_mean.tif', overwrite = TRUE)

struct_mid_sd <- focal_window(struct_ras,  res = 225, func = "sd")
struct_mid_sd <- resample(struct_mid_sd, ref_ras, method = "bilinear", progress = TRUE)
writeRaster(struct_mid_sd, filename = 'data/spatial_data/structure/structure_450m_sd.tif', overwrite = TRUE)

# separate structure vars by year
# Create Structure layers
struct_files <- list.files('data/spatial_data/structure/', 
                           pattern = "*.tif$", 
                           full.names = TRUE)
struct_ras <- rast(struct_files[1:15])
struct_90m <- rast('data/spatial_data/structure/structure_90m_stack.tif')
struct_mid_u <- rast('data/spatial_data/structure/structure_450m_mean.tif')
struct_mid_sd <- rast('data/spatial_data/structure/structure_450m_sd.tif')
struct_coarse_u <- rast('data/spatial_data/structure/structure_990m_mean.tif')
struct_coarse_sd <- rast('data/spatial_data/structure/structure_990m_sd.tif')

names(struct_ras)
names(struct_90m)
years <- c("2017","2018","2019", "2020", "2021", "2022")
for(y in years){
  print(y)
  # 30m
  # writeRaster(struct_ras[y], paste0('data/spatial_data/structure/by_year/structure_vars_', y, '.tif'), overwrite = TRUE)
  
  # 90m
  writeRaster(struct_90m[y], paste0('data/spatial_data/structure/by_year/structure_vars_', y, '_90m.tif'), overwrite = TRUE)
  
  # coarse scale
  names(struct_coarse_u) <- paste0(names(struct_coarse_u), "_990m_mean")
  names(struct_coarse_sd) <- paste0(names(struct_coarse_sd), "_990m_sd")
  writeRaster(struct_coarse_u[y], paste0('data/spatial_data/structure/by_year/structure_vars_', y, '_990m_mean.tif'), overwrite = TRUE)
  writeRaster(struct_coarse_sd[y], paste0('data/spatial_data/structure/by_year/structure_vars_', y, '_990m_sd.tif'), overwrite = TRUE)

  # mid scale
  names(struct_mid_u) <- paste0(names(struct_mid_u), "_450m_mean")
  names(struct_mid_sd) <- paste0(names(struct_mid_sd), "_450m_sd")
  writeRaster(struct_mid_u[y], paste0('data/spatial_data/structure/by_year/structure_vars_', y, '_450m_mean.tif'), overwrite = TRUE)
  writeRaster(struct_mid_sd[y], paste0('data/spatial_data/structure/by_year/structure_vars_', y, '_450m_sd.tif'), overwrite = TRUE)
}

struct_coarse <- c(struct_coarse_u, struct_coarse_sd)
struct_mid <- c(struct_mid_u, struct_mid_sd)
writeRaster(struct_coarse, filename = 'data/spatial_data/structure/structure_990m.tif', overwrite = TRUE)
writeRaster(struct_mid, filename = 'data/spatial_data/structure/structure_450m.tif', overwrite = TRUE)

##### Geomorphology #####
ref_ras <- rast('data/spatial_data/reference_grids/reference_90m.tif')
geomorph_files <- list.files('data/spatial_data/geomorphology/', pattern = "*.tif$", full.names = TRUE)
geomorph_ras <- rast(geomorph_files)
names(geomorph_ras)
names(geomorph_ras) <- c("dist_coast", "dem", "slope", "dist_streams", "tpi", "tpi_classified")
writeRaster(geomorph_ras, filename = 'data/spatial_data/geomorphology/geomorphology_30m.tif', overwrite = TRUE)

# resample to 90m
geomorph_90m <- resample(geomorph_ras, ref_ras, method = "bilinear", progress = TRUE)
names(geomorph_90m)
writeRaster(geomorph_90m, filename = 'data/spatial_data/geomorphology/geomorphology_90m.tif', overwrite = TRUE)

##### Phenology #####
raster_90m <- rast("data/spatial_data/reference_grids/reference_90m.tif")

# Process by-year
years <- c("2017", "2018", "2019", "2020", "2021")

for(y in years){
  print(y)
  
  # mosaic
  pheno_files <- list.files('data/spatial_data/phenology/', pattern = "*.tif$", full.names = TRUE)
  pheno_files <- pheno_files[grepl(y, pheno_files)]
  pheno_files <- pheno_files[!grepl("*90m*|*30m*", pheno_files)]
  pheno_tiles <- lapply(pheno_files, function(x) rast(x))
  pheno_ras <- terra::mosaic(sprc(pheno_tiles))
  
  # assign proper names
  # names(pheno_ras)
  names(pheno_ras) <- c("DHIcum", "DHImin", "DHIvar", "NDVI05pct", "NDVImedian", "NDVI95pct", "NDVISeasDiff")
  names(pheno_ras) <- paste0(names(pheno_ras), "_", y)
  # names(pheno_ras)
  writeRaster(pheno_ras, filename = paste0('data/spatial_data/phenology/phenology_30m_', y,'.tif'), overwrite = TRUE)
  
  # resample to 90m
  pheno_90m <- resample(pheno_ras, raster_90m, method = "bilinear", progress = TRUE)
  writeRaster(pheno_90m, filename = paste0('data/spatial_data/phenology/phenology_90m_',y,'.tif'), overwrite = TRUE)
}


for(y in years){
  temp_pheno <- rast(paste0('data/spatial_data/phenology/phenology_90m_',y,'.tif'))
  names(temp_pheno) <- paste0(names(temp_pheno), "_", y)
  writeRaster(temp_pheno, filename = paste0('data/spatial_data/phenology/phenology_90m_',y,'-new.tif'), overwrite = TRUE)
}

##### Climate #####
# Stack every year's vars (AET, CWD, PET, PPT, TMN, TMX)
raster_90m <- rast("data/spatial_data/reference_grids/reference_90m.tif")

# Process by-year
years <- c("2017", "2018", "2019", "2020", "2021")

for(y in years){
  print(y)
  
  if(y != "2021") {
    # select proper water year
    climate_files <- list.files(paste0('data/spatial_data/climate/wy_', y, '/'), pattern = "*.tif$", full.names = TRUE)
    climate_ras <- rast(climate_files)
    names(climate_ras) <- gsub("sonoma_","", names(climate_ras))
    
  } else if (y == "2021"){
    # WY 2021 does not exist - instead apply WY 2020
    climate_files <- list.files(paste0('data/spatial_data/climate/wy_2020/'), pattern = "*.tif$", full.names = TRUE)
    climate_ras <- rast(climate_files)
    names(climate_ras) <- gsub("sonoma_","", names(climate_ras))
    names(climate_ras) <- gsub("2020", "2021", names(climate_ras)) 
  }
  
  # resample to 90m
  climate_90m <- resample(climate_ras, raster_90m, method = "bilinear", progress = TRUE)
  writeRaster(climate_90m, filename = paste0('data/spatial_data/climate/climate_90m_',y,'.tif'), overwrite = TRUE)
}

##### VIIRS #####
viirs_file <- "data/spatial_data/anthropogenic/viirs_90m_annual_mean.tif"
raster_90m <- rast("data/spatial_data/reference_grids/reference_90m.tif")
viirs_ras <- rast(viirs_file)

plot(viirs_ras)
viirs_ras

# resample to 90m
viirs_90m <- resample(viirs_ras, raster_90m, method = "bilinear", progress = TRUE)
writeRaster(viirs_90m, filename = 'data/spatial_data/anthropogenic/viirs_90m.tif', overwrite = TRUE)
