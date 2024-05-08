# NOTES
# Generates cropped, masked, stack of predictors based on reference grid
#     of wall to wall data

library(raster)
library(terra)
library(ursa) #read ENVI
getwd()

# READ RASTERS (per year: n = 122; total = 621)
# 90m reference
reference_raster <- rast('data/spatial_data/reference_grids/reference_90m.tif')

# Anthropogenic (n = 15 total; 5 annual)
anthro_files <- 'data/spatial_data/anthropogenic/anthropogenic_90m.tif'
anthro_ras <- rast(anthro_files)
names(anthro_ras)

anthro_annual <- 'data/spatial_data/anthropogenic/viirs_90m.tif'
anthro_annual <- rast(anthro_annual)

# Climate (n = 24 * annual; 120 total)
climate_files <- list.files('data/spatial_data/climate/', pattern = "*.tif$", full.names = TRUE, recursive = FALSE)
climate_ras <- rast(climate_files)
names(climate_ras)

# Geomorphology (n = 6)
geomorph_files <- 'data/spatial_data/geomorphology/geomorphology_90m.tif'
geomorph_ras <- rast(geomorph_files)

# Phenology (n = 7 * annual; 35 total) 
pheno_files <- list.files('data/spatial_data/phenology/', pattern = "*.tif$", full.names = TRUE, recursive = FALSE)
pheno_files <- pheno_files[grepl("*90m*",pheno_files)]
pheno_ras <- rast(pheno_files)
names(pheno_ras)

# Structure (n = 75 * annual; 450 total)
struct_files <- paste0("data/spatial_data/structure/", c("structure_90m.tif","structure_450m.tif","structure_990m.tif"))
struct_ras <- rast(struct_files)


# PROCESS DATA
# ensure proper CRS
crs(anthro_ras, proj = TRUE) == crs(reference_raster, proj = TRUE)
crs(anthro_annual, proj = TRUE) == crs(reference_raster, proj = TRUE)
crs(climate_ras, proj = TRUE) == crs(reference_raster, proj = TRUE)
crs(geomorph_ras, proj = TRUE) == crs(reference_raster, proj = TRUE)
crs(pheno_ras, proj = TRUE) == crs(reference_raster, proj = TRUE)
crs(struct_ras, proj = TRUE) == crs(reference_raster, proj = TRUE)

# Process all rasters to minimum extent
anthro_ras <- crop(anthro_ras, reference_raster)
anthro_annnual <- crop(anthro_annual, reference_raster)
climate_ras <- crop(climate_ras, reference_raster)
geomorph_ras <- crop(geomorph_ras, reference_raster)
pheno_ras <- crop(pheno_ras, reference_raster)
struct_ras <- crop(struct_ras, reference_raster)

# stack annual and non-annual temporal resolution separately
predictor_stack <- rast(list(anthro_ras, geomorph_ras))
predictor_stack_annual <- rast(list(anthro_annual, climate_ras, pheno_ras, struct_ras))

# Create spatial predictor stack for 2021
annual_subset <- predictor_stack_annual["2021"]
names(annual_subset) <- gsub(paste0("pred_2021-06-15_|_2021|wy2021_|2021_"), "", names(annual_subset)) # clean structure, BCM, pheno names
predictor_stack_2021 <- rast(list(predictor_stack, annual_subset))
names(predictor_stack_2021)

# save predictor stack and clean up
date_stamp <- format(Sys.Date(), '%Y%m%d')
writeRaster(predictor_stack, 
            paste0('data/spatial_data/predictors/predictors_', date_stamp, '.tif'),
            overwrite = TRUE)
writeRaster(predictor_stack_annual, 
            paste0('data/spatial_data/predictors/predictors_annual_', date_stamp, '.tif'),
            overwrite = TRUE)
writeRaster(predictor_stack_2021, 
            paste0('data/spatial_data/predictors/predictors_2021_', date_stamp, '.tif'),
            overwrite = TRUE)
write_csv(tibble(group = "", predictors = names(predictor_stack_2021)), 
                 "data/spatial_data/predictors/predictor_ls.csv")
gc()