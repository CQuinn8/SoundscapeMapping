# Random Forest model to predict ABGQ from spatial stack
# Created: 15-Nov-2022
# Author: Colin Quinn
# Project: Soundscapes 2 Landscapes
# NAU - SICCS

args <- commandArgs(TRUE)
slurm_id <- args[1]
slurm_id <- as.integer(slurm_id) 
wd <- ""
in_k <- 10
vif <- 3
date_in <- "20230226"

##########################################
# Libraries
# ll = '/home/cq73/R/4.1.2/'
ll = "/projects/tropics/users/cquinn/conda_env/geospatial-cq/lib/R/library"
.libPaths(c( .libPaths(), ll))

library(ranger)    # RF implementation
library(caret)     # ML help
library(terra)     # geospatial data
library(raster)    # gridded geospatial data
library(sf)        # geospatial data
library(dplyr)     # general data processing
library(data.table)
library(tidyr)
library(tibble)
library(readr)
library(stringr)

source(paste0(wd,"code/utils.R"))

# specifies manually setting the cores
options(future.availablecores.methods = "mc.cores")

# SLURM_CPUS_PER_TASK is the amount of cores specified in the job environment
options(mc.cores = Sys.getenv("SLURM_CPUS_PER_TASK"))

##########################################
# Data setup: modeling_params consists of table with geoCV seed, period-of-day, response, and fold 1:10
modeling_params <- read_csv(paste0(wd,"modeling_params.csv"))[slurm_id,]

# Subset to HPC iteration
cv_seed <- as.integer(modeling_params$seed %>% str_replace("seed", ""))
target_time <- modeling_params$period
response <- modeling_params$response
fold <- as.integer(modeling_params$Fold %>% str_replace("Fold", ""))

print(slurm_id)
print(paste0("CV seed: ", cv_seed))
print(paste0("period: ", target_time))
print(paste0("response: ", response))
print(paste0("Fold: ", fold))

##########################################
# Data import
if(response != "bird_richness"){
  response_df <- read_csv(paste0(wd,"data/acoustic_data/response_df_200623.csv")) %>%
    filter(period == target_time)
} else {
  response_df <- read_csv(paste0(wd,"data/acoustic_data/response_df_200623.csv")) %>%
    filter(period == "dawn") %>% 
    mutate(period = ifelse(period == "dawn", "all_day", period))
}

# Predictors
predictor_df <- read.csv(paste0(wd,'data/spatial_data/predictors/extracted_predictors_', date_in,'.csv'))

# VIF selected predictors
final_preds <- read.csv(paste0(wd,'data/spatial_data/predictors/final_predictors_vif', vif,'.csv'))

# Fold 
folds <- read_rds(paste0(wd,'data/spatial_data/sites/clustered_folds_k', in_k))[paste0("Seed-",cv_seed)][[1]]
  
# prediction surface
pred_stack <- stack(paste0(wd,'data/spatial_data/predictors/predictors_subset_2021_', date_in,'.tif'))
pred_names <- read_csv(paste0(wd,'data/spatial_data/predictors/predictors_subset_2021_', date_in,'_names.csv'))
names(pred_stack) <- pred_names$names

# sites in analysis
sites <- read.csv(paste0(wd,'data/final_sites_2017-2021.csv'))

##########################################
# Modeling
# remove known sites with errors
response_df <- response_df[response_df$site %in% sites$sites, ]
model_df <- response_df %>%
  merge(predictor_df, by.x = 'site', by.y = 'SiteID')

# filter VIF preds to the current response 
temp_vif_preds <- final_preds %>%
  filter(response == !!response)
temp_model_df <- model_df %>%
  select(all_of(c(response, temp_vif_preds$final_preds)))
pred_stack <- raster::subset(pred_stack, as.numeric(which(names(pred_stack) %in% temp_vif_preds$final_preds)))

print("read in all data")
print(paste0("Model rows:", nrow(model_df)))

# subset to current fold
print(paste0("Fold: ", fold))
k = fold

# use Fold ID to separate training and test and drop any NA values in the response
training_df <- temp_model_df[folds[[k]], ] %>%
  drop_na(response)
testing_df <- temp_model_df[-folds[[k]], ] %>%
  drop_na(response)
fx <- as.formula(paste0(response, " ~ ", paste0(names(training_df)[-c(1:2)], collapse = " + "))) # by hour

# Fit RF
print("Fitting RF")
tgrid <- expand.grid(mtry = 2:(sqrt(nrow(temp_vif_preds))+1),
                     splitrule = "variance",
                     min.node.size = c(3,5,7,9,11,13))

ranger_fit <- train(fx,
                    data = training_df,
                    method = 'ranger',
                    num.trees = 500,
                    trControl = trainControl(method = "repeatedCV",
                                             number = 5,
                                             repeats = 3,
                                             allowParallel = TRUE),
                    preProcess = c("center", "scale"),
                    tuneGrid = tgrid,
                    num.threads = 2, 
                    importance = 'permutation')

print(ranger_fit)

# model prediction
test_pred <- predict(ranger_fit, testing_df)
train_pred <- predict(ranger_fit, training_df)

print("Prediction surface")
# spatial prediction
prediction_surface <- raster::predict(object = pred_stack, model = ranger_fit)

print("Performance metrics")
# performance metrics
performance_df <- tibble(
  # MSE 
  test_mse = mean((test_pred - testing_df[response])^2 %>% pull),
  tr_mse = mean((train_pred - training_df[response])^2 %>% pull),
  oob_mse = ranger_fit$finalModel$prediction.error,
  
  # RMSE
  test_rmse = sqrt(test_mse),
  tr_rmse = sqrt(tr_mse),
  
  # MAE 
  test_mae = mean((test_pred - (testing_df[response] %>% pull))),
  tr_mae = mean((train_pred - (training_df[response] %>% pull))),
  
  # R-squared
  test_rsq = rsq(testing_df[response] %>% pull, test_pred),
  tr_rsq = rsq(training_df[response] %>% pull, train_pred),
  oob_rsq = ranger_fit$finalModel$r.squared
)

print("Variable Importance")
# Variable Importance
varImps <- varImp(ranger_fit)$importance %>%
  mutate(pred = rownames(.)) %>%
  remove_rownames()
varImps_raw <- varImp(ranger_fit, scale = F)$importance %>%
  mutate(pred = rownames(.)) %>%
  rename(raw_imp = Overall) %>%
  remove_rownames()

# save loop-specific products
# loop ID for files
print("Saving objects")
id <- paste0(response, 
             "_",target_time,
             "_seed", cv_seed,
             "_Fold", fold)
date_stamp <- format(Sys.Date(), '%Y%m%d')

# fit object rds
print("fit")
write_rds(ranger_fit, paste0(wd, "results/rf_vif", vif,"/fit_objects/", date_stamp, "_", id))

# model performance csv
print("performance")
write_csv(performance_df, paste0(wd, "results/rf_vif", vif,"/performance/", date_stamp, "_", id, ".csv"))

# tr/test data rds
print("tr/test")
data_out <- list("test_y_hat" = test_pred,
                 "test_y" = testing_df[response],
                 "train_y" = training_df[response])
write_rds(data_out, paste0(wd,"results/rf_vif", vif,"/pred_data/", date_stamp, "_", id))

# prediction surface tif
print("surface")
raster::writeRaster(prediction_surface, filename = paste0(wd,"results/rf_vif", vif,"/prediction_surfaces/2021/", date_stamp, "_", id, ".tif"), overwrite = TRUE)

# variable importance csv
varImp_out <- varImps %>%
  full_join(varImps_raw, by = "pred") %>%    
  mutate(response = response,
         period = target_time,
         cvSeed = cv_seed,
         fold = fold) %>%
  relocate(response, period, pred)

print("varimp")
write_csv(varImp_out, file = paste0(wd,"results/rf_vif", vif,"/variable_importance/", date_stamp, "_", id, ".csv"))
