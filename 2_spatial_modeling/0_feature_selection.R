
library(tidyverse)
library(caret)
library(terra)
library(spatialRF)
source("code/utils.R")

# Data import
# Using site average ecoacoustic values
response_abg <- read_csv('data/acoustic_data/site_avg_ABGQI_2017-2021.csv') %>%
  select(-contains('var')) %>%
  select(site, contains('mean'), -Unidentified_mean) %>%
  rename_with(~str_remove(., '_mean'))
predictor_df <- read.csv('data/spatial_data/predictors/extracted_predictors_20230221.csv')

# acoustic indices
response_ac_in <- read_csv('data/acoustic_data/site_avg_acoustic_indices.csv') %>%
  select(site, ACI, H, NDSI)

# bird spp
bird_spp <- read_csv('data/acoustic_data/sonoma_s2l_predictions_SoundscapeMaxF05_230118_summarized.csv') %>%
  select(-Richness) %>%
  gather(spp, count, -site) %>% # convert df into long format
  mutate(presAbs = ifelse(count >= 3, 1, 0)) %>% # mutate new column into pres/abs if count > 3
  group_by(site) %>% # group by site 
  summarise(bird_richness = sum(presAbs)) 

# remove known sites with errors
sites <- read.csv('data/final_sites_2017-2021.csv')
response_abg <- response_abg[response_abg$site %in% sites$sites, ]

# combine acoustic index and ABQ
response_df <- response_abg %>%
  left_join(response_ac_in) %>%
  left_join(bird_spp) %>%
  select(-c(Interference))

##########################################
# Prep data 
model_df <- response_df %>%
  left_join(predictor_df, by = c('site'= 'SiteID')) %>%
  drop_na() # temporary

(site_counts <- model_df %>%
    count())

responses <- c("Anthropophony", "Biophony", "Quiet", "Geophony", "ACI", "H", "NDSI", "bird_richness") # NDSI_A and NDSI_B
predictors <- colnames(model_df)[!colnames(model_df) %in% c(responses, "site", "easting","northing")]

##########################################
##### Univariate RF ######################
response_ls <- list()
for(i in 1:length(responses)){
  temp_response <- responses[i]
  print("====================================")
  print(temp_response)
  
  predictor_ls <- list()
  for(j in 1:length(predictors)){
    temp_pred <- predictors[j]
    print(paste0(j, " : ", temp_pred))
    fx <- as.formula(paste0(temp_response, " ~ ", temp_pred))
    folds <- createFolds(model_df$site, k = 10)
    temp_df <- model_df %>%
      select(all_of(temp_response), all_of(temp_pred))
  
    # Fit RF k = 10 folds
    temp_fold <- list()
    for(k in 1:length(folds)){
      # subset fold
      temp_tr <- temp_df[-folds[k][[1]],]
      temp_ts <- temp_df[folds[k][[1]], ]
      ranger_fit <- train(fx,
                          data = temp_tr,
                          method = 'ranger',
                          tuneGrid = expand.grid(mtry = 1, 
                                                 splitrule = "variance", 
                                                 min.node.size = c(1,3,5,7,9,11)),
                          trControl = trainControl(method = "oob",
                                                   allowParallel = TRUE),
                          preProcess = c("center", "scale"),
                          importance = 'impurity')
      
      # Model Rsq
      pred <- predict(ranger_fit, temp_ts)
      (rsq_pred <- rsq(temp_df[temp_response][folds[k][[1]], ] %>% pull, pred))
      temp_fold[[k]] <- rsq_pred
    }
    mean_rsq <- mean(unlist(temp_fold))
    predictor_ls[[j]] <- tibble("pred" = temp_pred, "Rsq" = rsq_pred)
  
  }
  response_ls[[i]] <- predictor_ls %>%
    bind_rows() %>%
    mutate("response" = temp_response)
  
  rm(predictor_ls)
}

# create response
univar_rf <- response_ls %>%
  bind_rows() 

write_csv(univar_rf, "data/spatial_data/predictors/univariate_rf_rsq_kfold10_20220226.csv")

###################################################
##### auto VIF predictor selection for groups #####
univar_rf <- read_csv("data/spatial_data/predictors/univariate_rf_rsq_kfold10_20220226.csv")
groups <- c("anthropogenic", "climate", "geomorphology", "phenology", "structure")
predictor_groups <- read_csv("data/spatial_data/predictors/predictor_ls.csv")

vif_ls <- list()
vif_th <- 3
for(i in 1:length(groups)){
  temp_group <- groups[i]
  print(temp_group)
  temp_preds <- predictor_groups %>%
    filter(group %in% temp_group)
  
  # select group pred data
  temp_group_preds <- model_df %>%
    select(all_of(temp_preds$predictors))
  
  # order predictors within group
  temp_rsq <- univar_rf %>%
    filter(pred %in% temp_preds$predictors)
  
  temp_vif_ls <- list()
  for(j in 1:length(responses)){
    temp_response <- responses[j]
  
    temp_resp_rsq <- temp_rsq %>%
      filter(response == temp_response)
    
    temp_order <- temp_resp_rsq %>%
      arrange(desc(Rsq)) %>%
      pull(pred)

    avif <- auto_vif(x = temp_group_preds, 
                     preference.order = temp_order,
                     vif.threshold = vif_th,
                     verbose = FALSE)
    temp_vif_ls[[j]] <- tibble(response = temp_response,
                            group = temp_group,
                            selected_preds = avif$selected.variables)
  }
  
  vif_ls[[i]] <- temp_vif_ls %>%
    bind_rows()
}

vif_df <- vif_ls %>%
  bind_rows()

#####################################################
##### auto VIF predictor selection for response #####
by_group_vif <- TRUE
final_vif_ls <- list()
for(i in 1:length(responses)){
  temp_response <- responses[i]
  print(temp_response)
  
  if(by_group_vif) {
    # response group VIF
    temp_group_vif <- vif_df %>%
      filter(response == temp_response)
  
    # subset dataset
    temp_preds <- model_df %>%
      select(all_of(temp_group_vif$selected_preds))
  } else { 
    temp_preds <- model_df %>%
      select(all_of(predictors))
  }
  
  # order predictors within response
  temp_rsq <- univar_rf %>%
    filter(response == temp_response) %>%
    filter(pred %in% all_of(colnames(temp_preds))) %>%
    arrange(desc(Rsq)) %>%
    pull(pred)
  
  # auto vif with more conservative threshold
  avif <- auto_vif(x = temp_preds, 
                   vif.threshold = 7,
                   preference.order = temp_rsq,
                   verbose = FALSE)
  
  # number of preds dropped
  print(paste0(ncol(temp_preds), " to ", length(avif$selected.variables),  " predictors"))
  
  final_vif_ls[[i]] <- tibble(response = temp_response,
                              final_preds = avif$selected.variables)
  
}

final_preds <- final_vif_ls %>%
  bind_rows()

summary_table <- final_preds %>%
  group_by(final_preds) %>%
  mutate(rn = str_c("Response", row_number())) %>% 
  ungroup %>%
  pivot_wider(names_from = rn, values_from = response) %>%
  full_join(tibble(pred = predictors), by = c("final_preds" = "pred")) %>%
  left_join(predictor_groups, by = c("final_preds" = "predictors"))

write_csv(final_preds, "data/spatial_data/predictors/final_predictors_vif3.csv")
write_csv(summary_table, "data/spatial_data/predictors/final_predictors_summary_table_vif3.csv")

predictor_groups %>%
  group_by(group) %>%
  count()
final_preds %>%
  left_join(predictor_groups, by = c("final_preds" = "predictors")) %>%
  group_by(response, group) %>%
  count() %>%
  ggplot(aes(x = response, y = n, fill = group)) +
    geom_col(position = "dodge")


summary_table = read_csv('data/spatial_data/predictors/final_predictors_summary_table_vif3.csv')
na_pred <- summary_table %>%
  filter(is.na(Response1)) %>%
  select(final_preds, group)

#####################################
##### VIF thin out raster stack #####
preds <- read_csv("data/spatial_data/predictors/final_predictors_vif3.csv")
full_pred_stack <- terra::rast('data/spatial_data/predictors/predictors_2021_20230223.tif')

unique_preds <- unique(preds$final_preds) # n = 67

# 2021 stack
subset_stack <- full_pred_stack[[unique_preds]]
terra::writeRaster(subset_stack, 'data/spatial_data/predictors/predictors_subset_2021_20230226.tif', overwrite = TRUE)
temp_names <- tibble(names = names(subset_stack))
write_csv(temp_names, 'data/spatial_data/predictors/predictors_subset_2021_20230226_names.csv')

# Create spatial predictor sub stack for 2017-2020
date_in <-  "20230223"
date_out <- "20230226"
predictor_stack_annual <- rast(paste0('data/spatial_data/predictors/predictors_annual_', date_in, '.tif'))
predictor_stack <- rast(paste0('data/spatial_data/predictors/predictors_', date_in, '.tif'))

years <- c("2017", "2018", "2019", "2020")
for(y in years){
  print(y)
  # subset annual raster layers
  annual_subset <- predictor_stack_annual[y]
  names(annual_subset) <- gsub(paste0("pred_",y,"-06-15_|_",y,"|wy",y,"_|",y,"_"), "", names(annual_subset))
  
  # combine annual layers and constant layers
  predictor_stack_yyyy <- rast(list(predictor_stack, annual_subset))
  subset_stack <- predictor_stack_yyyy[[unique_preds]]
  print(names(subset_stack))
  
  # save predictor stack and clean up
  writeRaster(subset_stack, 
              paste0('data/spatial_data/predictors/predictors_subset_', y,'_', date_out, '.tif'),
              overwrite = TRUE)
  gc()
}
