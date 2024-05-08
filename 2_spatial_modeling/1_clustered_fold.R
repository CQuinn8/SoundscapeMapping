
library(terra)
library(sf)
library(tidyverse)
library(caret)
source("code/utils.R")

# 10 repeat CV seed initial values
seeds <- tibble(seed = c(143, 513, 386, 273, 681, 81, 178, 416, 601, 240))
periods <- tibble(period = c("dawn","midday","dusk","night"))
responses <- tibble(response = c("Anthropophony", "Biophony", "Geophony", "Quiet", "ACI", "H", "NDSI"))
folds <- tibble(Fold = paste0("Fold", seq(1, 10)))

# expand params for job ID
modeling_params <- periods %>%
  expand(periods, seeds, responses, folds)

# add bird spp richness (no period-of-day)
bird_rich <- tibble(period = rep("all_day" , 10), 
                    seeds, 
                    response = rep("bird_richness", 10)) %>% 
  expand(period, seeds, response, folds)

# combine all params
modeling_params <- modeling_params %>% 
  bind_rows(bird_rich)

write_csv(modeling_params, file = "code/2_spatial_modeling/incomplete_params.csv")

# modeling sites
response_abg <- read.csv('data/acoustic_data/site_by_hour_ABGQI_2017-2021.csv') %>%
  select(site) %>%
  distinct(site)
predictor_df <- read.csv('data/spatial_data/predictors/extracted_predictors_20230226.csv')
final_preds <- read.csv('data/spatial_data/predictors/final_predictors_vif3.csv')

# remove known sites with errors
sites <- read.csv('data/final_sites_2017-2021.csv')
response_abg <- response_abg[response_abg$site %in% sites$sites, ]
model_df <- tibble(site = response_abg) %>%
  merge(predictor_df, by.x = 'site', by.y = 'SiteID')
length(model_df$site)

# Use model params to grab seeds
# create k = 10 clustered Fold for each seed
# create separate csv with all sites and their respective fold and seed to pluck off in ranger script

# 10 geo-CV folds with 10 repeats (seeds)
k <- 10
temp_folds <- list()
seeds <- seeds$seed
for(i in 1:length(seeds)) {
  temp_seed <- seeds[i]
  print(paste0(i, " : ", temp_seed))
  set.seed(temp_seed)
  
  write_sf(site_coords, "data/spatial_data/sites/sites_1170_clustered.shp")
  
  # read in clustered shp
  site_coords <- st_as_sf(vect('data/spatial_data/sites/sites_1170_clustered.shp'))
  
  max_group <- max(site_coords$CLUSTER_ID)
  new_groups <- seq(from = max_group, to = nrow(site_coords))
  
  # assign new groups to -1 values
  cnt <- 1
  for(i in 1:nrow(site_coords)){
    if(site_coords$CLUSTER_ID[i] == -1){
      site_coords$CLUSTER_ID[i] <- new_groups[cnt]
      cnt <- cnt + 1
    }
  }
  groups <- site_coords$CLUSTER_ID
  folds <- groupKFold(groups, k = k)
  
  # training membership
  folds_i <- lapply(folds, function(i) unique(groups[i]))
  folds_i <- unname(unlist(folds_i))
  #print(table(folds_i))
  
  temp_folds[[paste0("Seed-",temp_seed)]] <- folds

  
  # create a dataframe of training/testing for reference
  for(j in 1:k){
    if(k >= 10 & j < 10){
      temp <- paste0("Fold0",j)
    } else {
      temp <- paste0("Fold",j)
    }
    fold_sites <- site_coords$SiteID[folds[temp][[1]]]
    site_coords <- site_coords %>%
      mutate(!!paste0("Fold", j) := ifelse(SiteID %in% fold_sites, "1", "0"))
  }
  
  # testing membership
  x = folds[[1]]
  folds_ts <- lapply(folds, function(x) as.numeric(row.names(site_coords[-x,])))
  folds_ts_i <- lapply(folds_ts, function(i) unique(groups[i]))
  folds_ts_i <- unname(unlist(folds_ts_i))
  #print(table(folds_ts_i))
  
  # save shp for each cvFold (n = 10 total) 
  cluster_folds_shp <- vect(site_coords)
  writeVector(cluster_folds_shp, filename = paste0("data/spatial_data/sites/clusterFolds_k",k,"_seed", temp_seed,".shp"), overwrite = TRUE)
}

temp_ln <- list()
cnt = 1
for(i in temp_folds){
  for(j in i){
    temp_ln[[cnt]] <- length(j)
    cnt = cnt + 1
  }
}

range(temp_ln)
mean(unlist(temp_ln))

# save all folds in a single RDS file
write_rds(temp_folds, paste0("data/spatial_data/sites/clustered_folds_k",k))