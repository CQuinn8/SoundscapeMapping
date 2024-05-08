# Variable importance by response not accounting for period
library(stringr)
library(readr)
library(dplyr)
library(tibble)
library(data.table)

args <- commandArgs(TRUE)
slurm_id <- args[1]
slurm_id <- as.integer(slurm_id)
wd <- ""

# gather variable importance csvs and modeling parameters
varimp_files <- list.files(paste0(wd, 'results/rf_vif3/variable_importance/'), full.names = TRUE)
modeling_params <- read_csv(paste0(wd, "data/model_params_n290.csv"))

# read in predictor groupings csv
predictor_groups <- read_csv(paste0(wd, "data/spatial_data/predictors/predictor_ls.csv"))
responses <- c("Anthropophony", "Biophony", "Geophony","Quiet", "bird_richness", "ACI", "H", "NDSI")

# based on job ID work on one response
temp_resp <- responses[slurm_id]
print(temp_resp)

# k * 10 models * 4 periods == 400 models or 100 for bird spp richness
temp_files <- varimp_files[grepl(temp_resp, varimp_files)]

# read in files 
temp_varimp <- lapply(temp_files, function(x) read_csv(x, show_col_types = FALSE) %>%
                      mutate(file = x))
                      
# Method 1: order varimp within model
# combine all model varimps to a single df
varimp_ranked <- temp_varimp %>%
  bind_rows() %>%
  group_by(period, cvSeed, fold) %>%
  mutate(ranks = dense_rank(desc(Overall))) # use model ID to create rankings based on scaled [0-100] importance

# add group context
varimp_ranked <- varimp_ranked %>%
  left_join(predictor_groups, by = c("pred" = "predictors"))
  
# count the number of times a group is present in the top 5 of each model
varimp_group <- varimp_ranked %>% 
  ungroup() %>% 
  filter(ranks <= 5) %>%  # select only ranks 1-5
  mutate(n_top = n()) %>% # total top5 preds
  select(response, period, ranks, group, n_top) %>%
  group_by(group, n_top) %>%
  count(group) %>% # count each groups occurrence in top5
  mutate(prevalence_top5 = n / n_top) %>% 
  select(-n)

# get prevalence of groups in VIF pred final set
varimp_prev <- varimp_ranked %>% 
  ungroup() %>% 
  select(response, pred, group) %>% 
  distinct() %>% 
  count(group) %>% 
  mutate(prevalence_total = n / sum(n)) %>% 
  select(-n)

# join dataframes 
varimp_group_prev <- varimp_group %>% 
  right_join(varimp_prev, by = "group") %>% 
  mutate(response = temp_resp) %>% 
  relocate(response)


# Method 2: get average overall importance without considering model average and +/-
varimp_avg_value <- varimp_ranked %>%
  group_by(pred) %>% 
  summarise(med_raw = median(raw_imp),
            mean_raw = mean(raw_imp),
            sd_raw = sd(raw_imp),
            med_rel = median(Overall),
            mean_rel = mean(Overall),
            sd_rel = sd(Overall))  %>%
  mutate(response = temp_resp) %>%
  mutate(ranks_raw = dense_rank(desc(mean_raw)),
         ranks_rel = dense_rank(desc(mean_rel)))


write_csv(varimp_group_prev, paste0(wd, "results/rf_vif3/processed/variable_importance/method1_ranked_varimp_", temp_resp, ".csv"))
write_csv(varimp_avg_value, paste0(wd, "results/rf_vif3/processed/variable_importance/method2_raw_varimp_", temp_resp, ".csv"))
