library(pdp)
library(stringr)
library(readr)
library(dplyr)
library(tibble)
library(tidyr)
library(data.table)

args <- commandArgs(TRUE)
slurm_id <- args[1]
slurm_id <- as.integer(slurm_id)
wd <- ""

results <- list.files(paste0(wd, 'results/rf_vif3/fit_objects/'), full.names = T)
modeling_params <- read_csv(paste0(wd, "data/model_params_n290.csv"))
job_params <- modeling_params %>% 
  select(response, period) %>% 
  distinct()

temp_resp <- job_params$response[slurm_id]
print(temp_resp)
temp_period <- job_params$period[slurm_id]
print(temp_period)

# get model fit
temp_files <- results[grepl(temp_resp, results) & grepl(temp_period, results)]

all_list <- list()
for(i in seq_along(temp_files)){
  temp_out <- paste0(wd, "results/rf_vif3/processed/PDPs/response_period/", temp_resp, "_", temp_period, "-", i, ".csv")
  
  if(file.exists(temp_out)){
    print(paste0("Model ",i, " already computed"))
    temp_df <- read_csv(temp_out, show_col_types = FALSE)
    
  } else {
    temp_file <- temp_files[i]
    temp_fit <- readRDS(temp_file)
    
    # get the predictors
    pred_vars <- temp_fit$finalModel$xNames
    print(paste0(i, " : " , length(temp_files)))
    
    # cycle over each predictor to get PDP
    temp_list <- list()
    for(j in seq_along(pred_vars)){
      # print(paste0("Pred:", j))
      temp_pred <- pred_vars[j]
      
      # get predictor range in model 
      x_range <- temp_fit$trainingData %>%
        dplyr::select(all_of(temp_pred)) %>%
        summarise(min = min(.),
                  max = max(.))
      
      # generate 100 values along predictor range
      x_df <- tibble(!!(temp_pred) := seq(from = x_range$min, to = x_range$max, length = 100))
      
      # generate single predictor pdp with range of x values and restrict to non-extrapolation (redundant)
      temp_pdp <- pdp::partial(temp_fit, pred.var = temp_pred, pred.grid = x_df, chull = TRUE)
      
      # store in list
      temp_list[[j]] <- temp_pdp %>%
        rename(x := !!(temp_pred)) %>%
        mutate(pred = temp_pred,
               response = temp_resp,
               period = temp_period,
               model = i)
  
    }
    temp_df <- temp_list %>%
      bind_rows()
      
    write_csv(temp_df, temp_out)
  }
  
  all_list[[i]] <- temp_df
}

df <- all_list %>%
  bind_rows()

# get unique preds to save a csv for each
unique_preds <- unique(df$pred)

for(i in seq_along(unique_preds)){
  temp_pred <- unique_preds[i]
  
  temp_df <- df %>%
    filter(pred == temp_pred)
  write_csv(temp_df, paste0(wd, "results/rf_vif3/processed/PDPs/response_period/",
                            temp_resp, "_", temp_period, "_", temp_pred, ".csv"))
  
}

