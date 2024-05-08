library(tidyverse)
source("code/utils.R")

results_files <- list.files("results/rf_vif3/processed/PDPs/response_period/", full.names = TRUE)
job_params <- read_csv("data/model_params_n290.csv") %>%
  distinct(response, period)
top_5 <- read_csv("results/rf_vif3/processed/variable_importance/top_5_preds.csv")

responses <- unique(job_params$response)
periods <- c("dawn","midday","dusk","night")

# check which jobs are complete (range == 3 hr : 8 hr)
# 8 responses with fold-specifc pdp csvs * 4 periods = 2,400
# ACI = 21, H = 26, NDSI = 25, Anthro = 25, Bio = 26, Geo = 25, Quiet = 14, bird spp = 26
job_params$n_jobs <- rep(NA, nrow(job_params))
for(i in 1:nrow(job_params)){
  
  temp_resp <- job_params$response[i]
  print(temp_resp)
  temp_period <- job_params$period[i]
  print(temp_period)
  
  # get model fit
  job_params$n_jobs[i] <- length(results_files[grepl(temp_resp, results_files) & grepl(temp_period, results_files)])
  
}

# read in pdps for each top 5
for(i in responses){
  print(i)
  top_5_temp <- top_5 %>%
    filter(response == i)
  
  temp_pdp_files <- results_files[grepl(i, results_files) & 
                                    grepl(paste0(top_5_temp$pred, collapse = "|"), results_files)]
  
  temp_pdps <- temp_pdp_files %>%
    read_csv(show_col_types = FALSE) %>% 
    mutate(pred = factor(pred, 
                         levels = top_5_temp %>% 
                           arrange(ranks_raw) %>% 
                           select(pred) %>% 
                           pull()))

  temp_gg <- temp_pdps %>%
    mutate(model = paste0(model, period)) %>% 
    ggplot(aes(x = x, y = yhat, colour = period, group = model)) +
    geom_line(alpha = 0.3) +
    facet_wrap(. ~ pred, 
               scales = "free_x", 
               nrow = 1,
               strip.position = "bottom") +
    theme_Publication() +
    theme(strip.background = element_blank(),
          strip.placement = "outside") +
    ggtitle(paste0("Top 5 overall variable importance PDPs for: ", i)) +
    labs(y = "Variable effect",
         x = NULL)
  
  temp_out <- paste0("top5_pdps_",i,".png")
  ggsave(filename = temp_out, 
         plot = temp_gg, 
         path = "",
         units = "in",
         width = 10,
         height = 5,
         dpi = 500)
}