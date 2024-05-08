# combine performance files by response x hour

library(tidyverse)
source("code/utils.R")

perf_files <- list.files('results/rf_vif3/performance/', full.names = TRUE)
modeling_params <- read_csv("./model_params_n290.csv")
responses <- unique(modeling_params$response)

model_ls <- list()
# iterate over each response
for(i in 1:length(responses)){
  temp_resp <- responses[i]
  print(temp_resp)
  
  if(temp_resp != "bird_richness"){
    periods <- c("dawn","midday","dusk","night")
  } else {
    periods <- "all_day"
  }
    
  # iterate over each period of day
  for(j in 1:length(periods)){
    temp_period <- periods[j]
    print(temp_period)
    
    # k * 10 models
    temp_files <- perf_files[grepl(temp_period, perf_files) & grepl(temp_resp, perf_files)]
    
    # read in files 
    temp_perf <- lapply(temp_files, function(x) read_csv(x, show_col_types = FALSE) %>%
                          mutate(file = tools::file_path_sans_ext(x),
                                 period = temp_period,
                                 resp = temp_resp))

    # add fold and seed cols
    if(temp_resp != "bird_richness"){
      temp_perf <- temp_perf %>%
        bind_rows() %>%
        mutate(Fold = str_split(file, "_")[[1]][6],
               seed = str_split(file, "_")[[1]][5])
    } else {
      temp_perf <- temp_perf %>%
        bind_rows() %>%
        mutate(Fold = str_split(file, "_")[[1]][8],
               seed = str_split(file, "_")[[1]][7])
      temp_period = "allday"
    }
    
    avg_df <- temp_perf %>%
      summarize_if(is.numeric, list(mean = mean, sd = sd)) %>%
      mutate(response = temp_resp,
             period = temp_period) %>%
      relocate(response, period)
  
    model_ls[[(i-1)*100+j]] <- avg_df
  }
}

performance_df <- model_ls %>%
  bind_rows() %>% 
  mutate(response = ifelse(response == "bird_richness", "Bird Richness", response))

# write_csv(performance_df, "results/rf_vif3/performance_20230623.csv")
performance_df <- read_csv("results/rf_vif3/performance_20230623.csv")

# overall rsq
performance_df %>%
  select(test_rsq_mean, tr_rsq_mean) %>%
  summarise_all(.funs = c(mean = "mean",
                          sd = "sd"))

# period-agnostic rsq
performance_df %>%
  select(response, test_rsq_mean, tr_rsq_mean) %>%
  group_by(response) %>%
  summarise_all(.funs = c(mean = "mean")) %>%
  arrange(factor(response, levels = c("Anthropophony", "Biophony", "Quiet", "Geophony", "Bird Richness","ACI", "H", "NDSI")))

# anthro specific performance
temp <- performance_df %>% 
  select(response, period, test_rsq_mean, test_rsq_sd) %>%
  filter(response == "Anthropophony")

# overall best period of day
performance_df %>% 
  select(period, test_rsq_mean) %>%
  group_by(period) %>%
  summarise_all(.funs = c(mean = "mean",
                          sd = "sd"))

# Plot the mean test Rsq with +/- sd
model_perf_gg <- function(df, dataset, eval_metric){
  temp_mean <- paste0(dataset, "_", eval_metric, "_mean")
  temp_sd <- paste0(dataset, "_", eval_metric, "_sd")
  gg <- df %>%
    dplyr::select(response, period, all_of(c(temp_mean, temp_sd))) %>%
    mutate(response = factor(response, levels = c("Anthropophony", "Biophony", "Quiet", "Geophony", 
                                                  "Bird Richness","ACI", "H", "NDSI")),
           period = factor(period, levels = c("dawn", "midday", "dusk", "night"))) %>%
    filter(response != "Bird Richness") %>% 
    ggplot(aes(x = period, y = get(temp_mean)), colour = period) +
    geom_point(size = 2, alpha = 0.7) + 
    geom_errorbar(aes(ymin = get(temp_mean) - get(temp_sd), 
                      ymax = get(temp_mean) + get(temp_sd))) +
    xlab("Period") +
    ylab(paste0(temp_mean)) +
    theme_Publication() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          panel.border = element_rect(colour = "black", fill = NA, size = 0.75))
  if(eval_metric == "rmse"){
    gg + facet_wrap(. ~ response, scales = "free", nrow = 1)
  } else {
    gg + facet_wrap(. ~ response, nrow = 1)
  }
}

# dataset: tr, test, oob
# eval metric: rsq, rmse
(gg <- model_perf_gg(df = performance_df, dataset = "test", eval_metric = "rsq"))
(gg <- gg +
  labs(y = expression(paste("Avg. test  ", R^2))))
gg
ggsave(filename = "test_rsq_performance.png", 
       plot = gg, 
       path = "./figures/",
       units = "in",
       width = 11,
       height = 5,
       dpi = 500)