
library(tidyverse)
library(scales)
source("code/utils.R")

responses <- c("Anthropophony", "Biophony", "Geophony","Quiet", "bird_richness", "ACI", "H", "NDSI")

# Variable rankings using raw permutation for predictors
varimp_ranks <- list.files("results/rf_vif3/processed/variable_importance/", pattern = "^method2_*", full.names = TRUE)
varimps <- list()
for(i in responses){
  print(i)
  temp_files <- varimp_ranks[grepl(i, varimp_ranks)]
  
  temp_varimp <- temp_files %>%
    read_csv(col_types = "") %>%
    bind_rows()
  
  varimps[[i]] <- temp_varimp
}
varimps <- varimps %>%
  bind_rows()

# add group context
predictor_groups <- read_csv("data/spatial_data/predictors/predictor_ls.csv")
varimps <- varimps %>%
  left_join(predictor_groups, by = c("pred" = "predictors"))

# Manually look at VarImp using raw values (Table 3)
temp <- varimps %>% 
  filter(response == "Anthropophony")
temp <- varimps %>% 
  filter(response == "Biophony")
temp <- varimps %>% 
  filter(response == "Geophony")
temp <- varimps %>% 
  filter(response == "Quiet")

temp <- varimps %>% 
  filter(response == "bird_richness")

temp <- varimps %>% 
  filter(response == "ACI")
temp <- varimps %>% 
  filter(response == "H")
temp <- varimps %>% 
  filter(response == "NDSI")

# Top 5 Variable importance plot with by-group fill
varimps %>%
  group_by(response) %>%
  arrange(desc(mean_raw)) %>%
  slice(1:5) %>% 
    ggplot(aes(x = pred, y = mean_raw, group = response, fill = group)) +
    geom_bar(stat="identity", color="black", position = position_dodge(), alpha = 0.6) +
    geom_errorbar(aes(ymin = mean_raw - sd_raw, ymax = mean_raw + sd_raw), width = .2, position = position_dodge(.9)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    labs(title = "Random Forest variable importance raw permutation value",
         subtitle = 'metrics from 100 folds',
         y = "Avg permutation importance") +
    facet_wrap(~response, scales = "free")

# save top 5 preds for each responses
varimps %>%
  group_by(response) %>%
  arrange(desc(mean_raw)) %>%
  slice(1:5) %>%
  write_csv("results/rf_vif3/processed/variable_importance/top_5_preds.csv")

# grouped importance
varimps %>%
  group_by(response, group) %>%
  select(response, group, mean_raw) %>%
  summarise(mean_group = mean(mean_raw),
            sd_group = sd(mean_raw)) %>%
  ggplot(aes(x = group, y = mean_group, group = response, fill = group)) +
  geom_bar(stat="identity", color="black", position = position_dodge(), alpha = 0.6) +
  geom_errorbar(aes(ymin = mean_group - sd_group, ymax = mean_group + sd_group), width = .2, position = position_dodge(.9)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(title = "Random Forest variable importance raw permutation value",
       subtitle = 'metrics from 100 folds',
       y = "Avg permutation importance") +
  facet_wrap(~response, scales = "free")

############################################
# Grouped prevalence in top 5
varimp_ranks <- list.files("results/rf_vif3/processed/variable_importance/", pattern = "^method1_*", full.names = TRUE)

varimps <- list()
for(i in responses){
  print(i)
  temp_files <- varimp_ranks[grepl(i, varimp_ranks)]
  
  temp_varimp <- temp_files %>%
    read_csv(col_types = "") %>%
    bind_rows()
  
  varimps[[i]] <- temp_varimp
}
varimps <- varimps %>%
  bind_rows() %>%
  select(-n_top)

# Variable importance plot (Figure 4)
(temp_gg <- varimps %>%
  rename("Top-five" = prevalence_top5, VIF = prevalence_total) %>% 
  mutate(response = ifelse(response == "bird_richness", "Bird Richness", response),
         group = case_when(group == "anthropogenic" ~ "Anthropogenic",
                          group == "climate" ~ "Climate",
                          group == "geomorphology" ~ "Geomorphology",
                          group == "phenology" ~ "Phenology",
                          group == "structure" ~ "Structure",
                          TRUE ~ group),
         response = factor(response, levels = c("Anthropophony", "Biophony", "Quiet", "Geophony", 
                                                "Bird Richness","ACI", "H", "NDSI"))) %>%
  pivot_longer(cols = c("Top-five", "VIF"),
               names_to = "prevalence",
               values_to = "value") %>% 
  group_by(response) %>%
  ggplot(aes(x = value, y = prevalence, fill = group)) +
  geom_bar(stat = "identity") +
  theme_Publication() +
    discrete_scale("fill", "Publication",
                   manual_pal(values = c("#F27314", # Anthro - orange
                                         "#386cb0", # climate - blue
                                         "#fdb462", # Geomorph - yellow
                                         "#7fc97f", # Pheno - green
                                         "#984ea3"  # Struct - purple
                                         ))) +
  labs(title = "Prevalence predictor groups in top 5",
       x = "Percent of top 5 predictors",
       y = "") +
  theme(legend.title = element_blank()) +
  scale_x_continuous(labels = scales::percent) +
  facet_wrap(. ~ response, 
             ncol = 2))

# save figure to local dir
ggsave(filename = "predictor_prevalence.png", 
       plot = temp_gg, 
       path = "",
       units = "in",
       width = 8,
       height = 5,
       dpi = 500)

# code to investigate top5 patterns
# difference in top 5
temp <- varimps %>%
  mutate(top5_prev = prevalence_top5 - prevalence_total) %>% 
  group_by(response) %>% 
  arrange(desc(prevalence_top5))

# most prevalent group
varimps %>%
  group_by(response) %>% 
  arrange(desc(prevalence_top5)) %>% 
  slice(1)

# average top5 prevalence by group
varimps %>%
  group_by(group) %>% 
  select(-response) %>% 
  mutate(prevalence_top5 = ifelse(is.na(prevalence_top5), 0, prevalence_top5)) %>% 
  summarise(mean(prevalence_top5))

# average overall prevalence by group
varimps %>%
  group_by(group) %>% 
  select(-response) %>% 
  mutate(prevalence_total = ifelse(is.na(prevalence_total), 0, prevalence_total)) %>% 
  summarise(mean(prevalence_total))
