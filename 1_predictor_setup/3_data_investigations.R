library(tidyverse)
library(gt)

# responses
response_df <- read_csv("data/acoustic_data/response_df_200623.csv")
response_df %>% 
  select(period, site) %>% 
  group_by(period) %>% 
  count()

# Predictors
predictor_df <- read.csv(paste0('data/spatial_data/predictors/extracted_predictors_20230226.csv'))

# VIF selected predictors
final_preds_df <- read.csv(paste0('data/spatial_data/predictors/final_predictors_vif3.csv'))

# prediction surface
# pred_stack <- stack(paste0(wd,'data/spatial_data/predictors/predictors_subset_2021_20230220.tif'))
# pred_names <- read_csv(paste0(wd,'data/spatial_data/predictors/predictors_subset_2021_20230220_names.csv'))
# names(pred_stack) <- pred_names$names

# sites in analysis
sites <- read.csv(paste0('data/final_sites_2017-2021.csv'))

##########################################
# Appendix : final pred table
(gt_table <- final_preds_df %>% 
  group_by(final_preds) %>%
  mutate(rn = str_c("Response", row_number())) %>%
  ungroup %>%
  pivot_wider(names_from = rn, values_from = response) %>% 
  mutate_at(c(2:9), ~replace_na(., "---")) %>% 
  arrange(final_preds) %>%  
  gt(rowname_col = "final_preds") |>
  tab_stubhead(label = "Predictor") |> 
  tab_options(table.font.size = 10,
              data_row.padding = px(3)))

gtsave(data = gt_table, filename = "final_preds_table.docx", path = "/figures/supplement/")

##########################################
# Modeling
# remove known sites with errors
response_df <- response_df[response_df$site %in% sites$sites, ]
model_df <- response_df %>%
  merge(predictor_df, by.x = 'site', by.y = 'SiteID')

responses <- c("Anthropophony", "Biophony", "Quiet", "Geophony","ACI", "H", "NDSI", "bird_richness")

# visualize hourly response
model_df %>%
  select(all_of(responses), period) %>%
  pivot_longer(names_to = "response", values_to = "value", -period) %>%
  ggplot(aes(x = period, y = value)) +
  geom_violin() +
  scale_fill_viridis_c() +
  facet_wrap(~ response, scales = "free")

# site count by period n = 1170
model_df %>%
  group_by(period) %>%
  count()

response_df %>%
  select(-site) %>% 
  group_by(period) %>% 
  drop_na() %>% 
  summarise(across(.fns = range)) %>% 
  slice(-1) 

# number of recordings
n_wavs <- read_csv("data/acoustic_data/site_avg_ABGQI_2017-2021.csv") %>% 
  select(site, wavs)
model_df %>% 
  select(site) %>% 
  left_join(n_wavs, by = c("site" = "site")) %>% 
  select(site, wavs) %>% 
  distinct() %>% 
  summarise(sum(wavs))

# Predictor count
temp <- read_csv("data/spatial_data/predictors/final_predictors_vif3.csv")
temp %>% 
  group_by(response) %>% 
  count()

temp <- read_rds("data/spatial_data/sites/clustered_folds_k10") 
temp_folds <- lapply(temp, function(x) 
  lapply(x, function(y) length(x[y])))
temp_folds %>% 
  bind_rows() %>% 
  pivot_longer(names_to = "Fold", values_to = "n_train", cols = everything()) %>% 
  mutate(prop = n_train / 1170) %>% 
  summarise(range(n_train))

# Correlation of predictors
library(corrplot)
df <- predictor_df %>% 
  select(-c(SiteID, easting, northing))

c <- cor(df, method = "spearman", use = "pairwise.complete.obs")
final_preds_temp <- final_preds_df %>% 
  filter(response == "bird_richness") %>% 
  select(final_preds) %>% pull()
c_sub <- tibble(pred = c[,colnames(c) == "rh_95_a0_90m"]) %>% 
  mutate(names = colnames(c),
    abs = abs(pred),
    in_mod = ifelse(names %in% final_preds_temp, "1", "0"))
corrplot(c, method = "color", order = "hclust", diag = FALSE,
         type = "upper", tl.col = "black", tl.cex = 0.5, tl.srt = 70,
         mar = c(1, 1, 1, 1))
dev.off()

# Bio ~ bird spp richness
cor_fx <- function(df, period){
  print(period)
  temp_df <- df %>% 
    filter(period == !!period) %>% 
    select(-c(period, site)) %>% 
    cor(method = "spearman", use = "pairwise.complete.obs")
  
  return(corrplot(temp_df, method = "color", diag = FALSE, addCoef.col = 'black',
           type = "upper", tl.col = "black"))
}

cor_fx(response_df, "dawn")
cor_fx(response_df, "midday")
cor_fx(response_df, "dusk")
cor_fx(response_df, "night")