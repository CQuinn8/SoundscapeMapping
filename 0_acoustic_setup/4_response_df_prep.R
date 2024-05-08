# 4_response_df_prep.R: gathers acoustic indices, soundscape components, and bird spp richness 
#    for modeling

library(tidyverse)

response_abg <- read_csv('data/acoustic_data/site_period_soundscapes.csv')
response_abg %>% 
  select(period, site) %>% 
  group_by(period) %>% 
  count()

# acoustic indices
response_ac_in <- read_csv('data/acoustic_data/site_period_acoustic_indices_190623.csv')
response_ac_in %>% 
  select(period, site) %>% 
  group_by(period) %>% 
  count()

# bird spp richness
bird_spp <- read_csv('data/acoustic_data/sonoma_s2l_predictions_SoundscapeMaxF05_230118_summarized.csv') %>%
  select(-c(Richness)) %>%
  gather(spp, count, -site) %>%
  mutate(count = ifelse(is.na(count), 0, count)) %>%
  mutate(presAbs = ifelse(count >= 3, 1, 0)) %>% # mutate new column into pres/abs if count > 3
  group_by(site) %>% # group by site 
  summarise(bird_richness = sum(presAbs))

bird_spp %>% 
  select(site) %>% 
  count()

# combine acoustic index and ABQ
response_df <- response_abg %>%
  left_join(response_ac_in) %>%
  left_join(bird_spp) %>%
  drop_na(Anthropophony, Biophony, Quiet, Geophony, bird_richness)
response_df %>% 
  select(period, site) %>% 
  group_by(period) %>% 
  count()

length(unique(response_df$site))
write_csv(response_df, "data/acoustic_data/response_df_200623.csv")


# double check sites (n = 1170) and n wavs
response_df <- read_csv("data/acoustic_data/response_df_200623.csv")
cnt_df <- read.csv("data/acoustic_data/site_period_wav_count.csv")
total_cnt_df <- read.csv("data/acoustic_data/site_wav_count.csv")
sites <- read.csv('data/final_sites_2017-2021.csv')
predictor_df <- read.csv('data/spatial_data/predictors/extracted_predictors_20230226.csv')

# filter to QC sites
response_df <- response_df[response_df$site %in% sites$sites, ]
model_df <- response_df %>%
  merge(predictor_df, by.x = 'site', by.y = 'SiteID')

model_df %>% 
  drop_na() %>% 
  select(period, site) %>% 
  group_by(period) %>% 
  count()
length(unique(model_df$site))

# number of wavs in periods-of-day
cnt_df <- cnt_df[cnt_df$site %in% model_df$site,]
length(unique(cnt_df$site))
sum(cnt_df$n)

# number of total wavs at sites
total_cnt_df <- total_cnt_df[total_cnt_df$site %in% model_df$site,]
length(unique(total_cnt_df$site))
sum(total_cnt_df$total_wavs)
mean(total_cnt_df$total_wavs)