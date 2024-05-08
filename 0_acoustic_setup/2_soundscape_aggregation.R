library(tidyverse)
library(lubridate)

# read in site-specific sun times and format df
suntimes_df <- read.csv("data/acoustic_data/suntimes.csv") %>%
  mutate(YY = as.numeric(YY),
         MM = as.numeric(MM),
         DD = as.numeric(DD),
         nightEnd = as.POSIXct(nightEnd, tz = "US/Pacific"),
         solarNoon = as.POSIXct(solarNoon, tz = "US/Pacific"),
         nauticalDusk = as.POSIXct(nauticalDusk, tz = "US/Pacific"),
         nadir = as.POSIXct(nadir, tz = "US/Pacific"))
by_min_csvs <- list.files("data/acoustic_data/soundscapes/", full.names = TRUE)

# Subset time blocks
# clean up any sites with errors
unique_sites <- unique(suntimes_df$SiteID)
avg_ls <- list()
cnt_ls <- list()
total_cnt_ls <- list()
tod_ls <- list()
options(dplyr.summarise.inform = FALSE)
for(i in seq_along(by_min_csvs)){
  temp_site <- read.csv(by_min_csvs[i])
  temp_site_name <- unique(temp_site$site)
  print(paste0(i," : ", temp_site_name))
  total_cnt_ls[[i]] <- tibble(site = temp_site_name, total_wavs = length(unique(temp_site$wav)))
  
  # create DDMMHH column based on wav name
  temp_site$YYYY <- as.numeric(paste0("20", substr(temp_site$site, 10, 11)))
  temp_site$DD <- as.numeric(substr(temp_site$wav, 25, 26))
  temp_site$MM <- as.numeric(substr(temp_site$wav, 22, 23))
  temp_site$hh <- as.numeric(substr(temp_site$wav, 28, 29))
  temp_site$mm <- as.numeric(substr(temp_site$wav, 31, 32))
  
  temp_df <- temp_site %>% 
    mutate(rec_date = ymd_hm(paste0(YYYY, "-", MM, "-", DD, " ", hh, ":", mm), tz = "US/Pacific")) %>%
    left_join(suntimes_df, by = c("site" = "SiteID", "YYYY" = "YY", "MM", "DD")) %>%
    select(-c(mfcc, Interference, date, lon, lat))
  
  # dawn : -1 hour to +3 hours
  # midday : -2 hour to +2 hour
  # dusk :  -3 hour to +1 hour
  # night : -2 hour to +2 hour
  periods <- temp_df %>%
    mutate(dawn = ifelse(between(as.numeric(rec_date - nightEnd, units = "secs"), -3600, 10800), 1, 0),
           midday = ifelse(between(as.numeric(rec_date - solarNoon, units = "secs"), -7200, 7200), 1, 0),
           dusk = ifelse(between(as.numeric(rec_date - nauticalDusk, units = "secs"), -10800, 3600), 1, 0),
           night = ifelse(!between(hh, 2, 21), 1, 0))
  
  # Count files in each period
  cnt_ls[[i]] <- periods %>% 
    select(wav, dawn, midday, dusk, night) %>%
    pivot_longer(values_to = "bin", names_to = "period", -wav) %>% 
    distinct() %>% 
    filter(bin == 1) %>% 
    group_by(period) %>% 
    count() %>% 
    mutate(site = temp_site_name)
  
  # create averages for each period
  # dawn
  dawn <- periods %>%
    select(dawn, Anthropophony, Biophony, Geophony, Quiet) %>%
    filter(dawn == 1) %>%
    summarise(across(.fns = mean)) %>%
    mutate(site = temp_site_name,
           period = "dawn") %>%
    select(-dawn)

  # midday
  midday <- periods %>%
    select(midday, Anthropophony, Biophony, Geophony, Quiet) %>%
    filter(midday == 1) %>%
    summarise(across(.fns = mean)) %>%
    mutate(site = temp_site_name,
           period = "midday") %>%
    select(-midday)

  # dusk
  dusk <- periods %>%
    select(dusk, Anthropophony, Biophony, Geophony, Quiet) %>%
    filter(dusk == 1) %>%
    summarise(across(.fns = mean)) %>%
    mutate(site = temp_site_name,
           period = "dusk") %>%
    select(-dusk)

  # night
  night <- periods %>%
    select(night, Anthropophony, Biophony, Geophony, Quiet) %>%
    filter(night == 1) %>%
    summarise(across(.fns = mean)) %>%
    mutate(site = temp_site_name,
           period = "night") %>%
    select(-night)

  avg_ls[[i]] <- dawn %>%
    bind_rows(midday, dusk, night) %>%
    relocate(site)
  
  
  # TODs
  tod_dawn <- periods %>%
    distinct(wav, .keep_all = TRUE) %>% 
    filter(dawn == 1)  %>% 
    select(site, rec_date) %>% 
    mutate(tod = "dawn")
  tod_midday <- periods %>%
    distinct(wav, .keep_all = TRUE) %>% 
    filter(midday == 1) %>% 
    select(site, rec_date) %>% 
    mutate(tod = "midday")
  tod_dusk <- periods %>%
    distinct(wav, .keep_all = TRUE) %>% 
    filter(dusk == 1) %>% 
    select(site, rec_date) %>% 
    mutate(tod = "dusk")
  tod_night <- periods %>%
    distinct(wav, .keep_all = TRUE) %>% 
    filter(night == 1) %>% 
    select(site, rec_date) %>% 
    mutate(tod = "night")
  
  tod_ls[[i]] <- tod_dawn %>% 
    bind_rows(tod_midday, tod_dusk, tod_night)
}

avg_df <- avg_ls %>%
  bind_rows() %>%
  drop_na()

cnt_df <- cnt_ls %>%  
  bind_rows()

total_cnt_df <- total_cnt_ls %>% 
  bind_rows()

tod_df <- tod_ls %>% 
  bind_rows

avg_df %>%
  drop_na() %>%
  pivot_longer(names_to = "index", values_to = "value", -c(site, period)) %>%
  ggplot(aes(x = period, y = value)) +
  geom_violin() +
  facet_wrap(. ~ index, scales = "free")

write_csv(tod_df, "data/acoustic_data/tod_df.csv")
write_csv(total_cnt_df, "data/acoustic_data/site_wav_count.csv")
write_csv(cnt_df, "data/acoustic_data/site_period_wav_count.csv")
write_csv(avg_df, "data/acoustic_data/site_period_soundscapes.csv")



###################################
avg_df <- read_csv("data/acoustic_data/site_period_soundscapes.csv")

# 12 sites each
avg_df %>%
  group_by(period) %>%
  drop_na() %>%
  count()
# A tibble: 4 × 2
# Groups:   period [4]
# 1 dawn    1243
# 2 dusk    1245
# 3 midday  1245
# 4 night   1258

####################################
# TOD
temp <- tod_df %>% 
  group_by(tod) %>% 
  summarise(range(rec_date))
