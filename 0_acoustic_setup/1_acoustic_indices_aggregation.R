library(tidyverse)
library(lubridate)
source("code/utils.R")

# read in site-specific sun times and format df
suntimes_df <- read.csv("data/acoustic_data/suntimes.csv") %>%
  mutate(YY = as.numeric(YY),
         MM = as.numeric(MM),
         DD = as.numeric(DD),
         nightEnd = as.POSIXct(nightEnd, tz = "US/Pacific"),
         solarNoon = as.POSIXct(solarNoon, tz = "US/Pacific"),
         nauticalDusk = as.POSIXct(nauticalDusk, tz = "US/Pacific"),
         nadir = as.POSIXct(nadir, tz = "US/Pacific"))

by_min_csvs <- list.files("data/acoustic_data/acoustic_indices/by_min_site_csvs/", full.names = TRUE)

# Subset time blocks
unique_sites <- unique(suntimes_df$SiteID)
avg_ls <- list()
for(i in seq_along(unique_sites)){
  print(i)
  temp_site_name <- unique_sites[i]
  temp_site <- read.csv(paste0("data/acoustic_data/acoustic_indices/by_min_site_csvs/", temp_site_name, ".csv")) %>%
    mutate(YYYY = as.numeric(YYYY),
           MM = as.numeric(MM),
           DD = as.numeric(DD))
  
  temp_df <- temp_site %>% 
    mutate(rec_date = ymd_hm(paste0(YYYY, "-", MM, "-", DD, " ", hh, ":", mm), tz = "US/Pacific")) %>%
    left_join(suntimes_df, by = c("site" = "SiteID", "YYYY" = "YY", "MM", "DD")) %>%
    select(-c(zcr_min, zcr_max, date, lon, lat))
  
  # dawn : -1 hour to +3 hours
  # midday : -2 hour to +2 hour
  # dusk :  -3 hour to +1 hour
  # night : -2 hour to +2 hour
  periods <- temp_df %>%
    mutate(dawn = ifelse(between(as.numeric(rec_date - nightEnd), -3600, 10800), 1, 0),
           midday = ifelse(between(as.numeric(rec_date - solarNoon), -7200, 7200), 1, 0),
           dusk = ifelse(between(as.numeric(rec_date - nauticalDusk), -10800, 3600), 1, 0),
           night = ifelse(!between(hh, 2, 21), 1, 0))
  
  # create averages for each period
  # dawn
  dawn <- periods %>%
    select(dawn, ACI, H, NDSI) %>%
    filter(dawn == 1) %>%
    summarise(across(.fns = mean)) %>%
    mutate(site = temp_site_name,
           period = "dawn") %>%
    select(-dawn)
  
  # midday
  midday <- periods %>%
    select(midday, ACI, H, NDSI) %>%
    filter(midday == 1) %>%
    summarise(across(.fns = mean)) %>%
    mutate(site = temp_site_name,
           period = "midday") %>% 
    select(-midday)
  
  # dusk 
  dusk <- periods %>%
    select(dusk, ACI, H, NDSI) %>%
    filter(dusk == 1) %>%
    summarise(across(.fns = mean)) %>%
    mutate(site = temp_site_name,
           period = "dusk") %>% 
    select(-dusk)
  
  # night
  night <- periods %>%
    select(night, ACI, H, NDSI) %>%
    filter(night == 1) %>%
    summarise(across(.fns = mean)) %>%
    mutate(site = temp_site_name,
           period = "night") %>% 
    select(-night)
  
  avg_ls[[i]] <- dawn %>%
    bind_rows(midday, dusk, night) %>%
    relocate(site)
}

avg_df <- avg_ls %>%
  bind_rows() %>% 
  drop_na()

avg_df %>%
  drop_na() %>%
  pivot_longer(names_to = "index", values_to = "value", -c(site, period)) %>%
  ggplot(aes(x = period, y = value)) +
  geom_violin() +
  facet_wrap(. ~ index, scales = "free")

temp <- read_csv("data/acoustic_data/site_period_acoustic_indices.csv")

write_csv(avg_df, "data/acoustic_data/site_period_acoustic_indices_190623.csv")

avg_df %>%
  group_by(period) %>%
  drop_na() %>%
  count()