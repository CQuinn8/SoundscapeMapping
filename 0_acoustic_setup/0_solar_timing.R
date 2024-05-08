
library(tidyverse)
library(terra)
library(suncalc)
library(lubridate)
library(sf)

# by min csvs
by_min_csvs <- list.files("data/acoustic_data/soundscapes/", full.names = TRUE)

# site shp (NOT PROVIDED)
sites <- vect("<SITES>.shp")
sites$YY <- paste0("20", substr(sites$SiteID, start = 10, stop = 11))

unique_day_fx <- function(df){
  temp_site <- read_csv(df, col_types = cols())  
  temp_site$YYYY <- as.numeric(paste0("20", substr(temp_site$site, 10, 11)))
  temp_site$DD <- as.numeric(substr(temp_site$wav, 25, 26))
  temp_site$MM <- as.numeric(substr(temp_site$wav, 22, 23))

  temp_site_day <- temp_site %>%
    select(site, YYYY, MM, DD) %>%
    distinct()
  return(temp_site_day)
}

# read in csvs and summarize unique DOYs for each site
days_deployed <- lapply(by_min_csvs, function(x) unique_day_fx(x))
days_deployed_df <- do.call("rbind", days_deployed)

# remove NA dates
days_deployed_df$YYYY <- paste0(20, as.integer(substr(days_deployed_df$site, 10, 11))) 
days_deployed_df <- days_deployed_df %>%
  drop_na()

# convert site UTM XY to Lon Lat
sites_proj <- project(sites, "+proj=longlat +datum=WGS84")
sites_xy <- sites_proj %>%
  st_as_sf() %>%
  sf::st_coordinates() %>%
  data.frame()

sites_df <- as.data.frame(sites) %>%
  bind_cols(sites_xy) %>%  # add xy data back
  left_join(days_deployed_df, by = c("SiteID" = "site")) %>%
  mutate(date = as.Date(paste0(YY,"-" ,MM, "-",DD)),
         DOY = as.numeric(format(date, "%j"))) %>%
  select(SiteID, X, Y, date, DOY, YY, MM, DD) %>%
  rename(lon = X,
         lat = Y) %>%
  drop_na()

glimpse(sites_df)

# Get sun up ,midday sun, sun down, night for each unique site day of deployment
suntimes <- getSunlightTimes(data = sites_df, tz = "US/Pacific", keep = c("nightEnd",     # start of astronomical dawn
                                                                          "solarNoon",    # highest sun position
                                                                          "nauticalDusk", # start of astronomical dusk
                                                                          "nadir"))       # darkest moment at night, lowest sun
# add site ID back in
suntimes_df <- suntimes %>%
  left_join(sites_df) %>%
  distinct() %>%
  mutate(YY = as.numeric(YY),
         MM = as.numeric(MM),
         DD = as.numeric(DD))

# write.csv does not change datatimes!
write.csv(suntimes_df, "data/acoustic_data/suntimes.csv", row.names = FALSE)
