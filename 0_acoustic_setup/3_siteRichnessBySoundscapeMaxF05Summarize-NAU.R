# Cross tabulate site by species and add total species count (richness)
# Uses output from siteRichnessByThreshold.R set at 0.65 sigmoid threshold
# Filters predictions to the single model and optimal sigmoid threshold based on F0.5 from Soundscape data 
# Matthew Clark, January 18, 2023

# "resnet50" is predictionsDatesetId = 2,7
# "resnet101 is predictionsDatasetId = 4,8
# "mobnet" is predictionsDatasetId = 6,9

library(tidyverse)
library(lubridate)

rm(list = ls())

# input file name
inFile <- "sonoma_s2l_predictions_sigmoid0dot65_220707.csv"

# input file with ROI optimal model statistics
optimizeFile <- "optimal_statistics_230115.csv"

# output file name
outFile <- "sonoma_s2l_predictions_SoundscapeMaxF05_230220_summarized-periods.csv"

# suntime periods 
suntimes_df <- read.csv("data/acoustic_data/suntimes.csv") %>%
  select(-c(date, lat, lon, DOY, YY, MM, DD)) %>%
  group_by(SiteID)  %>% 
  slice(1) %>%
  mutate(nightEnd = as.POSIXct(nightEnd, tz = "US/Pacific"),
         solarNoon = as.POSIXct(solarNoon, tz = "US/Pacific"),
         nauticalDusk = as.POSIXct(nauticalDusk, tz = "US/Pacific"),
         nadir = as.POSIXct(nadir, tz = "US/Pacific"))
period_count <- read_csv("data/acoustic_data/site_period_wav_count.csv") %>% 
  pivot_wider(values_from = n, names_from = period, names_glue = "{period}_wavs")

# read data .csv from siteRichnessByThreshold.R
data <- read_csv(inFile)


# read optimized model table
optimize <- read.csv(optimizeFile)
optimize <- filter(optimize, type == "ROI")

# lookup for PredictionsDatasetId to ModelName
lookup <- data.frame("PredictionsDatasetId" = c(2,4,6,7,8,9),
                     "ModelName" = c("ResNet50v2","ResNet101v2","MobileNetv2","ResNet50v2","ResNet101v2","MobileNetv2"),
                     "Model" = c("Resnet50::sigmoid","Resnet101::sigmoid","MobileNet::sigmoid","Resnet50::sigmoid","Resnet101::sigmoid","MobileNet::sigmoid"))

# filter out data for each species based on optimal model
filteredData <- data.frame()
for (species in optimize$SpeciesCode) {
  print(species)
  model <- optimize$Model[optimize$SpeciesCode==species]
  predictionID <- lookup$PredictionsDatasetId[lookup$ModelName == model]
  optimalThreshold <- optimize$Threshold[optimize$SpeciesCode==species]*10^7
  optimalData <- data %>% filter(PredictionsDatasetId %in% predictionID & 
                                   SpeciesCode == species & 
                                   PredictionScore >= optimalThreshold)
  filteredData<-rbind(filteredData,optimalData) 
}


########### SPECIES RICHNESS ###########
# cross-tabulate by site and species and sun period
tabulateData <- filteredData %>%
  left_join(suntimes_df, by = c("site" = "SiteID"))

tabulateData <- tabulateData %>%
  mutate(dawn = ifelse(between(as.numeric(Hour - hour(nightEnd)), -1, 3), 1, 0),
         midday = ifelse(between(as.numeric(Hour - hour(solarNoon)), -2, 2), 1, 0),
         dusk = ifelse(between(as.numeric(Hour - hour(nauticalDusk)), -3, 1), 1, 0),
         night = ifelse(!between(Hour, 2, 21), 1, 0))

tabulateData <- tabulateData %>% 
  left_join(period_count)

temp <- head(tabulateData) %>% 
# create averages for each period
# dawn
dawn <- tabulateData %>%
  filter(dawn == 1) %>%
  select(SpeciesCode, PredictionsDatasetId, PredictionScore, AudiofileId, site) %>%
  group_by(site, SpeciesCode) %>%
  tally() %>%
  spread(SpeciesCode, n) %>% 
  mutate(period = "dawn") %>%
  relocate(site, period)

# midday
midday <- tabulateData %>%
  filter(midday == 1) %>%
  select(SpeciesCode, PredictionsDatasetId, PredictionScore, AudiofileId, site) %>%
  group_by(site, SpeciesCode) %>%
  tally() %>%
  spread(SpeciesCode, n) %>% 
  mutate(period = "midday") %>%
  relocate(site, period)

# dusk 
dusk <- tabulateData %>%
  filter(dusk == 1) %>%
  select(SpeciesCode, PredictionsDatasetId, PredictionScore, AudiofileId, site) %>%
  group_by(site, SpeciesCode) %>%
  tally() %>%
  spread(SpeciesCode, n) %>% 
  mutate(period = "dusk") %>%
  relocate(site, period)

# night
night <- tabulateData %>%
  filter(night == 1) %>%
  select(SpeciesCode, PredictionsDatasetId, PredictionScore, AudiofileId, site) %>%
  group_by(site, SpeciesCode) %>%
  tally() %>%
  spread(SpeciesCode, n) %>% 
  mutate(period = "night") %>%
  relocate(site, period)


# add richness
dawn$Richness <- (dim(dawn)[2]-2)-rowSums(is.na(dawn))
midday$Richness <- (dim(midday)[2]-2)-rowSums(is.na(midday))
dusk$Richness <- (dim(dusk)[2]-2)-rowSums(is.na(dusk))
night$Richness <- (dim(night)[2]-2)-rowSums(is.na(night))

# make sure all sites that were in the analysis are present (as 0) in each period
sites <- tibble(sites = unique(c(dawn$site, midday$site, dusk$site, night$site)))
dawn <- dawn %>%
  full_join(sites, by = c("site" = "sites"), keep = TRUE) %>%
  mutate(site = ifelse(is.na(site), sites, site)) %>%
  mutate(period = "dawn") %>%
  select(-sites)
midday <- midday %>%
  full_join(sites, by = c("site" = "sites"), keep = TRUE) %>%
  mutate(site = ifelse(is.na(site), sites, site)) %>%
  mutate(period = "midday") %>%
  select(-sites)
dusk <- dusk %>%
  full_join(sites, by = c("site" = "sites"), keep = TRUE) %>%
  mutate(site = ifelse(is.na(site), sites, site)) %>%
  mutate(period = "dusk") %>%
  select(-sites)
night <- night %>%
  full_join(sites, by = c("site" = "sites"), keep = TRUE) %>%
  mutate(site = ifelse(is.na(site), sites, site)) %>%
  mutate(period = "night") %>%
  select(-sites)

richness <- bind_rows(dawn, midday, dusk, night) 

# write out CSV
write_csv(richness, outFile)

########### Bird Occupancy ###########
# cross-tabulate by site and species and sun period
tabulateData <- filteredData %>%
  left_join(suntimes_df, by = c("site" = "SiteID"))

tabulateData <- tabulateData %>%
  mutate(dawn = ifelse(between(as.numeric(Hour - hour(nightEnd)), -1, 3), 1, 0),
         midday = ifelse(between(as.numeric(Hour - hour(solarNoon)), -2, 2), 1, 0),
         dusk = ifelse(between(as.numeric(Hour - hour(nauticalDusk)), -3, 1), 1, 0),
         night = ifelse(!between(Hour, 2, 21), 1, 0))

# create averages for each period
# dawn
dawn <- tabulateData %>%
  filter(dawn == 1) %>%
  select(SpeciesCode, PredictionsDatasetId, PredictionScore, AudiofileId, site) %>%
  group_by(site, AudiofileId) %>%
  tally() %>%
  mutate(period = "dawn")
temp <- dawn %>% group_by(site) %>% count()

# midday
midday <- tabulateData %>%
  filter(midday == 1) %>%
  select(SpeciesCode, PredictionsDatasetId, PredictionScore, AudiofileId, site) %>%
  group_by(site, SpeciesCode) %>%
  tally() %>%
  spread(SpeciesCode, n) %>% 
  mutate(period = "midday") %>%
  relocate(site, period)

# dusk 
dusk <- tabulateData %>%
  filter(dusk == 1) %>%
  select(SpeciesCode, PredictionsDatasetId, PredictionScore, AudiofileId, site) %>%
  group_by(site, SpeciesCode) %>%
  tally() %>%
  spread(SpeciesCode, n) %>% 
  mutate(period = "dusk") %>%
  relocate(site, period)

# night
night <- tabulateData %>%
  filter(night == 1) %>%
  select(SpeciesCode, PredictionsDatasetId, PredictionScore, AudiofileId, site) %>%
  group_by(site, SpeciesCode) %>%
  tally() %>%
  spread(SpeciesCode, n) %>% 
  mutate(period = "night") %>%
  relocate(site, period)


# add richness
dawn$Richness <- (dim(dawn)[2]-2)-rowSums(is.na(dawn))
midday$Richness <- (dim(midday)[2]-2)-rowSums(is.na(midday))
dusk$Richness <- (dim(dusk)[2]-2)-rowSums(is.na(dusk))
night$Richness <- (dim(night)[2]-2)-rowSums(is.na(night))

# make sure all sites that were in the analysis are present (as 0) in each period
sites <- tibble(sites = unique(c(dawn$site, midday$site, dusk$site, night$site)))
dawn <- dawn %>%
  full_join(sites, by = c("site" = "sites"), keep = TRUE) %>%
  mutate(site = ifelse(is.na(site), sites, site)) %>%
  mutate(period = "dawn") %>%
  select(-sites)
midday <- midday %>%
  full_join(sites, by = c("site" = "sites"), keep = TRUE) %>%
  mutate(site = ifelse(is.na(site), sites, site)) %>%
  mutate(period = "midday") %>%
  select(-sites)
dusk <- dusk %>%
  full_join(sites, by = c("site" = "sites"), keep = TRUE) %>%
  mutate(site = ifelse(is.na(site), sites, site)) %>%
  mutate(period = "dusk") %>%
  select(-sites)
night <- night %>%
  full_join(sites, by = c("site" = "sites"), keep = TRUE) %>%
  mutate(site = ifelse(is.na(site), sites, site)) %>%
  mutate(period = "night") %>%
  select(-sites)

richness <- bind_rows(dawn, midday, dusk, night) 

# write out CSV
write_csv(richness, outFile)