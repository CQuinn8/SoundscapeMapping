library(terra)
library(exactextractr)
library(sf)
library(tidyverse)
library(ggpubr)
source("code/utils.R")

periods <- c("dawn","midday","dusk","night")

for(p in 1:4){
  response <- "Biophony"
  
  # response data associated with shapefile
  period <- periods[p]
  response_df <- read_csv("data/acoustic_data/response_df.csv") %>%
    filter(period == period) %>% 
    select(site, Biophony)
  sites <- read.csv('data/final_sites_2017-2021.csv')
  response_df <- response_df %>%
    filter(site %in% sites$sites)
  sites_shp <- vect("sonoma_s2l_locations.shp")
  sites_shp$YY <- paste0("20", substr(sites_shp$SiteID, start = 10, stop = 11))
  sites_shp <- merge(sites_shp, response_df, all.y = TRUE, by.x = "SiteID", by.y = "site")
  
  # fire analysis
  raster_files <- list.files("results/rf_vif3/processed/prediction_surfaces/", pattern = "*.tif$", full.names = TRUE)
  raster_files <- raster_files[grepl(paste0(response,"_",period), raster_files) & grepl("med", raster_files)]
  
  bio_ras <- rast(raster_files)
  names(bio_ras) <- c("2017","2018", "2019", "2020", "2021")
  
  fire_files <- tibble(firename = c("Kincade", "Glass", "Wallbridge"),
                       year = c("2019","2020", "2020"),
                       shp = c("kincade_clipped.shp",
                               "glass_clipped.shp",
                               "mtbs/2020/ca3855412253120200927_20200715_20210718_burn_bndy.shp"),
                       donuts = c("buffers/kincade_donut_clipped.shp",
                                  "buffers/glass_donut_clipped.shp",
                                  "buffers/walbridge_donut.shp"),
                       dnbr = c("2019/ca3879612276720191023_20190424_20200426_dnbr6.tif",
                                "2020/ca3867312307820200817_20200731_20210718_dnbr6.tif",
                                "2020/ca3855412253120200927_20200715_20210718_dnbr6.tif"))
  
  ggs <- list()
  for(i in 1:3){
    temp_fire_info <- fire_files[i,]
    print(temp_fire_info$firename)
    
    # read in fire data
    temp_fire_dnbr <- rast(paste0("data/spatial_data/fire_severity/mtbs/", temp_fire_info$dnbr))
    temp_fire_dnbr <- project(temp_fire_dnbr, "EPSG:32610", method = "near")
    temp_fire_shp <- sf::read_sf(paste0("data/spatial_data/fire_severity/", temp_fire_info$shp))
    temp_fire_shp <- st_transform(temp_fire_shp, crs = crs(temp_fire_dnbr))
    # crs(temp_fire_shp) == crs(temp_fire_dnbr)
    
    # "control" area (farther than 1 km and within 2 km buffer from QGIS)
    temp_donut <- sf::read_sf(paste0("data/spatial_data/fire_severity/", temp_fire_info$donuts))
    temp_donut$Layer_1 = 0 
    
    # remove increased green (5), non-processing area (6), and outside of perimeter (0)
    # unburned/low burn = 1, low = 2, moderate = 3, high = 4
    temp_fire <- clamp(temp_fire_dnbr, lower = 1, upper = 4, value = FALSE)
    
    # resample to 90m using mode
    temp_fire_90 <- resample(temp_fire, bio_ras, method = "mode")
    
    # convert to polygon to extract
    temp_fire_vec <- st_as_sf(as.polygons(temp_fire_90))
    temp_donut <- st_transform(st_as_sf(temp_donut)["Layer_1"], crs(temp_fire_vec))
    # crs(temp_donut) == crs(temp_fire_vec)
    
    fire_single <- st_cast(temp_fire_vec, "POLYGON")
    fire_severity <- rbind(fire_single, temp_donut)
      
    # extract the 90 m resampled class values
    extracted <- exact_extract(bio_ras, fire_severity)
    names(extracted) <- fire_severity$Layer_1
    extracted <- do.call(rbind, extracted) 
    extracted$fire_sev <- substr(row.names(extracted), 1,1)
    extracted_df <- extracted %>%
      drop_na() %>% 
      mutate(severity = case_when(fire_sev == 0 ~ "NotBurned",
                                  fire_sev == 1 ~ "Unburned/low",
                                  fire_sev == 2 ~ "Low",
                                  fire_sev == 3 ~ "Moderate",
                                  fire_sev == 4 ~ "High",
                                  TRUE ~ ""),
             severity = factor(severity, levels = c("NotBurned","Unburned/low","Low", "Moderate", "High")))
  
  
    # update bird richness text
    if(response == "bird_richness") response = "Bird Richness"

    # Severity on X and year as group
    temp_gg <- extracted_df %>%
        select(-c(fire_sev, coverage_fraction)) %>% 
        drop_na() %>% 
        pivot_longer(names_to = "year", values_to = paste0(response), -severity) %>%
        ggplot(aes(y = get(response), x = severity, fill = year)) +
        geom_boxplot(alpha = 0.8, width = 0.8, outlier.shape = NA) +
        theme_Publication() +
        scale_fill_Publication() +
      labs(title = paste0(temp_fire_info$firename,
                          " fire ",
                          period, 
                          " ",
                          response,
                          " (fire year : ",
                          temp_fire_info$year,
                          ")"),
           x = "Burn Severity",
           y = response)
    
    ggsave(temp_gg,
           filename = paste0(temp_fire_info$firename, "_", response, "_", period,".png"),
           path = "figures/fires/",
           width = 8,
           height = 6,
           unit = "in",
           dpi = 500)
    ggs[[i]] = temp_gg
  }

  
  (all_gg <- ggarrange(ggs[[1]] + rremove("xlab"), 
            ggs[[2]] + rremove("xlab"), 
            ggs[[3]], 
            ncol = 1, nrow = 3,
            common.legend = TRUE,
            legend = "bottom"))
  ggsave(all_gg,
         filename = paste0("wildfires_", response, "_", period, ".png"),
         path = "figures/fires/",
         width = 8,
         height = 12,
         unit = "in",
         dpi = 500)
}