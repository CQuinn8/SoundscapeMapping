

focal_window <- function(ras_stack, res, shape = "circle", func){
  
  # create blank focal kernel
  fw <- focalWeight(ras_stack[[1]], res, as.character(shape))
  
  # change all values from weights to 1/0
  fw[fw > 0] <- 1
  fw[fw == 0] <- NA 
  
  # run focal window
  focal_ras <- terra::focal(ras_stack, fw, fun = func, na.rm = T)
  
  return(focal_ras)
}

rsq <- function(x,y) cor(x, y)^2


# Lookup table for predictor groups
# temp <- read_csv("data/spatial_data/predictors/predictor_ls.csv")
# anthropogenic_preds <- temp %>% filter(group == "anthropogenic") %>% select(predictors) %>% pull
# cliamte_preds <- temp %>% filter(group == "climate") %>% select(predictors) %>% pull
# geomorph_preds <- temp %>% filter(group == "geomorphology") %>% select(predictors) %>% pull
# pheno_preds <- temp %>% filter(group == "phenology") %>% select(predictors) %>% pull
# structure_preds <- temp %>% filter(group == "structure") %>% select(predictors) %>% pull


# grouped CV for geographically clustered site trainign data split
group_cv <- function(x, k = length(unique(x))) {
  # create indices for groups
  dat <- data.frame(index = seq(along = x), group = x)
  
  # find the unique group names
  groups <- data.frame(group = unique(dat$group))
  
  # fold the groups
  group_folds <- createFolds(groups$group, returnTrain = TRUE, k = k)
  group_folds <- lapply(group_folds, function(x, y) y[x, , drop = FALSE], y = groups)
  dat_folds <- lapply(group_folds, function(x, y) merge(x, y), y = dat)
  lapply(dat_folds, function(x) sort(x$index))
}

# Formated index names used for plotting
pred_names <- list(
  "DHImin" = expression(bold(DHI[min])),
  "rh_75_a0_990m_mean" = expression(bold(paste("RH 75%"["990m mean"]))),
  "dist_coast" = "Dist. to Coast",
  "bcm_ppt_q2" = "PPT Q2",
  "dem" = "DEM",
  "avg_rad" = "Nighttime Lights",
  "pavd_5_10_990m_mean" = expression(bold(paste("PAVD 5-10m"["990m mean"]))),
  "rh_25_a0_990m_sd" = expression(bold(paste("RH 25%"["990m sd"]))),
  "slope" = "Slope",
  "bcm_ppt_q1" = "PPT Q1",
  "rh_95_a0_450m_mean" = expression(bold(paste("RH 95%"["450m mean"]))),
  "DHIcum" = expression(bold(DHI[cumulative])),
  "DHIvar" = expression(bold(DHI[variance])),
  "bcm_tmx_q3" = "TMX Q3",
  "bcm_tmn_q1" = "TMN Q1",
  "bcm_tmx_q1" = "TMX Q1",
  "bcm_pet_q4" = "PET Q4",
  "bcm_tmx_q2" = "TMX Q2",
  "tpi" = "TPI",
  "rh_25_a0_450m_sd" = expression(bold(paste("RH 25%"["450m sd"]))),
  "rh_50_a0_990m_mean" = expression(bold(paste("RH 50%"["990m mean"]))),
  "pavd_0_5_990m_mean" = expression(bold(paste("PAVD 0-5m"["990m mean"]))),
  "cover_a0_90m" = "Canopy Cover",
  "bcm_ppt_q3" = "PPT Q3",
  "bcm_tmx_q4" = "PPT Q4",
  "vdr_b_450m_mean" = expression(bold(paste("VDR Bottom"["540m mean"]))),
  "bcm_pet_q3" = "PET Q3")

# helper function that looks up unformatted index name with formatted
pred_labeller <- function(variable, value){
  return(pred_names[value])
}


hexs = c("#7fc97f","#386cb0","#984ea3","#fdb462","#fb9a99","#a6cee3","#ffff33")
scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = hexs), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = hexs), ...)
  
}

theme_Publication <- function(base_size=14, base_family="Arial") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "plain",
                                      size = rel(1.2), 
                                      hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            
            axis.title = element_text(size = rel(1)),
            axis.title.y = element_text(angle=90, vjust =2),
            axis.title.x = element_text(vjust = -0.2, face = "plain"),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.5, "cm"),
            # legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}