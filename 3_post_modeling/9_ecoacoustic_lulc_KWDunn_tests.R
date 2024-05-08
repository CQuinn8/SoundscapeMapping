library(tidyverse)
library(rstatix)
library(ggpubr)


setwd("")
wd = ""
periods <- c("dawn","midday","dusk","night")
responses <- c("Anthropophony", "Biophony", "Geophony", "Quiet", "ACI", "NDSI", "H", "birdrichness")


# read in csvs of ecoacoustic indices relative to 100% lulc pixels
df <- list.files(paste0(wd, "results/processed/lulc_response/"), pattern = "*.csv$", recursive = FALSE, full.names = TRUE) %>%
  map_dfr(read_csv, show_col_types = FALSE)

df_prepped <- df %>%
  mutate(lulc = factor(list_name)) %>%
  filter(year == 2021) %>%
  dplyr::select(-c(list_name, coverage_fraction, year))

for(i in responses){
  if(i == "birdrichness") {
    temp_periods <- "allday"
  } else {
    temp_periods = periods
  }
  for(j in temp_periods){
    print(paste0(i, "-", j))
    temp_df <- df_prepped %>%
      filter(response == i, period == j)
    temp_kw <- kruskal.test(value ~ lulc, data = temp_df)
    if(temp_kw$p.value <= 0.05){
      print("Significant...")
      temp_dunn <- dunn_test(value ~ lulc, data = temp_df, p.adjust.method = "bonferroni")
      temp_dunn
      write.csv(temp_dunn, file = paste0(wd, "results/processed/lulc_response/kw_dunn/", i, "_", j, "-dunn.csv"))

      temp_dunn_xy <- temp_dunn %>% add_xy_position(x = "lulc")
      temp_gg <- ggboxplot(temp_df,
                           x = "lulc",
                           y = "value",
                           add = "jitter",
                           notch = TRUE,
                           add.params = list("jitter", color = "black", alpha = 0.1, size = 0.1),
                           legend.title = "",
                           xlab = FALSE,
                           ylab = "Percent Present",
                           main = paste0(i, " Dunn Test LCLU - ", j)) +
        stat_pvalue_manual(temp_dunn_xy, hide.ns = TRUE) +
        rotate_x_text(angle = 45)
      ggsave(paste0(wd, "results/processed/lulc_response/kw_dunn/", i, "_", j, "-dunn.png"),
             plot = temp_gg, width = 6, height = 6, units = "in", dpi = 300)

    } else {
      print("Not significant...")
    }
    out_kw <- tidy(temp_kw)
    write_csv(out_kw, file = paste0(wd, "results/processed/lulc_response/kw_dunn/", i, "_", j, "-kw.csv"))
  }
}