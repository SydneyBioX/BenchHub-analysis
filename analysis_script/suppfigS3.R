library(readr)
library(stringr)
library(tidyverse)
library(BenchHub)

spatialsimbench_result <- read_csv("spatialsimbench_result.csv")
spatialsimbench_result <- spatialsimbench_result %>%
  mutate(
    method = case_when(
      method %in% c("scDesign3_gau_rf", "scDesign3_gau(rf)") ~ "scDesign3_gaurf",
      method %in% c("scDesign3_nb_rf", "scDesign3_nb(rf)")   ~ "scDesign3_nbrf",
      method %in% c("scDesign3_poi_rf", "scDesign3_poi(rf)") ~ "scDesign3_poirf",
      method %in% c("SRTsim_rf", "SRTsim(rf)")               ~ "SRTsimrf",
      TRUE ~ method
    )
  )  

bmi <- BenchmarkInsights$new(spatialsimbench_result)
g1 <- bmi$getCorplot(bmi$evalSummary, "method") + theme(legend.position = "bottom")
g2 <- getScatterplot(bmi$evalSummary, c("recall", "precision"))

simbench_result <- readRDS("simbench_result.rds")
simbench_result <- simbench_result %>%
  transmute(
    datasetID = title,
    method = method,
    evidence = "KDEstat",
    metric = parameter,
    result = -sum_kde_zstat
  )
techwise <- read_excel("techwise.xlsx")
plot_df <- simbench_result %>%
  left_join(techwise, by = "datasetID")
plot_df <- plot_df %>%
  filter(!is.na(technology))

g3 <- ggplot(plot_df, aes(x = technology, y = result, fill = technology)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = quantile(plot_df$result, c(0.05, 0.95), na.rm = TRUE)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  theme(legend.position = "none")

ggpubr::ggarrange(g1, g2, g3, ncol = 2, nrow = 2)

