library(tidyverse)
library(ggpubr)

my_colors <- c("#B9DAD3", "#CBD38A", "#DF7488", "#F9C49C", "#C99158", "#AD848C", "#8C8C8C")

# Define individual datasets and plots
mob <- read_csv("MOB.csv") %>%
  drop_na(col, row, ground_truth) %>%
  mutate(ground_truth = as.character(ground_truth))

p1 <- ggplot(mob, aes(x = col, y = row, color = ground_truth)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(values = my_colors) +
  theme_void() +
  coord_fixed() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, size = 10),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))


pdac <- read_csv("PDAC.csv") %>%
  drop_na(col, row, ground_truth) %>%
  mutate(ground_truth = as.character(ground_truth))

p2 <- ggplot(pdac, aes(x = col, y = row, color = ground_truth)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(values = my_colors) +
  theme_void() +
  coord_fixed() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, size = 10),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))


seqfish <- read_csv("seqfish.csv") %>%
  drop_na(col, row, ground_truth) %>%
  mutate(ground_truth = as.character(ground_truth))

group_map <- c(
  "Forebrain/Midbrain/Hindbrain" = "Neural",
  "Spinal cord" = "Neural",
  "Neural crest" = "Neural",
  
  "Anterior somitic tissues" = "Mesoderm",
  "Cranial mesoderm" = "Mesoderm",
  "Intermediate mesoderm" = "Mesoderm",
  "Lateral plate mesoderm" = "Mesoderm",
  "Presomitic mesoderm" = "Mesoderm",
  "Sclerotome" = "Mesoderm",
  "Cardiomyocytes" = "Mesoderm",
  "Dermomyotome" = "Mesoderm",
  
  "Definitive endoderm" = "Endoderm",
  "Gut tube" = "Endoderm",
  "Splanchnic mesoderm" = "Endoderm",
  
  "Erythroid" = "Blood/Endothelium",
  "Endothelium" = "Blood/Endothelium",
  "Haematoendothelial progenitors" = "Blood/Endothelium",
  
  "Surface ectoderm" = "Surface",
  
  "Allantois" = "Other/Low Quality",
  "Low quality" = "Other/Low Quality",
  "Mixed mesenchymal mesoderm" = "Other/Low Quality"
)

seqfish <- seqfish %>%
  mutate(group6 = recode(ground_truth, !!!group_map))

p3 <- ggplot(seqfish, aes(x = col, y = row, color = group6)) +
  geom_point(size = 0.75, alpha = 0.8) +
  scale_color_manual(values = my_colors) +
  theme_void() +
  coord_fixed() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, size = 10),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))


mgastrula <- read_csv("MGASTRULA.csv") %>%
  drop_na(col, row, ground_truth) %>%
  mutate(ground_truth = as.character(ground_truth))

p4 <- ggplot(mgastrula, aes(x = col, y = row, color = ground_truth)) +
  geom_point(size = 0.75, alpha = 0.8) +
  scale_color_manual(values = my_colors) +
  theme_void() +
  coord_fixed() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, size = 10),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))


baristaSeq <- read_csv("baristaSeq.csv") %>%
  drop_na(col, row, ground_truth) %>%
  mutate(ground_truth = as.character(ground_truth))

p5 <- ggplot(baristaSeq, aes(x = col, y = row, color = ground_truth)) +
  geom_point(size = 0.75, alpha = 0.8) +
  scale_color_manual(values = my_colors) +
  theme_void() +
  coord_fixed() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, size = 10),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

libd <- read_csv("LIBD.csv") %>%
  drop_na(col, row, ground_truth) %>%
  mutate(ground_truth = as.character(ground_truth))

p6 <- ggplot(libd, aes(x = col, y = row, color = ground_truth)) +
  geom_point(size = 0.75, alpha = 0.8) +
  scale_color_manual(values = my_colors) +
  theme_void() +
  coord_fixed() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, size = 10),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))


merfish <- read_csv("MERFISH.csv") %>%
  drop_na(col, row, ground_truth) %>%
  mutate(ground_truth = as.character(ground_truth))

group_map <- c(
  "MPA" = "Medial Preoptic",
  "MPN" = "Medial Posterior",
  "PV"  = "Paraventricular",
  "PVH" = "Paraventricular",
  "PVT" = "Paraventricular",
  "BST" = "Brainstem",
  "V3"  = "Ventral",
  "fx"  = "Other/FX"
)

merfish <- merfish %>%
  mutate(group6 = recode(ground_truth, !!!group_map))

p7 <- ggplot(merfish, aes(x = col, y = row, color = group6)) +
  geom_point(size = 0.75, alpha = 0.8) +
  scale_color_manual(values = my_colors) +
  theme_void() +
  coord_fixed() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, size = 10),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

# Combine all plots into 1 row
combined_plot <- ggarrange(p1, p2, p3, p4, p5, p6, p7, nrow = 1, ncol = 7)

ggsave("combined_plotV0.pdf", combined_plot, width = 20, height = 5, dpi = 300)


