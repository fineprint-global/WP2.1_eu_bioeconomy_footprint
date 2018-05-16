##########################################################################
##  Plot a mosaic chart
##########################################################################
library(tidyverse)
library(magrittr)
library(ggmosaic)

data <- readr::read_csv(file = "./input/data_mosaic_agg.csv")
data %<>% 
  tidyr::gather(key = Region, value = value, -Group, -Item) %>% 
  dplyr::mutate(Item = factor(Item, levels = unique(Item)), Region = factor(Region, levels = unique(Region)))

data$Item <- factor(data$Item, levels = unique(data$Item))
data$Region <- factor(data$Region, levels = unique(data$Region))

mycols=c("dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00", "black",
         "gold1", "skyblue2", "#FB9A99", "palegreen2", "maroon", "#CAB2D6", "orchid1", "deeppink1", "blue1",
         "steelblue4", "darkturquoise", "green1", "yellow4", "yellow3",
         "darkorange4", "brown")

mosaic <-
  ggplot(data = data) + 
  geom_mosaic(aes(weight = value, x = product(Item, Region), fill = factor(Item)), na.rm=T, divider=ddecker(), offset = 0.005) +
  # theme(axis.text.x=element_text(angle=-25, hjust= .1), legend.position="right") +
  theme(legend.position="right") + 
  theme_bw() +
  scale_y_continuous(labels = scales::percent) + 
  # facet_grid(Group~.) +
  labs(x = "", y = "Share per commodity group") + 
  guides(fill=guide_legend(title = "Commodities", reverse = TRUE)) +
  scale_fill_manual(values = mycols)

mosaic <- mosaic + 
  geom_text(data = ggplot_build(mosaic)$data[[1]], aes(x = (xmin+xmax)/2, y = (ymin+ymax)/2, label=replace(.wt, .wt < 300, "")))

mosaic

ggplot2::ggsave(paste0("EU_footprint_mosaic.tif"), plot = mosaic, device = "tiff", path = "./output/",
                scale = 1, width = 200, height = 150, units = "mm", dpi = 300)


