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

mycols=c("dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00", "black",
         "gold1", "skyblue2", "#FB9A99", "palegreen2", "maroon", "#CAB2D6", "orchid1", "deeppink1", "blue1",
         "steelblue4", "darkturquoise", "green1", "yellow4", "yellow3",
         "darkorange4", "brown")

# data$Region = as.character(data$Region)

scale_x_productlist <- function (name = waiver(), breaks = ggmosaic:::product_breaks(), minor_breaks = NULL, 
          labels = ggmosaic:::product_labels(), limits = NULL, expand = waiver(), 
          oob = scales:::censor, na.value = NA_real_, trans = "identity", 
          position = "bottom", sec.axis = waiver()) 
{
  sc <- ggplot2::continuous_scale(c("x", "xmin", "xmax", "xend", 
                                    "xintercept", "xmin_final", "xmax_final", "xlower", 
                                    "xmiddle", "xupper"), "position_c", identity, name = name, 
                                  breaks = breaks, minor_breaks = minor_breaks, labels = labels, 
                                  limits = limits, expand = expand, oob = oob, na.value = na.value, 
                                  trans = trans, guide = "none", position = position, 
                                  super = ScaleContinuousProduct)
  if (!ggplot2:::is.waive(sec.axis)) {
    if (ggplot2:::is.formula(sec.axis)) 
      sec.axis <- ggplot2::sec_axis(sec.axis)
    is.sec_axis = getFromNamespace("is.sec_axis", "ggplot2")
    if (!ggplot2:::is.sec_axis(sec.axis)) 
      stop("Secondary axes must be specified using 'sec_axis()'")
    sc$secondary.axis <- sec.axis
  }
  sc
}

breaks_values <- data %>% 
  dplyr::group_by(Region) %>% 
  dplyr::summarise(value = sum(value) / 1000) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(value = cumsum(value) )

# debugonce(check_breaks_labels)
mosaic <-
  ggplot(data = data) + 
  geom_mosaic(aes(weight = value, x = product(Item, Region), fill = Item), na.rm=T, divider=ddecker(), offset = 0.005) +
  # theme(axis.text.x=element_text(angle=-25, hjust= .1), legend.position="right") +
  theme(legend.position="right") + 
  theme_bw() +
  scale_y_continuous(labels = scales::percent) + 
  scale_x_productlist(position = "top", sec.axis = ggplot2::sec_axis(~.*sum(data$value/1000), breaks = c(0,breaks_values$value) + c(0,-0.1,0,0,0.1,0.2,0), labels = round(c(0,breaks_values$value), 1), name = "Million hectares")) + 
  # facet_grid(Group~.) +
  labs(x = "", y = "Share per commodity group") + 
  guides(fill=guide_legend(title = "Commodities", reverse = TRUE)) +
  scale_fill_manual(values = mycols) 


# debugonce(geom_text)
mosaic <- mosaic + 
  geom_text(data = ggplot_build(mosaic)$data[[1]], aes(x = (xmin+xmax)/2, y = (ymin+ymax)/2, label=replace(.wt, .wt < 300, "")))

ggplot2::ggsave(paste0("EU_footprint_mosaic.tif"), plot = mosaic, device = "tiff", path = "./output/",
                scale = 1, width = 200, height = 150, units = "mm", dpi = 300)



