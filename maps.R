library(tidyverse)
library(magrittr)
library(sf)    # Install from GitHub: library(devtools); devtools::install_github("rstats-db/DBI"); devtools::install_github("r-spatial/sf")
library(stars) # Install from GitHub: library(devtools); devtools::install_github("r-spatial/stars")
library(raster)
library(maps)
library(maptools)
library(rgeos)
library(ggthemes)
raster::rasterOptions(progress = "text")
raster::rasterOptions(maxmemory = 1e+3)

# Load functions 
sources_files <- list.files(path = "./R", pattern = "*.R", full.names = TRUE)
sources_files <- sapply(X = sources_files, FUN = source, local = .GlobalEnv)

# Load footprint data and region classification
data <- readr::read_csv("./input/data.csv")
regions <- readr::read_csv("./input/regions.csv")
crop_list <- readr::read_csv("./input/crop_concordance.csv")
country_list <- readr::read_csv("./input/country_conconrdance.csv")

# Tidy data 
data %<>% 
  tidyr::gather(Region, total_footprint, -Item, -Group)

# Tidy regions
regions %<>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(Countries = list(tibble::tibble(Countries = unlist(stringr::str_split(string = Countries, pattern = ", "))))) %>% 
  tidyr::unnest()

# Get MapSPAM data
spam_data <- download_process_spam(output_dir = "./input", layer = "harvested-area")

# Get poligon world map
world_map = sf::st_as_sf(maps::map('world', plot = FALSE, fill = TRUE))

# Filter SPAM data
spam_data %<>% 
  dplyr::filter(crop_system == "Total") %>% 
  dplyr::left_join(crop_list, by = c("crop" = "spam_crop")) %>% 
  dplyr::filter(crop != "Oil Crops Other")

# Create raster stack 
spam_stack <- spam_data %>% 
  dplyr::select(file, Group) %>% 
  dplyr::group_by(Group) %>% 
  dplyr::summarise(stack = list(file)) %>%  
  dplyr::mutate(stack = lapply(.$stack, raster::stack))

# Sum raster by Group
spam_stack$total <- lapply(spam_stack$stack, FUN = function(x) sum(x, na.rm = TRUE))

# Join world_map with region names
world_map %<>% 
  dplyr::left_join(country_list, by = c("ID" = "ID"))

# Group world map regions 
region_map <- world_map %>% 
  dplyr::select(-ID, -Continent) %>% 
  dplyr::filter(!is.na(Region)) %>% 
  dplyr::group_by(Region) %>% 
  dplyr::summarise()

# Compute total SPAM by region and group 
spam_total_stack <- raster::stack(spam_stack$total)
names(spam_total_stack) <- spam_stack$Group
region_map %<>% 
  raster::extract(x = spam_total_stack, y = ., df = TRUE, fun = sum) %>% 
  tibble::as_tibble() %>% 
  dplyr::select(-ID) %>% 
  dplyr::bind_cols(region_map, .) %>% 
  tidyr::gather(Group, total_spam, -Region, -geometry) %>% 
  dplyr::filter(Group %in% c("alc", "oil", "ind")) 

# Compute total Footprint by region and group 
region_map <- data %>% 
  dplyr::select(-Item) %>% 
  dplyr::group_by(Region, Group) %>% 
  dplyr::summarise(total_footprint = sum(total_footprint, na.rm = TRUE)) %>% 
  dplyr::filter(Group %in% c("alc", "oil", "ind")) %>% 
  dplyr::right_join(region_map, by = c("Region" = "Region", "Group" = "Group")) %>% 
  dplyr::ungroup() %>% 
  sf::st_as_sf()

# Compute scale factor between footprint and SPAM area by region and group 
region_map %<>% 
  dplyr::mutate(scale = (total_footprint * 1000) / total_spam)

cell_area <- raster::area(spam_stack$total[[1]]) * 100

# Downscale footprint area to SPAM cells 
region_map %<>% 
  dplyr::filter(Region != "European Union") %>% 
  dplyr::left_join(spam_stack, by = c("Group" = "Group")) %>% 
  dplyr::select(-stack) %>% 
  dplyr::mutate(mask = purrr::pmap(.l = list(x = .$total, y = .$geometry), .f = mask_regions, crs = sf::st_crs(.),
                                   .pb = dplyr::progress_estimated(length(.$Region)))) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(raster_footprint = list(mask * scale), raster_footprint_per = list(mask * scale / cell_area))

# Mosaic footprint raster by group
spatial_footprint <- region_map %>% 
  dplyr::ungroup() %>% 
  dplyr::select(Group, raster_footprint, raster_footprint_per) %>% 
  dplyr::group_by(Group) %>% 
  dplyr::summarise(footprint_mosaic = list(sum(raster::stack(raster_footprint), na.rm = TRUE)),
                   footprint_mosaic_per = list(sum(raster::stack(raster_footprint_per), na.rm = TRUE))) 

# Save spatial footprint to file 
spatial_footprint_stack <- raster::stack(spatial_footprint$footprint_mosaic)
names(spatial_footprint_stack) <- spatial_footprint$Group
raster::writeRaster(spatial_footprint_stack, filename = "./output/spatial_footprint_stack.tif", overwrite = TRUE)

spatial_footprint_stack_per <- raster::stack(spatial_footprint$footprint_mosaic_per)
names(spatial_footprint_stack_per) <- spatial_footprint$Group
raster::writeRaster(spatial_footprint_stack_per, filename = "./output/spatial_footprint_stack_per.tif", overwrite = TRUE)

# Prepare footprint map for plot
footprint_map <- stars::read_stars("./output/spatial_footprint_stack.tif") %>% 
  as.data.frame(spatial_footprint_stack) %>% 
  tibble::as_tibble() %>% 
  dplyr::filter(spatial_footprint_stack.tif > 0.00001) %>% 
  dplyr::transmute(x = x, y = y, Group = band, Area = spatial_footprint_stack.tif) 

# Get world map
map_world <- ggplot2::map_data(map = "world") %>% 
  dplyr::filter(region != "Antarctica") %>% 
  dplyr::rename(ID = region) %>% 
  dplyr::left_join(country_list, by = c("ID" = "ID")) %>% 
  tibble::as_tibble() %>% 
  dplyr::mutate(Region = factor(dplyr::if_else(Region == "European Union", 1, 0)))

eu_map <- map_world %>% 
  dplyr::filter(Region == 1)

# Filter outliers = 0.1% highest values
footprint_map %<>% 
  dplyr::group_by(Group) %>% 
  dplyr::mutate(Area = replace(Area, Area > quantile(Area, probs = c(0.999)), quantile(Area, probs = c(0.999))))
  # dplyr::mutate(max = quantile(Area, probs = c(0.99))) %>% 
  # dplyr::select(-max)

# Aggregate all crops
footprint_map_total <- footprint_map %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(x, y) %>% 
  dplyr::summarise(Group = 4, Area = sum(Area)) %>% 
  dplyr::bind_rows(footprint_map, .) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(Group = factor(Group, levels = c(1,3,2,4), labels = c("Maize and sugarcane", "Oil crops", "Fibre crops", "Total"), ordered = TRUE))

# define colors
# colors <- list()
# colors[[1]] <- viridis::viridis(20, direction = -1)[2:20] #c("#dfee91", "#bae4bc", "#56c5b8", "#0096c8", "#0868ac", "#00507d", "#000a32")
# colors[[2]] <- viridis::magma(20, direction = -1) #c("#f9e3f5", "#d57dd6", "#c800b6", "#a208ac", "#63007d", "#180032")
# colors[[3]] <- viridis::cividis(20, direction = -1)[2:20] #c("#e4e4ba", "#c5b556", "#c86500", "#ac5208", "#7d0d00", "#320000") #c("#e4deba", "#c5a456", "#c86500", "#ac3a08", "#7d2600", "#320a00")
viridis_colors <- list("viridis", "magma", "cividis")

# Plot map layers background+products+borders
i=1
for(i in 1:3){
  # # ggplot colours 
  # gp_global_map <- ggplot2::ggplot(map_world, aes(x = long, y = lat, group = group)) + 
  #   ggplot2::geom_polygon(fill = "#f0f0f0") +
  #   ggplot2::geom_polygon(data = eu_map, mapping = aes(x = long, y = lat, group = group), fill = "#cccccc") +
  #   ggthemes::theme_map() +
  #   ggplot2::coord_quickmap(xlim = c(-160, 175), ylim = c(-55, 80)) +
  #   ggplot2::geom_tile(data = footprint_map_total[footprint_map_total$Group==levels(footprint_map_total$Group)[i],], aes(x = x, y = y, fill = Area, group = Group)) +
  #   # ggplot2::facet_wrap(~ Group, ncol = 1) +
  #   # ggplot2::scale_fill_distiller(name = paste0(letters[i],")\n\n",levels(footprint_map_total$Group)[i],"\n[Area in hectares]"), palette = "YlGnBu", direction = 1) +
  #   # ggplot2::scale_fill_gradientn(colours=terrain.colors(10), breaks = quantile(footprint_map_total$Area, probs = c(0, 0.25, 1))) +
  #   # ggplot2::scale_fill_manual(palette = "Greens", breaks = quantile(footprint_map_total$Area)) + 
  #   # ggplot2::scale_fill_gradient2(name = paste0(letters[i],")\n\n",levels(footprint_map_total$Group)[i],"\n[Area in hectares]"), high = "#084081", low = "#f7fcf0") +
  #   ggplot2::scale_fill_gradientn(name = paste0(letters[i],")\n\n",levels(footprint_map_total$Group)[i],"\n[Area in hectares]"), colors = colors[[i]]) +
  #   ggplot2::geom_path(data = map_world, mapping = aes(long, lat), colour = "#b5b5b5", size = 0.1) + 
  #   ggplot2::theme(legend.position = c(0.01, 0.01), plot.margin = grid::unit(c(0,0,0,0), "mm"))
  # 
  # ggplot2::ggsave(paste0("global_footprint_map_",levels(footprint_map_total$Group)[i],".tif"), plot = gp_global_map, device = "tiff", path = "./output",
  #                 scale = 1, width = 207, height = 90, units = "mm", dpi = 300)
  # ggplot2::ggsave(paste0("global_footprint_map_",levels(footprint_map_total$Group)[i],".png"), plot = gp_global_map, device = "png", path = "./output",
  #                 scale = 1, width = 207, height = 90, units = "mm", dpi = 300)
  
  # viridis colours
  gp_global_map <- ggplot2::ggplot(map_world, aes(x = long, y = lat, group = group)) +
    ggplot2::geom_polygon(fill = "white") +
    ggplot2::geom_polygon(data = eu_map, mapping = aes(x = long, y = lat, group = group), fill = "#d6d8d8") +
    ggthemes::theme_map() +
    ggplot2::coord_quickmap(xlim = c(-160, 175), ylim = c(-55, 80)) +
    ggplot2::geom_tile(data = footprint_map_total[footprint_map_total$Group==levels(footprint_map_total$Group)[i],], aes(x = x, y = y, fill = Area, group = Group)) +
    viridis::scale_fill_viridis(option = viridis_colors[[i]], direction = -1, name = paste0(letters[i],")\n\n",levels(footprint_map_total$Group)[i],"\n[Area in hectares]")) +
    ggplot2::geom_path(data = map_world, mapping = aes(long, lat), colour = "#b5b5b5", size = 0.1) +
    ggplot2::theme(legend.position = c(0.01, 0.01), plot.margin = grid::unit(c(0,0,0,0), "mm"), panel.background = element_rect("#e7f5fb", size = 0, linetype = "blank"), legend.background = element_blank())
    
  ggplot2::ggsave(paste0("global_footprint_map_viridis_",levels(footprint_map_total$Group)[i],".tif"), plot = gp_global_map, device = "tiff", path = "./output",
                  scale = 1, width = 207, height = 90, units = "mm", dpi = 300)
  ggplot2::ggsave(paste0("global_footprint_map_viridis_",levels(footprint_map_total$Group)[i],".png"), plot = gp_global_map, device = "png", path = "./output",
                  scale = 1, width = 207, height = 90, units = "mm", dpi = 300)
  
}

