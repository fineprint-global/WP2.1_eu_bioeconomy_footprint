library(tidyverse)
library(magrittr)
library(sf)    # Install from GitHub: library(devtools); devtools::install_github("rstats-db/DBI"); devtools::install_github("r-spatial/sf")
library(stars) # Install from GitHub: library(devtools); devtools::install_github("r-spatial/stars")
library(raster)
library(maps)
library(maptools)
library(rgeos)
library(ggthemes)
rasterOptions(progress = "text")

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
spam_data <- download_process_spam(output_dir = "./output", layer = "physical-area")

# Get poligon world map
world_map = st_as_sf(map('world', plot = FALSE, fill = TRUE))

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

# Comput total SPAM by region and group 
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
  dplyr::rowwise() %>% 
  dplyr::mutate(mask = mask_regions(x = total, y = geometry, crs = sf::st_crs(region_map), .pb = dplyr::progress_estimated(length(region_map$Region))), 
                raster_footprint = list(mask * scale), raster_footprint_per = list(mask * scale / cell_area))

# Mosaic footprint raster by group
spatial_footprint <- region_map %>% 
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

# Filter outliers = 1% highest values
footprint_map %<>% 
  dplyr::group_by(Group) %>% 
  dplyr::mutate(max = quantile(Area, probs = c(0.999))) %>% 
  dplyr::filter(Area < max) %>% 
  dplyr::select(-max)

# Aggregate all crops
footprint_map_total <- footprint_map %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(x, y) %>% 
  dplyr::summarise(Group = 4, Area = sum(Area)) %>% 
  dplyr::bind_rows(footprint_map, .) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(Group = factor(Group, levels = c(1,3,2,4), labels = c("Ethanol crops", "Oilseeds", "Industrial crops", "Total"), ordered = TRUE))

# Plot map layers background+products+borders
for(i in 1:4){
  gp_global_map <- ggplot2::ggplot(map_world, aes(x = long, y = lat, group = group)) + 
    ggplot2::geom_polygon(fill = "#ececec") +
    ggplot2::geom_polygon(data = eu_map, mapping = aes(x = long, y = lat, group = group), fill = "Grey") +
    ggthemes::theme_map() +
    ggplot2::coord_quickmap(xlim = c(-160, 175), ylim = c(-55, 80)) +
    ggplot2::geom_tile(data = footprint_map_total[footprint_map_total$Group==levels(footprint_map_total$Group)[i],], aes(x = x, y = y, fill = Area, group = Group)) +
    # ggplot2::facet_wrap(~ Group, ncol = 1) +
    ggplot2::scale_fill_distiller(name = paste0(letters[i],")\n\n",levels(footprint_map_total$Group)[i],"\n[Area in hectares]"), palette = "YlGnBu", direction = 1) +
    # ggplot2::scale_fill_gradientn(colours=terrain.colors(10), breaks = quantile(footprint_map_total$Area, probs = c(0, 0.25, 1))) +
    # ggplot2::scale_fill_manual(palette = "Greens", breaks = quantile(footprint_map_total$Area)) + 
    # ggplot2::scale_fill_gradient2(high = "#238b45", low = "#e5f5e0") +
    ggplot2::geom_path(data = map_world, mapping = aes(long, lat), colour = "#b5b5b5", size = 0.1) + 
    ggplot2::theme(legend.position = c(0.01, 0.01), plot.margin = grid::unit(c(0,0,0,0), "mm"))
  
  ggplot2::ggsave(paste0("global_footprint_map_",levels(footprint_map_total$Group)[i],".tif"), plot = gp_global_map, device = "tiff", path = "./output",
                  scale = 1, width = 207, height = 90, units = "mm", dpi = 300)
}

# Plot map for the graphical abstract
gp_global_map <- ggplot2::ggplot(map_world, aes(x = long, y = lat, group = group)) + 
  ggplot2::geom_polygon(fill = "#ececec") +
  ggplot2::geom_polygon(data = eu_map, mapping = aes(x = long, y = lat, group = group), fill = "Grey") +
  ggthemes::theme_map() +
  ggplot2::coord_quickmap(xlim = c(-160, 175), ylim = c(-55, 80)) +
  ggplot2::geom_tile(data = footprint_map_total[footprint_map_total$Group==levels(footprint_map_total$Group)[4],], aes(x = x, y = y, fill = Area, group = Group)) +
  ggplot2::scale_fill_distiller(name = "Area in hectares", palette = "YlGnBu", direction = 1) +
  ggplot2::geom_path(data = map_world, mapping = aes(long, lat), colour = "#b5b5b5", size = 0.1) + 
  ggplot2::theme(legend.position = c(0.01, 0.01), plot.margin = grid::unit(c(0,0,0,0), "mm"))

ggplot2::ggsave(paste0("global_footprint_map_abstract.tif"), plot = gp_global_map, device = "tiff", path = "./output",
                scale = 1, width = 207, height = 90, units = "mm", dpi = 300)
