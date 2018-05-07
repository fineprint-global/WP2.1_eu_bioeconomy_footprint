library(tidyverse)
library(magrittr)
library(sf)    # Install from GitHub: library(devtools); devtools::install_github("rstats-db/DBI"); devtools::install_github("r-spatial/sf")
library(stars) # Install from GitHub: library(devtools); devtools::install_github("r-spatial/stars")
library(raster)
library(maps)
library(maptools)
library(rgeos)
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
  dplyr::left_join(crop_list, by = c("crop" = "spam_crop"))

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

# Compute total Footprint by regional and group 
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
  dplyr::mutate(scale = total_footprint / total_spam)

# Downscale footprint area to SPAM cells 
region_map %<>% 
  dplyr::left_join(spam_stack, by = c("Group" = "Group")) %>% 
  dplyr::select(-stack, raster_spam = total) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(mask = mask_regions(x = raster_spam, y = geometry, crs = sf::st_crs(region_map)), raster_footprint = list(mask * scale))

# Mosaic footprint raster by group
spatial_footprint <- region_map %>% 
  dplyr::select(Group, raster_footprint, geometry) %>% 
  dplyr::group_by(Group) %>% 
  dplyr::summarise(footprint_mosaic = list(sum(raster::stack(raster_footprint), na.rm = TRUE))) 
  

# Save spatial footprint to file 
spatial_footprint_stack <- raster::stack(spatial_footprint$footprint_mosaic)
names(spatial_footprint_stack) <- spatial_footprint$Group
raster::writeRaster(spatial_footprint_stack, filename = "./output/spatial_footprint_stack.tif", overwrite = TRUE)

# PLOT TODO 
# plot(spatial_footprint_stack, col = RColorBrewer::brewer.pal(n = 9, name = 'YlOrRd'))



