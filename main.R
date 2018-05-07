library(tidyverse)
library(magrittr)
library(sf)   # Install from GitHub: library(devtools); devtools::install_github("rstats-db/DBI"); devtools::install_github("r-spatial/sf")
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
  tidyr::gather(Region, Area, -Item, -Group)

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

# Join data and world map 
region_map <- world_map %>% 
  dplyr::select(-ID, -Continent) %>% 
  dplyr::filter(!is.na(Region)) %>% 
  dplyr::group_by(Region) %>% 
  dplyr::summarise()

# Compute region total 
spam_total_stack <- raster::stack(spam_stack$total)
names(spam_total_stack) <- spam_stack$Group
spam_total <- raster::extract(x = spam_total_stack, y = as(region_map, "Spatial"), sp = TRUE, fun = sum)



region_map <- data %>% 
  dplyr::select(-Item) %>% 
  dplyr::group_by(Region, Group) %>% 
  dplyr::summarise(Area = sum(Area, na.rm = TRUE)) %>% 
  dplyr::filter(Group %in% c("alc", "oil", "ind")) %>% 
  dplyr::right_join(region_map, by = c("Region" = "Region")) %>% 
  dplyr::ungroup() %>% 
  sf::st_as_sf()


# Join SPAM stack and world map 
region_map %<>% 
  dplyr::left_join(spam_stack, by = c("Group" = "Group")) %>% 
  dplyr::select(-stack)




