library(tidyverse)
library(sf)   # Install from GitHub: library(devtools); devtools::install_github("rstats-db/DBI"); devtools::install_github("rstats-db/DBI"); devtools::install_github("r-spatial/sf")
library(raster)

# Load functions 
sources_files <- list.files(path = "./R", pattern = "*.R", full.names = TRUE)
sources_files <- sapply(X = sources_files, FUN = source, local = .GlobalEnv)


data <- readr::read_delim("./input/data.csv", delim = ";")
regions <- readr::read_delim("./input/regions.csv", delim = ";")

regions <- regions %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(Countries = list(tibble::tibble(Countries = unlist(stringr::str_split(string = Countries, pattern = ", "))))) %>% 
  tidyr::unnest()


spam_data <- download_process_spam(output_dir = "./output", layer = "physical-area")
  


data %>% 
  tidyr::gather()
  
  dplyr::left_join(regions) 
  
  





