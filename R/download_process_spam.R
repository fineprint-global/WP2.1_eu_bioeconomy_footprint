#' @title Download SPAM data 
#' 
#' @description This fucntion downloads SPAM data 
#' 
#' @param output_dir a character with the destination folder 
#' 
#' @param layer a character with the MapSPAM layer: "harvested-area", "physical-area", "production", or "yield". 
#' Default is "physical-area"
#' 
#' @param ... other arguments to pass to download.file
#' 
#' @return The function returns a tibble with a summary of downloaded data files 
#' 
#' @author Victor Maus, \email{victor.maus@@wu.ac.at}
#' 
download_process_spam <- function(output_dir = "./output", layer = "physical-area", ...){
  
  require(tidyverse)
  require(raster)
  
  dir.create(output_dir, showWarnings = FALSE)
  
  # Download SPAM production data v3r2 
  cat("\nDownloading SPAM production raster files\n")
  spam_url <- paste0("http://spam05.harvestchoice.org/v3r2/tiff/", layer)
  spam_dir <- paste(output_dir, "spam_tiff", sep = "/")
  dir.create(spam_dir, showWarnings = FALSE, recursive = TRUE)
  spam_input <- xml2::read_html(spam_url) %>% 
    rvest::html_nodes("a") %>% 
    rvest::html_attr("href") %>% 
    tibble::tibble(file = .) %>% 
    dplyr::filter(grepl("tiff$", file)) %>% 
    dplyr::mutate(url  = paste(spam_url, file, sep = "/")) %>%
    dplyr::mutate(file = paste(spam_dir, file, sep = "/")) %>% 
    dplyr::rowwise() %>% 
    dplyr::bind_cols(dplyr::do(., download = unlist(download_file(url = .$url, destfile = .$file)))) %>% 
    tidyr::unnest(download)
  
  spam_input <- basename(spam_input$file) %>% 
    stringr::str_replace(".tiff", "") %>% 
    stringr::str_split(pattern = "_", n = 3, simplify = TRUE) %>% 
    tibble::as_tibble() %>% 
    dplyr::mutate(V4 = stringr::str_split(V3, pattern = "_")) %>% 
    dplyr::transmute(product = V1, 
                     crop = stringr::str_to_title(sapply(V4, FUN = function(s) paste(s[-length(s)], collapse = " "))),
                     crop_system = stringr::str_to_title(sapply(V4, tail, 1))) %>% 
    dplyr::bind_cols(spam_input, .)
  
  # Compute SPAM total production 
  cat("\nComputing SPAM total production from raster files\n")
  spam_input <- spam_input %>% 
    dplyr::mutate(production = purrr::map_dbl(.x = .$file, .f = raster_cellStats_progress, .pb = dplyr::progress_estimated(length(.$file)), stat = 'sum', na.rm = TRUE)) %>% 
    dplyr::group_by(crop_system) %>% 
    dplyr::arrange(desc(production)) %>% 
    dplyr::mutate(acc.p = cumsum(production) / sum(production))
  
  return(spam_input)
  
}
