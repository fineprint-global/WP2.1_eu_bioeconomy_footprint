#' @title Mask raster for each sf geometry 
#' 
#' @description This function masks the rester according to the corresponding geometry 
#' 
#' @param x an object rasterLayer 
#' 
#' @param y a sf geometry 
#' 
#' @param crs coordenate reference system 
#'  
#' @return The function a raster maked by the geometry
#' 
#' @author Victor Maus, \email{victor.maus@@wu.ac.at}
#' 
mask_regions <- function(x, y, crs, .pb = NULL){
  
  if ((!is.null(.pb)) && inherits(.pb, "Progress") && (.pb$i < .pb$n)) .pb$tick()$print()
  
  y <- sf::st_sfc(y, crs = crs) %>% 
    as("Spatial")
  
  raster::rasterOptions(progress = "")  
  raster::mask(x = x, mask = y)
  
}
