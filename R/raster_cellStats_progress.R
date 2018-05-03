#' @title Raster cellStats with progress bar
#' 
#' @description This function includes a progress bar to raster::cellStat
#'  
#' @param x a Raster* object 
#' 
#' @param .pb a progressbar, for details see ?dplyr::progress_estimated 
#' 
#' @return The function returns the raster statistics across cells, for details see ?raster::cellStats
#' 
#' @author Victor Maus, \email{victor.maus@@wu.ac.at}
#' 
raster_cellStats_progress <- function(x, .pb=NULL, ...) {
  
  if ((!is.null(.pb)) && inherits(.pb, "Progress") && (.pb$i < .pb$n)) .pb$tick()$print()
  
  raster::cellStats(raster::raster(x), ...)
  
}