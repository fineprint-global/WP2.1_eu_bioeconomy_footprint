#' @title Prepare cropland extensions for all crop input commodities and years
#' 
#' @description This function loads the input data for crop commodities generated with IIASA's LANDFLOW model and generates 
#' extensions to the MRIO model using the previously derived allocation shares.
#' 
#' @param years An integer vector specifying the years to be parsed. Default is 1995:2010.
#' 
#' @param nrreg An integer specifying the number of regions in the MRIO table. Default is 21.
#' 
#' @param nrsec An integer specifying the number of sectors or products in the MRIO table. Default is 200.
#' 
#' @param nrinput An integer specifying the number of input commodities allocated the MRIO model. Default is 17.
#' 
#' @return The function returns an integer code 0 for success and non-zero for failure.
#' 
#' @author Martin Bruckner, \email{martin.bruckner@@wu.ac.at}
#' 
#' 

prepare_mrio_extensions_crop <- function(years = 1995:2010, nrreg = 21, nrsec = 200, nrinput = 17, ...){

  library(openxlsx)
  library(reshape2)
  
  # Read input data generated with IIASA's LANDFLOW model
  data <- read.xlsx(paste0("./input/LANDFLOW_Crop_v4_28Jun2015_final.xlsx"), sheet = 5, startRow = 1, colNames = TRUE, rowNames = FALSE)
  # Filter data with unit=3 (1000 ha)
  data <- data[data$UNIT==3,c(1:4,14)]
  dataset <- list()
  for(year in years){
    temp <- matrix(0,nrinput+1,nrreg)
    # make pivot tables for each year with inputs x regions
    dataset[[year]] <- as.matrix(cast(data[data$YEAR==year,c(2,4,5)], COM ~ REGC, value='Other.Use'))
    # d = data.frame(dataset[[1995]])
    # aggregate 28 regions to 21 and rearrange order
    temp[,1] <- dataset[[year]][,3]
    temp[,2] <- dataset[[year]][,17]
    temp[,3] <- dataset[[year]][,18]
    temp[,4] <- dataset[[year]][,7]
    temp[,5] <- dataset[[year]][,15]
    temp[,6] <- dataset[[year]][,11]
    temp[,7] <- dataset[[year]][,6]
    temp[,8] <- dataset[[year]][,5]
    temp[,9] <- dataset[[year]][,22] + dataset[[year]][,23] + dataset[[year]][,24] + dataset[[year]][,28]
    temp[,10] <- dataset[[year]][,4]
    temp[,11] <- dataset[[year]][,9]
    temp[,12] <- dataset[[year]][,12]
    temp[,13] <- dataset[[year]][,13]
    temp[,14] <- dataset[[year]][,2]
    temp[,15] <- dataset[[year]][,19]
    temp[,16] <- dataset[[year]][,14]
    temp[,17] <- dataset[[year]][,10]
    temp[,18] <- dataset[[year]][,16]
    temp[,19] <- dataset[[year]][,1] + dataset[[year]][,20] + dataset[[year]][,21]
    temp[,20] <- dataset[[year]][,26] + dataset[[year]][,27]
    temp[,21] <- dataset[[year]][,8] + dataset[[year]][,25]
    dataset[[year]] <- temp[-(nrinput+1),]
  }
  rm(data,temp)
  
  for(year in years){
    # for the years 1990-1995 use the IO model for the year 1995
    yearIO <- year
    if(year<1995) yearIO <- 1995
    # Read output data from MRIO
    load(paste0("./output/exiobase/",yearIO,"_x.RData"))
    
    # load Z & Y shares
    load(paste0("./output/exiobase/",yearIO,"_ZYshares.RData"))
    # make empty Extension matrices
    extensions_abs <- matrix(nrow=nrreg*nrsec,ncol=nrinput)
    FDextensions <- matrix(nrow=nrreg,ncol=nrinput)
    
    for(region in 1:nrreg){
      
      for(input in 1:nrinput){
        ##########################################################################
        # Calculate further processing (Zshares) of input items (= Env. Extensions)
        # dimension Zshares[[1:21]][[1:17]][1:200]
        # dimension extensions[1:4200,1:17]
        ##########################################################################
        for(sector in 1:nrsec){
          extensions_abs[((region-1)*nrsec+sector),input] <- as.matrix(apply(as.matrix(dataset[[year]][input,region]),2,as.numeric) * Zshares[[region]][[input]][sector])
        }
        
      }
      
      ##########################################################################
      # Calculate direct FD (Yshares) of input items
      # dimension Yshares[[1:21]][[1:17]][1]
      # dimension data[[1995:2010]][1:17,1:21]
      ##########################################################################
      FDextensions[region,] <- as.matrix(apply(as.matrix(dataset[[year]][,region]),2,as.numeric) * as.matrix(apply(as.matrix(Yshares[[region]]),2,as.numeric)[1:nrinput,]))
      
    }
    ##########################################################################
    # Divide absolute extension values by sector output
    ##########################################################################
    extensions <- extensions_abs/as.vector(as.matrix(x))
    # delete Nan and Inf values resulting from division by 0
    extensions[extensions=="NaN"] <- 0
    extensions[extensions=="Inf"] <- 0
    
    
    ##########################################################################
    # Save results
    ##########################################################################
    save(extensions,extensions_abs,FDextensions, file = paste0("./output/exiobase/",year,"_extensions_cropland.RData"))
    
    # write.xlsx(extensions,paste0("./output/exiobase/",year,"_extensions_cropland.xlsx"))
    # write.xlsx(FDextensions,paste0("./output/exiobase/",year,"_FDextensions_cropland.xlsx"))
    
    
  }
  return(0)
}


