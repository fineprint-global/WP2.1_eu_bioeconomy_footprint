##########################################################################
##  Build Env. Extensions for all SUAs and years
##########################################################################

rm(list=ls())
path = "~/Arbeitsordner/MRIO codes & data/R/UBA Land/"
setwd(path)
library("openxlsx")
library(reshape)

nrsua = 25     # number of cropland + livestock sua commodities May 2015 (excl. total)
startsua = 18  # livestock suas start from sua 18 in the total list of sua commodities
nrreg = 21     # number of regions
nrsec = 200    # number of IO sectors
years = 1995:2010   # years considered in the analysis
extension.type = 3:5  # 3 --> 1000 ha cropland  /  4 --> 1000 ha pasture land  /  5 --> 1000 ha equiv. pasture land


##########################
# Read data
##########################
# Read SUA data from IIASA
rawdata <- read.xlsx(paste0(path,"IIASA_data/LvstDb_v4_28Jun2015_final.xlsx"), sheet = 5, startRow = 1, colNames = TRUE, rowNames = FALSE)

for(unit in extension.type){
  print(paste0("unit ",unit))
  # Filter data with unit=3 (1000 ha cropland)  /  unit=4 --> 1000 ha pasture land  /  unit=5 --> 1000 ha equiv. pasture land
  data <- rawdata[rawdata$UNIT==unit,c(1:4,14)]
  dataset <- list()
  
  # unit = 3
  # year = 1995
  for(year in years){
    temp <- matrix(0,nrsua-startsua+2,nrreg)
    # make pivot tables for each year with SUAs x regions
    dataset[[year]] <- as.matrix(cast(data[data$YEAR==year,c(2,4,5)], COM ~ REGC, value='Other.Use'))
    # dataset[[1995]]
    # aggregate 28 regions to 21 and rearrange order
    if(unit>3){   # for pastures, only 4 suas are relevant, all others are zero
      temp[1:5,1] <- dataset[[year]][,3]
      temp[1:5,2] <- dataset[[year]][,17]
      temp[1:5,3] <- dataset[[year]][,18]
      temp[1:5,4] <- dataset[[year]][,7]
      temp[1:5,5] <- dataset[[year]][,15]
      temp[1:5,6] <- dataset[[year]][,11]
      temp[1:5,7] <- dataset[[year]][,6]
      temp[1:5,8] <- dataset[[year]][,5]
      temp[1:5,9] <- dataset[[year]][,22] + dataset[[year]][,23] + dataset[[year]][,24] + dataset[[year]][,28]
      temp[1:5,10] <- dataset[[year]][,4]
      temp[1:5,11] <- dataset[[year]][,9]
      temp[1:5,12] <- dataset[[year]][,12]
      temp[1:5,13] <- dataset[[year]][,13]
      temp[1:5,14] <- dataset[[year]][,2]
      temp[1:5,15] <- dataset[[year]][,19]
      temp[1:5,16] <- dataset[[year]][,14]
      temp[1:5,17] <- dataset[[year]][,10]
      temp[1:5,18] <- dataset[[year]][,16]
      temp[1:5,19] <- dataset[[year]][,1] + dataset[[year]][,20] + dataset[[year]][,21]
      temp[1:5,20] <- dataset[[year]][,26] + dataset[[year]][,27]
      temp[1:5,21] <- dataset[[year]][,8] + dataset[[year]][,25]
    }
    else{
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
    }
    
    temp2 <- matrix(0,nrsua,nrreg)
    temp2[startsua:nrsua,] <- temp[-(nrsua-startsua+2),] # excl. commodity 11 (= sum over commodities 1 to 8)
    dataset[[year]] <- temp2
  }
  rm(temp, temp2)
  
  for(year in years){
    # for the years 1990-1995 use the IO model for the year 1995
    yearIO <- year
    if(year<1995) yearIO <- 1995
    # Read output data from MRIO
    load(paste0(path,yearIO,"/",yearIO,"_x.RData"))
    
    # load Z & Y shares
    load(paste0(path,yearIO,"/",yearIO,"_ZYshares.RData"))
    # make empty Extension matrices
    extensions_abs <- matrix(nrow=nrreg*nrsec,ncol=nrsua)
    FDextensions <- matrix(nrow=nrreg,ncol=nrsua)
    
    for(region in 1:nrreg){
      
      for(sua in startsua:nrsua){
        ##########################################################################
        # Calculate further processing (Zshares) of SUA items (= Env. Extensions)
        # dimension Zshares[[1:21]][[1:17]][1:200]
        # dimension extensions[1:4200,1:17]
        ##########################################################################
        for(sector in 1:nrsec){
          extensions_abs[((region-1)*nrsec+sector),sua] <- as.matrix(apply(as.matrix(dataset[[year]][sua,region]),2,as.numeric) * Zshares[[region]][[sua]][sector])
        }
        
      }
      
      ##########################################################################
      # Calculate direct FD (Yshares) of SUA items
      # dimension Yshares[[1:21]][[1:17]][1]
      # dimension data[[1995:2010]][1:17,1:21]
      ##########################################################################
      FDextensions[region,] <- as.matrix(apply(as.matrix(dataset[[year]][,region]),2,as.numeric) * as.matrix(apply(as.matrix(Yshares[[region]]),2,as.numeric)[1:nrsua,]))
      
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
    title <- c("","","_extensions_livestock_cropland.RData","_extensions_livestock_pastures.RData","_extensions_livestock_equ.pastures.RData")
    save(extensions,extensions_abs,FDextensions, file = paste0(path,year,"/",year,title[unit]))
    
    # write.xlsx(extensions,paste0(path,year,"/",year,"_extensions_livestock_cropland.xlsx"))
    # write.xlsx(FDextensions,paste0(path,year,"/",year,"_FDextensions_livestock_cropland.xlsx"))
    
    
  }
}



