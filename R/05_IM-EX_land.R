#' @title Aggregate Agricultural Land Area Imports & Exports per Major Region
#' 
#' @description This function aggregates the imports and exports for each major region, as defined in the code, from a list of detailed results.
#' 
#' @return The function returns a data.frame with all imports and exports per major region in long format.
#' 
#' @author Martin Bruckner, \email{martin.bruckner@@wu.ac.at}
#' 


aggregate_major_regions <- function(){
  
  library(reshape2)
  library(data.table)
  
  
  ##########################################################################
  # aggregate to 10 Major Regions (MREG)
  ##########################################################################
  aggreg <- data.frame(MRIO_No = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21),
                       MRIO_ID = c("AUT","REU1","REU2","DEU","USA","JPN","CHN","CAN","RASI","BRA","IND","MEX","RUS","AUS","REUR","TUR","IDN","ZAF","RLAM","RAFR","RMIE"),
                       MREG_No = c(102,102,102,102,101,108,105,101,107,104,106,104,103,108,103,109,107,110,104,110,109),
                       MREG_ID = c("EU28","EU28","EU28","EU28","NAM","JPAU","CHN","NAM","RASI","LAM","IND","LAM","OEUR","JPAU","OEUR","MEA","RASI","AFR","LAM","AFR","MEA"))
  
  #-------------------------------------------------------------------------
  # crops cropland
  #-------------------------------------------------------------------------
  load(file=paste0("./output/crops_cropland_list.RData"))
  all <- all[!all$com==18 & !all$value==0,]
  nrow(all)
  all <- merge(all, aggreg[,c(1,3)], by.x="from", by.y="MRIO_No")
  nrow(all)
  all <- aggregate(value ~ MREG_No + to + com + year + type, all, sum)
  names(all)[1] <- "from"
  nrow(all)
  all <- merge(all, aggreg[,c(1,3)], by.x="to", by.y="MRIO_No")
  nrow(all)
  all <- aggregate(value ~ from + MREG_No + com + year + type, all, sum)
  names(all)[2] <- "to"
  
  cropsIM <- aggregate(value ~ year + to + com, all[!all$from==all$to,], sum)
  cropsEX <- aggregate(value ~ year + from + com, all[!all$from==all$to,], sum)
  
  
  #-------------------------------------------------------------------------
  # lvst cropland
  #-------------------------------------------------------------------------
  load(file=paste0("./output/livestock_cropland_list.RData"))
  all <- all[!all$com==26 & !all$value==0,]
  all <- merge(all, aggreg[,c(1,3)], by.x="from", by.y="MRIO_No")
  all <- aggregate(value ~ MREG_No + to + com + year + type, all, sum)
  names(all)[1] <- "from"
  all <- merge(all, aggreg[,c(1,3)], by.x="to", by.y="MRIO_No")
  all <- aggregate(value ~ from + MREG_No + com + year + type, all, sum)
  names(all)[2] <- "to"
  
  lvstcropsIM <- aggregate(value ~ year + to + com, all[!all$from==all$to,], sum)
  lvstcropsEX <- aggregate(value ~ year + from + com, all[!all$from==all$to,], sum)
  
  
  #-------------------------------------------------------------------------
  # lvst grassland
  #-------------------------------------------------------------------------
  load(file=paste0("./output/livestock_pastures_list.RData"))
  all <- all[!all$com==26 & !all$value==0,]
  all <- merge(all, aggreg[,c(1,3)], by.x="from", by.y="MRIO_No")
  all <- aggregate(value ~ MREG_No + to + com + year + type, all, sum)
  names(all)[1] <- "from"
  all <- merge(all, aggreg[,c(1,3)], by.x="to", by.y="MRIO_No")
  all <- aggregate(value ~ from + MREG_No + com + year + type, all, sum)
  names(all)[2] <- "to"
  
  lvstgrassIM <- aggregate(value ~ year + to + com, all[!all$from==all$to,], sum)
  lvstgrassEX <- aggregate(value ~ year + from + com, all[!all$from==all$to,], sum)
  
  
  #-------------------------------------------------------------------------
  # add together
  #-------------------------------------------------------------------------
  names(cropsIM) <- c("year", "reg", "com", "value")
  names(cropsEX) <- c("year", "reg", "com", "value")
  names(lvstcropsIM) <- c("year", "reg", "com", "value")
  names(lvstcropsEX) <- c("year", "reg", "com", "value")
  names(lvstgrassIM) <- c("year", "reg", "com", "value")
  names(lvstgrassEX) <- c("year", "reg", "com", "value")
  
  cropsIM$land <- "crops.cropland"
  cropsEX$land <- "crops.cropland"
  lvstcropsIM$land <- "lvst.cropland"
  lvstcropsEX$land <- "lvst.cropland"
  lvstgrassIM$land <- "lvst.pastures"
  lvstgrassEX$land <- "lvst.pastures"
  cropsIM$flow <- "Import"
  cropsEX$flow <- "Export"
  lvstcropsIM$flow <- "Import"
  lvstcropsEX$flow <- "Export"
  lvstgrassIM$flow <- "Import"
  lvstgrassEX$flow <- "Export"
  
  all <- rbind(cropsIM,lvstcropsIM,lvstgrassIM,cropsEX,lvstcropsEX,lvstgrassEX)
  
  return(all)
}




