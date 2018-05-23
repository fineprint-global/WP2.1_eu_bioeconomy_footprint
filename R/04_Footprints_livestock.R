##########################################################################
##  Allocate non-food-SUAs to Final Demand (=non-food-Footprint)
##  (Footprint Calculus with diagonalized Direct Intensity)
##########################################################################

rm(list=ls())
path = "~/Arbeitsordner/MRIO codes & data/R/UBA Land/"
setwd(path)
library("openxlsx")
library(reshape)
library(XLConnect)
library("OpenMx")

nrsua = 25     # number of cropland + livestock sua commodities May 2015 (excl. total)
startsua = 18  # livestock suas start from sua 18 in the total list of sua commodities
endsua = 25   # last sua to be considered
nrreg = 21    # number of regions
nrsec = 200   # number of IO sectors
nrfd = 7      # number of final demand categories
years = 1995:2010   # years considered in the analysis
ID <- vector(mode="integer")
for(regions in 1:nrreg)  ID <- c(ID,rep(regions,nrsec))
extension.type = 3:5  # 3 --> 1000 ha cropland  /  4 --> 1000 ha pasture land  /  5 --> 1000 ha equiv. pasture land
land.type <- c("","","_livestock_cropland","_livestock_pastures","_livestock_equ.pastures")


##########################################################################
# Calculate Footprint with Y[1:7]
##########################################################################

# unit = 3
for(unit in extension.type){
  print(paste0("unit ",unit))
  # Filter data with unit=4 --> 1000 ha pasture land  /  unit=5 --> 1000 ha equiv. pasture land
  lvst <- land.type[unit]
  if(unit>3) endsua <- 21
  FP_all <- list()
  FP_year <- list()
  
  # year = 2010
  for(year in years){
    print(paste0("year ",year))
    # for the years 1990-1995 use the IO model for the year 1995
    yearIO <- year
    if(year<1995) yearIO <- 1995
    # load L and Y
    load(paste0(path,year,"/",year,"_extensions",lvst,".RData"))
    load(paste0(path,yearIO,"/",yearIO,"_L.RData"))
    load(paste0(path,yearIO,"/",yearIO,"_Y.RData"))
    
    FP_final <- matrix(0, nrow=nrreg,ncol=nrsua+1)
    
    # region = 4
    for(region in 1:nrreg){
      Yreg <- rowSums(Y[,(nrfd*region-nrfd+1):(nrfd*region)])
      print(paste0("region ",region))
      
      # sua = 18  # (18=Ruminant meat and offals)
      for(sua in startsua:endsua){
        # Calculate Multiplier Matrix (Ext * LINV)
        # MP <- as.matrix(vec2diag(extensions[,sua])) %*% as.matrix(L)
        # this one is faster and gives exactly the same results:
        MP <- extensions[,sua] * as.matrix(L)
        # Calculate Footprint (MP * FD)
        FP <- as.data.frame(rowSums(t(t(MP) * Yreg)))
        # Aggregate FP
        FP$ID <- ID
        FP = aggregate(. ~ ID, data = FP, sum)
        FP_final[,sua] <- as.matrix(FP[2])
      }
      # sum up all cropland footprints in additional column
      FP_final[,nrsua+1] <- .rowSums(X=FP_final,m=nrreg,n=nrsua)
      FP_year[[region]] <- FP_final
    }
    FP_all[[year]] <- FP_year
  }
  save(FP_all, file=paste0(path,"results/",lvst,".RData"))
  
  
  
  
  ##########################################################################
  # Rearrange and write results
  ##########################################################################
  
  
  load(file=paste0(path,"results/",lvst,".RData"))
  all <- data.frame()
  
  for(year in years){
    #FP_all[[year]] 
    #FP_exp_all[[year]][[4]]
    print(paste0("year ",year))
    
    f <- melt(FP_all[[year]])
    c <- cast(f, X1 ~ L1 ~ X2)
    
    f$year <- year
    f$type <- 1
    
    load(file=paste0(path,year,"/",year,"_extensions",lvst,".RData"))
    FD <- as.data.frame(FDextensions)
    colnames(FD) <- 1:nrsua
    FD <- melt(as.matrix(FD))
    FD$L1 <- FD$X1
    FD$year <- year
    FD$type <- 2
    FD_sum <- aggregate(. ~ X1, data = FD, sum)
    FD_sum$L1 <- FD_sum$X1
    FD_sum$X2 <- 18
    FD_sum$year <- year
    FD_sum$type <- 2
    FD <- rbind(FD,FD_sum)
    
    all <- rbind(all,f)
    all <- rbind(all,FD)
    
  }
  
  colnames(all) <- c("from","com","value","to","year","type")
  write.csv(file=paste0(path,"results/",lvst,".csv"), x = all)
  save(all, file=paste0(path,"results/",lvst,"_list.RData"))
  
}

