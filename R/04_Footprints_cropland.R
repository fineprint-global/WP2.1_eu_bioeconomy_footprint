##########################################################################
##  Allocate non-food-SUAs to Final Demand (=non-food-Footprint)
##  (Footprint Calculus with diagonalized Direct Intensity)
##########################################################################

rm(list=ls())
path = "~/Arbeitsordner/MRIO codes & data/R/UBA Land/"
setwd(path)
library("openxlsx")
library(reshape)
library("OpenMx")

nrsua = 17    # number of sua commodities
nrreg = 21    # number of regions
nrsec = 200   # number of IO sectors
nrfd = 7      # number of final demand categories
years = 1995:2010   # years considered in the analysis
ID <- vector(mode="integer")
for(regions in 1:nrreg)  ID <- c(ID,rep(regions,nrsec))


##########################################################################
# Calculate Footprint with Y[1:7]
##########################################################################

FP_all <- list()
FP_year <- list()

# year = 2010
for(year in years){
  print(paste0("year ",year))
  # for the years 1990-1995 use the IO model for the year 1995
  yearIO <- year
  if(year<1995) yearIO <- 1995
  # load L and Y
  load(paste0(path,year,"/",year,"_extensions_cropland.RData"))
  load(paste0(path,yearIO,"/",yearIO,"_L.RData"))
  load(paste0(path,yearIO,"/",yearIO,"_Y.RData"))
  
  FP_final <- matrix(nrow=nrreg,ncol=nrsua+1)
  
  # region = 4
  for(region in 1:nrreg){
    Yreg <- rowSums(Y[,(nrfd*region-nrfd+1):(nrfd*region)])
    print(paste0("region ",region))
    
    # sua = 9  # (9=veg oils, 16=alc)
    for(sua in 1:nrsua){
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
    
    
    #####################################
    # Save results
    #####################################
    FP_year[[region]] <- FP_final
    
  }
  FP_all[[year]] <- FP_year
  
}
save(FP_all, file=paste0(path,"results/crops_cropland.RData"))




##########################################################################
# Rearrange and write results
##########################################################################


load(file=paste0(path,"results/crops_cropland.RData"))
all <- data.frame()

for(year in years){
  print(paste0("year ",year))
  
  f <- melt(FP_all[[year]])
  c <- cast(f, X1 ~ L1 ~ X2)
  # write.xlsx(c[,,18], file=paste0(path,"cropland_FP_1995.xlsx"))
  
  f$year <- year
  f$type <- 1
  
  load(file=paste0(path,year,"/",year,"_extensions_cropland.RData"))
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
write.csv(file=paste0(path,"results/crops_cropland.csv"), x = all)
save(all, file=paste0(path,"results/crops_cropland_list.RData"))


