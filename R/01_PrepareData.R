##########################################################################
##   Prepare data   (ONLY ONCE!!)
##########################################################################

rm(list=ls())
path = "~/Arbeitsordner/MRIO codes & data/R/UBA Land/"
pathraw = "W:/WU/Projekte/SRU-Projekte/04_Daten/MRIO/IO data/EXIOBASE/EXIOBASE 3.1/raw/mrIOT"
pathparsed = "~/Arbeitsordner/MRIO codes & data/IO data/EXIOBASE/EXIOBASE 3.1/parsed/"
setwd(path)
library(reshape)
library("OpenMx")

nrsec = 200   # number of IO sectors
nrfd = 7      # number of final demand categories
years = 1995:2010   # years considered in the analysis
# regions = c('01_AUT','02_R-EU28','02_R-EU28','02_R-EU28','02_R-EU28','03_DEU','02_R-EU28','02_R-EU28','02_R-EU28','02_R-EU28','02_R-EU28','02_R-EU28','02_R-EU28','02_R-EU28','02_R-EU28','02_R-EU28','02_R-EU28','02_R-EU28','02_R-EU28','02_R-EU28','02_R-EU28','02_R-EU28','02_R-EU28','02_R-EU28','02_R-EU28','02_R-EU28','02_R-EU28','02_R-EU28','04_USA','05_JPN','06_CHN','07_R-NAM','08_R-ASI','09_BRA','10_IND','11_MEX','12_RUS','13_AUS','14_R-EUR','15_TUR','06_CHN','14_R-EUR','16_IDN','17_ZAF','08_R-ASI','18_R-LAM','14_R-EUR','19_R-AFR','20_R-MIE')
regions = c('01_AUT','02_REU1','03_REU2','03_REU2','03_REU2','04_DEU','02_REU1','03_REU2','02_REU1','02_REU1','02_REU1','02_REU1','03_REU2','03_REU2','02_REU1','02_REU1','03_REU2','02_REU1','03_REU2','03_REU2','02_REU1','03_REU2','02_REU1','03_REU2','02_REU1','03_REU2','03_REU2','02_REU1','05_USA','06_JPN','07_CHN','08_CAN','09_RASI','10_BRA','11_IND','12_MEX','13_RUS','14_AUS','15_REUR','16_TUR','09_RASI','15_REUR','17_IDN','18_ZAF','09_RASI','19_RLAM','15_REUR','20_RAFR','21_RMIE')

ID <- vector(mode="integer")
for(region in regions)  for(sector in 1:nrsec){
  if(sector<10) ID <- c(ID,paste0(region,"_00",sector))
  else if (sector<100) ID <- c(ID,paste0(region,"_0",sector))
  else ID <- c(ID,paste0(region,"_",sector))
} 
IDY <- vector(mode="integer")
for(region in regions) for(fd in 1:nrfd) IDY <- c(IDY,paste0(region,"_",fd))


# year = 1995
# run time per year: 7 min

for(year in years){
  ##########################################################################
  #  Read and save Z, Y and x in the original dimensions (9800*9800)
  ##########################################################################
  
  nrreg = 49    # number of regions
  
  Z <- read.delim(file = paste0(pathraw,year,"/mrIot_3.1_",year,".txt"),
                  header = F,
                  skip = 2,
                  nrows = nrreg*nrsec+2,
                  colClasses = c("character","character","character",rep("numeric",nrreg*nrsec))
                  )[,-(1:3)]

  Y <- read.delim(file = paste0(pathraw,year,"/mrFinalDemand_3.1_",year,".txt"),
                  header = F,
                  skip = 2,
                  nrows = nrreg*nrsec+2,
                  colClasses = c("character","character","character",rep("numeric",nrreg*nrfd))
                  )[,-(1:3)]

  x <- rowSums(Z) + rowSums(Y)

  # save(Z, Y, x, file=paste0(pathparsed,year,"_ZYx.RData"))
  # load(file=paste0(pathparsed,"/",year,"_ZYx.RData"))
  
  
  ##########################################################################
  # Aggregate Z, Y and x to 21 regions
  ##########################################################################
  nrreg = 21    # number of regions
  
  Z$ID <- ID
  Z <- aggregate(. ~ ID, data = Z, sum)
  Z$ID <- NULL
  Z <- as.data.frame(t(Z[nrow(Z):1,])) # rotates Z 90? to the right
  Z$ID <- ID
  Z <- aggregate(. ~ ID, data = Z, sum)
  Z$ID <- NULL
  Z <- as.data.frame(t(Z)[ncol(Z):1,]) # rotates Z 270? to the right
  
  Y$ID <- ID
  Y <- aggregate(. ~ ID, data = Y, sum)
  Y$ID <- NULL
  Y <- as.data.frame(t(Y[nrow(Y):1,])) # rotates Y 90? to the right
  Y$IDY <- IDY
  Y <- aggregate(. ~ IDY, data = Y, sum)
  Y$IDY <- NULL
  Y <- as.data.frame(t(Y)[ncol(Y):1,]) # rotates Y 270? to the right
  
  x <- rowSums(Z) + rowSums(Y)
  
  save(Z, file=paste0(path,year,"/",year,"_Z.RData"))
  save(Y, file=paste0(path,year,"/",year,"_Y.RData"))
  save(x, file=paste0(path,year,"/",year,"_x.RData"))
  # load(file=paste0(path,year,"/",year,"_Z.RData"))
  # load(file=paste0(path,year,"/",year,"_Y.RData"))
  # load(file=paste0(path,year,"/",year,"_x.RData"))

  
  ##########################################################################
  # Aggregate Z and Y
  ##########################################################################
  
  #-----------------------------------------------------------------------
  # Aggregate Z to a 200 x 4200 matrix
  #-----------------------------------------------------------------------
  Z$ID <- 1:nrsec
  Zagg <- aggregate(. ~ ID, data = Z, sum)
  save(Zagg,file=paste0(path,year,"/",year,"_Zagg.RData"))
  Z$ID <- NULL
  
  #-----------------------------------------------------------------------
  # Aggregate Y to a 200 x 147 matrix
  #-----------------------------------------------------------------------
  Y$ID <- 1:nrsec
  Yagg = aggregate(. ~ ID, data = Y, sum)
  save(Yagg,file=paste0(path,year,"/",year,"_Yagg.RData"))
  rm(Y,Yagg,Zagg)
  
  
  ##########################################################################
  # Calculate A and L
  ##########################################################################
  
  A <- t(t(Z)/x)
  A[!is.finite(A)] <- 0  # sets all NA/NaN/Inf values to zero
  # save(A, file=paste0(path,year,"/",year,"_A.RData"))
  # gc()
  # load(file=paste0(path,year,"/",year,"_A.RData"))
  I <- diag(nrsec*nrreg)
  L <- solve(I-A)  # takes 1 min (qr.solve takes 2 min)
  # L <- qr.solve(I-A)  # in some cases (1998) solve does not work
  save(L, file=paste0(path,year,"/",year,"_L.RData"))
  rm(L,A,I,Z,x)
  
}


