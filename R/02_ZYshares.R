##################################################################################
##  Calculate shares to allocate SUA-extensions
##################################################################################

rm(list=ls())
path = "~/Arbeitsordner/MRIO codes & data/R/UBA Land/"
setwd(path)
library(openxlsx)
library(reshape)

nrsua = 28    # number of sua commodities
nrreg = 21    # number of regions
nrsec = 200   # number of IO sectors
years = 1995:2010   # years considered in the analysis


# year = 1995
for(year in years){
  print(paste0("year ", year))
  
  # Load Zagg and Yagg
  load(paste0(path,year,"/",year,"_Zagg.RData"))
  load(paste0(path,year,"/",year,"_Yagg.RData"))
  
  
  ##################################################################################
  # Build SUA allocation share matrices for Z and Y for nrreg regions and nrsua SUAs
  ##################################################################################
  
  ##################################################################################
  # read nrsua SUA-MRIO-Link-matrices with 0 and 1 for each element in Z and Y
  ##################################################################################
  SUA_Z <- list()
  SUA_Y <- list()
  for(sua in 1:nrsua){
    data <- read.xlsx(paste0("Link_a.xlsx"), sheet = sua, startRow = 1, colNames = TRUE, rowNames = TRUE)
    SUA_Z[[sua]] <- data[,1:200]
    SUA_Y[[sua]] <- data[,201:207]
  }
  rm(data)
  
  ##################################################################################
  # make list with nrsua matrices with global Z and Y relevant for each SUA
  # dimension ZaggSUA[[1:23]][1:200,1:4200]
  # dimension YaggSUA[[1:23]][1:200,1:147]
  ##################################################################################
  ZaggSUA <- list()
  YaggSUA <- list()
  for(sua in 1:nrsua){
    ZaggSUA[[sua]] <- matrix(as.matrix(SUA_Z[[sua]]),200,nrreg*nrsec) * Zagg[,2:(nrreg*nrsec+1)]
    YaggSUA[[sua]] <- matrix(as.matrix(SUA_Y[[sua]]),200,nrreg*7) * Yagg[,2:(nrreg*7+1)]
  }
  rm(SUA_Z,SUA_Y)
  
  ##################################################################################
  # reshape ZaggSUA and YaggSUA to 2-dimensional lists with one matrix 
  # for each region (20) and SUA (15)
  # dimension ZaggSUA[[1:20]][[1:15]][1:200,1:200]
  # dimension YaggSUA[[1:20]][[1:15]][1:200,1:7]
  ##################################################################################
  Zlist <- list()
  Ylist <- list()
  for(region in 1:nrreg){
    Ztemp <- list()
    Ytemp <- list()
    for(sua in 1:nrsua){
      Ztemp[[sua]] <- ZaggSUA[[sua]][,(region*200-199):(region*200)]
      Ytemp[[sua]] <- YaggSUA[[sua]][,(region*7-6):(region*7)]
    }
    Zlist[[region]] <- Ztemp
    Ylist[[region]] <- Ytemp
  }
  ZaggSUA <- Zlist
  YaggSUA <- Ylist
  rm(Ztemp,Ytemp,Zlist,Ylist)
  
  ##################################################################################
  # make list with sums for all regions and SUAs
  # dimension X[[1:20]][[1:15]][1]
  ##################################################################################
  X <- list()
  for(region in 1:nrreg){
    Xtemp <- list()
    for(sua in 1:nrsua){
      Xtemp[[sua]] <- sum(colSums(ZaggSUA[[region]][[sua]]), colSums(YaggSUA[[region]][[sua]]))
    }
    X[[region]] <- Xtemp
  }
  rm(Xtemp)
  
  ##################################################################################
  # make list with allocation shares for Z and Y
  # dimension Zshares[[1:20]][[1:15]][1:200]
  # dimension Yshares[[1:20]][[1:15]][1]
  ##################################################################################
  Zshares <- ZaggSUA
  Yshares <- YaggSUA
  for(region in 1:nrreg){
    for(sua in 1:nrsua){
      if(X[[region]][[sua]]==0){
        Zshares[[region]][[sua]] <- colSums(ZaggSUA[[region]][[sua]])
        Yshares[[region]][[sua]] <- sum(colSums(YaggSUA[[region]][[sua]]))
      }
      else{
        Zshares[[region]][[sua]] <- colSums(ZaggSUA[[region]][[sua]] / X[[region]][[sua]])
        Yshares[[region]][[sua]] <- sum(colSums(YaggSUA[[region]][[sua]] / X[[region]][[sua]]))
      }
    }
  }
  rm(ZaggSUA,YaggSUA,X,region,sua)
  
  
  
  #######################################################################################
  #######################################################################################
  ##
  ##   Repair errors
  ##
  #######################################################################################
  #######################################################################################
  
  
  
#   ##########################################################################
#   # 1. repair missing fibre crop sector for Germany (region 4)
#   #   -> allocate fibre crops (sua 15) to the textiles sector (55)
#   ##########################################################################
#   Zshares[[4]][[15]][55] <- 1
#   
#   
  
  #############################################################################
  # 2. repair missing biofuels sector for JPN and RUS (regions 6, 13)
  #   -> REPLACE  SUA 16 (Alcohol) 
  #############################################################################
  
  ##################################################################################
  # read Version b of SUA-MRIO-Link-matrices
  ##################################################################################
  SUA_Z <- list()
  SUA_Y <- list()
  for(sua in c(16)){
    data <- read.xlsx(paste0("Link_b.xlsx"), sheet = sua, startRow = 1, colNames = TRUE, rowNames = TRUE)
    SUA_Z[[sua]] <- data[,1:200]
    SUA_Y[[sua]] <- data[,201:207]
  }
  rm(data)
  
  ##################################################################################
  # make list with 15 matrices with global Z and Y relevant for each SUA
  # dimension ZaggSUA[[1:15]][1:200,1:4000]
  # dimension YaggSUA[[1:15]][1:200,1:140]
  ##################################################################################
  ZaggSUA <- list()
  YaggSUA <- list()
  for(sua in c(16)){
    ZaggSUA[[sua]] <- matrix(as.matrix(SUA_Z[[sua]]),200,nrreg*nrsec) * Zagg[,2:(nrreg*nrsec+1)]
    YaggSUA[[sua]] <- matrix(as.matrix(SUA_Y[[sua]]),200,nrreg*7) * Yagg[,2:(nrreg*7+1)]
  }
  rm(SUA_Z,SUA_Y)
  
  ##################################################################################
  # reshape ZaggSUA and YaggSUA to 2-dimensional lists with one matrix 
  # for each region (20) and SUA (15)
  # dimension ZaggSUA[[1:20]][[1:15]][1:200,1:200]
  # dimension YaggSUA[[1:20]][[1:15]][1:200,1:7]
  ##################################################################################
  Zlist <- list()
  Ylist <- list()
  for(region in 1:nrreg){
    Ztemp <- list()
    Ytemp <- list()
    for(sua in c(16)){
      Ztemp[[sua]] <- ZaggSUA[[sua]][,(region*200-199):(region*200)]
      Ytemp[[sua]] <- YaggSUA[[sua]][,(region*7-6):(region*7)]
    }
    Zlist[[region]] <- Ztemp
    Ylist[[region]] <- Ytemp
  }
  ZaggSUA <- Zlist
  YaggSUA <- Ylist
  rm(Ztemp,Ytemp,Zlist,Ylist)
  
  ##################################################################################
  # make list with sums for all regions and SUAs
  # dimension X[[1:20]][[1:15]][1]
  ##################################################################################
  X <- list()
  for(region in 1:nrreg){
    Xtemp <- list()
    for(sua in c(16)){
      Xtemp[[sua]] <- sum(colSums(ZaggSUA[[region]][[sua]]), colSums(YaggSUA[[region]][[sua]]))
    }
    X[[region]] <- Xtemp
  }
  rm(Xtemp)
  
  ##################################################################################
  # make list with allocation shares for Z and Y
  # dimension Zshares[[1:20]][[1:15]][1:200]
  # dimension Yshares[[1:20]][[1:15]][1]
  ##################################################################################
  ZsharesB <- ZaggSUA
  YsharesB <- YaggSUA
  for(region in 1:nrreg){
    for(sua in c(16)){
      if(X[[region]][[sua]]==0){
        ZsharesB[[region]][[sua]] <- colSums(ZaggSUA[[region]][[sua]])
        YsharesB[[region]][[sua]] <- sum(colSums(YaggSUA[[region]][[sua]]))
      }
      else{
        ZsharesB[[region]][[sua]] <- colSums(ZaggSUA[[region]][[sua]] / X[[region]][[sua]])
        YsharesB[[region]][[sua]] <- sum(colSums(YaggSUA[[region]][[sua]] / X[[region]][[sua]]))
      }
    }
  }
  rm(ZaggSUA,YaggSUA,X,region,sua)
  
  
  ##################################################################################
  # REPLACE  SUA 16 (Alcohol) for JPN and RUS (regions 6, 13) for specific years
  ##################################################################################
  
  sua <- 16
  if(year <= 2003){
    region <- 13
    Zshares[[region]][[sua]] <- ZsharesB[[region]][[sua]]
    Yshares[[region]][[sua]] <- YsharesB[[region]][[sua]]
  }
  if(year >= 2002){
    region <- 6
  Zshares[[region]][[sua]] <- ZsharesB[[region]][[sua]]
  Yshares[[region]][[sua]] <- YsharesB[[region]][[sua]]
  }
  
  
  ##########################################################################
  # save data
  ##########################################################################
  save(Zshares, Yshares, file=paste0(path,year,"/",year,"_ZYshares.RData"))
  # write.xlsx(Yshares,paste0(path,year,"/",year,"_Yshares.xlsx"))
  # write.xlsx(Zshares,paste0(path,year,"/",year,"_Zshares.xlsx"))
  rm(Yshares,Zshares)
  
}



##########################################################################
##  check for 0 values in the Z & Y shares
##########################################################################

rm(list=ls())
path = "~/Arbeitsordner/MRIO codes & data/R/UBA Land/"
setwd(path)
library("openxlsx")

nrsua = 23    # number of sua commodities
nrreg = 20    # number of regions
years = 1995:2010   # years considered in the analysis
protocol <- "year;region;sua"   # error (zero) protocol

for(year in years){
  # load Z & Y shares
  load(paste0(path,year,"/",year,"_ZYshares.RData"))
  
  for(region in 1:nrreg){
    
    for(sua in 1:nrsua){
      
      if(sum(apply(as.matrix(Zshares[[region]][[sua]]),2,as.numeric)) + as.numeric(Yshares[[region]][[sua]]) == 0){
        protocol <- c(protocol,paste0("y",year,";r",region,";s",sua))
      }
      
    }
  }
}

write.xlsx(protocol,paste0(path,"Error Protocol.xlsx"))


