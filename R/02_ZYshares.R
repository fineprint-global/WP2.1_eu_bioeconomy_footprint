#' @title Calculate shares to allocate input commodities to economic sectors.
#' 
#' @description This function calculates the shares that are used later to assign the land use data to economic sectors.
#' 
#' @param years An integer vector specifying the years to be parsed. Default is 1995:2010.
#' 
#' @param nrreg An integer specifying the number of regions in the MRIO table. Default is 21.
#' 
#' @param nrsec An integer specifying the number of sectors or products in the MRIO table. Default is 200.
#' 
#' @param nrinput An integer specifying the number of input commodities allocated the MRIO model. Default is 28.
#' 
#' @return The function returns an integer code 0 for success and non-zero for failure.
#' 
#' @author Martin Bruckner, \email{martin.bruckner@@wu.ac.at}
#' 
#' 

calculate_allocation_shares <- function(years = 1995:2010, nrreg = 21, nrsec = 200, nrinput = 28, ...){
  
  require(reshape2)
  require(openxlsx)
  
  # year = 1995
  for(year in years){
    print(paste0("year ", year))
    
    # Load Zagg and Yagg
    load(paste0("./output/exiobase/",year,"_Zagg.RData"))
    load(paste0("./output/exiobase/",year,"_Yagg.RData"))
    
    
    ##################################################################################
    # Build input allocation share matrices for Z and Y for nrreg regions and nrinput commodities
    ##################################################################################
    
    ##################################################################################
    # read nrinput Commodity-MRIO-Link-matrices with 0 and 1 for each element in Z and Y
    ##################################################################################
    input_Z <- list()
    input_Y <- list()
    for(input in 1:nrinput){
      data <- read.xlsx(paste0("./input/mrio_link_a.xlsx"), sheet = input, startRow = 1, colNames = TRUE, rowNames = TRUE)
      input_Z[[input]] <- data[,1:200]
      input_Y[[input]] <- data[,201:207]
    }
    rm(data)
    
    ##################################################################################
    # make list with nrinput matrices with global Z and Y relevant for each input commodity
    # dimension Zagg_input[[1:23]][1:200,1:4200]
    # dimension Yagg_input[[1:23]][1:200,1:147]
    ##################################################################################
    Zagg_input <- list()
    Yagg_input <- list()
    for(input in 1:nrinput){
      Zagg_input[[input]] <- matrix(as.matrix(input_Z[[input]]),200,nrreg*nrsec) * Zagg[,2:(nrreg*nrsec+1)]
      Yagg_input[[input]] <- matrix(as.matrix(input_Y[[input]]),200,nrreg*7) * Yagg[,2:(nrreg*7+1)]
    }
    rm(input_Z,input_Y)
    
    ##################################################################################
    # reshape Zagg_input and Yagg_input to 2-dimensional lists with one matrix 
    # for each region (20) and input (15)
    # dimension Zagg_input[[1:20]][[1:15]][1:200,1:200]
    # dimension Yagg_input[[1:20]][[1:15]][1:200,1:7]
    ##################################################################################
    Zlist <- list()
    Ylist <- list()
    for(region in 1:nrreg){
      Ztemp <- list()
      Ytemp <- list()
      for(input in 1:nrinput){
        Ztemp[[input]] <- Zagg_input[[input]][,(region*200-199):(region*200)]
        Ytemp[[input]] <- Yagg_input[[input]][,(region*7-6):(region*7)]
      }
      Zlist[[region]] <- Ztemp
      Ylist[[region]] <- Ytemp
    }
    Zagg_input <- Zlist
    Yagg_input <- Ylist
    rm(Ztemp,Ytemp,Zlist,Ylist)
    
    ##################################################################################
    # make list with sums for all regions and input commodities
    # dimension X[[1:20]][[1:15]][1]
    ##################################################################################
    X <- list()
    for(region in 1:nrreg){
      Xtemp <- list()
      for(input in 1:nrinput){
        Xtemp[[input]] <- sum(colSums(Zagg_input[[region]][[input]]), colSums(Yagg_input[[region]][[input]]))
      }
      X[[region]] <- Xtemp
    }
    rm(Xtemp)
    
    ##################################################################################
    # make list with allocation shares for Z and Y
    # dimension Zshares[[1:20]][[1:15]][1:200]
    # dimension Yshares[[1:20]][[1:15]][1]
    ##################################################################################
    Zshares <- Zagg_input
    Yshares <- Yagg_input
    for(region in 1:nrreg){
      for(input in 1:nrinput){
        if(X[[region]][[input]]==0){
          Zshares[[region]][[input]] <- colSums(Zagg_input[[region]][[input]])
          Yshares[[region]][[input]] <- sum(colSums(Yagg_input[[region]][[input]]))
        }
        else{
          Zshares[[region]][[input]] <- colSums(Zagg_input[[region]][[input]] / X[[region]][[input]])
          Yshares[[region]][[input]] <- sum(colSums(Yagg_input[[region]][[input]] / X[[region]][[input]]))
        }
      }
    }
    rm(Zagg_input,Yagg_input,X,region,input)
    
    
    
    #######################################################################################
    #######################################################################################
    ##
    ##   Repair errors
    ##
    #######################################################################################
    #######################################################################################
    
    
    
    #   ##########################################################################
    #   # 1. repair missing fibre crop sector for Germany (region 4)
    #   #   -> allocate fibre crops (input commodity 15) to the textiles sector (55)
    #   ##########################################################################
    #   Zshares[[4]][[15]][55] <- 1
    #   
    #   
    
    #############################################################################
    # 2. repair missing biofuels sector for JPN and RUS (regions 6, 13)
    #   -> REPLACE  input commodity 16 (Alcohol) 
    #############################################################################
    
    ##################################################################################
    # read Version b of Commodity-MRIO-Link-matrices
    ##################################################################################
    input_Z <- list()
    input_Y <- list()
    for(input in c(16)){
      data <- read.xlsx(paste0("./input/mrio_link_b.xlsx"), sheet = input, startRow = 1, colNames = TRUE, rowNames = TRUE)
      input_Z[[input]] <- data[,1:200]
      input_Y[[input]] <- data[,201:207]
    }
    rm(data)
    
    ##################################################################################
    # make list with 15 matrices with global Z and Y relevant for each input commodity
    # dimension Zagg_input[[1:15]][1:200,1:4000]
    # dimension Yagg_input[[1:15]][1:200,1:140]
    ##################################################################################
    Zagg_input <- list()
    Yagg_input <- list()
    for(input in c(16)){
      Zagg_input[[input]] <- matrix(as.matrix(input_Z[[input]]),200,nrreg*nrsec) * Zagg[,2:(nrreg*nrsec+1)]
      Yagg_input[[input]] <- matrix(as.matrix(input_Y[[input]]),200,nrreg*7) * Yagg[,2:(nrreg*7+1)]
    }
    rm(input_Z,input_Y)
    
    ##################################################################################
    # reshape Zagg_input and Yagg_input to 2-dimensional lists with one matrix 
    # for each region (20) and input (15)
    # dimension Zagg_input[[1:20]][[1:15]][1:200,1:200]
    # dimension Yagg_input[[1:20]][[1:15]][1:200,1:7]
    ##################################################################################
    Zlist <- list()
    Ylist <- list()
    for(region in 1:nrreg){
      Ztemp <- list()
      Ytemp <- list()
      for(input in c(16)){
        Ztemp[[input]] <- Zagg_input[[input]][,(region*200-199):(region*200)]
        Ytemp[[input]] <- Yagg_input[[input]][,(region*7-6):(region*7)]
      }
      Zlist[[region]] <- Ztemp
      Ylist[[region]] <- Ytemp
    }
    Zagg_input <- Zlist
    Yagg_input <- Ylist
    rm(Ztemp,Ytemp,Zlist,Ylist)
    
    ##################################################################################
    # make list with sums for all regions and input commodities
    # dimension X[[1:20]][[1:15]][1]
    ##################################################################################
    X <- list()
    for(region in 1:nrreg){
      Xtemp <- list()
      for(input in c(16)){
        Xtemp[[input]] <- sum(colSums(Zagg_input[[region]][[input]]), colSums(Yagg_input[[region]][[input]]))
      }
      X[[region]] <- Xtemp
    }
    rm(Xtemp)
    
    ##################################################################################
    # make list with allocation shares for Z and Y
    # dimension Zshares[[1:20]][[1:15]][1:200]
    # dimension Yshares[[1:20]][[1:15]][1]
    ##################################################################################
    ZsharesB <- Zagg_input
    YsharesB <- Yagg_input
    for(region in 1:nrreg){
      for(input in c(16)){
        if(X[[region]][[input]]==0){
          ZsharesB[[region]][[input]] <- colSums(Zagg_input[[region]][[input]])
          YsharesB[[region]][[input]] <- sum(colSums(Yagg_input[[region]][[input]]))
        }
        else{
          ZsharesB[[region]][[input]] <- colSums(Zagg_input[[region]][[input]] / X[[region]][[input]])
          YsharesB[[region]][[input]] <- sum(colSums(Yagg_input[[region]][[input]] / X[[region]][[input]]))
        }
      }
    }
    rm(Zagg_input,Yagg_input,X,region,input)
    
    
    ##################################################################################
    # REPLACE  input commodity 16 (Alcohol) for JPN and RUS (regions 6, 13) for specific years
    ##################################################################################
    
    input <- 16
    if(year <= 2003){
      region <- 13
      Zshares[[region]][[input]] <- ZsharesB[[region]][[input]]
      Yshares[[region]][[input]] <- YsharesB[[region]][[input]]
    }
    if(year >= 2002){
      region <- 6
      Zshares[[region]][[input]] <- ZsharesB[[region]][[input]]
      Yshares[[region]][[input]] <- YsharesB[[region]][[input]]
    }
    
    
    ##########################################################################
    # save data
    ##########################################################################
    save(Zshares, Yshares, file=paste0("./output/exiobase/",year,"_ZYshares.RData"))
    # write.xlsx(Yshares,paste0("./output/exiobase/",year,"_Yshares.xlsx"))
    # write.xlsx(Zshares,paste0("./output/exiobase/",year,"_Zshares.xlsx"))
    rm(Yshares,Zshares)
    
  }
  return(0)
}



##########################################################################
##  check for 0 values in the Z & Y shares
##########################################################################

rm(list=ls())
library("openxlsx")

nrinput = 23    # number of input commodities
nrreg = 20    # number of regions
years = 1995:2010   # years considered in the analysis
protocol <- "year;region;input"   # error (zero) protocol

for(year in years){
  # load Z & Y shares
  load(paste0("./output/exiobase/",year,"_ZYshares.RData"))
  
  for(region in 1:nrreg){
    
    for(input in 1:nrinput){
      
      if(sum(apply(as.matrix(Zshares[[region]][[input]]),2,as.numeric)) + as.numeric(Yshares[[region]][[input]]) == 0){
        protocol <- c(protocol,paste0("y",year,";r",region,";i",input))
      }
      
    }
  }
}

write.xlsx(protocol,paste0("./output/exiobase/","Error Protocol.xlsx"))


