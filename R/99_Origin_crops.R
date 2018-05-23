##########################################################################
##  Add information on the origin of the biomass in Other Use-commodities
##########################################################################

rm(list=ls())
path = "~/Arbeitsordner/MRIO codes & data/R/UBA Land/"
setwd(path)
land <- "crops_cropland"
library("openxlsx")
library(reshape)


######################################
# Read crops data
######################################
# Read SUA data from IIASA
rawdata <- read.xlsx(paste0(path,"IIASA_data/OthUseR_v4_28Jun2015_final.xlsx"), sheet = 4, startRow = 1, colNames = TRUE, rowNames = FALSE)
# Filter data with unit=3 (1000 ha)
rawdata <- rawdata[,c(1,2,4,7,8:35)]


######################################
# Aggregate columns in rawdata
######################################
data <- rawdata
temp <- data[,1:25]
temp[,1+4] <- data[,3+4]
temp[,2+4] <- data[,17+4]
temp[,3+4] <- data[,18+4]
temp[,4+4] <- data[,7+4]
temp[,5+4] <- data[,15+4]
temp[,6+4] <- data[,11+4]
temp[,7+4] <- data[,6+4]
temp[,8+4] <- data[,5+4]
temp[,9+4] <- data[,22+4] + data[,23+4] + data[,24+4] + data[,28+4]
temp[,10+4] <- data[,4+4]
temp[,11+4] <- data[,9+4]
temp[,12+4] <- data[,12+4]
temp[,13+4] <- data[,13+4]
temp[,14+4] <- data[,2+4]
temp[,15+4] <- data[,19+4]
temp[,16+4] <- data[,14+4]
temp[,17+4] <- data[,10+4]
temp[,18+4] <- data[,16+4]
temp[,19+4] <- data[,1+4] + data[,20+4] + data[,21+4]
temp[,20+4] <- data[,26+4] + data[,27+4]
temp[,21+4] <- data[,8+4] + data[,25+4]
data <- temp
rm(temp, rawdata)
colnames(data) <- c("YEAR","REGC","COM","UNIT",1:21)


######################################
# Aggregate REGC in data
######################################
regfit <- as.data.frame(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,
                   19,14,1,10,8,7,4,21,11,17,6,12,13,16,5,18,2,3,15,19,19,9,9,9,21,20,20,9), nrow=28, ncol=2))
colnames(regfit) <- c("REGC","REGio")
data <- merge(regfit, data, by = 'REGC')
data <- data[,c(2:26)]
data <- aggregate(. ~ REGio + YEAR + COM + UNIT, data=data, sum)

######################################
# calculate shares of origin
######################################
shares <- data
shares[,5:25] <- shares[,5:25] / rowSums(shares[,5:25])
shares[shares[,5]=="NaN",5:25] <- 0
colnames(shares) <- c("from","year","com","unit",1:21)


##########################################################################
# Add information on the origin to footprints
##########################################################################
load(file=paste0(path,"results/",land,"_list.RData"))
all_origin <- merge(shares, all, by = c("from","year","com"))
all_origin[,5:25] <- all_origin[,5:25] * all_origin$value
s <- as.data.frame(matrix(c(rowSums(all_origin[,5:25]),all_origin$value,all_origin$from,all_origin$year,all_origin$com), nrow(all_origin)))
s$check <- s$V1-s$V2
s[s$check < -0.01,]
t <- all_origin[rowSums(all_origin[,5:25])==0,]
l <- t[t$value > 0,]
write.csv(file=paste0(path,"results/check_",land,".csv"), x = l)

write.csv(file=paste0(path,"results/",land,"_origin.csv"), x = all_origin)
save(all_origin, file=paste0(path,"results/",land,"_list_origin.RData"))


