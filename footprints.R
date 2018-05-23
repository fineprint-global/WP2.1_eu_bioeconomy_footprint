##########################################################################
##  Calculate Footprints
##########################################################################
library(tidyverse)
library(magrittr)

# Load functions 
sources_files <- list.files(path = "./R", pattern = "*.R", full.names = TRUE)
sources_files <- sapply(X = sources_files, FUN = source, local = .GlobalEnv)


# -----------------------------------------------
# prepare MRIO variables from EXIOBASE raw data
# -----------------------------------------------
# please note that you need to download EXIOBASE first from www.exiobase.eu,
# unzip the files and place the contained folders at ./input/exiobase/

prepare_mrio_data(years = 1995:2010, nrsec = 200, nrfd = 7)


# -----------------------------------------------
# derive allocation shares for allocating land use data to economic sectors
# -----------------------------------------------
calculate_allocation_shares(years = 1995:2010, nrreg = 21, nrsec = 200, nrinput = 28)


# -----------------------------------------------
# prepare extensions for the MRIO using the previously derived allocation shares
# -----------------------------------------------
prepare_mrio_extensions_crop(years = 1995:2010, nrreg = 21, nrsec = 200, nrinput = 17)

# specify the land type for which the livestock commodity extensions should be prepared
land_type_names <- c("","","livestock_cropland","livestock_pastures","livestock_equ.pastures")
land_type <- 3  # cropland
prepare_mrio_extensions_livestock(years = 1995:2010, nrreg = 21, nrsec = 200, nrinput = 25, start = 18, land_type = land_type, land_type_names = land_type_names)


# -----------------------------------------------
# calculate footprints
# -----------------------------------------------
footprint_crop <- calculate_footprints_crop(years = 1995:2010, nrreg = 21, nrsec = 200, nrfd = 7, nrinput = 17)
write.csv(file=paste0("./output/footprint_crop.csv"), x = footprint_crop)
save(footprint_crop, file=paste0("./output/footprint_crop_list.RData"))

# specify the land type for which the livestock commodity footprint should be calculated
land_type_names <- c("","","livestock_cropland","livestock_pastures","livestock_equ.pastures")
land_type <- 3  # cropland
footprint_livestock <- calculate_footprints_livestock(years = 1995:2010, nrreg = 21, nrsec = 200, nrinput = 25, start = 18, end = 25, land_type = land_type, land_type_names = land_type_names)
write.csv(file=paste0("./output/footprint_",land_type_name[land_type],".csv"), x = footprint_livestock)
save(footprint_livestock, file=paste0("./output/footprint_",land_type_name[land_type],"_list.RData"))


# -----------------------------------------------
# calculate imports and exports for 10 major regions
# -----------------------------------------------
# please note that this function only works if all crop and livestock footprints have been calculated and saved previously
MREG_imp_exp <- aggregate_major_regions()


# -----------------------------------------------
# add information on the origin of the biomass in crop input commodities to the footprint results
# -----------------------------------------------
footprints_origin <- add_origin_crop(footprint_crop)
write.csv(file=paste0("./output/footprint_crop_origin.csv"), x = footprints_origin)
save(footprints_origin, file=paste0("./output/footprint_crop_list_origin.RData"))

# specify the land type for which the information on the origin should be added
land_type_names <- c("","","livestock_cropland","livestock_pastures","livestock_equ.pastures")
land_type <- 3  # cropland
footprints_origin <- add_origin_livestock(footprint_livestock, land_type = land_type, land_type_names = land_type_names)
write.csv(file=paste0("./output/footprint_crop_origin.csv"), x = footprints_origin)
save(footprints_origin, file=paste0("./output/footprint_crop_list_origin.RData"))



