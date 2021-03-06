# Install packages from GitHub and CRAN/MRAN:
install.packages("devtools")
devtools::install_github("tidyverse/tidyverse")
devtools::install_github("tidyverse/ggplot2")
devtools::install_github("r-spatial/sf")
devtools::install_github("r-spatial/stars")
devtools::install_github("haleyjeppson/ggmosaic")
install.packages(c("data.table", "reshape2", "FAOSTAT", "xlsx", "openxlsx", "XLConnect"))
install.packages(c("raster", "maps", "maptools", "rgeos", "ggthemes", "rgdal", "viridis"))
install.packages(c("ggmosaic"), repos = "https://cloud.r-project.org")
