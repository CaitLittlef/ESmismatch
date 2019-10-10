#####################################
# Install packages if not already installed
required.packages <- c("ncdf4", "ggplot2", "raster", "sf", "rgdal", "dplyr",
                       "tidyverse", "maptools", "rgeos", "Rcpp", "fasterize")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) install.packages(new.packages)
rm(required.packages, new.packages)

# Libraries
library(ggplot2)
library(raster)
library(sf)
library(rgdal)
library(dplyr)
library(tidyverse)
library(rgeos)
library(ncdf4)
library(fasterize)


## Turn off scientific notation
options(scipen=999) 

## Get current date for saving files
currentDate <- Sys.Date()

## Set directories
setwd("D:/Shared/BackedUp/Caitlin/ES-SupplyDemand")
# setwd("//goshawk.sefs.uw.edu/Space_Lawler/Shared/BackedUp/Caitlin/ES-SupplyDemand")
wd <- ("D:/Shared/BackedUp/Caitlin/ES-SupplyDemand")
# wd <- ("//goshawk.sefs.uw.edu/Space_Lawler/Shared/BackedUp/Caitlin/ES-SupplyDemand")

lulc.dir <- paste0(wd,"/Sohl_2014_LULC/Landcover/")
for.dir <- paste0(wd,"/Sohl_2014_LULC/Forest/")
c.dir <- paste0(wd,"/Sleeter_2018_C/")
county.dir <- paste0(wd,"/counties_2016/")
nlcd.dir <- ("D:/Shared/Scratch/Data/NLCD_2016/")
out.dir <- paste0(wd,"/out/")


##############################################
## To plot raster in ggplot, extract values into tibble
# ref: https://stackoverflow.com/questions/47116217/overlay-raster-layer-on-map-in-ggplot2-in-r
# Define function to extract raster values into a tibble
gplot_data <- function(x, maxpixels = 50000)  {
  x <- raster::sampleRegular(x, maxpixels, asRaster = TRUE)
  coords <- raster::xyFromCell(x, seq_len(raster::ncell(x)))
  ## Extract values
  dat <- utils::stack(as.data.frame(raster::getValues(x))) 
  names(dat) <- c('value', 'variable')
  
  dat <- dplyr::as.tbl(data.frame(coords, dat))
  
  if (!is.null(levels(x))) {
    dat <- dplyr::left_join(dat, levels(x)[[1]], 
                            by = c("value" = "ID"))
  }
  dat
}



# Text extraction functions
left = function(text, num_char) {
  substr(text, 1, num_char)
}

mid = function(text, start_num, num_char) {
  substr(text, start_num, start_num + num_char - 1)
}

right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}
