#####################################
# Install packages if not already installed
required.packages <- c("ncdf4", "ggplot2", "raster", "sf", "rgdal", "dplyr",
                       "tidyverse", "maptools", "rgeos", "Rcpp")
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


## Turn off scientific notation
options(scipen=999) 

## Get current date for saving files
currentDate <- Sys.Date()

## Set directories
# setwd("D:/Shared/BackedUp/Caitlin/ES-SupplyDemand")
setwd("//goshawk.sefs.uw.edu/Space_Lawler/Shared/BackedUp/Caitlin/ES-SupplyDemand")
# wd <- ("D:/Shared/BackedUp/Caitlin/ES-SupplyDemand")
wd <- ("//goshawk.sefs.uw.edu/Space_Lawler/Shared/BackedUp/Caitlin/ES-SupplyDemand")

lulc.dir <- paste0(wd,"/Sohl_2014_LULC/Landcover/")
for.dir <- paste0(wd,"/Sohl_2014_LULC/Forest/")
c.dir <- paste0(wd,"/Sleeter_C/")
nlcd.dir <- paste0(wd,"/NLCD_2016/")
out.dir <- paste0(wd,"/out/")

