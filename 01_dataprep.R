#################################
##CARBON#########################
#################################

## Extract NetCDF data (from Sleeter et al. 2018)
# data: https://www.sciencebase.gov/catalog/item/5b86bfbae4b0702d0e794697
# Annual carbon variables cover 1971-2015 at 1-km (960m) spatial resolution with 3052 rows and 4823 columns. Carbon stock and flux units are in kgC/m2 and kgC/m2/yr, respectively. Data are in NetCDF format and Albers equal area projection.
# NoData val: -3.40282346639e+038
# In correspondence with Liu (190930), metadata units (KgC/m2/yr) are NOT accurate for totecoc: it's KgC/m2

# Define crs -- Albers Equal area, per metadata, but not on spatialreference.org.
# See home-growth solution (for, coincidentally, related dataset) here (nb there's diff one on proj4.org):
# https://gis.stackexchange.com/questions/291734/usa-albers-equal-area-conic-usgs-version-landfire-projection-not-read-by-r
# This matches what's in meta-data (p.4); setting parallels and meridian.
# N.b., this is designed for CONUS hence 0 thru middle of country.
crs <- crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")


## I can access data from the netCDF below, but it's unclear how to get it to line up, spatially.
## It's an array (grid), yet units are apparently in m, and bounding coordiantes are given in degrees.
## Others have struggled: https://gis.stackexchange.com/questions/271779/r-netcdf-to-raster

## Open datasource using netCDF package tools (ref: https://rpubs.com/boyerag/297592)
# nc_open(paste0(c.dir,'subset_totceco_m.nc'))
# 
# # has 1 variable (subset) which has 4 dimensions (diff than variables): long, lat, level, time
# nc_data <- nc_open(paste0(c.dir,'subset_totceco_m.nc'))
# totceco <- raster(paste0(c.dir, "subset_totceco_m.nc"))
# totcsoi <- raster(paste0(c.dir, "subset_totcsoi_m.nc"))
# # Save the print(nc) dump to a text file
# # {
# #   sink('subset_stddown_m_metadata.txt')
# #   print(nc_data)
# #   sink()
# # }
# # Gives list of 14.
# names(nc_data)
# names(nc_data$dim) # time, level, latitude, longitude
# names(nc_data$var) # subset <-- per metadata, prob from original 100 yr simulation
# dim(nc_data)
# 
# ## Unclear where values (c) are stored; extract to array (multi-dimensional)
# c.array <- ncvar_get(nc_data)
# class(c.array) # array
# dim(c.array) # 3-dim array: 4823, 3052, 45 years (1971-2015)
# c.slice <- c.array[, , 45] ; class(c.slice) # Should give last year: 2015
# # view/opening c.slice will only show first 50 of >3000 cols, that's why only "see" NAs
# # Scroll to row 1860, col 1506 and get ~5.88
# c.slice[1860, 1506]
# min(c.slice, na.rm = T) # 0
# max(c.slice, na.rm = T) # 428.7242; yet metadata says range domain max is 200; will have to drop.
# 
# # Transpose to orient correctly b/c netCDF recorded from bottom left corner
# r <- raster(t(c.slice), xmn=min(lon), xmx=max(lon),
#             ymn=min(lat), ymx=max(lat), crs=crs)
# # ^ That retained simple row & col numbers as "coordinates"
# 
# 
# # Extract spatio-temporal values
# lon <- ncvar_get(nc_data, "longitude") # 4923, these are just grid size
# lat <- ncvar_get(nc_data, "latitude") # 3052, these are just grid size
# t <- ncvar_get(nc_data, "time") # 45, these are just time size
# dim(nc_data)
# 
# head(lon) # 1, 2, 3, 4, 5, 6 <-- so, need to get actual coords
# head(lat) # 3052 3051 3050 3049 3048 3047 <-- so, need to get actual coords
# head(t) # 1, 2, 3, 4, 5, 6 <-- so, need to get actual years

## ^ typically, this should give me spatial info, but per correspondnece...
#...with data author jxliu@usgs.gov he did not include coordinates >:(
# Per his suggestion, I imported .nc into arc and saved each band (yr) as .tif.
# Then saved .tif corresponding to 2015 as float (.flt).
# Saved that in same folder with a .prj and .hdr so that .flt could find projection info.
# Then saved as .tif, which is loaded here.


## Load raster for 2015, but need to define crs else get NA for crs (IDKW). 
c2015ttl <- raster(paste0(c.dir, "r2015ttl.tif"), crs = crs)
c2015soil <- raster(paste0(c.dir, "r2015soil.tif"), crs = crs)
c2015ttl
c2015soil
# Per meta-data, NoData is -3.40e38, but in plot (below)...
#...no-data areas had v. lrg numbers (e.g., 9969209968386869047442886268468442020)
c2015ttl[c2015ttl > 9999999] <- NA
c2015soil[c2015soil > 9999999] <- NA
plot(c2015ttl)
zoom(c2015ttl)
plot(c2015soil)
zoom(c2015soil)

## LULC won't affect deep (2m) soil carbon much -- remove from consideration?
# If yes, that leaves carbon dataset with living, standing/down dead, litter 
c2015 <- c2015ttl - c2015soil
c2015
plot(c2015)

# Project to nlcd (more recent datum)
c2015 <- projectRaster(c2015, crs=crs(nlcd)); crs(c2015)
writeRaster(c2015, paste0(c.dir, "c2015.p.tif"))

# Create rough bbox for NW WA zoom; get coords with click().
# click(c2015, n = Inf, id = T, xy = T, cell = T, type = "n", show = T)
# Turn these coords into a polygon (can also extract coords to bbox())
# ext <- as(raster::extent(-165, -145, 60, 70), "SpatialPolygons")
# bbox <- bbox(ext)
# plot(c2015ttl, ext = ext) # alt: plot(c2015ttl, ext = bbox)
# # Alt: click two pts to zoom in
# zoom(c2015)




#################################
##NLCD###########################
#################################

nlcd <- raster(paste0(nlcd.dir,"NLCD_2016_Land_Cover_L48_20190424.img"))
extent(nlcd)
res(nlcd) # 30x30m
crs(nlcd) # diff ellipsoid than carbon
hist(nlcd)
# ^ Different resolution than carbon, but create look-up of carbon(kgC/m2) by class by county...
#...at this 30 m resolution, averaging across all the pixels of that given class in that given county.
nlcd

# Reclassify NLCD dataset to fewer classes based on JEsse's look-up table
lu.nlcd <- read.csv(paste0(lulc.dir,"NLCD_reclass.csv"))
lu.nlcd <- lu.nlcd %>%
  select(NLCD_code, new_code) %>%
  dplyr::rename(from = NLCD_code, to = new_code)
# nlcd.reclass <- reclassify(nlcd, lu.nlcd)
writeRaster(nlcd.reclass, paste0(lulc.dir, "nlcd.reclass.tif"))

# Resample to coarser resolution of c2015 to reduce computation time
nlcd.reclass.960 <- resample(nlcd.reclass, c2015, method = "ngb",
                             filename = paste0(lulc.dir, "nlcd.reclass.960.tif"))



#################################
##COUNTIES#######################
#################################

## Load county shapefile
county <- st_read(paste0(county.dir, "tl_2016_us_county.shp"))
# Remove all but CONUS
levels(county$STATEFP)
# FIPS codes
# 02: AK
# 60: Am Samoa
# 66: GUAM
# 15: HI
# 69: N. Mariana Is
# 72: PR
# 78: VIs
# 11: uknown but drop
drops <- c(02, 11, 60, 66, 15, 69, 72, 78)
county <- county[!county$STATEFP %in% drops,]
county$STATEFP <- droplevels(county$STATEFP) # leaves 49 
# For some reason, AK gets retained
county <- county[!county$STATEFP == "02",]
county$STATEFP <- droplevels(county$STATEFP) # leaves 48 
levels(county$STATEFP) # leaves 48
county <- st_transform(county, st_crs(nlcd))
# plot(st_geometry(county))


# Project to NLCD
# county <- sf_project(county, crs) # This function needs string of proj4string
# Save JIC b/c it took forever to project
# st_write(county, paste0(county.dir, "CONUS.county.p.shp"))
county <- st_read(paste0(county.dir, "CONUS.county.p.shp"))
# plot(test)


## Try mapping c & counties just to see... TAKES LONG TIME
# Pull c data into table (function defined in 00_setup)
# c_data <- gplot_data(c2015.p)
# p <- ggplot() +
#   geom_tile(data = c_data, aes(x = x, y = y, fill = value)) +
#   scale_fill_gradient("carbon",
#                       low = "yellow", high = "green",
#                       na.value = NA) +
#   geom_sf(data = county, color = "#808B96", fill = "white") +
#   theme_bw(base_size = 18)
# p




## Load FORESCE datasets ** NOT NECESSARY! USE LOOK-UP TABLES JESSE MADE **
# a1b2020 <- raster(paste0(lulc.dir, "CONUS_Landcover_A1B/CONUS_A1B_y2020.tif"))
# crs(a1b2020)
# hist(a1b2020)
# plot(a1b2020)
# crs(a1b2006)
