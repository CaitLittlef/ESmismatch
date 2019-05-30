############
## CARBON ##
############

## Extract NetCDF data (from Sleeter et al. 2018)
# data: https://www.sciencebase.gov/catalog/item/5b86bfbae4b0702d0e794697
# Annual carbon variables cover 1971-2015 at 1-km (960m) spatial resolution with 3052 rows and 4823 columns. Carbon stock and flux units are in kgC/m2 and kgC/m2/yr, respectively. Data are in NetCDF format and Albers equal area projection.
# NoData val: -3.40282346639e+038


## Open datasource using netCDF package tools (ref: https://rpubs.com/boyerag/297592)
nc_data <- nc_open(paste0(c.dir,'subset_stddown_m.nc'))
# Save the print(nc) dump to a text file
# {
#   sink('subset_stddown_m_metadata.txt')
#   print(nc_data)
#   sink()
# }
# Gives list of 14.
names(nc_data)
names(nc_data$dim) # time, level, latitude, longitude
names(nc_data$var) # subset <-- per metadata, prob from original 100 yr simulation
class(nc_data$var$vals)

## Extract spatio-temporal values
lon <- ncvar_get(nc_data, "longitude") # 4923
lat <- ncvar_get(nc_data, "latitude") # 3052
t <- ncvar_get(nc_data, "time") # 45
head(lon) # 1, 2, 3, 4, 5, 6 <-- so, need to get actual coords
head(lat) # 1, 2, 3, 4, 5, 6 <-- so, need to get actual coords
head(t) # 1, 2, 3, 4, 5, 6

## Unclear where values (c) are stored; extract to array. 
c.array <- ncvar_get(nc_data)
class(c.array) # array
dim(c.array) # 3-dim array: 4823, 3052, 45 years (1971-2015)


## Done reading data, so close it
nc_close(nc_data)


## Per meta-data, NoData is -3.40e38, but in plot (below), no-data areas had v. lrg numbers
c.array[c.array > 9999999] <- NA
# Plot one year (one time slice)
c.slice <- c.array[, , 45] ; class(c.slice) # Should give last year: 2015
min(c.slice, na.rm = T) # 0
max(c.slice, na.rm = T) # 16.04029


## Save as a raster.
# Define crs -- Albers Equal area, per metadata, but not on spatialreference.org.
# See home-growth solution (for, coincidentally, related dataset) here (nb diff than one on proj4.org):
# https://gis.stackexchange.com/questions/291734/usa-albers-equal-area-conic-usgs-version-landfire-projection-not-read-by-r
# This matches what's in meta-data (p.4)
crs <- crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
# N.b., this is designed for CONUS hence 0 thru middle of country.


# Transpose to orient correctly b/c netCDF recorded from bottom left corner
# r <- raster(t(c.slice), xmn=min(lon), xmx=max(lon),
#             ymn=min(lat), ymx=max(lat), crs=crs)
# ^ That retained simple row & col numbers as "coordiantes"
# Instead, pulul coordinates from meta-data (tho it looks squished):
# West Bounding Coordinate: -179.147340
# East Bounding Coordinate: 179.778470
# North Bounding Coordinate: 71.35256
# South Bounding Coordinate: -14.552549
r <- raster(t(c.slice), xmn=c(-179.147340), xmx=c(179.778470),
            ymn=c(-14.552549), ymx=c(71.35256), crs=crs)
# What are corner values (from middle of cells so not perfect matches)?
xyFromCell(r, c(1, ncol(r), ncell(r)-ncol(r)+1, ncell(r))) # just gives cells...


## Plot raster
plot(r) # WHAT ARE THESE VALUES??
# Create rough bbox for NW WA zoom; get coords with click().
# click(r, n = Inf, id = T, xy = T, cell = T, type = "n", show = T)
# Turn these coords into a polygon (can also extract coords to bbox())
ext <- as(raster::extent(-165, -145, 60, 70), "SpatialPolygons")
bbox <- bbox(ext)
plot(r, ext = ext) # alt: plot(r, ext = bbox)
# zoom(r) # Click two points to zoom in


## Ok, looks ok. Still need to know what vals are. To get better sense, pull all yrs out.
r_brick <- brick(c.array, xmn=c(-179.147340), xmx=c(179.778470),
                 ymn=c(-14.552549), ymx=c(71.35256), crs=crs) 
# t() doesn't work well w/ brick, but then coords are messed up.
r_brick <- brick(c.array, xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=crs)
r_brick <- t(r_brick)
plot(r_brick[5])

## THESE VALUES ARE CLEARLY FLUXES. WHAT'S STORE???



## Load FORESCE datasets
a1b2006 <- raster(paste0(for.dir,"CONUS_Forest_History_A1B/a1b_forest_history_y2006.tif"))
identical(crs(a1b2006), crs(r))
crs(a1b2006)
plot(a1b2006)
crs(a1b2006)
