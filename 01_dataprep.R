#################################
##CARBON#########################
#################################

## Extract NetCDF data (from Sleeter et al. 2018)
# data: https://www.sciencebase.gov/catalog/item/5b86bfbae4b0702d0e794697
# Annual carbon variables cover 1971-2015 at 1-km (960m) spatial resolution with 3052 rows and 4823 columns. Carbon stock and flux units are in kgC/m2 and kgC/m2/yr, respectively. Data are in NetCDF format and Albers equal area projection.
# NoData val: -3.40282346639e+038

#################################
##FIXME##########################
#################################

## I can access data from the netCDF below, but it's unclear how to get it to line up, spatially.
## It's an array (grid), yet units are apparently in m, and bounding coordiantes are given in degrees.
## Others have struggled: https://gis.stackexchange.com/questions/271779/r-netcdf-to-raster

## Open datasource using netCDF package tools (ref: https://rpubs.com/boyerag/297592)
nc_open(paste0(c.dir,'subset_totceco_m.nc'))
# has 1 variable (subset) which has 4 dimensions (diff than variables): long, lat, level, time
nc_data <- nc_open(paste0(c.dir,'subset_totceco_m.nc'))
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
dim(nc_data$var)


## Extract spatio-temporal values
lon <- ncvar_get(nc_data, "longitude") # 4923, these are just grid size
lat <- ncvar_get(nc_data, "latitude") # 3052, these are just grid size
t <- ncvar_get(nc_data, "time") # 45, these are just time size
dim(nc_data)

head(lon) # 1, 2, 3, 4, 5, 6 <-- so, need to get actual coords
head(lat) # 1, 2, 3, 4, 5, 6 <-- so, need to get actual coords
head(t) # 1, 2, 3, 4, 5, 6 <-- so, need to get actual years

## Unclear where values (c) are stored; extract to array. 
c.array <- ncvar_get(nc_data)
class(c.array) # array
dim(c.array) # 3-dim array: 4823, 3052, 45 years (1971-2015)

## Done reading data, so close it
nc_close(nc_data)


## Per meta-data, NoData is -3.40e38, but in plot (below), no-data areas had v. lrg numbers
c.array[1, 1, 45]
c.array[c.array > 9999999] <- NA
# Plot one year (one time slice)
c.slice <- c.array[, , 45] ; class(c.slice) # Should give last year: 2015
min(c.slice, na.rm = T) # 0
max(c.slice, na.rm = T) # 428.7242


## Save as a raster.
# Define crs -- Albers Equal area, per metadata, but not on spatialreference.org.
# See home-growth solution (for, coincidentally, related dataset) here (nb there's diff one on proj4.org):
# https://gis.stackexchange.com/questions/291734/usa-albers-equal-area-conic-usgs-version-landfire-projection-not-read-by-r
# This matches what's in meta-data (p.4); setting parallels and meridian.
# N.b., this is designed for CONUS hence 0 thru middle of country.
crs <- crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")



## START HERE! HOW DO I GET THESE ARRAYS TO SHOW UP SPATIALLY?



## But bounding coordinates are given in degrees so I'm nixing +units=m...
crs <- crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")

# Transpose to orient correctly b/c netCDF recorded from bottom left corner
# r <- raster(t(c.slice), xmn=min(lon), xmx=max(lon),
#             ymn=min(lat), ymx=max(lat), crs=crs)
# ^ That retained simple row & col numbers as "coordinates"
# r <- raster(t(c.slice), crs=crs)
# ^ That sets lower left and bottom as (0,0) and upper right and top as (1,1). Looks squeezed from sides.
# Instead, pull coordinates from meta-data? (tho it looks squished):
# West Bounding Coordinate: -179.147340
# East Bounding Coordinate: 179.778470
# North Bounding Coordinate: 71.35256
# South Bounding Coordinate: -14.552549
# r <- raster(t(c.slice), xmn=c(-179.147340), xmx=c(179.778470),
#             ymn=c(-14.552549), ymx=c(71.35256), crs=crs)

## Plot raster slice
# plot(r) 
# Create rough bbox for NW WA zoom; get coords with click().
# click(r, n = Inf, id = T, xy = T, cell = T, type = "n", show = T)
# Turn these coords into a polygon (can also extract coords to bbox())
# ext <- as(raster::extent(250, 500, 2750, 3000), "SpatialPolygons")
# ext <- as(raster::extent(-165, -145, 60, 70), "SpatialPolygons")
# bbox <- bbox(ext)
# plot(r, ext = ext) # alt: plot(r, ext = bbox)
# # Alt: click two pts to zoom in
# zoom(r)


## Ok, looks ok. Still need to know what vals are. To get better sense, pull all yrs out.
# apply t() within brick(). brick() methods for array suggest following:
# c_brick <- brick(c.array, xmn=0, xmx=1, ymn=0, ymx=1, crs=crs, transpose = TRUE)
# plot(c_brick[[45]])
# Still want to associate those corners with real coordiantes tho. 



#######################################################
# But these are DEGREES!!! Get into meters??

c_brick <- brick(c.array, xmn=c(-179.147340), xmx=c(179.778470),
                    ymn=c(-14.552549), ymx=c(71.35256), crs=crs, transpose = TRUE)
plot(c_brick[[45]])
extent(c_brick)
ext <- as(raster::extent(-165, -145, 60, 70), "SpatialPolygons")
plot(c_brick[[45]], ext = ext)
res(c_brick) # 0.07441962 0.02814715 seems like degrees
crs(c_brick)

# Create tinier bbox; get coords with click()
# click(c_brick[[45]], n = Inf, id = T, xy = T, cell = T, type = "n", show = T)
# # Turn these coords into a polygon (can also extract coords to bbox())
# ext <- as(raster::extent(-156, -155, 22, 23), "SpatialPolygons")
# plot(c_brick[[45]], ext = ext)





#################################
##NLCD & COUNTIES################
#################################

nlcd <- raster(paste0(nlcd.dir,"NLCD_2016_Land_Cover_L48_20190424.img"))
plot(nlcd)
extent(nlcd)
res(nlcd) # 30x30m
crs(nlcd) # crs matches c_array above but in m


## Load county shapefile
county <- st_read(paste0(county.dir, "tl_2016_us_county.shp"))
# Remove all but CONUS
# FIPS codes
# 02: AK
# 60: Am Samoa
# 66: GUAM
# 15: HI
# 69: N. Mariana Is
# 72: PR
# 78: VIs
drops <- c(02, 60, 66, 15, 69, 72, 78)
county <- county[!county$STATEFP %in% drops,]
county$STATEFP <- droplevels(county$STATEFP) # leaves 51 (w/ DC)
county <- st_transform(county, st_crs(c_brick))


## Try mapping c & counties just to see...
# Pull c data into table (function defined in 00_setup)
c_data <- gplot_data(c_brick[[1]])
p <- ggplot() +
  geom_tile(data = c_data, aes(x = x, y = y, fill = value)) +
  scale_fill_gradient("carbon",
                      low = "yellow", high = "green",
                      na.value = NA) +
  geom_sf(data = county, color = "#808B96", fill = "white") +
  theme_bw(base_size = 18)
p
# ^ Doesn't work. They aren't aligned.



## Load FORESCE datasets
a1b2006 <- raster(paste0(for.dir,"CONUS_Forest_History_A1B/a1b_forest_history_y2006.tif"))
identical(crs(a1b2006), crs(r))
crs(a1b2006)
plot(a1b2006)
crs(a1b2006)
