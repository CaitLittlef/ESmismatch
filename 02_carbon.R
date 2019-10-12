## Goal: extract carbon value associated with each cover class (per NLCD) in each county.
# Use this lookup table (of carbon by cover by county) to compute total carbon in each county in the future.

## Set temporary storage in stractch drive. 
tmpDir(create = TRUE) # [1] "C:\\Users\\clittlef\\AppData\\Local\\Temp\\7\\RtmpQbFX3Z/raster/"
rasterOptions(tmpdir = "D://Shared//Scratch//Workspace//Littlefield//ESmismatch")
r.temp.dir <- "D://Shared//Scratch//Workspace//Littlefield//ESmismatch/" 
tmpDir(create = TRUE) # [1] "D://Shared//Scratch//Workspace//Littlefield//ESmismatch/"
rasterOptions(default=TRUE) # Doesn't change it back... FIXME


par(mfrow=c(1,1))

## Load all datasets
c2015 <- raster(paste0(c.dir, "c2015.p.tif")) ; names(c2015) <- "c2015"
nlcd <- raster(paste0(lulc.dir, "nlcd.reclass.960.tif")) ; names(nlcd) <- "nlcd"


county <- st_read(paste0(county.dir, "CONUS.county.p.shp"))
# Convert relevant attributes to numeric else factor index will get used in loop below.
county$STATEFP <- as.numeric(as.character(county$STATEFP))
county$GEOID <- as.numeric(as.character(county$GEOID))

# # Subset to only run 1/2 country at once; keep contiguous else fails I think.
# statefp <- unique(county$STATEFP)
# west <- c(53, 41, 6, 32, 16, 30, 56, 49, 4, 8, 35, 38, 46, 31, 20, 40, 48) # based on map
# east <- statefp[which(!statefp %in% west)]
# county.west <- county %>% filter(STATEFP %in% west)
# county.east <- county %>% filter(STATEFP %in% east)

# # Crop other vars for fasteriz and loop
# c2015.west <- c2015 %>% crop(county.west) %>% mask(county.west)
# c2015.east <- c2015 %>% crop(county.east) %>% mask(county.east)
# nlcd.west <- nlcd %>% crop(county.west) %>% mask(county.west)
# nlcd.east <- nlcd %>% crop(county.east) %>% mask(county.east)

# Convert to raster
r.county <- fasterize(county, nlcd, field = "GEOID"); names(r.county) <- "county"
# r.county.west <- fasterize(county.west, r.county, field = "GEOID") ; names(r.county.west <- "county.west")
# r.county.east <- fasterize(county.east, r.county, field = "GEOID") ; names(r.county.east <- "county.east")

plot(r.county) # Appears to be by state, but intra-state values differ.
# plot(r.county.west) # Appears to be by state, but intra-state values differ.
# plot(r.county.east) # Appears to be by state, but intra-state values differ.


###########################################
###########################################
#### VT test landscape ####################
###########################################
###########################################

# ## Set shapefiles and stack resters to use
# vt <- county %>% filter(STATEFP == "50")
# windsor <- vt %>% filter(GEOID == 50027)
# franklin <- vt %>% filter(GEOID == 50011)
# r.vt <- fasterize(vt, nlcd, field = "GEOID") %>% crop(vt) %>% mask(vt) ; names(r.vt) <- "vt"
# c2015.test <- c2015 %>% crop(vt) %>% mask(vt)
# nlcd.test <- nlcd %>% crop(vt) %>% mask(vt)
# stack.test <- stack(c2015.test, nlcd.test, r.vt)
# names(stack.test) <- c("c2015.test", "nlcd.test", "vt")
# 
# ## Looping thru counties to summarize carbon values by cover type.
# # N.b., some counties don't have all zones; keep zonal output as matrix to match zones in rbind
# l.vt <- list()
# loop.ready <- unique(values(r.vt)) # Grab unique county IDs
# loop.ready <- loop.ready[is.finite(loop.ready)] # Retain numbers, nix NA
# # loop.ready <- 50011
# for(i in loop.ready) {
#   mask <- r.vt %in% i # Filtering or r.vt==i don't work
#   mask[mask == 0] <- NA # remove NA areas
#   target <- stack.test %>% mask(mask) # Only keep those in mask; crop messes with extents so don't use.
#   byzone <- zonal(target$c2015.test, target$nlcd.test, fun = "mean", digits = 0) # Avg. c val per class
#   byzone <- data.frame(t(byzone)) # df else sets to vector when dropping row below; t() to set zones as col names
#   colnames(byzone) <- paste0("z",byzone[1,]) # skip rownames -- get dropped in rbind.fillmatrix()
#   byzone <- byzone[-c(1),] # Get rid of zone row
#   l.vt[[paste0(i)]] <- byzone # Fill in list
# }
# 
# # Bind all dataframes of carbon by class for each county.
# m.vt <- plyr::rbind.fill.matrix(l.vt) # Preserves column names (here, zones defined in loop)
# rownames(m.vt) <- paste0("c",loop.ready) # Assign names based on county IDs.
# head(m.vt, 10)


###########################################
###########################################
#### All of CONUS #########################
###########################################
###########################################

## Looping thru counties to summarize carbon values by cover type.
# N.b., some counties don't have all zones; keep zonal output as matrix to match zones in rbind

stack <- stack(c2015, nlcd, r.county)
names(stack)

start <- Sys.time()
l <- list() 
loop.ready <- unique(values(r.county)) # Grab unique county IDs
loop.ready <- loop.ready[is.finite(loop.ready)] # Retain numbers, nix NA
# i <- 49009
for(i in loop.ready) {
# for(i in loop.ready.redo) { 
  mask <- r.county %in% i # Filtering or r.vt==i don't work
  mask[mask == 0] <- NA # remove NA areas
  target <- stack %>% mask(mask) # Only keep those in mask; crop messes with extents so don't use.
  byzone <- zonal(target$c2015, target$nlcd, fun = "mean", digits = 0) # Avg. c val per class
  byzone <- data.frame(t(byzone)) # df else sets to vector when dropping row below; t() to set zones as col names
  colnames(byzone) <- paste0("z",byzone[1,]) # skip rownames -- get dropped in rbind.fillmatrix()
  byzone <- byzone[-c(1),] # Get rid of zone row
  l[[paste0(i)]] <- byzone # Fill in list
  print(paste0("Finished county ", i))
  
  # Stop clog in tmeporary directly. Invisibly remove temp files which get big.
  invisible(file.remove(list.files(r.temp.dir, full.names = T)))
}


###########################################################################
## Had errors, which may be b/c of temp storage

## Errors mid-loop:
# Error in `names<-`(`*tmp*`, value = lnames) : 
#   incorrect number of layer names
# >   byzone <- zonal(target$c2015, target$nlcd, fun = "mean", digits = 0) # Avg. c val per class
# Error in result[, i] <- readBin(raster@file@con, what = dtype, n = ncols,  : 
#                                   number of items to replace is not a multiple of replacement length

## So, subset data to east and west.
# If I DO need to re-set loop, follow:

# match(c(54019),loop.ready) # gives first occurrence: in 1560 position.
# # Alt, when there may be multiple occurrences
# # which(loop.ready %in% c(54019)) 
# loop.ready[1560+1] # 51115: apparently this county doesn't work
# loop.ready.redo <- loop.ready[-c(1:(1560+1))] # drop thru already-run and failed indices

# l[[1569]] # exists
# l[["30107"]] # but doesn't match with above
# l[[1560]] # exists
# l[["54019"]] # and THIS matches.
# l[[1559]] # exists
# l[["54043"]] # and THIS matches.
# l[[989]] # exists & corresponds to...
# l[["39019"]] # THIS.  So something with 1560 gets messed up...

###########################################################################3



# Bind all dataframes of carbon by class for each county.
m <- plyr::rbind.fill.matrix(l) # Preserves column names (here, zones defined in loop)
rownames(m) <- paste0("c",loop.ready) # Assign names based on county IDs. 
head(m, 10)





today <- Sys.Date()
write.csv(m, paste0("carbon_by_lu_by_county_", today, ".csv"))

Sys.time() - start
