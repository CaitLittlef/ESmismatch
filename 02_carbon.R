## Goal: extract carbon value associated with each cover class (per NLCD) in each county.
# Use this lookup table (of carbon by cover by county) to compute total carbon in each county in the future.

par(mfrow=c(1,1))

# Load all datasets
county <- st_read(paste0(county.dir, "CONUS.county.p.shp"))
# MUST convert GEOID to numeric else factor index get used
county$GEOID <- as.numeric(as.character(county$GEOID))
c2015 <- raster(paste0(c.dir, "c2015.p.tif")) ; names(c2015) <- "c2015"
nlcd <- raster(paste0(lulc.dir, "nlcd.reclass.960.tif")) ; names(nlcd) <- "nlcd"
r.county <- fasterize(county, nlcd, field = "GEOID"); names(r.county) <- "county"
plot(r.county) # Appears to be by state, but intra-state values differ.


###########################################
###########################################
#### VT test landscape ####################
###########################################
###########################################

## Set shapefiles and stack resters to use
vt <- county %>% filter(STATEFP == "50")
windsor <- vt %>% filter(GEOID == 50027)
franklin <- vt %>% filter(GEOID == 50011)
r.vt <- fasterize(vt, nlcd, field = "GEOID") %>% crop(vt) %>% mask(vt) ; names(r.vt) <- "vt"
c2015.test <- c2015 %>% crop(vt) %>% mask(vt)
nlcd.test <- nlcd %>% crop(vt) %>% mask(vt)
stack.test <- stack(c2015.test, nlcd.test, r.vt)
names(stack.test) <- c("c2015.test", "nlcd.test", "vt")

## Looping thru counties to summarize carbon values by cover type.
# N.b., some counties don't have all zones; keep zonal output as matrix to match zones in rbind
l.vt <- list() 
loop.ready <- unique(values(r.vt)) # Grab unique county IDs
loop.ready <- loop.ready[is.finite(loop.ready)] # Retain numbers, nix NA
# loop.ready <- 50011
for(i in loop.ready) {
  mask <- r.vt %in% i # Filtering or r.vt==i don't work
  mask[mask == 0] <- NA # remove NA areas
  target <- stack.test %>% mask(mask) # Only keep those in mask; crop messes with extents so don't use.
  byzone <- zonal(target$c2015.test, target$nlcd.test, fun = "mean", digits = 0) # Avg. c val per class
  byzone <- data.frame(t(byzone)) # df else sets to vector when dropping row below; t() to set zones as col names
  colnames(byzone) <- paste0("z",byzone[1,]) # skip rownames -- get dropped in rbind.fillmatrix()
  byzone <- byzone[-c(1),] # Get rid of zone row
  l.vt[[paste0(i)]] <- byzone # Fill in list
}

# Bind all dataframes of carbon by class for each county.
m.vt <- plyr::rbind.fill.matrix(l.vt) # Preserves column names (here, zones defined in loop)
rownames(m.vt) <- paste0("c",loop.ready) # Assign names based on county IDs. 
head(m.vt, 10)


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
  mask <- r.county %in% i # Filtering or r.vt==i don't work
  mask[mask == 0] <- NA # remove NA areas
  target <- stack %>% mask(mask) # Only keep those in mask; crop messes with extents so don't use.
  byzone <- zonal(target$c2015, target$nlcd, fun = "mean", digits = 0) # Avg. c val per class
  byzone <- data.frame(t(byzone)) # df else sets to vector when dropping row below; t() to set zones as col names
  colnames(byzone) <- paste0("z",byzone[1,]) # skip rownames -- get dropped in rbind.fillmatrix()
  byzone <- byzone[-c(1),] # Get rid of zone row
  l[[paste0(i)]] <- byzone # Fill in list
  print(paste0("Finished county", i))
}

# Bind all dataframes of carbon by class for each county.
m <- plyr::rbind.fill.matrix(l) # Preserves column names (here, zones defined in loop)
rownames(m) <- paste0("c",loop.ready) # Assign names based on county IDs. 
head(m, 10)

today <- Sys.Date()
write.csv(m, paste0("carbon_by_lu_by_county_", today, ".csv"))

Sys.time() - start
