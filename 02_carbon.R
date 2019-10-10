## Goal: extract carbon value associated with each cover class (per NLCD) in each county.
# Use this lookup table (of carbon by cover by county) to compute total carbon in each county in the future.

par(mfrow=c(1,1))

# Load all datasets
county <- st_read(paste0(county.dir, "CONUS.county.p.shp"))
c2015 <- raster(paste0(c.dir, "c2015.p.tif")) ; names(c2015) <- "c2015"
nlcd <- raster(paste0(lulc.dir, "nlcd.reclass.960.tif")) ; names(nlcd) <- "nlcd"
r.county <- fasterize(county, nlcd, field = "GEOID"); names(r.county) <- "county"
plot(r.county) # Appears to be by state, but intra-state values differ.
stack <- stack(c2015, nlcd, r.county)
names(stack)

## Do test landscape
vt <- county %>% filter(STATEFP == "50")
r.vt <- fasterize(vt, nlcd, field = "GEOID") %>% crop(vt) %>% mask(vt) ; names(r.vt) <- "vt"
c2015.test <- c2015 %>% crop(vt) %>% mask(vt)
nlcd.test <- nlcd %>% crop(vt) %>% mask(vt)
stack.test <- stack(c2015.test, nlcd.test, r.vt)
names(stack.test) <- c("c2015.test", "nlcd.test", "vt")

ctemp <- list() 
loop.ready <- unique(values(r.vt)) # Grab unique county IDs
loop.ready <- loop.ready[is.finite(loop.ready)] # Retain numbers, nix NA
for(i in loop.ready) {
  mask <- r.vt %in% i # works vs. filtering or with r.vt==i
  mask[mask == 0] <- NA # remove NA areas
  stack.test.2 <- stack.test %>% mask(mask) # extract carbon & lu vals
  byzone <- zonal(stack.test.2$c2015.test, stack.test.2$nlcd.test, fun = "mean", digits = 0)
  # Not all zones are in every county; keep coefficients as matrices (vs. byzone[,2])
  # Transpose to put zones as column names for use with rbind.fill.matrix()
  byzone <- data.frame(t(byzone))
  
  ## START HERE. NOTE ZONE NAMES (NUMBERS) ARE RETAINED IN ZONAL. SEE 13th COUNTY IN VT
  
  colnames(byzone) <- paste0("z",byzone[1,])
  byzone <- byzone %>% dplyr::select()
  rownames(byzone) <- paste0("c",loop.ready)
  ctemp[[paste0(i)]] <- byzone 
}

https://stackoverflow.com/questions/45374422/rename-column-of-dataframes-inside-a-list-with-its-dataframe-name
z <- lapply(ctemp, data.frame)
z <- map_dfr(ctemp)
z <- lapply(z, fun(x) {colnames(x) <- paste0("z", as.integer(x[1,])); x})
z[[13]]
test <- plyr::rbind.fill.matrix(ctemp)
rownames(test) <- paste0("county",loop.ready)
?plyr::rbind.fill.matrix

ctemp[[1]]
ctemp[[2]]
ctemp[[11]]
ctemp[[13]]


cs <- data.frame(cs)
rownames(cs) <- loop.ready
colnames(cs) <- 
class(ctemp[[1]])
test <- do.call(rbind(ctemp))
test <- bind
ctemp[[1]]
  }
plot(stack.test.2[[1]])
plot(mask)

i <- 2776
names(stemp)
plot(stemp$c2015)
plot(stemp$nlcd)
getValues(stemp$nlcd)
hist(stemp$nlcd)
plot(temp[[1]])
plot(temp[[2]])
  }
  i
  

  
# Extract (by polygon)
# Zonal (by NLCD raster)
