## Project carbon into the future for each county based on land cover class carbon coefficients (from 02_carbon_lu.R)

#######################################################################
#### Load carbon look-up table & land classes ####
lu_c <- read.csv(paste0(wd,"/carbon_by_lu_by_county_2019-10-12.csv"))
lu.nlcd <- read.csv(paste0(lulc.dir,"NLCD_reclass.csv"))

# Not sure where zeros are from in NLCD -- should only have 1-7.
# Must have been remnant class in reclassified NLCD raster. Drop.
# Also nix unnecessary column X1
summary(lu_c$z0) # Many NAs, low values (max 9.3)
lu_c <- lu_c %>% dplyr::select(X, z1, z2, z3, z4, z5, z6, z7) %>%
  rename(COUNTY = X)

# Open water should have zero for terrestrial carbon
min(!is.na(lu_c$z1)) 
max(!is.na(lu_c$z1)) # mostly tiny values that max at 1. Still, set all to zero.
lu_c$z1 <- 0

# Remove c prefix from counties; package readr faster than grep
lu_c$COUNTY <- as.character(lu_c$COUNTY)
lu_c$COUNTY <- parse_number(lu_c$COUNTY)

# Gather the data into tidy form; remove z from zone
lu_c <- lu_c %>% gather(key = LANDCOVER, value = CARBON, -COUNTY)
lu_c$LANDCOVER <- right(lu_c$LANDCOVER, 1) %>% as.numeric()

# Convert m^2 to ha; rename
lu_c$CARBON <- lu_c$CARBON * 10000
lu_c$CARBON_KGHA <- lu_c$CARBON
lu_c$CARBON <- NULL


#######################################################################
#### Load table of each county's landcover area, by decade by scenario (thx Jesse!) ####
proj.list <- list.files(path = paste0(lulc.dir,"CountySummary_CSVs"),
                        pattern="2..0.csv", full.names = TRUE) # get all decades
proj.list <- proj.list[1:40] # To drop historical_2000
# Also grab just names of csv for assigning
proj.names <- list.files(path = paste0(lulc.dir,"CountySummary_CSVs"),
                        pattern="2..0.csv", full.names = FALSE) # get all decades
proj.names <- proj.names[1:40] # To drop historical_2000
proj.names <- tools::file_path_sans_ext(proj.names) # To drop extension .csv
# A1B.list <- grep(pattern = "A1B", x = proj.list, value = TRUE) %>% sort(.)
# A2.list <- grep(pattern = "A2", x = proj.list, value = TRUE) %>% sort(.)
# B1.list <- grep(pattern = "B1", x = proj.list, value = TRUE) %>% sort(.)
# B2.list <- grep(pattern = "B2", x = proj.list, value = TRUE) %>% sort(.)


#######################################################################
#### TESTING GROUND ####
# cnty_lc_area <- read.csv(proj.list[1])
# count(cnty_lc_area, LANDCOVER) # Still has old (too many) codes (0-17)
# # Jesse says those zeros likely introduced during raster processing: delete
# cnty_lc_area <- cnty_lc_area %>% filter(LANDCOVER > 0)
# count(cnty_lc_area, LANDCOVER) # Just 1-17
# # Reclassify dataset to fewer classes based on JEsse's look-up table
# lu.foresce <- read.csv(paste0(lulc.dir,"FORESCE_reclass.csv"))
# count(lu.foresce, FORESCE_code) # (1-17 -- missing zero)???
# 
# # Join look-up to projections (many codes), redefine (to one) LANDCOVER codes.
# p <- cnty_lc_area %>%
#   left_join(lu.foresce, by = c("LANDCOVER" = "FORESCE_code")) %>%
#   dplyr::select(-LANDCOVER, -FORESCE_name, -new_name) %>%
#   dplyr::rename(LANDCOVER = new_code)
# 
# # Should no longer be any NAs
# p <- p[!is.na(p$LANDCOVER),]
# 
# # Because it was many->one, have dupe LANDCOVERS
# p <- p %>%
#   group_by(SCENARIO, YEAR, COUNTY, LANDCOVER) %>%
#   summarise(AREA_HA = sum(AREA_HA)) %>%
#   ungroup() %>% data.frame() # Else remains grouped dataframe
# 
# # Join carbon look-up to this table
# pc <- left_join(p, lu_c, by=c("COUNTY", "LANDCOVER"))
# # Checks out: open-water continues to have zero.
# 
# # Mult LANDCOVER code by c coeff.
# # Replace on-the-fly any NAs for "new" landcover (never in county before) with county avg.
# # Alt: recompute carbon coefficient for that class within the county. Checks out:
# pc <- pc %>%
#   group_by(SCENARIO, YEAR, COUNTY) %>%
#   mutate(CARBON_KG = AREA_HA * replace_na(CARBON_KGHA, mean(CARBON_KGHA, na.rm=TRUE))) #%>%
# #   mutate(CARBON_KGHA2 = ifelse(is.na(CARBON_KGHA), mean(CARBON_KGHA, na.rm=TRUE), CARBON_KGHA)) %>%
# #   mutate(CARBON_KG2 = AREA_HA * CARBON_KGHA2)
# # identical(pc$CARBON_KG, pc$CARBON_KG2)
# 
# # Summarise to county level, adding all landclasses together
# cnty_c <- pc %>% group_by(SCENARIO, YEAR, COUNTY) %>%
#   summarise(CARBON_KG_TTL = sum(CARBON_KG))
# 
# # Conver to metric tons
# cnty_c$CARBON_Mg <-  round(cnty_c$CARBON_KG_TTL*0.001,0)
# cnty_c$CARBON_KG_TTL <- NULL


#######################################################################
#### LOOP THRU ALL PROJETIONS ####
carbon.list <- list()
for(i in 1:length(proj.list)){
  p <- read.csv(proj.list[i])
  p <- p %>% filter(LANDCOVER > 0) # drop zeros, artifact of raster calc
  # Reclassify to fewer landcover classes
  p <- p %>%
    left_join(lu.foresce, by = c("LANDCOVER" = "FORESCE_code")) %>% # join 
    dplyr::select(-LANDCOVER, -FORESCE_name, -new_name) %>%
    dplyr::rename(LANDCOVER = new_code)
  # Aggregate areas now in the same cover class
  p <- p %>%
    group_by(SCENARIO, YEAR, COUNTY, LANDCOVER) %>%
    summarise(AREA_HA = sum(AREA_HA)) %>%
    ungroup() %>% data.frame() # Else remains grouped dataframe
  pc <- left_join(p, lu_c, by=c("COUNTY", "LANDCOVER"))
  # Compute total carbon stocks by landcover
  pc <- pc %>%
    group_by(SCENARIO, YEAR, COUNTY) %>%
    mutate(CARBON_KG = AREA_HA * replace_na(CARBON_KGHA, mean(CARBON_KGHA, na.rm=TRUE))) %>%
    ungroup() %>% data.frame() 
  # Summarise to county
  cnty_c <- pc %>% group_by(SCENARIO, YEAR, COUNTY) %>%
    summarise(CARBON_KG_TTL = sum(CARBON_KG)) %>% # Convert to metric tons
    ungroup() %>% data.frame() 
  cnty_c$CARBON_Mg <- round(cnty_c$CARBON_KG_TTL*0.001)
  cnty_c$CARBON_KG_TTL <- NULL
  carbon.list[[i]] <- cnty_c
}

# Combine into one table
carbon_by_county <- do.call(rbind, carbon.list)
currentDate <- Sys.Date()
write.csv(carbon_by_county,
          paste0("carbon_by_county_",currentDate,".csv"),
          row.names = FALSE)
