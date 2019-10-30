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
#### Load table of each county's landcover coverage (in ha) for each decade and each scenario ####
proj.list <- list.files(path = paste0(lulc.dir,"CountySummary_CSVs"),
                        pattern="20.0.csv", full.names = TRUE) # get all decades
A1B.list <- grep(pattern = "A1B", x = proj.list, value = TRUE) %>% sort(.)
A2.list <- grep(pattern = "A2", x = proj.list, value = TRUE) %>% sort(.)
B1.list <- grep(pattern = "B1", x = proj.list, value = TRUE) %>% sort(.)
B2.list <- grep(pattern = "B2", x = proj.list, value = TRUE) %>% sort(.)
rm(proj.list)



#######################################################################
#### TESTING GROUND ####
cnty_lu_area <- read.csv(A1B.list[1])
count(cnty_lu_area, LANDCOVER) # Still has old (too many) codes (0-17)
# Reclassify dataset to fewer classes based on JEsse's look-up table
lu.foresce <- read.csv(paste0(lulc.dir,"FORESCE_reclass.csv"))
count(lu.foresce, FORESCE_code) # (1-17 -- missing zero)???

# FIXME:
############ *****************************************
############ *****************************************
#  EMAILED JESSE 101929 TO ASK WHAT ZERO IS. FOR NOW, JUST DELETE
cnty_lu_area <- cnty_lu_area %>% filter(LANDCOVER > 0)
############ *****************************************
############ *****************************************

# Join look-up to projections (too many codes), redefine (fewer) LANDCOVER codes.
cnty_lu_area <- cnty_lu_area %>%
  left_join(lu.foresce, by = c("LANDCOVER" = "FORESCE_code")) %>%
  dplyr::select(-LANDCOVER, -FORESCE_name, -new_name) %>%
  dplyr::rename(LANDCOVER = new_code)

# Should no longer be any NAs
cnty_lu_area <- cnty_lu_area[!is.na(cnty_lu_area$LANDCOVER),]

# Because it was many->one, have dupe LANDOVERS
cnty_lu_area <- cnty_lu_area %>%
  group_by(SCENARIO, YEAR, COUNTY, LANDCOVER) %>%
  summarise(AREA_HA = sum(AREA_HA)) %>%
  ungroup() %>% data.frame() # Else remains grouped dataframe

# Join carbon look-up to this table
cnty_lu_area_c <- left_join(cnty_lu_area, lu_c, by=c("COUNTY", "LANDCOVER"))
# Checks out: open-water continues to have zero.

# Summarize to carbon stocks per county, mult LANDCOVER code by c coeff.
# NAs arise b/c new landcover appears for a given county. Set to NA carbon for now.

# FIXME:
#################*********************########################
#################*********************########################
#### WHAT TO DO WITH NEW LANDCOVER CLASS?? 
#################*********************########################
#################*********************########################

cnty_lc_c <- cnty_lu_area_c %>%
  group_by(SCENARIO, YEAR, COUNTY) %>%
  mutate(CARBON_KG = AREA_HA * replace_na(CARBON_KGHA, 0))

# Summarise to total carbon per county.
cnty_c <- cnty_lc_c %>%
  group_by(SCENARIO, YEAR, COUNTY) %>%
  summarise(CARBON_KG_TOT = sum(CARBON_KG), na.rm = TRUE)


class(cnty_lu_area)



            
sum(rowwise(cnty_lu_area$AREA_HA * cnty_lu_area$CARBON_KGHA))
?rowwise
