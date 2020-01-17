## What's change in wellbeing (benefit or non-benefit) of carbon storage?
# Compute marginal change in "value" of carbon storage based on...
#...social cost of carbon at 3% discount rate. N.b., cost increases...
#...decadally because accumulated emissions exacerbate each additional ton.
# Gov't working grp generated based on emissions and population projections.
# Group since disbanded, hence using projected SCC values from linear regression.

# Load scc for each year. 3% discount rate. 2015$/metric ton.
# Extrapolated (linear reg) from given 2010-2050 values.
lu.scc <- read.csv("scc_2015dollars.csv", header = FALSE)
colnames(lu.scc) <- c("YEAR", "USD2015")
# Only keep decadal values
decades <- seq(2020, 2100, 10)
lu.scc <- lu.scc %>%
  filter(YEAR %in% decades) 
lu.scc$USD2015 <- round(lu.scc$USD2015, 0)

# For pass of each decade, compute marginal change in value of metric tons carbon stored.
# Sum with prior decade's value for accumulated benefit.

carbon_by_county <- read.csv("carbon_by_county_2020-01-15.csv")
carbon_benefit <- carbon_by_county %>%
  # Baseline is 2020 so remove all 2010s.
  filter(!YEAR<2020) %>%
  # Group by scenario and county for decadal changes within each.
  group_by(SCENARIO, COUNTY) %>%
  # Tack on SCC values for each decade (YEAR)
  right_join(lu.scc) %>%
  # Compute change in C from each decade to the next
  mutate(CHNG_C_Mg = CARBON_Mg - lag(CARBON_Mg)) %>%
  # Compute value of baseline carbon -- initial stock in 2020. 
  # Also compute value of decadal change (delta) in carbon for each non-baseline year.
  # So note that this col contains baseline benefit (2020) AND marginal benefits.
  mutate(BASE_AND_DELTA_VAL_C_USD2015 = ifelse(YEAR == 2020,
                                          CARBON_Mg * USD2015,
                                          CHNG_C_Mg * USD2015)) %>%
  # Compute accumulated change in value. This is accumulated benefit.
  mutate(CHNG_VAL_C_USD2015 = cumsum(BASE_AND_DELTA_VAL_C_USD2015)) #%>%
  # # Alt: compute baseline and delta into separate columns .
  # mutate(VAL_BASE_C_Mg = ifelse(YEAR == 2020, CARBON_Mg * USD2015, NA)) %>%
  # mutate(VAL_DELTA_C_Mg = ifelse(YEAR > 2020, CHNG_C_Mg * USD2015, NA))
  
test <- carbon_benefit %>%
  filter(SCENARIO == "A1B", COUNTY == 1001)

# Save
currentDate <- Sys.Date()
write.csv(carbon_benefit, paste0("carbon_supp-dmnd-bene_", currentDate, ".csv"))
