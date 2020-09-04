# Process CMIP6 emissions from 
# https://www.rcmip.org/
# CH 09/03/20


path = "C:/Users/chartin/Documents/GitHub/SSP_emissions/"

ssps <- read.csv("C:/Users/chartin/Documents/GitHub/SSP_emissions/rcmip-emissions-annual-means-v4-0-0.csv") %>% 
  select(-Model, -Activity_Id) %>% 
  filter(Region == "World" & Mip_Era == "CMIP6") %>% 
  select(-Region, -Mip_Era) %>% 
  filter(Scenario %in% c( "ssp119", "ssp370", "ssp126", "ssp245","ssp434", "ssp460", "ssp534-over", "ssp580")) %>% 
  filter(Variable %in% c("Emissions|CO2","Emissions|CO2|MAGICC AFOLU", "Emissions|CH4",  "Emissions|N2O", "Emissions|Sulfur", "Emissions|CO",
                         "Emissions|VOC", "Emissions|NOx", "Emissions|BC", "Emissions|OC", "Emissions|F-Gases|PFC|CF4", "Emissions|F-Gases|PFC|C2F6", 
                         "Emissions|F-Gases|HFC|HFC23",
                         "Emissions|F-Gases|HFC|HFC32", "Emissions|F-Gases|HFC|HFC32", "Emissions|F-Gases|HFC|HFC4310mee", "Emissions|F-Gases|HFC|HFC125",
                         "Emissions|F-Gases|HFC|HFC134a", 
                         "Emissions|F-Gases|HFC|HFC143a", "Emissions|F-Gases|HFC|HFC227ea",  "Emissions|F-Gases|HFC|HFC245fa", "Emissions|F-Gases|SF6", 
                         "Emissions|Montreal Gases|CFC|CFC11", "Emissions|Montreal Gases|CFC|CFC12", "Emissions|Montreal Gases|CFC|CFC113", 
                         "Emissions|Montreal Gases|CFC|CFC114",
                         "Emissions|Montreal Gases|CFC|CFC115",  "Emissions|Montreal Gases|CCl4", "Emissions|Montreal Gases|CCl4", 
                         "Emissions|Montreal Gases|CH3CCl3", "Emissions|Montreal Gases|HCFC22",
                         "Emissions|Montreal Gases|HCFC141b", "Emissions|Montreal Gases|HCFC142b", "Emissions|Montreal Gases|Halon1211",  
                         "Emissions|Montreal Gases|Halon1301",
                         "Emissions|Montreal Gases|Halon2402", "Emissions|Montreal Gases|CH3Br",  "Emissions|Montreal Gases|CH3Cl", 
                        "Emissions|F-Gases|HFC|HFC236fa")) %>% 
  select(Scenario, Variable, Unit, X2015:X2300) %>% 
  separate(Variable, into =c("a", "b", "c", "d", "e")) %>% 
  select(-a)

historical <- read.csv("C:/Users/chartin/Documents/GitHub/SSP_emissions/rcmip-emissions-annual-means-v4-0-0.csv") %>% 
  select(-Model, -Activity_Id) %>% 
  filter(Region == "World" & Mip_Era == "CMIP6") %>% 
  select(-Region, -Mip_Era) %>% 
  filter(Scenario %in% c( "historical")) %>% 
  filter(Variable %in% c("Emissions|BC", "Emissions|CO2","Emissions|CO2|MAGICC AFOLU", "Emissions|CH4", "Emissions|CO", 
                         "Emissions|F-Gases|HFC|HFC125", "Emissions|F-Gases|HFC|HFC134a", "Emissions|F-Gases|HFC|HFC143a", 
                         "Emissions|F-Gases|HFC|HFC227ea", "Emissions|F-Gases|HFC|HFC23","Emissions|F-Gases|HFC|HFC32","Emissions|F-Gases|HFC|HFC236fa",
                         "Emissions|F-Gases|HFC|HFC245fa", "Emissions|F-Gases|HFC|HFC32", "Emissions|F-Gases|PFC|C2F6", 
                         "Emissions|F-Gases|PFC|CF4", "Emissions|F-Gases|SF6", "Emissions|Montreal Gases|CCl4", 
                         "Emissions|Montreal Gases|CFC|CFC11", "Emissions|Montreal Gases|CFC|CFC113", "Emissions|Montreal Gases|CFC|CFC114",
                         "Emissions|Montreal Gases|CFC|CFC115", "Emissions|Montreal Gases|CFC|CFC12", "Emissions|Montreal Gases|CH3Br",
                         "Emissions|Montreal Gases|CH3CCl3", "Emissions|Montreal Gases|CH3Cl", "Emissions|Montreal Gases|HCFC141b",
                         "Emissions|Montreal Gases|HCFC142b", "Emissions|Montreal Gases|HCFC22", "Emissions|Montreal Gases|Halon1211",
                         "Emissions|Montreal Gases|Halon1301", "Emissions|Montreal Gases|Halon2402", "Emissions|N2O",
                         "Emissions|NOx", "Emissions|OC", "Emissions|Sulfur", "Emissions|VOC")) %>% 
  select(Scenario, Variable, Unit, X1750:X2014)

###################################
# next up
# 1. what variables are in hector
# 2. pull out for each scenario
# 3. need to check and update units
# 4. flip to long/wide to match Hector?

# Gases included in RCMIP but not included in Hector
"Emissions|NH3"
"Emissions|F-Gases|HFC|HFC152a"
"Emissions|F-Gases|HFC|HFC365mfc"
"Emissions|F-Gases|NF3"
"Emissions|F-Gases|PFC|C3F8"
"Emissions|F-Gases|PFC|C4F10"
"Emissions|F-Gases|PFC|C5F12"
"Emissions|F-Gases|PFC|C6F14"
"Emissions|F-Gases|PFC|C7F16"
"Emissions|F-Gases|PFC|C8F18"
"Emissions|F-Gases|PFC|cC4F8" 
"Emissions|F-Gases|SO2F2"
"Emissions|Montreal Gases|CH2Cl2"
"Emissions|Montreal Gases|CHCl3"
"Emissions|Montreal Gases|Halon1202"
"Emissions|F-Gases|HFC|HFC236fa"

# Gases included in Hector but not RCMIP
# C2F14
# HFC4310
# C2F6

#################################################
ssp119 <- filter(ssps, Scenario == "ssp119")
ssp370 <- filter(ssps, Scenario == "ssp370")
ssp245 <- filter(ssps, Scenario == "ssp245")
ssp434 <- filter(ssps, Scenario == "ssp434")
ssp460 <- filter(ssps, Scenario == "ssp460")
ssp534o <- filter(ssps, Scenario == "ssp534-over")
ssp580 <- filter(ssps, Scenario == "ssp580")

