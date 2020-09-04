# Process CMIP6 emissions from 
# https://www.rcmip.org/
# CH 09/03/20

library(tidyr)
library(dplyr)
library(stringr)
#path = "C:/Users/chartin/Documents/GitHub/SSP_emissions/"

Hector_emis <- c("BC_emissions","CH4_emissions", "CO_emissions", "ffi_emissions","luc_emissions",
                 "HFC125_emissions", "HFC134a_emissions","HFC143a_emissions",
                 "HFC227ea_emissions",  "HFC23_emissions", "HFC245fa_emissions", "HFC32_emissions",
                 "HFC4310_emissions",  "C2F6_emissions", "CF4_emissions","SF6_emissions",
                 "CCl4_emissions", "CFC11_emissions",	"CFC113_emissions", "CFC114_emissions",	"CFC115_emissions",	"CFC12_emissions",
                 "CH3Br_emissions","CH3CCl3_emissions", "CH3Cl_emissions", "HCFC141b_emissions",	"HCFC142b_emissions",
                 "HCFC22_emissions",	"halon1211_emissions", "halon1301_emissions", "halon2402_emissions",
                 "N2O_emissions", "NOX_emissions", "OC_emissions", "SO2_emissions","NMVOC_emissions")



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
                         "Emissions|Montreal Gases|Halon2402", "Emissions|Montreal Gases|CH3Br",  "Emissions|Montreal Gases|CH3Cl")) %>% 
  select(Scenario, Variable, Unit, X2015:X2300) 

historical <- read.csv("C:/Users/chartin/Documents/GitHub/SSP_emissions/rcmip-emissions-annual-means-v4-0-0.csv") %>% 
  select(-Model, -Activity_Id) %>% 
  filter(Region == "World" & Mip_Era == "CMIP6") %>% 
  select(-Region, -Mip_Era) %>% 
  filter(Scenario %in% c( "historical")) %>% 
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
  select(Scenario, Variable, Unit, X1750:X2014)


###################################
# next up
# 1. what variables are in hector
# 2. pull out each scenario
# 3. need to check and update units
#     a. CO2 from MtCO2 to GtC
#     b. N2O from ktN2O to MtN2O
#     c. SO2 from Mt to Gg (is it S or SO2?)
# 4. flip to long/wide to match Hector

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
# C2F6

#################################################
#transpose dataframe so the years on the rows and the variables are columns
ssp119 <- filter(ssps, Scenario == "ssp119") %>% 
  select(-Scenario)

ssp119 <- as.data.frame(t(as.matrix(ssp119))) %>% 
  rename("BC_emissions" = "V1") %>% 
  rename("CH4_emissions" = "V2") %>% 
  rename("CO_emissions" = "V3") %>% 
  rename("ffi_emissions" = "V4") %>% 
  rename("luc_emissions" = "V5") %>% 
  rename("HFC125_emissions" = "V6") %>% 
  rename("HFC134a_emissions" = "V7") %>% 
  rename("HFC143a_emissions" = "V8") %>% 
  rename("HFC227ea_emissions" = "V9") %>% 
  rename("HFC23_emissions" = "V10") %>% 
  rename("HFC245fa_emissions" = "V11") %>% 
  rename("HFC32_emissions" = "V12") %>% 
  rename("HFC4310_emissions" = "V13") %>% 
  rename("C2F6_emissions" = "V14") %>% 
  rename("CF4_emissions" = "V15") %>% 
  rename("SF6_emissions" = "V16") %>% 
  rename("CCl4_emissions" = "V17") %>% 
  rename("CFC11_emissions" = "V18") %>% 
  rename("CFC113_emissions" = "V19") %>% 
  rename("CFC114_emissions" = "V20") %>% 
  rename("CFC115_emissions" = "V21") %>% 
  rename("CFC12_emissions" = "V22") %>% 
  rename("CH3Br_emissions" = "V23") %>% 
  rename("CH3CCl3_emissions" = "V24") %>% 
  rename("CH3Cl_emissions" = "V25") %>% 
  rename("HCFC141b_emissions" = "V26") %>% 
  rename("HCFC142b_emissions" = "V27") %>% 
  rename("HCFC22_emissions" = "V28") %>% 
  rename("halon1211_emissions" = "V29") %>% 
  rename("halon1301_emissions" = "V30") %>% 
  rename("halon2402_emissions" = "V31") %>% 
  rename("N2O_emissions" = "V32") %>% 
  rename("NOX_emissions" = "V33") %>% 
  rename("OC_emissions" = "V34") %>% 
  rename("SO2_emissions" = "V35") %>% 
  rename("NMVOC_emissions" = "V36")

ssp370 <- filter(ssps, Scenario == "ssp370") %>% 
  select(-Scenario)
ssp370 <- as.data.frame(t(as.matrix(ssp370)))

ssp245 <- filter(ssps, Scenario == "ssp245") %>% 
  select(-Scenario)
sspp245 <- as.data.frame(t(as.matrix(ssp245)))

ssp434 <- filter(ssps, Scenario == "ssp434")
ssp460 <- filter(ssps, Scenario == "ssp460")
ssp534o <- filter(ssps, Scenario == "ssp534-over")
ssp580 <- filter(ssps, Scenario == "ssp580")

