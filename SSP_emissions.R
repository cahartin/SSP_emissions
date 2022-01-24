# Process CMIP6 emissions into Hector format
# Data from https://www.rcmip.org/
# Accessed 9/3/2020

# CH 09/03/20

library(tidyr)
library(dplyr)
library(stringr)


###########################
scenarios <- c("ssp119", "ssp370", "ssp126", "ssp245","ssp434", "ssp460", "ssp534-over", "ssp585")

###############################

ssps <- read.csv("rcmip-emissions-annual-means-v4-0-0.csv") %>% 
  select(-Model, -Activity_Id) %>% 
  filter(Region == "World" & Mip_Era == "CMIP6") %>% 
  select(-Region, -Mip_Era) %>% 
  filter(Scenario %in% c( scenarios)) %>% 
  filter(Variable %in% c("Emissions|CO2","Emissions|CO2|MAGICC AFOLU", "Emissions|CH4",  "Emissions|N2O", 
                         "Emissions|Sulfur", "Emissions|CO", "Emissions|VOC", "Emissions|NOx", 
                         "Emissions|BC", "Emissions|OC", "Emissions|F-Gases|PFC|CF4", "Emissions|F-Gases|PFC|C2F6", 
                         "Emissions|F-Gases|HFC|HFC23", "Emissions|F-Gases|HFC|HFC32", "Emissions|F-Gases|HFC|HFC32", 
                         "Emissions|F-Gases|HFC|HFC4310mee", "Emissions|F-Gases|HFC|HFC125",
                         "Emissions|F-Gases|HFC|HFC134a", "Emissions|F-Gases|HFC|HFC143a", "Emissions|F-Gases|HFC|HFC227ea",  
                         "Emissions|F-Gases|HFC|HFC245fa", "Emissions|F-Gases|SF6", 
                         "Emissions|Montreal Gases|CFC|CFC11", "Emissions|Montreal Gases|CFC|CFC12", "Emissions|Montreal Gases|CFC|CFC113", 
                         "Emissions|Montreal Gases|CFC|CFC114","Emissions|Montreal Gases|CFC|CFC115",  
                         "Emissions|Montreal Gases|CCl4", "Emissions|Montreal Gases|CCl4", 
                         "Emissions|Montreal Gases|CH3CCl3", "Emissions|Montreal Gases|HCFC22",
                         "Emissions|Montreal Gases|HCFC141b", "Emissions|Montreal Gases|HCFC142b", "Emissions|Montreal Gases|Halon1211",  
                         "Emissions|Montreal Gases|Halon1301", "Emissions|Montreal Gases|Halon2402", 
                         "Emissions|Montreal Gases|CH3Br",  "Emissions|Montreal Gases|CH3Cl")) %>% 
  select(Scenario, Variable, Unit, X1750:X2300) 



# Gases included in RCMIP but not included in Hector
# "Emissions|NH3"
# "Emissions|F-Gases|HFC|HFC152a"
# "Emissions|F-Gases|HFC|HFC365mfc"
# "Emissions|F-Gases|NF3"
# "Emissions|F-Gases|PFC|C3F8"
# "Emissions|F-Gases|PFC|C4F10"
# "Emissions|F-Gases|PFC|C5F12"
# "Emissions|F-Gases|PFC|C6F14"
# "Emissions|F-Gases|PFC|C7F16"
# "Emissions|F-Gases|PFC|C8F18"
# "Emissions|F-Gases|PFC|cC4F8" 
# "Emissions|F-Gases|SO2F2"
# "Emissions|Montreal Gases|CH2Cl2"
# "Emissions|Montreal Gases|CHCl3"
# "Emissions|Montreal Gases|Halon1202"
# "Emissions|F-Gases|HFC|HFC236fa"


#################################################

# pull out 1 scenario at a time - should be able to loop. but it's not working at the moment
ssp119 <- filter(ssps, Scenario == "ssp119") %>% 
  select(-Scenario, -Unit) %>% 
  
  ## get rid of strings before gas name
  mutate(Variable=as.character(Variable)) %>%
  mutate(Variable=(str_replace(Variable, "^.*\\|", ""))) %>% 
  
  # add "_emissions" to gas names
  mutate(Variable=paste0(Variable, "_emissions")) %>% 
  gather(Date, value, X1750:X2300) %>% 
  spread(Variable, value) %>% 
  mutate(Date=(str_replace(Date, "X", ""))) %>% # get rid of X in front of date
  
  ## Rename a few columns to match Hector
  
  rename( "ffi_emissions" = "CO2_emissions") %>% 
  rename( "luc_emissions" = "MAGICC AFOLU_emissions") %>%
  rename( "NMVOC_emissions" = "VOC_emissions") %>% 
  rename( "HFC4310_emissions"="HFC4310mee_emissions") %>% 
  rename( "SO2_emissions" ="Sulfur_emissions") %>% 
  rename( "NOX_emissions" ="NOx_emissions") %>% 
  rename( "halon2402_emissions" ="Halon2402_emissions") %>% 
  rename( "halon1211_emissions" = "Halon1211_emissions") %>% 
  rename("halon1301_emissions" = "Halon1301_emissions") %>% 

  ## Unit changes
  mutate(ffi_emissions = ((ffi_emissions/1000) *(12/44))) %>%  # Convert from MtCO2 to GtC/year
  mutate(luc_emissions = ((luc_emissions/1000) * (12/44))) %>% 
  mutate(N2O_emissions = ((N2O_emissions * 0.001) *(14.0067/44.0128))) %>%  # convert from ktN2O to GtN
  mutate(SO2_emissions = ((SO2_emissions * 1000) * (32.01/64.07))) %>%   # convert from MtSO2 to GgS
  mutate(NOX_emissions = (NOX_emissions * (14.0067/44.0128))) %>% 
  
    drop_na() # data is every 10 years in future - delete rows w NAs

write.table(ssp119, 'ssp119_emissions.csv',sep=",", row.names=FALSE)

#####  
  ssp126 <- filter(ssps, Scenario == "ssp126") %>% 
    select(-Scenario, -Unit) %>% 
    
    ## get rid of strings before gas name
    mutate(Variable=as.character(Variable)) %>%
    mutate(Variable=(str_replace(Variable, "^.*\\|", ""))) %>% 
    
    # add "_emissions" to gas names
    mutate(Variable=paste0(Variable, "_emissions")) %>% 
    gather(Date, value, X1750:X2300) %>% 
    spread(Variable, value) %>% 
    mutate(Date=(str_replace(Date, "X", ""))) %>% # get rid of X in front of date
    
    ## Rename a few columns to match Hector
    
    rename( "ffi_emissions" = "CO2_emissions") %>% 
    rename( "luc_emissions" = "MAGICC AFOLU_emissions") %>%
    rename( "NMVOC_emissions" = "VOC_emissions") %>% 
    rename( "HFC4310_emissions"="HFC4310mee_emissions") %>% 
    rename( "SO2_emissions" ="Sulfur_emissions") %>% 
    rename( "NOX_emissions" ="NOx_emissions") %>% 
    rename( "halon2402_emissions" ="Halon2402_emissions") %>% 
    rename( "halon1211_emissions" = "Halon1211_emissions") %>% 
    rename("halon1301_emissions" = "Halon1301_emissions") %>% 
    
    ## Unit changes
    mutate(ffi_emissions = ((ffi_emissions/1000) *(12/44))) %>%  # Convert from MtCO2 to GtC/year
    mutate(luc_emissions = ((luc_emissions/1000) * (12/44))) %>% 
    mutate(N2O_emissions = ((N2O_emissions * 0.001) *(14.0067/44.0128))) %>%  # convert from ktN2O to GtN
    mutate(SO2_emissions = ((SO2_emissions * 1000) * (32.01/64.07))) %>%   # convert from MtSO2 to GgS
    mutate(NOX_emissions = (NOX_emissions * (14.0067/44.0128))) %>% 
    
    drop_na() # data is every 10 years in future - delete rows w NAs
  
  

  write.table(ssp126, 'ssp126_emissions.csv',sep=",", row.names=FALSE)
  
  
  ####
  ssp245 <- filter(ssps, Scenario == "ssp245") %>% 
    select(-Scenario, -Unit) %>% 
    
    ## get rid of strings before gas name
    mutate(Variable=as.character(Variable)) %>%
    mutate(Variable=(str_replace(Variable, "^.*\\|", ""))) %>% 
    
    # add "_emissions" to gas names
    mutate(Variable=paste0(Variable, "_emissions")) %>% 
    gather(Date, value, X1750:X2300) %>% 
    spread(Variable, value) %>% 
    mutate(Date=(str_replace(Date, "X", ""))) %>% # get rid of X in front of date
    
    ## Rename a few columns to match Hector
    
    rename( "ffi_emissions" = "CO2_emissions") %>% 
    rename( "luc_emissions" = "MAGICC AFOLU_emissions") %>%
    rename( "NMVOC_emissions" = "VOC_emissions") %>% 
    rename( "HFC4310_emissions"="HFC4310mee_emissions") %>% 
    rename( "SO2_emissions" ="Sulfur_emissions") %>% 
    rename( "NOX_emissions" ="NOx_emissions") %>% 
    rename( "halon2402_emissions" ="Halon2402_emissions") %>% 
    rename( "halon1211_emissions" = "Halon1211_emissions") %>% 
    rename("halon1301_emissions" = "Halon1301_emissions") %>% 
    
    ## Unit changes
    mutate(ffi_emissions = ((ffi_emissions/1000) *(12/44))) %>%  # Convert from MtCO2 to GtC/year
    mutate(luc_emissions = ((luc_emissions/1000) * (12/44))) %>% 
    mutate(N2O_emissions = ((N2O_emissions * 0.001) *(14.0067/44.0128))) %>%  # convert from ktN2O to GtN
    mutate(SO2_emissions = ((SO2_emissions * 1000) * (32.01/64.07))) %>%   # convert from MtSO2 to GgS
    mutate(NOX_emissions = (NOX_emissions * (14.0067/44.0128))) %>% 
    
    drop_na() # data is every 10 years in future - delete rows w NAs
  
  
 write.table(ssp245, 'ssp245_emissions.csv',sep=",", row.names=FALSE)
  