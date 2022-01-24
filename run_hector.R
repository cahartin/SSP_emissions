# run hector with SSPs

# 9/24/2021


install.packages("devtools")
#remove.packages("")
library(devtools)
library(dplyr)

devtools::load_all("C:/Users/chartin/OneDrive - Environmental Protection Agency (EPA)/Documents/GitHub/hector/") 

library(hector)

path = ("C:/Users/chartin/OneDrive - Environmental Protection Agency (EPA)/Documents/GitHub/hector/inst/input/")

ini_file <- system.file("input/hector_ssp119.ini", package = "hector") 

core <- newcore(ini_file)
core 

run(core) 

results <- fetchvars(core, 1850:2200) %>% 
  mutate("scenario"= "ssp119") %>% 
  filter(variable == "Tgav")
head(results)


ini_file <- system.file("input/hector_ssp126.ini", package = "hector") 

core <- newcore(ini_file)
core 

run(core) 

results_126 <- fetchvars(core, 1850:2200) %>% 
  mutate("scenario"= "ssp126") %>% 
  filter(variable == "Tgav")
head(results_126)


ini_file <- system.file("input/hector_ssp245.ini", package = "hector") 

core <- newcore(ini_file)
core 

run(core) 

results_245 <- fetchvars(core, 1850:2200) %>% 
  mutate("scenario"= "ssp245") %>% 
  filter(variable == "Tgav")
head(results_245)

ssps <- rbind(results, results_126, results_245)

write.table(ssps, 'ssps_hector_output.csv',sep=",",append=TRUE, row.names=FALSE)
