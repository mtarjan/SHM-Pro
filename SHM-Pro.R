##Species Habitat Model Products
##Jurisdictional analysis
##create plots from Cameron's intersection of FWS interest layer and acquisition layer with species habitat models
##Dec 8, 2021
##M Tarjan

##summary
##1. Counts of models overall (MoBI, DoD, BLM)	2736
##2. Counts of models intersecting in SE	658

library(readxl)
library(tidyverse)
data<-read_excel("G:/cameron/FWS_Refuge_202111/Analysis/FWS_NWR_spp_20211205.xlsx", sheet = "FWS_NWR_spp_20211205") %>% data.frame()

