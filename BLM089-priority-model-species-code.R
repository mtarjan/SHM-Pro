##DRECP plants for modeling
##input data - EOs from 3/2022 snapshot of plants in the DRECP boundary

library(readxl)
library(tidyverse)
#library(ggplot2)

##read in EO info
##biotics fields: https://natureserve01-my.sharepoint.com/:x:/g/personal/shara_howie_natureserve_org/EYznjVG_BAJGi40urSIos_kBbKaNQ0gYaaQ0Uyy8AutZ1A?e=J0HS2O&CID=01d1f82e-5dbc-3e50-37b6-635d6815158a
data<-read_excel("Data/drecp-plants.xlsx", sheet = "Sheet1") %>% data.frame()

##read 1B plant list
plant1b<-read_excel("Data/plants-1b.xlsx") %>% data.frame()
##add EGT_ID
#plant1b<- left_join(x = plant1b, y = unique(subset(data, select=c("SNAME", "EGT_ID", "PS_EGT_ID"))), by = c("ScientificName" = "SNAME"))
plant1b<- left_join(x = plant1b, y = unique(subset(data, select=c("ELCODE_BCD", "EGT_ID", "PS_EGT_ID"))), by = c("ElementCode" = "ELCODE_BCD"))
##add EGTID for synonyms

length(which(is.na(plant1b$EGT_ID)))

##Include any EO with 100% overlap
##If EO has Very High or High RA then include with any overlap > 0%
##If EO has null or Medium RA then include with overlap > 50%
##If EO has Low or Very Low RA then include with overlap > 75%

##subset drecp plants to 1b plants and by other biotics restrictions
data.sub<-subset(data,
       EGT_ID %in% c(plant1b$EGT_ID, plant1b$PS_EGT_ID) ##include parent species for subspecies
       & POLY_AREA/Shape_Area >= 0.5
       & (LOBS_Y >= 1982 | is.na(LOBS_Y))
       & (LOBS_MAX_Y >= 1982 | is.na(LOBS_MAX_Y))
       & (EORANK_CD %in% c("E", "B") | is.na(EORANK_CD)) #can exclude X or H. extirpated historic
       & (ID_CONF == "Y" | is.na(ID_CONF)) #ID confirmed !=N
       );dim(data.sub)
##Restrict 1b plants to get species to model
model.spp<-subset(plant1b, EGT_ID %in% data.sub$EGT_ID | PS_EGT_ID %in% data.sub$EGT_ID)
##add number of EOs in desert region
eo.count.drecp<-data.sub %>% group_by(ELCODE_BCD) %>% count() %>% data.frame()
colnames(eo.count.drecp)[2] <- "EO.Count.DRECP"
##join to model.spp
model.spp <- left_join(x = model.spp, y = eo.count.drecp, by = c("ElementCode" = "ELCODE_BCD"))

##check species with more eos in desert than total
subset(model.spp, model.spp$EO.Count.DRECP/model.spp$EOTotal>1, select=c("ScientificName", "EOTotal", "EO.Count.DRECP", "EOX", "EOExtant"))