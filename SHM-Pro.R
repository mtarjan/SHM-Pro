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
library(ggplot2)
data<-read_excel("G:/cameron/FWS_Refuge_202111/Analysis/FWS_NWR_spp_20211205.xlsx", sheet = "FWS_NWR_spp_20211205") %>% data.frame()

##add species names
##need a different table that has more of the species names
#SpeciesMasterLookupRaster <- read_csv("G:/tarjan/Model_Review_Management/Data/SpeciesMasterLookupRaster.csv") %>% dplyr::mutate(species = strsplit((strsplit(`Scientific Name`, " \\(") %>% purrr::map(1)) %>% unlist(), ",") %>% purrr::map(1) %>% unlist())
#SpeciesMasterLookupRaster$cutecode.model<-SpeciesMasterLookupRaster$cutecode
#SpeciesMasterLookupRaster$cutecode<-str_split(SpeciesMasterLookupRaster$cutecode.model, pattern = "_", simplify = T)[,1]

##add species names to data
#data <- left_join(x = data, y = subset(SpeciesMasterLookupRaster, select=c("cutecode", "Common Name")))

##SUMMARY STATS
##number of models in the NWRs
subset(data, RSL_TYPE=="NWR")

##percent of species range within NWR - bar plot
data.plot <- subset(data, RSL_TYPE=="NWR" & FWS_SOURCE=="IS" & is.na(Superceded_by)) %>% group_by(cutecode) %>% summarise(`Percent of Range` = (sum(VALUE_1)/Total.model.area)*100) %>% unique() %>% arrange(desc(`Percent of Range`))
data.plot$cutecode <- factor(data.plot$cutecode, levels = data.plot %>% arrange(`Percent of Range`) %>% pull(cutecode))

fig <- ggplot(data = data.plot[1:20,], aes(x = cutecode, y = `Percent of Range`, fill=grey(0.5))) +
  geom_bar(stat="identity", position = "dodge", color = grey(0.2), size = 0.5) +
  coord_flip() +
  theme_bw() +
  theme(rect = element_blank(), 
        panel.grid.major.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(hjust = 0),
        legend.position = "none"
  ) +
  ylab("Percent of Species Range in National Wildlife Refuges")
fig

##percent of species range within Acquisition layer, but not in interest layer (FWS does not currently own, but can acquire)
data.plot <- subset(data, FWS_SOURCE =="AANIS" & is.na(Superceded_by) & RSL_TYPE=="NWR") %>% group_by(cutecode) %>% summarise(`Percent of Range` = (sum(VALUE_1)/Total.model.area)*100) %>% unique() %>% arrange(desc(`Percent of Range`))
data.plot$cutecode <- factor(data.plot$cutecode, levels = data.plot %>% arrange(`Percent of Range`) %>% pull(cutecode))

fig <- ggplot(data = data.plot[1:20,], aes(x = cutecode, y = `Percent of Range`, fill=grey(0.5))) +
  geom_bar(stat="identity", position = "dodge", color = grey(0.2), size = 0.5) +
  coord_flip() +
  theme_bw() +
  theme(rect = element_blank(), 
        panel.grid.major.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(hjust = 0),
        legend.position = "none"
  ) +
  ylab("Percent of Species Range outside Interest Layer and in Acquisition Area")
fig
