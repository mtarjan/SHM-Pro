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
mobimodels<-read_excel("Data/MoBI Modeling Summary by Species January 2021.xlsx", sheet = "MoBI_Model_Assessment", skip = 2) %>% data.frame()
colnames(mobimodels)[3:7]<-c("cutecode", "Broad Group", "Taxonomic Group", "Scientific Name", "Common Name")

##add species names to data
mobi.sub<-subset(mobimodels, select=c("cutecode", "Scientific Name", "Common Name", "Broad Group", "Jan21.Rounded.G.Rank", "ESA.Status"))
colnames(mobi.sub)[4:5]<-c("Taxonomic Group","G Rank")
data <- left_join(x = data, y = mobi.sub) ##NOTE THAT PITUMUGI HAD A TAXONOMY CHANGE SO DOESN'T GET NAME ASSIGNED HERE

##SUMMARY STATS
##number of models in the NWRs
length(unique(subset(data, RSL_TYPE=="NWR" & FWS_SOURCE =="IS" & is.na(Superceded_by))$cutecode))

##number of models in the acquisition boundary
length(unique(subset(data, FWS_SOURCE =="AA")$cutecode)) ##need to filter out dod if need to sum numbers (some species have mobi and dod models)

##number of models in acquisition boundary that are not in interest layer
aa.spp<-subset(data, FWS_SOURCE =="AA")
is.spp<-subset(data, FWS_SOURCE =="IS")
aanis.spp<-subset(aa.spp, !(cutecode %in% is.spp$cutecode), select=c("cutecode", "Taxonomic Group", "Common Name", "G Rank")) %>% unique()
nrow(aanis.spp)
##which species?
table(aanis.spp$`Taxonomic Group`)

##superceeded by means there were two models
##anyting from AA has AANIS superceed field

##percent of species range within NWR - bar plot
data.plot <- subset(data, RSL_TYPE=="NWR" & FWS_SOURCE=="IS" & is.na(Superceded_by)) %>% group_by(cutecode, `Common Name`, `G Rank`, `Taxonomic Group`) %>% summarise(`Percent of Range` = (sum(VALUE_1)/Total.model.area)*100) %>% unique() %>% arrange(desc(`Percent of Range`))
data.plot$`Common Name` <- factor(data.plot$`Common Name`, levels = data.plot %>% arrange(`Percent of Range`) %>% pull(`Common Name`))

fig <- ggplot(data = data.plot[1:20,], aes(x = `Common Name`, y = `Percent of Range`, fill=`G Rank`)) +
  geom_bar(stat="identity", position = "dodge", color = grey(0.2), size = 0.5) +
  coord_flip() +
  theme_bw() +
  theme(rect = element_blank(), 
        panel.grid.major.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(hjust = 0)#,
        #legend.position = "none"
  ) +
  geom_text(aes(label = `Taxonomic Group`), size = 3, y=data.plot$`Percent of Range`[1:20]/2) +
  scale_fill_brewer(palette = "Greens", direction = -1) +
  ylab("Percent of Species Range in National Wildlife Refuges")
fig

##percent of species range within Acquisition layer, but not in interest layer (FWS does not currently own, but can acquire)
data.plot <- subset(data, FWS_SOURCE =="AANIS" & is.na(Superceded_by) & RSL_TYPE=="NWR") %>% group_by(cutecode, `Common Name`, `G Rank`, `Taxonomic Group`) %>% summarise(`Percent of Range` = (sum(VALUE_1)/Total.model.area)*100) %>% unique() %>% arrange(desc(`Percent of Range`))
data.plot$`Common Name` <- factor(data.plot$`Common Name`, levels = data.plot %>% arrange(`Percent of Range`) %>% pull(`Common Name`))

fig <- ggplot(data = data.plot[1:20,], aes(x = `Common Name`, y = `Percent of Range`, fill=`G Rank`)) +
  geom_bar(stat="identity", position = "dodge", color = grey(0.2), size = 0.5) +
  coord_flip() +
  theme_bw() +
  theme(rect = element_blank(), 
        panel.grid.major.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(hjust = 0)#,
        #legend.position = "none"
  ) +
  geom_text(aes(label = `Taxonomic Group`), size = 3, y=data.plot$`Percent of Range`[1:20]/2) +
  scale_fill_brewer(palette = "Greens", direction = -1) +
  ylab("Percent of Species Range outside Interest Layer and in Acquisition Area for NWR")
fig

##pull out mobi pollinators
pollinators<- unique(subset(mobimodels,`Broad Group`=="Pollinators" & Included.in.MoBI =="yes", select =c("ELEMENT_GLOBAL_ID...1", "Taxonomic Group","Scientific Name", "Common Name", "Rounded.G.Rank", "ESA.Status")))

##priority list for modeling

priority<-unique(subset(data, select=c(Project, cutecode, `Scientific Name`, `Common Name`, `G Rank`, `Taxonomic Group`, `ESA.Status`))) ##get unique list of species
##calculate percent of model in NWR
data.temp <- subset(data, RSL_TYPE=="NWR" & FWS_SOURCE=="IS" & is.na(Superceded_by)) %>% group_by(cutecode) %>% summarise(`Percent.range.NWR` = (sum(VALUE_1)/Total.model.area)*100) %>% unique()
priority<-left_join(priority, data.temp)
##calculate percent of model in acquisition area
##percent of species range within Acquisition layer, but not in interest layer (FWS does not currently own, but can acquire)
data.temp <- subset(data, FWS_SOURCE =="AANIS" & is.na(Superceded_by) & RSL_TYPE=="NWR") %>% group_by(cutecode) %>% summarise(`Percent.range.acqu` = (sum(VALUE_1)/Total.model.area)*100) %>% unique()
priority<-left_join(priority, data.temp)

##add species from habitat project 036
hab<-read_excel("Data/FWS_SE_Habitat_species.xlsx") %>% data.frame()
priority<-left_join(x=priority, y=hab, by = c("Scientific Name" = "Scientific.Name"))

##priority are species that are not ESA listed and occur in the NWR or acquisition layer
##need to include more ESA statuses that are not "listed": https://help.natureserve.org/biotics/content/record_management/Element_Files/Element_Tracking/ETRACK_USESA_Status.htm
listed<-c("E", "T")
priority<-subset(priority, 
                 USW0R036 == T 
                 | (is.na(ESA.Status) 
                    & (str_detect(`G Rank`, "G1") | str_detect(`G Rank`, "G2"))  
                    & (Percent.range.NWR>=20 | Percent.range.acqu>=20)
                    )
                 )
