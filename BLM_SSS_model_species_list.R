##Query species models
##November 8, 2021
##M Tarjan

##find species for modeling

##read in list of BLM species
library(tidyverse)
library(readxl)
#library(dplyr)
blm.spp<-read_excel("Data/BLM - Compiled SSS List Information - September 2021.xlsx", sheet = "1b. BLM-NS SSS Data Summary", col_names=T, skip=1) %>% data.frame()
##add column showing which states the species intersects with
spp.states<-blm.spp %>% tidyr::gather("state", "presence", 12:ncol(blm.spp)) %>% subset(presence !="-") %>% group_by(Scientific.Name) %>% summarize(states=str_c(unique(str_sub(state, 1, 2)), collapse=", ")) %>% data.frame()
blm.spp<-left_join(blm.spp, spp.states)

##add number of EOs from earlier version of list
eo.count<-read_excel("Data/BLM - Information for T & E Strategic Decision-Making - April 2021.xlsx", sheet = "BLM SSS Information by State", col_names=T, skip=1) %>% data.frame()
blm.spp<-left_join(blm.spp, subset(eo.count, select=c("Element.Global.ID", "Total.Occurrences.Rangewide")))

blm.spp<-subset(blm.spp, select = c("Element.Global.ID", "Taxonomic.Group", "Scientific.Name", "Common.Name", "Global.Rank", "ESA.Status", "Pollinator.Species","Percent.of.Element.Occurrences.across.all.BLM.West.Lands", "Total.Occurrences.Rangewide","states", "MoBI.Model.Review.Status")) #"Total.Occurrences.Rangewide" not in the newer spreadsheet
names(blm.spp)[names(blm.spp) == 'Percent.of.Element.Occurrences.across.all.BLM.West.Lands'] <- 'Prop.on.BLM.Lands.West'

##read Regan's spreadsheet of Mobi models
#mobi<-read_excel("Data/MoBI Modeling Summary by Species January 2021.xlsx", sheet = "MoBI_Model_Assessment", col_names = T, skip = 2) %>% data.frame()
#mobi<-mobi[,1:30]
#colnames(mobi)[1:6] <- c("Element.Global.ID", "global.id.2018", "Cutecode", "broad.group", "taxonomic.Group", "scientific.Name")

##read Year 1 BLM species list
blm.spp.y1<-read_excel("Data/BLMSSS-Year1-models-delivered_20211005.xlsx", sheet =
                         "Sheet1", col_names = T, skip = 0) %>% data.frame()

##add mobi model info to blm species list
#blm.mobi<-dplyr::left_join(x = blm.spp, y = mobi)
plants<-c("Flowering Plants - Dicots", "Flowering Plants - Monocots", "Leptosporangiate Ferns", "Mosses", "Conifers", "Spikemosses and Quillworts", "Clubmosses", "Hornworts", "Adder's-tongues, Grapeferns, and Moonworts")

##get a list of species to model in Year 2
yr2.spp <- subset(blm.spp, 
                      (str_detect(Global.Rank, "G1") | str_detect(Global.Rank, "G2") | str_detect(Global.Rank, "G3"))
                      #& Taxonomic.Group != "Plant"
                      #& str_length(states) >2
                      & (str_length(states) >2 | !(Taxonomic.Group %in% plants))
                      & as.numeric(Prop.on.BLM.Lands.West) >= 0.2
                      & !(Scientific.Name %in% unique(blm.spp.y1$scientific_name))
                      & Total.Occurrences.Rangewide >= 3 ##guessing at the number
                      & !(ESA.Status %in% c("-", "DL"))
                      #& Included.in.MoBI == "yes"
                      #& !is.na(Included.in.MoBI)
                      #& Overal.All.Confidence == "Medium"
                      #& Model.Review %in% c("High", "Medium", "Low")
                      #& Validation.Stats %in% c("High", "Medium")
)
dim(yr2.spp)
dim(subset(yr2.spp, !(Taxonomic.Group %in% plants)))
subset(yr2.spp, select=c("Taxonomic.Group", "Common.Name", "Global.Rank", "ESA.Status", "Prop.on.BLM.Lands.West", "MoBI.Model.Review.Status", "states")) %>% arrange(Common.Name)

data.write<-subset(yr2.spp, select=c("Element.Global.ID", "Taxonomic.Group", "Scientific.Name", "Common.Name", "Global.Rank", "ESA.Status", "Prop.on.BLM.Lands.West", "states")) %>% arrange(Common.Name)
##add plant designation
data.write$Taxonomic.Group2<-NA
data.write$Taxonomic.Group2[which(data.write$Taxonomic.Group%in% plants)] <- "Plant"
names(data.write)<-c("Element.Global.ID", "Taxonomic.Group", "Scientific.Name", "Common.Name", "Global.Rank", "ESA.Status", "Prop.on.BLM.Lands.West", "States", "Taxonomic.Group2")

write.csv(data.write, file = str_c("Outputs/BLM_year2_model_species_", format(Sys.Date(), "%Y%m%d"),".csv"), row.names=F)

          