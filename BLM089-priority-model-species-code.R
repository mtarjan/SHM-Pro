##DRECP plants for modeling
##input data - EOs from 3/2022 snapshot of plants in the DRECP boundary

library(readxl)
library(tidyverse)

##try pulling plants from api
#install.packages("natserv")
#library(natserv)

##read in EO info
##biotics fields: https://natureserve01-my.sharepoint.com/:x:/g/personal/shara_howie_natureserve_org/EYznjVG_BAJGi40urSIos_kBbKaNQ0gYaaQ0Uyy8AutZ1A?e=J0HS2O&CID=01d1f82e-5dbc-3e50-37b6-635d6815158a

data<-read_excel("Data/drecp-plants-20230216.xls", sheet = "drecp-plants-20230216") %>% data.frame()
##read bld dataframe for plants selected by ID (to get total EOs)
bld.sub<-read_excel("Data/bld-plants-20230216.xls", sheet = "bld-plants-20230216") %>% data.frame()
bld.sub<-subset(bld.sub, ID_CONF == "Y" | is.na(ID_CONF))

##add mobi models
mobimodels<-read_excel("C:/Users/max_tarjan/NatureServe/Map of Biodiversity Importance - Summary Tables/MoBI Modeling Summary by Species January 2021.xlsx", sheet = "MoBI_Model_Assessment", skip = 3) %>% data.frame()

##read 1B plant list
plant1b<-read_excel("Data/plants-1b.xlsx", sheet= "GlobalID") %>% data.frame() ##1B plants iwthin 5 mi of DRECP
##use new list sent from BLM 4/13 to include criteria of being on BLM SSS list (occurring on BLM land)
plant1b.blm<-read_excel("Data/BLMCASSPlist_20220324_forweb.xlsx") %>% data.frame()

##IRRELEVANT AFTER RECEIVING PLANT1B LIST WITH ELEMENT GLOBAL ID
##add EGT_ID
#plant1b<- left_join(x = plant1b, y = unique(subset(data, select=c("SNAME", "EGT_ID", "PS_EGT_ID"))), by = c("ScientificName" = "SNAME"))
#plant1b<- left_join(x = plant1b, y = unique(subset(data, select=c("ELCODE_BCD", "EGT_ID", "PS_EGT_ID"))), by = c("ElementCode" = "ELCODE_BCD")) ##add EGTID for synonyms
#length(which(is.na(plant1b$EGT_ID)))

##add EGT_ID to plant1b.blm
plant1b.blm<- left_join(x = plant1b.blm, y = unique(subset(data, select=c("SNAME", "EGT_ID", "PS_EGT_ID"))), by = c("Scientific.Name" = "SNAME")) ##add EGTID for synonyms
plant1b.blm$blm<-T

##add parent ID
plant1b<- left_join(x = plant1b, y = unique(subset(data, select=c("EGT_ID", "PS_EGT_ID"))), by = c("ELEMENT_GLOBAL_ID" = "EGT_ID"))

##add whether on blm land
plant1b <- left_join(x = plant1b, y= subset(plant1b.blm, select=c("EGT_ID", "blm")), by = c("ELEMENT_GLOBAL_ID" = "EGT_ID"))

##add whether plant has a mobi model
temp.mobi<- left_join(x = plant1b, y = subset(mobimodels, !is.na(ELEMENT_GLOBAL_ID...1), select=c("ELEMENT_GLOBAL_ID...1", "Included.in.MoBI")), by = c("ELEMENT_GLOBAL_ID" = "ELEMENT_GLOBAL_ID...1"))
temp.mobi<- left_join(x = temp.mobi, y = subset(mobimodels, !is.na(ELEMENT_GLOBAL_ID...1), select=c("ELEMENT_GLOBAL_ID...1", "Included.in.MoBI")), by = c("PS_EGT_ID" = "ELEMENT_GLOBAL_ID...1")) ##add for synonyms
plant1b$Included.in.MoBI<-mutate(.data = temp.mobi, Included.in.MoBI=coalesce(Included.in.MoBI.x, Included.in.MoBI.y))$Included.in.MoBI

##Include any EO with 100% overlap
##If EO has Very High or High RA then include with any overlap > 0%
##If EO has null or Medium RA then include with overlap > 50%
##If EO has Low or Very Low RA then include with overlap > 75%

##subset drecp plants to 1b plants and by other biotics restrictions, including whether the plant is known to occur on blm lands
##EGT_ID is same as element global ID
data.sub<-subset(data,
       EGT_ID %in% c(plant1b$ELEMENT_GLOBAL_ID, plant1b$PS_EGT_ID) ##include parent species for subspecies
       & POLY_AREA/Shape_Area >= 0.5
       & (LOBS_Y >= 1982 | is.na(LOBS_Y))
       & (LOBS_MAX_Y >= 1982 | is.na(LOBS_MAX_Y))
       & (!EORANK_CD %in% c("X", "H", "H?") | is.na(EORANK_CD)==F) #can exclude X or H. extirpated historic
       & (ID_CONF == "Y" | is.na(ID_CONF)) #ID confirmed !=N
       );dim(data.sub)
##Restrict 1b plants to get species to model
model.spp<-subset(plant1b, blm==T & (ELEMENT_GLOBAL_ID %in% data.sub$EGT_ID | PS_EGT_ID %in% data.sub$EGT_ID))
##add number of EOs in desert region
eo.count.drecp<-data.sub %>% group_by(EGT_ID) %>% count() %>% data.frame()
colnames(eo.count.drecp)[2] <- "EO.Count.DRECP"
##get number of EOs total for the species list
eo.total<-bld.sub %>% group_by(EGT_ID) %>% count() %>% data.frame()
colnames(eo.total)[2] <- "EO.Total"

##join to model.spp
model.spp <- left_join(x = model.spp, y = eo.count.drecp, by = c("ELEMENT_GLOBAL_ID" = "EGT_ID"))
model.spp <- left_join(x = model.spp, y = eo.total, by = c("ELEMENT_GLOBAL_ID" = "EGT_ID"))
model.spp$perc.eo.drecp<- model.spp$EO.Count.DRECP/model.spp$EO.Total*100
##order by perc in drecp
model.spp <- model.spp[order(-model.spp$perc.eo.drecp),]

##denote if there is a USGS model available
prev.models <- read_excel("Data/Previous-SHM-sources.xlsx", sheet = "Sheet1") %>% data.frame()
model.spp$existing.model<-F
model.spp$existing.model[which(model.spp$SNAME %in% prev.models$Scientific.name)]<-T
model.spp$existing.model[which(model.spp$CNAME %in% prev.models$Common.name)]<-T

##denote if NS has already modeled this for Federal projects
fed.spp <- read.csv("G:/tarjan/Model_Review_Management/Outputs/Modeled-species-federal-FY21.csv")
model.spp$ns.model<-F
model.spp$ns.model[which(model.spp$SNAME %in% fed.spp$Scientific.Name)]<-T
model.spp$ns.model[which(model.spp$CNAME %in% fed.spp$Common.Name)]<-T

write.csv(model.spp, str_c("Output/BLM0R089-priority-model-species-", Sys.Date(), ".csv"), row.names = F)

##create sql for elcodes
elcode.sql<-function(x, y) {str_c(y," = '", x, "' Or")}
query.sql<-elcode.sql(x = model.spp$ElementCode, y = "ELCODE_BCD")
#query.sql<-elcode.sql(x=plant1b$SNAME, y = "SNAME")
query.sql<-elcode.sql(x=plant1b$ELMCODE, y = "ELCODE_BCD")
write.csv(query.sql, str_c("Output/BLM0R089-test-sql-query-", Sys.Date(), ".csv"), row.names = F)

##test that new method yields the same species
#snap1b<-read_excel("Data/plants-1b-snapshot.xlsx") %>% data.frame()
#snap1be<-read_excel("Data/plants-1b-snapshot.xlsx", sheet = "elcode") %>% data.frame()

##write a function to compare two lists of identifiers
#compare <- function(a,b) {
#  df.a<-data.frame(x=a, included_a=T)
#  df.b<-data.frame(x=b, included_b=T)
#  full_join(df.a, df.b) %>% subset(is.na(included_a) | is.na(included_b)) %>% print()
#}
#compare(a = snap1b$SNAME, b = plant1b$SNAME)

##CREATE LIST OF EXISTING MODELS AND THEIR USES
spp<-read_excel("Data/BLM0R089-priority-model-species-2022-05-26.xlsx", sheet = "BLM0R089-priority-model-species")

mobi<-read_excel("C:/Users/max_tarjan/NatureServe/Map of Biodiversity Importance - Summary Tables/MoBI Modeling Summary by Species January 2021.xlsx", sheet = "MoBI_Model_Assessment", skip = 2)# %>% data.frame()
names(mobi)[1:8]<-c("Scientific.Name",	"ELEMENT_GLOBAL_ID_2021",	"ELEMENT_GLOBAL_ID_2018",	"Cutecode",	"Broad.Group",	"Taxonomic.Group",	"Scientific.Name",	"Common.Name")

##add model confidence and preliminary assessment to list of blm ca desert species
spp2 <- left_join(spp, subset(mobi, select = c("Scientific.Name",	"ELEMENT_GLOBAL_ID_2021","Overal All Confidence", "Preliminary Model Assessment")), by = c("ELEMENT_GLOBAL_ID" = "ELEMENT_GLOBAL_ID_2021")) %>% data.frame()

#write.csv(spp2, "output/BLM0R089-model-species-confidence-20220921.csv", row.names=F, na = "")

##donut charts for methodology report
##proportion with each G rank
##proportion of EOs in DRECP region
data.plot<-data.frame(table(spp2$Included.in.MoBI))

##get the label positions
data.plot <- data.plot %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq) %>%
  data.frame()
data.plot

fig <- ggplot(data.plot, aes(x = 2, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = Freq), color = "black", size=8)+
  #geom_text(aes(y = 1, x = 1, label = paste0(round(label*100,0), "%")), color = c("black"), size = 6) +
  scale_fill_brewer(palette = "Greens", name="", direction = -1) +
  theme_void() +
  xlim(.9, 2.5) +
  theme(text = element_text(size = 10), legend.position="right")
fig
