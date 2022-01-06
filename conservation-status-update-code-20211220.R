##BLM SSS
##Conservation Status Update
##Create a list of priority species for update
##mtarjan
##Dec 20, 2021

library(readxl)
library(tidyverse)

sss<-read_excel("Data/BLM - Information for T & E Strategic Decision-Making - April 2021.xlsx", sheet = "BLM SSS Information by State", skip = 1) %>% data.frame()
reviews<-read_excel("Data/GRank_Review_Dates_20211214.xlsx", sheet = "U.S. EGT GRank Review info") %>% data.frame()
spp.ur<-read_excel("Data/National Listing Workplan FY21-FY25 Excel Format.xlsx", sheet = "5-yr Workplan") %>% data.frame() ##species under review
spp.ur$ESA.Review.Status<-spp.ur$USESA.Status
spp.ur[which(is.na(spp.ur$ESA.Review.Status)),]$ESA.Review.Status<-"5 year workplan"
#hab<-read_excel("Data/BLM-SSS-habitat-association-20220103.xlsx") %>% data.frame()
#hab<-read_excel("Data/_NatureServe_2016_Species-Habitat Relationships_Appendix_B_C_D.xlsx", sheet = "Appx C Systems-Species") %>% data.frame()
#hab$habitat[which(str_detect(hab$System.Name, "Sagebrush"))]<-"Sagebrush"
#hab$habitat[which(str_detect(hab$System.Name, "Pinyon-Juniper"))]<-"Pinyon-Juniper"
pj<-read_excel("Data/Pinyon-Juniper Woodland types.xlsx", sheet = "with links to species") %>% data.frame()
#pj$habitat<-"Pinyon-Juniper"
pj.spp.id<-pj$SP_ID
sb<-read_excel("Data/Sagebrush types with taxa.xlsx", sheet = "with links to species") %>% data.frame()
#sb$habitat[which(str_detect(sb$PJM_recommendation...note, "include"))]<-"Sagebrush"
sb.spp.id<-sb$SP_ID[which(str_detect(sb$PJM_recommendation...note, "include"))]
##add year 1 species
yr1<-read_excel("Data/BLM-SSS-Year1-Species-Rank-Review.xlsx", sheet = "Sheet1") %>% data.frame()

sss.grank.date<-left_join(x=sss[,1:11], y = reviews[,c(1,2,3,9,13,14,15,17,20)], by = c("Element.Global.ID" = "ELEMENT_GLOBAL_ID"))

##Inactive Global IDs; add IDs that have become inactive since the start of the BLM SSS project; it is not possible to match the Grank update date for these species
inactive.ids<-data.frame(Element.Global.ID = c(102436, 102200, 104141, 100144,107797,160730,106265,806561,105893,101770), Inactive=T)
sss.grank.date<-left_join(x=sss.grank.date, y = inactive.ids)

##add status for species from ESA 5-year workplan
sss.grank.date<-left_join(x=sss.grank.date, y=subset(spp.ur, select = c("Scientific.Name", "ESA.Review.Status", "Last.Reviewed")))
##create a new column for esa status that include the 5-year workplan info
sss.grank.date<-unite(sss.grank.date, col = "ESA.new", c(USESA, ESA.Review.Status, Last.Reviewed), sep = ".", na.rm=T)

##add habitat association
##not ideal because some species associated with two types of habitat
#sss.grank.date<-left_join(x=sss.grank.date, y = unique(subset(hab, !is.na(habitat), select=c("Species.Scientific.Name", "habitat"))), by=c("Scientific.Name"= "Species.Scientific.Name"))
#sss.grank.date<-left_join(x=sss.grank.date, y = unique(subset(pj, !is.na(habitat), select=c("SP_GNAME_UNFORMATTED", "habitat"))), by=c("Scientific.Name"= "SP_GNAME_UNFORMATTED"))

#sss.grank.date$pj<-F
#sss.grank.date$pj[which(sss.grank.date$Scientific.Name %in% pj$SP_GNAME_UNFORMATTED)]<-T
#sss.grank.date$pj[which(sss.grank.date$Scientific.Name %in% hab$Species.Scientific.Name[which(str_detect(hab$System.Name, "Pinyon-Juniper"))])]<-T
#sss.grank.date$sb<-F
#sss.grank.date$sb[which(sss.grank.date$Scientific.Name %in% hab$Species.Scientific.Name[which(str_detect(hab$System.Name, "Sagebrush"))])]<-T

sss.grank.date$habitat<-NA
sss.grank.date$habitat[which(sss.grank.date$Element.Global.ID %in% pj.spp.id)] <- "Pinyon-Juniper"
sss.grank.date$habitat[which(sss.grank.date$Element.Global.ID %in% sb.spp.id)] <- "Sagebrush"
sss.grank.date$habitat[which(sss.grank.date$Element.Global.ID %in% sb.spp.id & sss.grank.date$Element.Global.ID %in% pj.spp.id)] <- "Pinyon-Juniper, Sagebrush"

##add review year
sss.grank.date$G_RANK_REVIEW_DATE[which(sss.grank.date$G_RANK_REVIEW_DATE=="-")]<-NA
sss.grank.date$G_RANK_REVIEW_DATE<-as.Date(sss.grank.date$G_RANK_REVIEW_DATE, "%Y-%m-%d") ##format as date and get year
sss.grank.date$`Review Year`<-lubridate::year(sss.grank.date$G_RANK_REVIEW_DATE)

##get subset of plants, listed, longtime without status reviewed
#sss.review<-subset(sss.grank.date, Taxonomic.Group=="Plant" & !(USESA %in% c(NA, "DL: Delisted")), select=c("Element.Global.ID", "Scientific.Name", "Common.Name", "USESA","G_RANK", "G_RANK_REVIEW_DATE", "Inactive", "Occurrences.on.BLM.Lands..West....Total.Occurrences.Rangewide")) %>% arrange(G_RANK_REVIEW_DATE)

sss.review<-subset(sss.grank.date, select=c("Element.Global.ID", "Taxonomic.Group","Scientific.Name", "Common.Name", "ESA.new", "G_RANK", "G_RANK_REVIEW_DATE","Review Year", "Occurrences.on.BLM.Lands..West....Total.Occurrences.Rangewide", "habitat")) %>% arrange(G_RANK_REVIEW_DATE)

sss.review.short<-subset(sss.grank.date, !(Scientific.Name %in% yr1$Scientific.Name) & `Review Year` <=2016 & !(ESA.new %in% c(NA, "DL: Delisted", "Not Listed.2021", "")) & as.numeric(Occurrences.on.BLM.Lands..West....Total.Occurrences.Rangewide)>=0.5 & (str_detect(Global.Rank..18July2020., "G1\\w*") | str_detect(Global.Rank..18July2020., "G2\\w*") | !is.na(habitat)), select=c("Element.Global.ID", "Taxonomic.Group","Scientific.Name", "Common.Name", "ESA.new", "G_RANK", "G_RANK_REVIEW_DATE", "Occurrences.on.BLM.Lands..West....Total.Occurrences.Rangewide", "habitat")) %>% arrange(G_RANK_REVIEW_DATE)

write.csv(sss.review, "Output/BLM-SSS-GRank-review-date-20220106.csv", row.names = F)
write.csv(sss.review.short, "Output/BLM-SSS-shortlist-GRank-review-date-20220106.csv", row.names = F)

##histogram of grank review date
library(ggplot2)
#data.plot<-subset(sss.grank.date, Taxonomic.Group=="Plant" & !(USESA %in% c(NA, "DL: Delisted")))
#data.plot<-subset(sss.grank.date[which(str_detect(sss.grank.date$Global.Rank..18July2020., "G1\\w*") | str_detect(sss.grank.date$Global.Rank..18July2020., "G2\\w*")),], Taxonomic.Group=="Plant")
#data.plot<-subset(sss.grank.date[which(str_detect(sss.grank.date$Global.Rank..18July2020., "G1\\w*") | str_detect(sss.grank.date$Global.Rank..18July2020., "G2\\w*")),], Taxonomic.Group!="Plant" & !is.na(Taxonomic.Group) & Occurrences.on.BLM.Lands..West....Total.Occurrences.Rangewide>=0.5)
data.plot<-sss.review
data.plot$G_RANK_REVIEW_DATE[which(data.plot$G_RANK_REVIEW_DATE=="-")]<-NA
data.plot$G_RANK_REVIEW_DATE<-as.Date(data.plot$G_RANK_REVIEW_DATE, "%Y-%m-%d") ##format as date and get year
data.plot$`Review Year`<-lubridate::year(data.plot$G_RANK_REVIEW_DATE)

fig <- ggplot(data = data.plot, aes(x=`Review Year`)) +
  geom_histogram() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust=1, color="black"),
  ) +
  scale_x_continuous(breaks = function(x) pretty(x, n = 10), expand = c(0, 0)) +
  scale_y_continuous(breaks = function(x) pretty(x), expand = c(0, 0)) +
  xlab("Last review date for G1/G2 listed/workplan species")
fig