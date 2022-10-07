##get list of species to model for BLM SSS project in year 3
##M Tarjan
##Oct 5, 2022

library(readxl)
library(tidyverse)

##read in list of blm sss
sss<-read_excel("C:/Users/max_tarjan/NatureServe/BLM - BLM SSS Distributions and Rankings Project-FY21/Provided to BLM/BLM - Information for T & E Strategic Decision-Making - April 2021.xlsx", sheet = "BLM SSS Information by State", skip =1) %>% data.frame() %>% subset(Elements.Matched.between.BLM.SSS.List.and.NatureServe.Data !="-")

##first get uid from biotics
#library(RODBC)

##NEED TO FIRST CONNECT TO VPN
#con<-odbcConnect("BIOSNAPDB07", uid="biotics_report", pwd=rstudioapi::askForPassword("Password")) ##open connection to database

##put a wrapper around query to get more than 1000 records
#id.vector<-sss$Element.Global.ID
#max.length <- 999
#x <- 1
#y <- min(c(max.length,length(id.vector)))
#eguid<-dim(0)

#for (j in 1:ceiling((length(id.vector)/max.length))) {
  #id.temp<-paste0("(", paste0(id.vector[x:y], collapse = ", "), ")")
  ##get the uids using the element global ids
  #qry<- paste0("select element_global_id, 'ELEMENT_GLOBAL.'||element_global_ou_uid||'.'||element_global_seq_uid eguid
#from element_global
#where (element_global_id in ",id.temp ,")")
  
  ##import the queried table
  #eguid.temp<-sqlQuery(con, qry)
  #eguid<-rbind(eguid, eguid.temp)
  ##move forward x and y
  #x <- y +1
  #y <- min(c(x-1+max.length,length(id.vector)))
#}
# close the connection
#odbcClose(con)

##get up to date info from API
library(natserv)

##create function to search api for species, or tell why search failed if there is no match
# Do something, or tell me why it failed
api_search <- function(id){
  tryCatch(
    # This is what I want to do...
    {
      y <- natserv::ns_altid(id=id)
      return(y)
    },
    # ... but if an error occurs, tell me what happened: 
    error=function(error_message) {
      message("An error was retured when searching for this species:")
      message(error_message)
      return(NA)
    }
  )
}

sss.api<-dim(0)
missing.ids<-dim(0)
##for every sss
for (j in 1:nrow(sss)) {
  ##query the api for element info
  
  ##if the api throws an error because it cannot find the id
  if (is.na(api_search(id = as.character(sss$Element.Global.ID[j])))) {
    ##add the id to the list of missing ids
    missing.ids<-c(missing.ids, sss$Element.Global.ID[j])
  } else {
    ##otherwise extract the desired fields
    list.temp<-api_search(id = as.character(sss$Element.Global.ID[j]))
    
    ##create a new dataframe from relevant data relevant data
    data.temp <- data.frame(
      Element.Global.ID = sss$Element.Global.ID[j], 
      roundedGRank = list.temp$roundedGRank, 
      grankReviewDate = ifelse(is.null(list.temp$grankReviewDate), NA, list.temp$grankReviewDate), 
      USESA = ifelse(is.null(list.temp$speciesGlobal$usesa$usesaCode), NA, list.temp$speciesGlobal$usesa$usesaCode)
      )
    
    ##add to output
    sss.api<-rbind(sss.api, data.temp)
  }
}

##get the number of states for each sss
sss.states<-sss[,c(1, 12:25)] %>% gather(key = "state", value = "value", 2:15) %>% mutate(state=str_sub(state, start = 1, end = 2)) %>% subset(value!="-") %>% group_by(Element.Global.ID) %>% summarise(states = paste0(state, collapse = ","), n.states = n()) %>% data.frame()
head(sss.states); dim(sss.states)

##join api info to sss list
sss2<-left_join(sss[,1:11], sss.api)
##add number of states
sss2<-left_join(sss2, sss.states)

##prioritize species for modeling
##read in spp that were modeled in years 1 and 2
##read Year 1 BLM species list
yr1<-read_excel("C:/Users/max_tarjan/NatureServe/BLM - BLM SSS Distributions and Rankings Project-FY21/Provided to BLM/Year1-models-delivered_20211005.xlsx", sheet = "Sheet1", col_names = T, skip = 0) %>% data.frame()
yr2 <- read_excel("C:/Users/max_tarjan/NatureServe/BLM - BLM SSS Distributions and Rankings Project-FY21/Task 3-0 - Distributions/BLM_yr2_modeling_target_species_list_20220404.xlsx", sheet = "Year2 Models Delivered") %>% data.frame()

exclude.list<-c("Sierra Nevada Bighorn Sheep", "Gunnison Sage-Grouse", "Utah Prairie Dog", "Chinook Salmon - Oregon Coast Spring Run", "Polar Bear", "Steelhead - Central Valley", "Black-footed Ferret", "Jaguar", "Coho Salmon - Southern Oregon/northern California Coast", "Coho Salmon - Oregon Coast", "Chinook Salmon - Snake River Fall Run")

yr3.spp <- subset(sss2,
                  !(Element.Global.ID %in% yr2$Element.Global.ID)
                  &!(Scientific.Name %in% yr1$scientific_name)
                  & USESA !="DL"
                  & !is.na(USESA)
                  & roundedGRank %in% c("G1", "G2", "G3", "T1", "T2", "T3")
                  & as.numeric(Occurrences.on.BLM.Lands..West....Total.Occurrences.Rangewide) >= 0.25
                  & Total.Occurrences.Rangewide >= 3
                  & (n.states>1 | Taxonomic.Group != "Plant")
                  & !(Common.Name %in% exclude.list))
dim(yr3.spp)
table(yr3.spp$Taxonomic.Group)
subset(yr3.spp, select = c(Taxonomic.Group, Common.Name, USESA))
write.csv(yr3.spp, "output/BLMSSS-yr3-modeling-targets-species-list-20221007.csv", row.names = F)

##Prioritize Species for Conservation Status Assessments
##year 3 rank review
##list of species to include from Bruce
include.list<-c(102241, 105276, 104046, 115612, 116379, 107133)

yr3.rr<- subset(sss2,
                Element.Global.ID %in% include.list |
                (USESA !="DL"
                & !is.na(USESA)
                & roundedGRank %in% c("G1", "G2", "G3")#,"T1", "T2", "T3")
                & as.numeric(Occurrences.on.BLM.Lands..West....Total.Occurrences.Rangewide) >= 0.2
                & lubridate::year(as.Date(grankReviewDate, format = "%Y-%m-%d")) < 2012)) %>% arrange(grankReviewDate)
yr3.rr$zoo<-NA
yr3.rr$zoo[which(yr3.rr$Element.Global.ID %in% include.list)]<-T
dim(yr3.rr)
table(yr3.rr$Taxonomic.Group)
subset(yr3.rr, select = c(Element.Global.ID, Taxonomic.Group, Scientific.Name, Common.Name, USESA, roundedGRank, grankReviewDate, zoo))

##Write out list of species for conservation status assessments in year 3
write.csv(yr3.rr, "output/BLMSSS-yr3-rank-review-list-20221007.csv", row.names=F, na = "")
