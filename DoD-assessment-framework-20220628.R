##New prioritization for dod assessment framework and species list for year 3

library(readxl)
library(tidyverse)
library(stringr)
library(dplyr)

data<-read_excel("Data/NatureServe_Assessment_Framework_Year_1_deliverable_Sep_2021.xlsx", sheet = NULL) %>% data.frame()

data$Installations_NH_Count<-subset(data, select = c(Installations_NatureServe, Installations_HerpMapper)) %>%
  apply(MARGIN = 1, FUN = function(x) paste(x[!is.na(x)], collapse = ", ")) %>%
  str_split(pattern = ", ") %>% 
  lapply(unique) %>%
  lapply(function(x){ifelse(nchar(x)==0, 0, length(x))[1]}) %>%
  unlist()
data$Installations_NH_Count[which(!is.na(data$Installation_Count_Obscured_Bases))] <- data$Installations_NH_Count[which(!is.na(data$Installation_Count_Obscured_Bases))] + data$Installation_Count_Obscured_Bases[which(!is.na(data$Installation_Count_Obscured_Bases))] ##add the number of obscured bases
#subset(data, select= c(Installations_NatureServe, Installations_HerpMapper, Installations_NH_Count)) %>% head()

table(data$Priority_NatureServe, data$Priority_DoD_Legacy)

data$new.priority<-NA

##assign priorities using ruleset
data$new.priority[which(
  data$Installations_NH_Count > 1 ##found on more than 1 installation
  & (!is.na(data$USESA_Status) | !is.na(data$FWS_5.yr_Work_Plan) | !is.na(data$DoD_Current_Priority)) ##USESSA listed, FWS 5-yr plan, or existing TER-S
  #& (data$Percent_MoBI_overlap >= .25 | data$Percent_EOs_Overlapping >= .25)
  )] <- "I"

data$new.priority[which(
  is.na(data$new.priority) ##not priority 1 to x-1
  & data$Installations_NH_Count > 0
  & (!is.na(data$USESA_Status) | !is.na(data$FWS_5.yr_Work_Plan) | !is.na(data$DoD_Current_Priority)) ##USESSA listed, FWS 5-yr plan, or existing TER-S
)] <- "II"

data$new.priority[which(
  is.na(data$new.priority) ##not priority 1 to x-1
  & data$Installation_Count_All > 0 ##at least 1 installation with mobi prediction or observation
  & (!is.na(data$USESA_Status) | !is.na(data$FWS_5.yr_Work_Plan) | !is.na(data$DoD_Current_Priority)) ##USESSA listed, FWS 5-yr plan, or existing TER-S
)] <- "III"

data$new.priority[which(
  is.na(data$new.priority) ##not priority 1 to x-1
  & data$Installation_Count_All > 0 ##at least 1 installation with mobi prediction or observation
  & (data$Percent_MoBI_overlap >= .1 | data$Percent_EOs_Overlapping >= .1)
  & !is.na(data$Include_Reason)
)] <- "IV"

data$new.priority[which(
  is.na(data$new.priority) ##not priority 1 to x-1
  & data$Installation_Count_All > 0 ##at least 1 installation with mobi prediction or observation
  & !is.na(data$Include_Reason)
)] <- "V"

table(data$new.priority, data$Priority_NatureServe) ##check whether the new priority ranking matches what bruce came up with
table(data$new.priority)

##create list of species for modeling in year 3
##add which species have been modeled in previous years
mod.spp<-read_excel("G:/tarjan/Model_Review_Management/Data/Model_Species_FY22.xlsx") %>% data.frame()
dod.yr1.spp<-read_excel("G:/tarjan/Model_Review_Management/Data/DoD-species-20211213.xlsx") %>% data.frame()
##NS priority 1 and 2; DoD priority 1; plus specific list from dod
dod.request<-c("Eastern Indigo Snake", "Monarch", "Tricolored Bat", "Northern Long-eared Bat", "Little Brown Myotis", "Pinyon Jay", "Spotted Turtle", "Gopher Frog", "Palos Verdes Blue")

dod.yr3.spp <- subset(data, 
       (Common_Name %in% dod.request 
              | (data$Priority_NatureServe %in% c("I", "II") & data$Priority_DoD_Legacy == "I")) 
       & !(Scientific_Name %in% c(dod.yr1.spp$Scientific.name, subset(mod.spp, Project =="DOD")$Scientific.Name))
       ) %>% arrange(Priority_NatureServe, Global_Rank)
dod.yr3.spp$Requested<-NA
dod.yr3.spp$Requested[which(dod.yr3.spp$Common_Name %in% dod.request)] <- T
write.csv(dod.yr3.spp, "Output/DoD-model-species-FY23-20220629.csv", row.names=F)
