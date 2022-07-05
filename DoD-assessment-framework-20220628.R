##New prioritization for dod assessment framework and species list for year 3

library(readxl)
library(tidyverse)
library(stringr)
library(dplyr)

data<-read_excel("Data/NatureServe_Assessment_Framework_Year_1_deliverable_Sep_2021.xlsx", sheet = NULL) %>% data.frame()

data$Installations_NH_Count<-ifelse(is.na(data$Installations_NatureServe), 0, 1) + ifelse(is.na(data$Installations_HerpMapper), 0, 1) + ifelse(is.na(str_count(string = data$Installations_NatureServe, pattern =",")), 0, str_count(string = data$Installations_NatureServe, pattern =",")) + ifelse(is.na(data$Installations_HerpMapper), 0, 1) + ifelse(is.na(str_count(string = data$Installations_HerpMapper, pattern =",")), 0, str_count(string = data$Installations_HerpMapper, pattern =",")) ## add up number of installations plus commas (implying another installation)

data$Installations_NH_Count<-subset(data, select = c(Installations_NatureServe, Installations_HerpMapper)) %>%
  apply(MARGIN = 1, FUN = function(x) paste(x[!is.na(x)], collapse = ", ")) %>%
  str_split(pattern = ", ") %>% 
  lapply(unique) %>%
  lapply(function(x){ifelse(nchar(x)==0, 0, length(x))[1]}) %>%
  unlist()
#subset(data, select= c(Installations_NatureServe, Installations_HerpMapper, Installations_NH_Count)) %>% head()

data$new.priority<-NA

##assign priorities
data$new.priority[which(
  (data$Percent_MoBI_overlap >= .25 
   & data$Installation_Count_All > 1) 
  | (data$Percent_EOs_Overlapping >= .25 
     & data$Installation_Count_All > 1)
  )] <- "I"

data$new.priority[which(
  is.na(data$new.priority)
  & data$Installations_NH_Count > 1
)] <- "II"

data$new.priority[which(
  is.na(data$new.priority) 
  & data$Installations_NH_Count == 1
)] <- "III"

table(data$new.priority, data$Priority_NatureServe)

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
