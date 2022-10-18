##select states for which DOD would want to access data on NSXP
##M Tarjan
##Oct 18, 2022

library(readxl)
library(tidyverse)

##read in potential TER-S
data<-read_excel("C:/Users/max_tarjan/NatureServe/DoD - DoD At-Risk Species Distributions - FY21/Activity 1. Assessment Framework/NatureServe_Assessment_Framework_Revised_20221003.xlsx", sheet = "Potential_TERS") %>% data.frame()

##find out in which states they occur using natureserve api
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

data.api<-dim(0)
missing.ids<-dim(0)
##for every species
for (j in 1:nrow(data)) {
  ##query the api for element info
  
  ##if the api throws an error because it cannot find the id
  if (is.na(api_search(id = as.character(data$NatureServe_ID[j]))[[1]])) {
    ##add the id to the list of missing ids
    missing.ids<-c(missing.ids, data$NatureServe_ID[j])
  } else {
    ##otherwise extract the desired fields
    list.temp<-api_search(id = as.character(data$NatureServe_ID[j]))
    
    ##create a new dataframe from relevant data relevant data
    data.temp <- data.frame(
      Element.Global.ID = data$NatureServe_ID[j], 
      subnation = ifelse(is.null(list.temp$elementNationals$elementSubnationals[[1]]$subnation$subnationCode), NA, list.temp$elementNationals$elementSubnationals[[1]]$subnation$subnationCode)
    )
    
    ##add to output
    data.api<-rbind(data.api, data.temp)
  }
}

##select states that appear the most times and calculate what percent of species would result from having data from those states
print(paste0("Potential TER-S occur in ", length(unique(data.api$subnation)), " subnations."))

##table of subnations with number of species occurring in that subnation
api.table<-table(data.api$subnation) %>% data.frame() %>% arrange(desc(Freq))

##remove subnations that are not in the US
api.table<-subset(api.table, Var1 %in% state.abb)

##portion of species for which you want to capture data with the data license
portion.cutoff <- 0.9

for (j in 1:nrow(api.table)) {
  subn.temp<-api.table$Var1[1:j]
  ##species that occur in subnations not included in the current selection
  spp.excluded<-subset(data.api, !(subnation %in% subn.temp))$Element.Global.ID %>% unique()
  ##species that have all subnations in the current selection
  spp.included<-subset(data.api, !(Element.Global.ID %in% spp.excluded))$Element.Global.ID %>% unique()
  ##proportion of species included in this data license
  prop.in<-length(spp.included)/nrow(data)
  if(prop.in > portion.cutoff) {
    print(paste0("Including data for ", j, " subnations gives data for ", round(prop.in, 3), " of potential TER-S."))
    print(subn.temp)
    break}
}

##could also select species based on their DOD priority
spp.temp<-subset(data, Priority_Jul2022 %in% c("I", "II", "III"))$NatureServe_ID
subn.temp<-subset(data.api, Element.Global.ID %in% spp.temp & subnation %in% state.abb)$subnation %>% unique()
##number of subnations required to get all priority 1, 2, 3 species observations
length(subn.temp)

##if those subnations are included in the license, what portion of species data would you get?
##0.95
write.csv(subn.temp, "output/dod-subnations-NSXP-license.csv", row.names = F)
