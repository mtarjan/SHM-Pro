##check states with EOs for list of species for permissions
library(readxl)
library(tidyverse)
library(stringr)

##model database
models <- read_excel("Data/Network_SHM_database_design_20220415.xlsx", sheet = "Model_Status")

##list of species for permissions in FY22
spp <- read.csv("Data/FY22-model-species.csv")[,1:3]

##taxonomic groups of spp
taxon<- read_excel("Data/Network_SHM_database_design_20220415.xlsx", sheet = "Taxon table")

models <- left_join(subset(models, select = -taxonomic_group), subset(taxon, select = c(element_global_id, taxonomic_group)))

##add permissions rules
subn.consent<-c("AL", "AR", "AZ", "CA", "CO", "CT", "FL","GA", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MT", "NC", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "SC", "SD", "TN", "UT", "VA", "VT", "WI", "WV", "WY")
subn.decline<-c("ID", "MS", "NE", "NN", "TV")

##see which subnations have not given permission
models$need.permission<-NA
for (j in 1:nrow(models)) {
  
  ##if the species is not in the list of species that have undergone permissions requests then move to the next row; we can't evaluate the permissions for that species
  if (!(models$element_global_id[j] %in% spp$ELEMENT_GLOBAL_ID)) {next}
  
  ##get the list of subnations used for modeling
  subn.eo.included<-models$subn_EO_included[j] %>% str_split(pattern = " ") %>% unlist()
  
  ##if the species occurs in WA and is a plant, add WA to consent list
  subn.consent.temp<-subn.consent
  if("WA" %in% subn.eo.included & models$taxonomic_group[j] %in% c("plant", "Plant")) {
    subn.consent.temp<-c(subn.consent, "WA")
  }
  
  ##find the subnations for which we don't have consent
  need.permission<-setdiff(x = subn.eo.included, y = subn.consent.temp)
  
  ##add subnations for which permissions are needed to spp dataframe
  models$need.permission[j]<- ifelse(length(need.permission)==0, "None", need.permission)
}

models$Permission_received[which(models$need.permission=="None")] <- T

subset(models, select=c("subn_EO_included", "need.permission", "Permission_received"), !is.na(subn_EO_included)) %>% data.frame()


##summarize responses by species
write.csv(models, "output/model-permissions-by-species-FY22-20220824.csv", row.names = F)
