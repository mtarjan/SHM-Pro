##check states with EOs for list of species for permissions
library(readxl)
library(tidyverse)
library(stringr)

##model database
models <- read_excel("Data/Network_SHM_database_design_20220415.xlsx", sheet = "Model_Status")

##list of species for permissions in FY22
spp <- read.csv("Data/FY22-model-species.csv")[,1:3]

##EOs available by states and species
eos <- read_xlsx("Data/Biotics_EO_Summary.xlsx", sheet="EO_Summary_202207")

##get list of subnations with EOs for each species
spp.subn <- subset(eos, ELEMENT_GLOBAL_ID %in% spp$ELEMENT_GLOBAL_ID & NUM_EOS > 0, select = c(ELEMENT_GLOBAL_ID, SUBNATION_CODE)) %>% group_by(ELEMENT_GLOBAL_ID) %>% mutate(subn_eo_biotics=paste0(SUBNATION_CODE, collapse = ",")) %>% subset(select = -SUBNATION_CODE) %>% data.frame() %>% unique()
head(spp.subn)

##add taxonomic group to models
models <- left_join(subset(models, select = c(model_version, created_for_project, element_global_id, scientific_name, subn_EO_included)), unique(subset(eos, select = c(NAME_CAT_DESC, ELEMENT_GLOBAL_ID))), by = c("element_global_id" = "ELEMENT_GLOBAL_ID")) %>% data.frame()

##add number of eos per subnation available; increases number of rows
models2 <- left_join(subset(models, select = c(model_version, created_for_project, element_global_id, scientific_name)), subset(eos, select = c(NAME_CAT_DESC, GNAME, ELEMENT_GLOBAL_ID, SUBNATION_CODE, NUM_EOS, NUM_CURRENT_EOS)), by = c("element_global_id" = "ELEMENT_GLOBAL_ID")) %>% data.frame()

##check that there are are matches for all species
#subset(models2, is.na(GNAME), select = c(scientific_name, created_for_project))

##add permissions rules
subn.consent<-c("AL", "AR", "AZ", "CA", "CO", "CT", "FL","GA", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MT", "NC", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "SC", "SD", "TN", "UT", "VA", "VT", "WI", "WV", "WY", "TV", "ID", "NE", "ND", "TX")
##Add other subnations that we don't have data for so consent is no applicable and set to yes by default
subn.consent<-c(subn.consent, "AK", "NN", "SK", "BC", "NT")
subn.decline<-c("MS", "NN")

##see which subnations have not given permission
models$need.permission<-NA
models$notes<-NA
for (j in 1:nrow(models)) {
  
  ##if the species is not in the list of species that have undergone permissions requests then move to the next row; we can't evaluate the permissions for that species
  if (!(models$element_global_id[j] %in% spp$ELEMENT_GLOBAL_ID) & !(models$scientific_name[j] %in% spp$Scientific.Name)) {
    models$notes[j]<-"species not in permissions request"; next}
  
  ##get the list of subnations used for modeling
  subn.eo.included<-models$subn_EO_included[j] %>% gsub(pattern = " ", replacement = "") %>% str_split(pattern = ",") %>% unlist()
  
  ##if the species occurs in WA and is a plant, add WA to consent list
  subn.consent.temp<-subn.consent
  if("WA" %in% subn.eo.included & models$NAME_CAT_DESC[j] %in% c("Vascular Plant", "Nonvascular Plant")) {
    subn.consent.temp<-c(subn.consent, "WA")
  }
  
  ##add AB and ON to consent because NA
  subn.consent.temp<-c(subn.consent.temp, "AB", "ON")
  
  ##find the subnations for which we don't have consent
  need.permission<-setdiff(x = subn.eo.included, y = subn.consent.temp)
  
  ##add subnations for which permissions are needed to spp dataframe
  models$need.permission[j]<- ifelse(length(need.permission)==0, "None", need.permission)
}

models$Permission_received<-NA
models$Permission_received[which(models$need.permission=="None")] <- T
models$Permission_received[which(models$created_for_project %in% c("SYN_FWS", "BLMSSS_YR1", "DOD_YR1", "DOD_YR1,FWS_SE", "CABLM"))] <- T
models$Permission_received[which(models$notes=="species not in permissions request")] <- T ##assume they were part of a previous round of permissions; assumption will not hold true as more species get added. will need to change this to FWS_SE project

subset(models, select=c("subn_EO_included", "need.permission", "Permission_received"), is.na(Permission_received)) %>% data.frame() %>% head()

models.out <- subset(models, select = c(model_version,	created_for_project,	element_global_id,	scientific_name,	subn_EO_included,	NAME_CAT_DESC,	need.permission, Permission_received, notes
))

##summarize responses by species
write.csv(models.out, "output/model-permissions-by-species-FY22-20220825.csv", row.names = F, na = "")
