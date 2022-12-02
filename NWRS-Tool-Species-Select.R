## Define universe of species for the NWRS Tool
## M Tarjan
## Nov 2, 2022

## Load packages
library(readxl)
library(tidyverse)
library(RODBC)

## LOAD DATA
## SGCN species
sgcn <- read.csv("Data/SGCN_matched_CONUS.csv")

## Species with models
models <- read_excel("C:/Users/max_tarjan/NatureServe/Conservation Science - Applied Data Science/Species Habitat Modeling Program/Network-SHM-database.xlsx", sheet="model_status_table") %>% 
  filter(model_status != "planned") %>% 
  select(element_global_id) %>% 
  rbind(subset(read_excel("C:/Users/max_tarjan/NatureServe/Conservation Science - Applied Data Science/Species Habitat Modeling Program/Network-SHM-database.xlsx", sheet="taxon_table"), select = element_global_id)) %>%
  unique()

## G1, G2, G3, T1, T2, T3, ESA listed
## Number of EOs per species
eos <- read_excel("Data/Biotics_EO_Summary.xlsx", sheet= "EO_Summary_202207")

## Get the number of current eos and all relevant subnations
eos.conus <- eos %>% filter(!(SUBNATION_CODE %in% c("AK", "HI")) & NATION == "United States" & NUM_CURRENT_EOS > 0) %>% group_by(ELEMENT_GLOBAL_ID) %>% summarise(NUM_CURRENT_EOS = sum(NUM_CURRENT_EOS), STATES = paste0(SUBNATION_CODE, collapse = ", "))

## Query Biotics for relevant species
## NEED TO FIRST CONNECT TO VPN
con<-odbcConnect("centralbiotics", uid="biotics_report", pwd=rstudioapi::askForPassword("Password")) ##open connection to database

qry <- "SELECT DISTINCT egt.element_global_id, gname.scientific_name, egt.g_primary_common_name, nc.name_category_desc, egt.rounded_g_rank, ESA_TG.D_USESA_ID
FROM  element_global egt
LEFT JOIN scientific_name gname
  ON egt.gname_id = gname.scientific_name_id
LEFT JOIN d_name_category nc
  ON gname.d_name_category_id = nc.d_name_category_id
LEFT JOIN element_global_rank egr
    ON egt.element_global_id = egr.element_global_id
LEFT JOIN taxon_global esa_tg
    ON egt.element_global_id = esa_tg.element_global_id
WHERE
/* criteria that applies to all records - active, regular and confident in US */ 
  egt.inactive_ind = 'N' 
  and egt.element_global_id in ( 
    (SELECT ent.element_global_id 
      FROM element_national ent 
      where ent.nation_id in (225) 
       and ent.element_national_id in  
       (select tnd.element_national_id from taxon_natl_dist tnd 
        where tnd.d_regularity_id = 1 /* Regularly occurring */ and tnd.d_dist_confidence_id = 1 /* confident */)))"

spp <- sqlQuery(con, qry); head(spp) ##import the queried table

# When finished, it's a good idea to close the connection
odbcClose(con)

dat <- left_join(spp, data.frame(ELEMENT_GLOBAL_ID = sgcn$elementGlobalId, sgcn=T))
dat <- left_join(dat, data.frame(ELEMENT_GLOBAL_ID = as.numeric(models$element_global_id), modeled=T))
dat <- left_join(dat, data.frame(ELEMENT_GLOBAL_ID = eos.conus$ELEMENT_GLOBAL_ID, eos=T)) %>% unique()

## Subset element global ids to include those that meet each parameter (candidates)
## G1,g2,g3, t1, t2, t3, esa listed, or sgcn that is tracked by the network for which we have data (eos or models) - how many have eos, how many have models 
## CONUS - no AK or HI
cand <- dat %>%
  ##esa id 39 is delisted
  ##can also include sgcns
  filter(ROUNDED_G_RANK %in% c("G1", "G2", "T1", "T2") | (!is.na(D_USESA_ID) & D_USESA_ID != 39) | sgcn) %>%
  group_by(modeled, eos) %>% summarise(n = n()) %>%
  data.frame()
cand