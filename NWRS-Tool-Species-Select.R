## Define universe of species for the NWRS Tool
## M Tarjan
## Nov 2, 2022

## Load packages
library(readxl)
library(tidyverse)

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

##G1,g2,g3, t1, t2, t3, esa listed, sgcn that is tracked by the network for which we have data (eos or models) - how many have eos how many have models 

##Gio will share list of sgcn  

##Ones that are "tracked by the network" 

##Which have eos, which have models 

##Conus – no ak, hi 

##For tomorrow – just ignore if they are tracked 

##List of models is all in taxon table and model_status_table (exclude planned) 