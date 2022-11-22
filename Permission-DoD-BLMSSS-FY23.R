## Permissions request FY23

library(readxl)
library(tidyverse)

## Read in BLM SSS year 3 and DOD year 3 species lists

blm <- read_excel("C:/Users/max_tarjan/NatureServe/BLM - BLM SSS Distributions and Rankings Project-FY21/Task 3-0 - Distributions/BLMSSS-yr3-modeling-targets-species-list-20221007.xlsx", sheet = "BLMSSS-yr3-modeling-targets-spe")

dod <- read_excel("C:/Users/max_tarjan/NatureServe/DoD - DoD At-Risk Species Distributions - FY21/Activity 3-Modeling/DoD-model-species-all-years-20221012.xlsx", sheet= "Models_planned_year3")

## Check if they were in the permissions request from FY22
perm <- read_excel("Data/Permissions request FY22.xlsx", sheet = "Species List by Project")
perm$permission <- "FY22"

blm <- left_join(blm, unique(subset(perm, select = c(Scientific.Name, permission))))
dod <- left_join(dod, unique(subset(perm, select = c(Scientific.Name, permission))), by = c("Scientific_Name" = "Scientific.Name"))
