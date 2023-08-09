##check whether a list of species has any fuzzing requirements according to the states
##input species list
library(readxl)
library(tidyverse)
species.df <- read.csv("Data/FY22-model-species.csv") ##read in the species to be modeled (for permissions)
# biotics <- read_xlsx("Data/Biotics_EO_Summary.xlsx", sheet="EO_Summary_202207") ##read in fuzzing requirements by species
biotics <- read_xlsx("C:/Users/max_tarjan/OneDrive - NatureServe/MJD & Data Requests Spreadsheet/Biotics_EO_Summary.xlsx", sheet="EO_Summary_202306") ##read in fuzzing requirements by species
species.fuzz <- left_join(species.df, subset(biotics, select = c(GNAME, ELEMENT_GLOBAL_ID, SUBNATION_CODE, NUM_EOS, NUM_DATA_SENS_EOS, EST_DATA_SENS)), by = c("ELEMENT_GLOBAL_ID" = "ELEMENT_GLOBAL_ID"))

##check which species didn't have a match in the EO data
subset(species.fuzz, is.na(EST_DATA_SENS), select = c(Scientific.Name, Project, SUBNATION_CODE, EST_DATA_SENS))
##which species are sensitive for a given state
subset(species.fuzz, EST_DATA_SENS != "No" & SUBNATION_CODE == "MD", select = c(Scientific.Name, Project, SUBNATION_CODE, EST_DATA_SENS))
subset(species.fuzz, EST_DATA_SENS != "No", select = c(Scientific.Name, Project, SUBNATION_CODE, EST_DATA_SENS))
