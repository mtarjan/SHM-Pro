##summarize data from Cameron on field offices for each model species

library(readxl)
library(tidyverse)

spp<-read_excel("Data/BLM-SSS-Year2-model-species-field-offices-20220208.xlsx", sheet = "Year2Species") %>% data.frame()
##field offices
fo<-read_excel("Data/BLM-SSS-Year2-model-species-field-offices-20220208.xlsx", sheet = "FieldOffices") %>% data.frame()

spp.fo<-fo %>% group_by(Element.Global.ID, Scientific.Name) %>% summarise(Field.Office=str_c(unique(Field.Office), collapse=", ")) %>% data.frame()
spp.fo<-left_join(spp, spp.fo)
write.csv(spp.fo, "BLM-SSS-Year2-model-species-field-offices-concat-20220208.csv", row.names=F)
