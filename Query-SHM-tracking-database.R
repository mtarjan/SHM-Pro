## Query SHM database
library(tidyverse)
library(googlesheets4)

models <- googlesheets4::read_sheet(ss = "https://docs.google.com/spreadsheets/d/1KRpadqcG-OWdknG0rVwbppxp1RRHYpHM_tqR9QyUyLI/edit#gid=2032498813&fvid=699721595", sheet = "model_status_table")

mort <- googlesheets4::read_sheet(ss = "https://docs.google.com/spreadsheets/d/186lF05P5jPVookkesbfntC91FIJ5YNdzMgkP0FTdNPU/edit#gid=0", sheet = "Sheet1")

models %>% filter(project1 == "BLM0086" & year_project1 == "three") %>% select(common_name, model_status) %>% data.frame()

##search by species name
models %>% filter(grepl(x = scientific_name, pattern = "Thomomys clusius") & project1 != "MOBI") %>% select(common_name, project1, year_project1, model_status, subn_eo_included) %>% data.frame()

##how many models created in 2022
models %>% filter(grepl(x = model_version...3, pattern = "_2022")) %>% select(model_status, author_organization) %>% table()

##how many reviews in 2022
mort %>% filter(grepl(x = timestamp, pattern = "2022-")) %>% dim()
