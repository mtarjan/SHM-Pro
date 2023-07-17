## Query SHM database
library(tidyverse)
library(googlesheets4)

models <- googlesheets4::read_sheet(ss = "https://docs.google.com/spreadsheets/d/1KRpadqcG-OWdknG0rVwbppxp1RRHYpHM_tqR9QyUyLI/edit#gid=2032498813&fvid=699721595", sheet = "model_status_table")

##overwrite with working sheet
models <- googlesheets4::read_sheet(ss = "https://docs.google.com/spreadsheets/d/1KRpadqcG-OWdknG0rVwbppxp1RRHYpHM_tqR9QyUyLI/edit#gid=2032498813&fvid=699721595", sheet = "model_status_table_working") ##working models sheet

mort <- googlesheets4::read_sheet(ss = "https://docs.google.com/spreadsheets/d/186lF05P5jPVookkesbfntC91FIJ5YNdzMgkP0FTdNPU/edit#gid=0", sheet = "Sheet1")

models %>% filter(project1 == "BLM0086" & year_project1 == "three") %>% select(common_name, model_status) %>% data.frame()

models %>% filter(project1 == "DOD0R004" & year_project1 == "three" & grepl(pattern = "MS", x = subn_eo_included)) %>% select(common_name, model_status, subn_eo_included) %>% data.frame

models %>% filter(project1 == "DOD0R004" & year_project1 == "three") %>% select(common_name, model_status) %>% data.frame() %>% arrange(model_status)

models %>% filter(project1 == "DOD0R004" & year_project1 == "three") %>% select(model_status) %>% table()

models %>% filter((project1 == "USW0R035" | project2 == "USW0R035") & year_project1 == "NWRS") %>% select(model_status) %>% table()

##search by species name
models %>% filter(grepl(x = scientific_name, pattern = "Hymenocallis henryae") & project1 != "MOBI") %>% select(common_name, project1, year_project1, model_status) %>% data.frame()

##how many models created in 2022
models %>% filter(grepl(x = model_version...3, pattern = "_2022")) %>% select(model_status, author_organization) %>% table()

##how many reviews in 2022
mort %>% filter(grepl(x = timestamp, pattern = "2022-")) %>% dim()

##models published
models %>% select(model_status, scientific_name) %>% unique() %>% select(model_status) %>% table()

##blm mobi model uploads
models %>% filter(year_project1 == "[BLMuploads]") %>% select(model_status) %>% table()

##models delivered to fws se
models.fwsse <- models %>% filter((project1 == "USW0R035" | project2 == "USW0R035")) %>% select(element_global_id, scientific_name, common_name) %>% data.frame() %>% unique() %>% mutate(element_global_id = unlist(element_global_id))
#write.csv(models.fwsse, "Models_delivered_FWS_SE_20230717.csv", row.names = F)
