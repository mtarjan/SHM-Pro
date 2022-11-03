## El Centro data

library(tidyverse)
dat <- read.csv("CNDDB DP Records.csv")
dat$record.id<-1:nrow(dat) ##assign a number to each row so know how the comments were originally grouped
dat$General.original<-dat$General ##save the original comments, since they will be split into rows later

actions<-c("CAPTURED/RELEASED", "CAUGHT/RELEASED", "TRAPPED & RELEASED","CAPTURED", "CAPT", "COLLECTED", "CAUGHT", "TRAPPED", "OBSERVED", "DETECTED", "OBS", "FOUND")
species<-"PUPFISH|NONE|NO|MOSQUITOFISH|MOLLY|JUVENILE|ADULT|JUVS|JUV|GAMBUSIA|CRAYFISH"
separators<-"\\.|&|;|,|AND"
numbers<-"NO|NONE|[0-9][0-9] |[0-9] |[0-9][0-9][0-9] |ONE|TWO|THREE|FOUR|FIVE|SIX|SEVEN|EIGHT|NINE|TEN"

dat2 <- dat %>% 
  separate_rows(General, sep = separators) %>% ##split comments into separate rows based on the separators defined above
  mutate(species = str_extract(General, pattern = species)) %>% ##extract anything matching the patterns in the species object above
  mutate(number = str_extract(General, pattern = numbers)) %>% ##extract anything matching the patterns in the numbers object defined above
  mutate(action = str_extract(General, pattern = paste0(actions, collapse="|"))) %>% ##extract anything matching the patterns in the actions object above
  mutate(date.text = str_remove(General, pattern = paste0(".*", actions, collapse="|"))) %>% ##remove any text that occurs before and includes action words (assume the date info is held here)
  mutate(date.text = str_remove_all(date.text, pattern = "ON |OR |IN THE | OF")) %>% ##remove extra words from the date text
  mutate(date = ifelse(str_detect(date.text, pattern = "[0-9]+/[0-9]+/[0-9]+"), str_extract(date.text, pattern = "[0-9]+/[0-9]+/[0-9]+"), NA)) %>% ##extract anything in the date text that matches the xx/xx/xx date format
  mutate(date = ifelse(str_detect(date.text, pattern = "[0-9]+ [A-Z][A-Z][A-Z] [0-9]+"), format(as.Date(str_extract(date.text, pattern = "[0-9]+ [A-Z][A-Z][A-Z] [0-9]+"), "%d %B %Y"), "%m/%d/%y"), date)) %>% ##extract anything in the date text that matches the xx month year format
  mutate(year = str_trim(str_extract(date.text, pattern = " [0-9][0-9][0-9][0-9]"))) %>% ##extract 4 digit sequences in the date text field
  mutate(month = ifelse(is.na(date), str_extract(date.text, pattern = paste0(toupper(month.abb), collapse="|")), NA)) %>% ##extract any month abbreviations from the date.text field
  mutate(day = ifelse(is.na(date), str_extract(date.text, pattern = "[0-9][0-9] |[0-9] "), NA)) %>% ##extract any x or xx digit sequences from the date.text field
  fill(c("species", "action"), .direction = "down") %>% ##fill blank species and actions from the top down
  mutate(number = ifelse(species %in% c("NO", "NONE"), 0, number)) %>% ##replace number with 0 if the species is equal to NO or NONE
  fill(c("year"), .direction = "up") %>% ##fill empty year fields from the bottom up
  mutate(year = ifelse(is.na(date), year, NA)) %>% ##if there is already a correctly formatted date, then make the year NA
  data.frame #%>% select(c(General.original, General, number, species, action, date.text, date, year, month, day)) #%>% head(20)

write.csv(dat2, "CNDDB-records-parsed-20221101.csv", row.names=F)
