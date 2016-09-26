library(dplyr)
library(stringr)

indian_female_names_ds <- read.csv("data_sets/input/misc/Indian-Female-Names.csv")
indian_male_names_ds <- read.csv("data_sets/input/misc/Indian-Male-Names.csv")

indian_names <- rbind(indian_female_names_ds, indian_male_names_ds)

indian_names <- indian_names %>% mutate(name=trimws(name))
indian_names <- indian_names %>% mutate(name=tolower(name))
indian_names <- indian_names %>% mutate(name=gsub("[ ]+(^shri|^smt|^smt.)","",name))
indian_names <- indian_names %>% mutate(first_name=word(name,1))
indian_names <- indian_names %>% mutate(last_name=substr(name, unlist(gregexpr(pattern ='[ ]+[a-z]*[\']{0,1}[\\-]{0,1}[a-z]*$',name))+1, nchar(name)))
indian_names <- indian_names %>% select(first_name, last_name, gender, race)
indian_names <- indian_names %>% mutate(first_name=trimws(first_name))
indian_names <- indian_names %>% mutate(last_name=trimws(last_name))

write.csv(indian_names, file = "data_sets/input/misc/indian_names.csv")
