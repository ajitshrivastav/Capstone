library(dplyr)
library(stringr)

hispanic_female_names_ds <- read.csv("data_sets/input/misc/Hispanic-Female-Names.csv")
hispanic_male_names_ds <- read.csv("data_sets/input/misc/Hispanic-Male-Names.csv")

hispanic_names <- rbind(hispanic_female_names_ds, hispanic_male_names_ds)
names(hispanic_names)[1] <-paste("last_name")
names(hispanic_names)[2] <-paste("first_name")
hispanic_names <- hispanic_names %>% mutate(first_name=trimws(first_name))
hispanic_names <- hispanic_names %>% mutate(last_name=trimws(last_name))
hispanic_names <- hispanic_names %>% mutate(first_name=word(first_name,1))
hispanic_names <- hispanic_names %>% mutate(race="hispanic")

write.csv(hispanic_names, file = "data_sets/input/misc/hispanic_names.csv")
