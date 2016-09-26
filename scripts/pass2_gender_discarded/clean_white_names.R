library(dplyr)
library(stringr)

white_female_names_ds <- read.csv("data_sets/input/misc/White-Female-Names.csv")
white_male_names_ds <- read.csv("data_sets/input/misc/White-Male-Names.csv")

white_names <- rbind(white_female_names_ds, white_male_names_ds)
names(white_names)[1] <-paste("last_name")
names(white_names)[2] <-paste("first_name")
white_names <- white_names %>% mutate(first_name=trimws(first_name))
white_names <- white_names %>% mutate(last_name=trimws(last_name))
white_names <- white_names %>% mutate(first_name=word(first_name,1))
white_names <- white_names %>% mutate(race="white")

write.csv(white_names, file = "data_sets/input/misc/white_names.csv")
