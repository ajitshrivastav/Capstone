library(dplyr)
library(stringr)

black_female_names_ds <- read.csv("data_sets/input/misc/Black-Female-Names.csv")
black_male_names_ds <- read.csv("data_sets/input/misc/Black-Male-Names.csv")

black_names <- rbind(black_female_names_ds, black_male_names_ds)
names(black_names)[1] <-paste("last_name")
names(black_names)[2] <-paste("first_name")
black_names <- black_names %>% mutate(first_name=trimws(first_name))
black_names <- black_names %>% mutate(last_name=trimws(last_name))
black_names <- black_names %>% mutate(first_name=word(first_name,1))
black_names <- black_names %>% mutate(race="black")

write.csv(black_names, file = "data_sets/input/misc/black_names.csv")

a <- " ajit shrivastav"
b <- word(a,1)
b