library(dplyr)
library(tidyr)
library(data.table)

####### Merges the training and the test sets to create one data set.

# read txt files
# V1 name
# V2 count
# V3 rank
# V4 rank_percent
# V5 us_rank
# V6 us_total_occurences

asian_data_set <- read.table("data_sets/input/misc/monga_bay/asian.txt")
white_data_set <- read.table("data_sets/input/misc/monga_bay/white.txt")
native_americans_data_set <- read.table("data_sets/input/misc/monga_bay/native_americans.txt")
hispanic_data_set <- read.table("data_sets/input/misc/monga_bay/hispanic.txt")
black_data_set <- read.table("data_sets/input/misc/monga_bay/black.txt")

colnames(asian_data_set) <- c("name", "count", "rank", "rank_percent", "us_rank", "us_total_occurences")
asian_data_set <- asian_data_set %>% mutate(name=tolower(name))
write.csv(asian_data_set, file = "data_sets/input/misc/monga_bay/asian.csv")

colnames(white_data_set) <- c("name", "count", "rank", "rank_percent", "us_rank", "us_total_occurences")
white_data_set <- white_data_set %>% mutate(name=tolower(name))
write.csv(white_data_set, file = "data_sets/input/misc/monga_bay/white.csv")

colnames(native_americans_data_set) <- c("name", "count", "rank", "rank_percent", "us_rank", "us_total_occurences")
native_americans_data_set <- native_americans_data_set %>% mutate(name=tolower(name))
write.csv(native_americans_data_set, file = "data_sets/input/misc/monga_bay/native_americans.csv")

colnames(hispanic_data_set) <- c("name", "count", "rank", "rank_percent", "us_rank", "us_total_occurences")
hispanic_data_set <- hispanic_data_set %>% mutate(name=tolower(name))
write.csv(hispanic_data_set, file = "data_sets/input/misc/monga_bay/hispanic.csv")

colnames(black_data_set) <- c("name", "count", "rank", "rank_percent", "us_rank", "us_total_occurences")
black_data_set <- black_data_set %>% mutate(name=tolower(name))
write.csv(black_data_set, file = "data_sets/input/misc/monga_bay/black.csv")

print("complete")