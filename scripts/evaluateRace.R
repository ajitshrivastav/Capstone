library(dplyr)
library(tidyr)
library(hashmap)

# load csv data files
asian_data_set <- read.csv("data_sets/input/misc/monga_bay/asian.csv")
white_data_set <- read.csv("data_sets/input/misc/monga_bay/white.csv")
native_americans_data_set <- read.csv("data_sets/input/misc/monga_bay/native_americans.csv")
hispanic_data_set <- read.csv("data_sets/input/misc/monga_bay/hispanic.csv")
black_data_set <- read.csv("data_sets/input/misc/monga_bay/black.csv")

# load the name, rank_percent as vector for Asians
asian_last_name_vector <- c()
asian_rank_percent_vector <- c()
for(i in 1:nrow(asian_data_set)) {
  row <- asian_data_set[i,]
  asian_last_name_vector <- append(asian_last_name_vector, as.character(row$name))
  asian_rank_percent_vector <- append(asian_rank_percent_vector,  as.character(row$rank_percent))
}
# Initialize hashmap with the vectors (<last_name>, <rank_percent>) for Asians
asian_last_name_map <- hashmap(asian_last_name_vector,asian_rank_percent_vector)

# load the name, rank_percent as vector for Whites
white_last_name_vector <- c()
white_rank_percent_vector <- c()
for(i in 1:nrow(white_data_set)) {
  row <- white_data_set[i,]
  white_last_name_vector <- append(white_last_name_vector, as.character(row$name))
  white_rank_percent_vector <- append(white_rank_percent_vector,  as.character(row$rank_percent))
}
# Initialize hashmap with the vectors (<last_name>, <rank_percent>) for Whites
white_last_name_map <- hashmap(white_last_name_vector,white_rank_percent_vector)

# load the name, rank_percent as vector for NativeAmericans
native_americans_last_name_vector <- c()
native_americans_rank_percent_vector <- c()
for(i in 1:nrow(native_americans_data_set)) {
  row <- native_americans_data_set[i,]
  native_americans_last_name_vector <- append(native_americans_last_name_vector, as.character(row$name))
  native_americans_rank_percent_vector <- append(native_americans_rank_percent_vector,  as.character(row$rank_percent))
}
# Initialize hashmap with the vectors (<last_name>, <rank_percent>) for NativeAmericans
native_americans_last_name_map <- hashmap(native_americans_last_name_vector,native_americans_rank_percent_vector)

# load the name, rank_percent as vector for Hspanics
hispanic_last_name_vector <- c()
hispanic_rank_percent_vector <- c()
for(i in 1:nrow(hispanic_data_set)) {
  row <- hispanic_data_set[i,]
  hispanic_last_name_vector <- append(hispanic_last_name_vector, as.character(row$name))
  hispanic_rank_percent_vector <- append(hispanic_rank_percent_vector,  as.character(row$rank_percent))
}
# Initialize hashmap with the vectors (<last_name>, <rank_percent>) for Hispanics
hispanic_last_name_map <- hashmap(hispanic_last_name_vector,hispanic_rank_percent_vector)

# load the name, rank_percent as vector for AfricanAmerican
black_last_name_vector <- c()
black_rank_percent_vector <- c()
for(i in 1:nrow(black_data_set)) {
  row <- black_data_set[i,]
  black_last_name_vector <- append(black_last_name_vector, as.character(row$name))
  black_rank_percent_vector <- append(black_rank_percent_vector,  as.character(row$rank_percent))
}
# Initialize hashmap with the vectors (<last_name>, <rank_percent>) for AfricanAmerican
black_last_name_map <- hashmap(black_last_name_vector,black_rank_percent_vector)

# method to evaluate race,
# input -> last_name vector
# output -> race vector
find_race_local <- function(last_name){
  race <- c()
  for(idx in 1:length(last_name)){
    ln <- last_name[idx]
    rc <- NA
    # initialize the percent to 0
    rc_percent_curr <- 0
    
    # lookup in Asians data set
    rc_percent <- asian_last_name_map$find(ln)
    if(!is.na(rc_percent) & rc_percent > rc_percent_curr){
      rc <- "Asian"
      rc_percent_curr <- rc_percent
    }
    
    # lookup in Whites data set
    rc_percent <- white_last_name_map$find(ln)
    if(!is.na(rc_percent) & rc_percent > rc_percent_curr){
      rc <- "White"
      rc_percent_curr <- rc_percent
    }
    
    # lookup in NativeAmericans data set
    rc_percent <- native_americans_last_name_map$find(ln)
    if(!is.na(rc_percent) & rc_percent > rc_percent_curr){
      rc <- "Native American"
      rc_percent_curr <- rc_percent
    }
    
    # lookup in Hispanics data set
    rc_percent <- hispanic_last_name_map$find(ln)
    if(!is.na(rc_percent) & rc_percent > rc_percent_curr){
      rc <- "Hispanic"
      rc_percent_curr <- rc_percent
    }
    
    # lookup in AfricanAmerican data set
    rc_percent <- black_last_name_map$find(ln)
    if(!is.na(rc_percent) & rc_percent > rc_percent_curr){
      rc <- "African American"
      rc_percent_curr <- rc_percent
    }
    race[idx] = rc
    print(paste("Race for ", ln, " is ", rc))
    remove('rc')
    
  }
  # return race vector
  race
}

# read input salary file
salary <- read.csv("data_sets/intermediate/salary.csv")

#process race using the data set
salary <- salary %>% mutate(race=find_race_local(last_name))

# write result
write.csv(salary, file = "data_sets/intermediate/salary.csv")

print("complete")