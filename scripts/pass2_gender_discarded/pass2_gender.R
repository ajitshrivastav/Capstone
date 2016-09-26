library(dplyr)
library(stringr)
library(hashmap)

# load various data sets to evaluate gender
indian_gender_ds <- read.csv("data_sets/input/misc/indian_names.csv")
hispanic_gender_ds <- read.csv("data_sets/input/misc/hispanic_names.csv")
white_gender_ds <- read.csv("data_sets/input/misc/white_names.csv")
black_gender_ds <- read.csv("data_sets/input/misc/black_names.csv")

# function to lookup gender from the data loaded (hashmap)
findgender_local <- function(first_name, gender_in, gender_map){
  gender <- c()
  for(idx in 1:length(first_name)){
    nm <- first_name[idx]
    gn <- gender_in[idx]
    
    # skip, if gender is evaluated already
    if(!is.na(gn)){
      if(gn == "male" || gn == "female"){
        gender[idx] <- as.character(gn)
        next
      }
    }
    
    # set NA if length is < 2
    if(is.null(nm) || nchar(as.character(nm)) < 2){
      gender[idx] <- as.character(gn)
      next
    }
    
    # lookup gender in the map
    gn <- gender_map$find(nm)
    gender[idx] <- gn
    remove('gn')
  }
  # return gender vector
  gender
}

name_vector <- c()
gender_vector <- c()

# load indian gender data in name/gender vectors
for(i in 1:nrow(indian_gender_ds)) {
  row <- indian_gender_ds[i,]
  first_name <- trimws(row$first_name)
  gender <- trimws(row$gender)
  if(length(first_name) <= 0){
    print(paste("Invalid length *** ",first_name))
    next
  }
  name_vector <- append(name_vector, as.character(first_name))
  gender_vector <- append(gender_vector,  as.character(gender))
}

# load hispanic gender data in name/gender vectors
for(i in 1:nrow(hispanic_gender_ds)) {
  row <- hispanic_gender_ds[i,]
  first_name <- trimws(row$first_name)
  gender <- trimws(row$gender)
  if(length(first_name) <= 0){
    print(paste("Invalid length *** ",first_name))
    next
  }
  name_vector <- append(name_vector, as.character(first_name))
  gender_vector <- append(gender_vector,  as.character(gender))
}

# load white gender data in name/gender vectors
for(i in 1:nrow(white_gender_ds)) {
  row <- white_gender_ds[i,]
  first_name <- trimws(row$first_name)
  gender <- trimws(row$gender)
  if(length(first_name) <= 0){
    print(paste("Invalid length *** ",first_name))
    next
  }
  name_vector <- append(name_vector, as.character(first_name))
  gender_vector <- append(gender_vector,  as.character(gender))
}

# load black gender data in name/gender vectors
for(i in 1:nrow(black_gender_ds)) {
  row <- black_gender_ds[i,]
  first_name <- trimws(row$first_name)
  gender <- trimws(row$gender)
  if(length(first_name) <= 0){
    print(paste("Invalid length *** ",first_name))
    next
  }
  name_vector <- append(name_vector, as.character(first_name))
  gender_vector <- append(gender_vector,  as.character(gender))
}

# initialize hashmap with name/gender vector
gender_map <- hashmap(name_vector,gender_vector)
salary <- read.csv("/data_sets/intermediate/pass1_gender_salary.csv")
salary <- salary %>% mutate(gender=findgender_local(first_name, gender, gender_map))
write.csv(salary, file = "data_sets/intermediate/tmp_gender_salary.csv")

