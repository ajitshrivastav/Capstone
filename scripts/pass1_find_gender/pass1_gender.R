library(dplyr)
library(stringr)
library(hashmap)

#data source - https://github.com/OpenGenderTracking/gender-api
us_name_gender_ds <- read.csv("data_sets/input/misc/usprocessed_names.csv")
uk_name_gender_ds <- read.csv("data_sets/input/misc/ukprocessed_names.csv")

# create vector to capture names / gender
name_vector <- c()
gender_vector <- c()

# map name and gender to name/gendor vectors
for(i in 1:nrow(us_name_gender_ds)) {
  row <- us_name_gender_ds[i,]
  name_vector <- append(name_vector, as.character(row$Name))
  gender_vector <- append(gender_vector, as.character(row$prob.gender))
}

for(i in 1:nrow(uk_name_gender_ds)) {
  row <- uk_name_gender_ds[i,]
  name_vector <- append(name_vector, as.character(row$Name))
  gender_vector <- append(gender_vector, as.character(row$prob.gender))
}

# create hashmap for easy gender lookup based on name
gender_map <- hashmap(name_vector, gender_vector)

# method to evaluate gender based on first name
findgender_local <- function(first_name){
  # gender vector t be returned
  gender <- c()
  
  # iterate through name vector
  for(idx in 1:length(first_name)){
    # set gender as NA if first name is not valid
    if(is.null(first_name[idx]) || nchar(first_name[idx]) < 3){
      gender[idx] <- NA
      next
    } else{
      # lookup gender from the hashmap
      gender[idx] <- gender_map$find(first_name[idx])
    }
    # return gender vector
    gender
  }
}

# Load csv file in a data frame
original_salary <- read.csv("data_sets/input/primary/Salaries.csv")
# clean up employee names
# trim spaces
salary <- original_salary %>% mutate(EmployeeName=trimws(EmployeeName))

# remove suffixes such as I, II, JR, SR
salary <- salary %>% mutate(EmployeeName=gsub("[ ]+(I$|II$|III$|IV$|JR|SR)","",EmployeeName))

# standardize to lower case
salary <- salary %>% mutate(EmployeeName=tolower(EmployeeName))

# create new columns for first name and last name
# first name is the first word in the name
#salary <- salary %>% mutate(first_name=substr(EmployeeName, 1, unlist(gregexpr(pattern ="^[a-z]*[\']{0,1}[\\-]{0,1}[a-z]*",EmployeeName))-1))
salary <- salary %>% mutate(first_name=word(EmployeeName,1))

# last name is the last word in the name

salary <- salary %>% mutate(last_name=substr(EmployeeName, unlist(gregexpr(pattern ='[ ]+[a-z]*[\']{0,1}[\\-]{0,1}[a-z]*$',EmployeeName))+1, nchar(EmployeeName)))
#salary$first_name
#salary$last_name
salary <- salary %>% mutate(gender=findgender_local(first_name))
#salary$gender
write.csv(salary, file = "data_sets/intermediate/pass1_gender_salary.csv")


