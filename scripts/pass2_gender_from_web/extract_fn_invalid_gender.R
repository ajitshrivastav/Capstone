library(dplyr)
library(stringr)


# read all the first names where gender is to be evaluated
# make them unique
# store it in a separate file

get_names_with_invalid_gender <- function(first_name, gender_in){
  first_name_v <- c()
  for(idx in 1:length(first_name)){
    if(!is.na(gender_in[idx])){
      if(gender_in[idx] == "male" || gender_in[idx] == "female"){
        # gender valid, so skip
        next
      }
    }
    if(!(first_name[idx] %in% first_name_v)){
      first_name_v <- append(first_name_v, as.character(first_name[idx]))
    }
  }
  first_name_v
}

# Read CSV
salary <- read.csv("data_sets/intermediate/pass1_gender_salary.csv")

# Get all the unique names where local gender evaluation was not successful
nm_v <- get_names_with_invalid_gender(salary$first_name, salary$gender) 

# create a new DF with gender as NA
df <- data.frame(nm_v)
df$gender <- NA

# Save all the names where gender is to be evaluated
write.csv(df, file = "data_sets/intermediate/nm_wihout_gender.csv")

