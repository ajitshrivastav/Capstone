library(dplyr)
library(stringr)
# library('RCurl')
# library('rjson')
# 
# #global map to save name -> gender map, don't want to repeat the operations of gender evaluation.
# name_gender_map <- new.env()
# 
# 
# findgender_web <- function(first_name){
#   gender <- c()
#   cat("num of elements ", length(first_name))
#   for(idx in 1:length(first_name)){
#     
#     print(paste("Try getting gender from map for ", first_name[idx]))
#     if(is.null(first_name[idx]) || nchar(first_name[idx]) < 3){
#       gender[idx] <- NA
#       next
#     }
#     g <- try(get(as.character(first_name[idx]), name_gender_map))
#     if (class(g) == "try-error") {
#       #url_string <- paste("https://gender-api.com/get?key=cGVLPhBfVJnXhggBgm&name=",first_name[idx],sep="")
#       url_string <- paste("https://api.genderize.io/?name=",first_name[idx],sep="")
#       person_info = getURL(url_string)
#       if (class(person_info) == "try-error") {
#         gender[idx] <- NA
#       } else{
#         person_info_json <- try(fromJSON(person_info))
#         if (class(person_info_json) == "try-error") {
#           gender[idx] <- NA
#         } else{
#           print(paste("json - ", person_info_json))
#           if(is.null(person_info_json$gender)){
#             gender[idx] <- NA
#           } else {
#             gender[idx] <- person_info_json$gender
#             assign(as.character(first_name[idx]), person_info_json$gender, name_gender_map)
#           }
#         }
#       }
#       
#     } else{
#       gender[idx] = g
#     }
#     remove('g')
#   }
#   gender
# }

name_gender_us_map <- new.env()
name_gender_uk_map <- new.env()

#https://github.com/OpenGenderTracking/gender-api
us_name_gender_ds <- read.csv("/Users/ajitshrivastav/my_Files/study/Data_Science/CapStone/SF_Salary/data_sets/name_data_sets/clean/usprocessed_names.csv")
uk_name_gender_ds <- read.csv("/Users/ajitshrivastav/my_Files/study/Data_Science/CapStone/SF_Salary/data_sets/name_data_sets/clean/ukprocessed_names.csv")

for(i in 1:nrow(us_name_gender_ds)) {
  row <- us_name_gender_ds[i,]
  assign(as.character(tolower(row$Name)), tolower(row$prob.gender), name_gender_us_map)
}

for(i in 1:nrow(uk_name_gender_ds)) {
  row <- uk_name_gender_ds[i,]
  assign(as.character(tolower(row$Name)), tolower(row$prob.gender), name_gender_uk_map)
}

findgender_local <- function(first_name){
    gender <- c()
    cat("num of elements ", length(first_name))
    for(idx in 1:length(first_name)){
      print(paste("Try getting gender from map for ", first_name[idx]))
      if(is.null(first_name[idx]) || nchar(first_name[idx]) < 3){
        gender[idx] <- NA
        next
      } else{
        # try in US name map
        g <- try(get(as.character(first_name[idx]), name_gender_us_map))
        if (class(g) == "try-error") {
          # try in UK name map
          g <- try(get(as.character(first_name[idx]), name_gender_uk_map))
          if (class(g) == "try-error") {
            gender[idx] <- NA
            next
          } else{
            r <- try(gender[idx] <- g)
            if (class(r) == "try-error") {
              gender[idx] <- NA
            }
          }
        } else{
          r <- try(gender[idx] <- g)
          if (class(r) == "try-error") {
            gender[idx] <- NA
          }
        }
      }
      remove('g')
    }
    gender
}

# Load csv file in a data frame
original_salary <- read.csv("/Users/ajitshrivastav/my_Files/study/Data_Science/CapStone/SF_Salary/data_sets/Salaries.csv")
original_salary$EmployeeName
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
write.csv(salary, file = "/Users/ajitshrivastav/my_Files/study/Data_Science/CapStone/SF_Salary/data_sets/pass1_gender_salary.csv")

# load data sets for first name, last name , gender from external sources

# Loop through all the employee names, outer loop

# Inner loop, loop through each data sets,
# match, first name, match last name, if first name and last name match is found, break since it is a best match found
# If either of the first name or last name is matched, then calculate the weightage, last name match has high 
# weight than first name. Based on the match, assign race for the person
# If no match found for both first/last name, assign NA 

# based on the first name match, assign the gender for the person

# Already have a data sets for Blacks/Whites/Indian/Hispanic. Need more data sets for Asian, 

