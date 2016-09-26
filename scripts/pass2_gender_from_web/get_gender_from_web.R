library(dplyr)
library(stringr)
library('RCurl')
library('rjson')

# method that invokes REST api call (https://api.genderize.io/) to evaluate gender
# API sets a limit of 500 calls per day, hence pass the start index & end index, to invoke REST max 500 times
findgender_web <- function(first_name_in, gender_in, start_index, end_index){
  gender <- c()
  for(idx in 1:length(first_name_in)){
    # if index is not in the call range don't go for REST invocation
    if(idx > end_index | idx < start_index){
      gender[idx] <- as.character(gender_in[idx])
      next
    }
    if(!is.na(gender_in[idx])){
      if(gender_in[idx] == "male" || gender_in[idx] == "female"){
        gender[idx] <- as.character(gender_in[idx])
        next
      }
    }
    
    # invoke REST call to evaluate Gender
    url_string <- paste("https://api.genderize.io/?name=",first_name_in[idx],sep="")
    #url_string <- paste("https://gender-api.com/get?key=cGVLPhBfVJnXhggBgm&name=",first_name[idx],sep="")
    person_info = getURL(url_string)
    if (class(person_info) == "try-error") {
      gender[idx] <- NA
    } else{
      person_info_json <- try(fromJSON(person_info))
      if (class(person_info_json) == "try-error") {
        gender[idx] <- NA
      } else{
        print(paste("json - ", person_info_json))
        if(is.null(person_info_json$gender)){
          gender[idx] <- NA
        } else {
          gender[idx] <- person_info_json$gender
        }
      }
    }
  }
  # return gender vector
  gender
}

name_csv <- read.csv("data_sets/intermediate/nm_wihout_gender.csv")
name_csv <- name_csv %>% mutate(gender=findgender_web(nm_v, gender,1, 499))
write.csv(name_csv, file = "data_sets/intermediate/nm_with_gender.csv")

