library(dplyr)
library(stringr)
library('RCurl')
library('rjson')

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
    url_string <- paste("http://api.namsor.com/onomastics/api/json/gendre/:",first_name_in[idx],"/:",first_name_in[idx],"/:countryIso2",sep="")
    print(paste("URL getting invoked -",url_string))
    person_info = getURL(url_string)
    print(paste("Response -",person_info))
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

name_csv <- read.csv("data_sets/intermediate/salary.csv")
name_csv <- name_csv %>% mutate(gender=findgender_web(nm_v, gender,1, 4000))
write.csv(name_csv, file = "data_sets/intermediate/salary.csv")

