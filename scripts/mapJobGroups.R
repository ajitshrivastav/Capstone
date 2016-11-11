library(dplyr)
library(stringr)
library(hashmap)

# load job_title_code_mapping.csv
j_t_c <- read.csv("data_sets/input/misc/job_title_code_mapping.csv")

# load job_groups.csv
salary <- read.csv("data_sets/intermediate/salary.csv")
salary <- salary  %>% mutate(JobTitle=toupper(JobTitle))
# job_code_vector
job_title_vector <- c()
job_group_vector <- c()
service_group_vector <- c()

for(i in 1:nrow(j_t_c)) {
  row <- j_t_c[i,]
  job_title_vector <- append(job_title_vector, as.character(row$JOB_TITLE))
  job_group_vector <- append(job_group_vector, as.character(row$JOB_GROUP))
  service_group_vector <- append(service_group_vector, as.character(row$SERVICE_GROUP))
}

jt_j_group_map <- hashmap(job_title_vector, job_group_vector)
jt_s_group_map <- hashmap(job_title_vector, service_group_vector)


# function to get job group / service group
# floor the job id to nearest 100
# get the ref from job groups, populate job group / service group

evaluate_jg_sg <- function(jt_in){
  jg_v <- c()
  sg_v <- c()
  for(idx in 1:length(jt_in)){
    jt <- as.character(jt_in[idx])
    jt <- toupper(jt)
    if(grepl("(FIRE DEPARTMENT)", jt)){
      jg_v[idx] <- "Fire Fighter"
      sg_v[idx] <- "FIRE, POLICE"
    } else if(grepl("(POLICE DEPARTMENT)", jt)){
      jg_v[idx] <- "Police Officer"
      sg_v[idx] <- "FIRE, POLICE"
    } else {
      jg_v[idx] <- jt_j_group_map$find(jt)
      sg_v[idx] <- jt_s_group_map$find(jt)
      
      # in case, not found, try converting numerals to roman numerals and vice versa and re-search
      if(is.na(jg_v[idx])){
        if(grepl("[ ]+1$", jt)){
          jt <- gsub("[ ]+1$", " I", jt)
        } else if(grepl("[ ]+2$", jt)){
          jt <- gsub("[ ]+2$", " II", jt)
        } else if(grepl("[ ]+3$", jt)){
          jt <- gsub("[ ]+3$", " III", jt)
        } else if(grepl("[ ]+III$", jt)){
          jt <- gsub("[ ]+III$", " 3", jt)
        } else if(grepl("[ ]+II$", jt)){
          jt <- gsub("[ ]+II$", " 2", jt)
        } else if(grepl("[ ]+I$", jt)){
          jt <- gsub("[ ]+I$", " 1", jt)
        } else if(grepl("[ ]+1\"$", jt)){
          jt <- gsub("[ ]+1\"$", " I\"", jt)
        } else if(grepl("[ ]+2\"$", jt)){
          jt <- gsub("[ ]+2\"$", " II\"", jt)
        } else if(grepl("[ ]+3\"$", jt)){
          jt <- gsub("[ ]+3\"$", " III\"", jt)
        } else if(grepl("[ ]+III\"$", jt)){
          jt <- gsub("[ ]+III\"$", " 3\"", jt)
        } else if(grepl("[ ]+II\"$", jt)){
          jt <- gsub("[ ]+II\"$", " 2\"", jt)
        } else if(grepl("[ ]+I\"$", jt)){
          jt <- gsub("[ ]+I\"$", " 1\"", jt)
        }
        
        jg_v[idx] <- jt_j_group_map$find(jt)
        sg_v[idx] <- jt_s_group_map$find(jt)
      }
    }
    
  }
  ret_df <- data.frame( "JOB_GROUP" = jg_v, "SERVICE_GROUP" = sg_v)
  ret_df
}


j_df <- evaluate_jg_sg(salary$JobTitle)
#j_df
# write back job_title_code_mapping
salary <- salary  %>% mutate(JOB_GROUP=j_df$JOB_GROUP)
salary <- salary  %>% mutate(SERVICE_GROUP=j_df$SERVICE_GROUP)
write.csv(salary, file = "data_sets/intermediate/salary.csv")

