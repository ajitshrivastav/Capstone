library(dplyr)
library(stringr)
library(hashmap)

# load job_title_code_mapping.csv
j_t_c <- read.csv("data_sets/input/misc/job_title_code_mapping_raw.csv")

# load job_groups.csv
j_g <- read.csv("data_sets/input/misc/job_groups.csv")

# job_code_vector
job_code_vector <- c()
job_group_vector <- c()
service_group_vector <- c()

for(i in 1:nrow(j_g)) {
  row <- j_g[i,]
  job_code_vector <- append(job_code_vector, as.numeric(row$JOB_ID_GROUP))
  job_group_vector <- append(job_group_vector, as.character(row$JOB_GROUP))
  service_group_vector <- append(service_group_vector, as.character(row$SERVICE_GROUP))
}

jc_j_group_map <- hashmap(job_code_vector, job_group_vector)
jc_s_group_map <- hashmap(job_code_vector, service_group_vector)


# function to get job group / service group
# floor the job id to nearest 100
# get the ref from job groups, populate job group / service group

evaluate_jg_sg <- function(jc_in, jt_in){
  jg_v <- c()
  sg_v <- c()
  for(idx in 1:length(jc_in)){
    jc <- as.numeric(jc_in[idx])
    jt <- as.character(jt_in[idx])
    if(!is.na(jc) & is.numeric(jc)){
      jg_v[idx] <- jc_j_group_map$find(jc)
      sg_v[idx] <- jc_s_group_map$find(jc)
    } else{
      jg_v[idx] = NA
      sg_v[idx] = NA
    }
  }
  ret_df <- data.frame( "JOB_GROUP" = jg_v, "SERVICE_GROUP" = sg_v)
  ret_df
}
j_t_c <- j_t_c %>% mutate(JOB_CODE = gsub("[A-Z]+", "", JOB_CODE))
j_t_c$JOB_CODE
j_t_c$JOB_CODE <- 100*floor(as.integer(j_t_c$JOB_CODE)/100)
j_t_c$JOB_CODE
j_df <- evaluate_jg_sg(j_t_c$JOB_CODE, j_t_c$JOB_TITLE)

# write back job_title_code_mapping
j_t_c <- j_t_c  %>% mutate(JOB_GROUP=j_df$JOB_GROUP)
j_t_c <- j_t_c  %>% mutate(SERVICE_GROUP=j_df$SERVICE_GROUP)
write.csv(j_t_c, file = "data_sets/input/misc/job_title_code_mapping.csv")

