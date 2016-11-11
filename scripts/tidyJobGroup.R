library(dplyr)
library(tidyr)

salary <- read.csv("data_sets/intermediate/salary.csv")
salary$BasePay <- as.integer(as.numeric(salary$BasePay)/1000)
salary$OtherPay <- as.integer(as.numeric(salary$OtherPay)/1000)
salary$OvertimePay <- as.integer(as.numeric(salary$OvertimePay)/1000)
salary$TotalPay <- as.integer(as.numeric(salary$TotalPay)/1000)
salary$TotalPayBenefits <- as.integer(as.numeric(salary$TotalPayBenefits)/1000)
salary <- salary  %>% mutate (basePayToBase50 = paste(as.character(as.integer(BasePay/20)*20), as.character(as.integer(BasePay/20)*20 + 20),sep="-"))

salary <- salary %>%
  mutate(yesno = 1) %>%
  distinct %>%
  spread(JOB_GROUP, yesno, fill = 0)

write.csv(salary, file = "data_sets/intermediate/salary.csv")
