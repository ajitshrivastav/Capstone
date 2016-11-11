library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(e1071)
library(caret)
library(caTools)
# read in file
salary <- read.csv("data_sets/intermediate/salary.csv")

# plotting

# plot BasePay to TotalPay to review linear relationship (BasePay_TotalPay.pdf)
plot <- ggplot(data=salary, aes(BasePay, TotalPay))+geom_point()
plot

# Plot BasePay total count grouped by race and faceted by year 
plot <- ggplot(data=salary, aes(x=BasePay, group = race, colour = race)) +
  geom_line(stat="count")
plot <- plot + facet_grid(Year ~ .)
plot

# Plot Base Pay to count, faceted with race (BasePay_count_facet_race.pdf)
plot <- ggplot(data=salary, aes(x=factor(basePayToBase20))) +
  geom_bar(stat="count", position=position_dodge()) +
  scale_x_discrete(limits=c("20-40","40-60","60-80","80-100","100-120","120-140","140-160","160-180","180-200","200-220","220-240","240-260","260-280","280-300","300-320"))
plot <- plot + facet_grid(race ~ .)
plot

# Plot Base Pay to count distribution (BasePay_count.pdf)
plot <- ggplot(data=salary, aes(x=factor(basePayToBase20))) +
  geom_bar(stat="count", position=position_dodge()) +
  scale_x_discrete(limits=c("20-40","40-60","60-80","80-100","100-120","120-140","140-160","160-180","180-200","200-220","220-240","240-260","260-280","280-300","300-320"))
plot <- plot + facet_grid(Year ~ .)
plot

# Plot SERVICE_GROUP to count, faceted with race (SERVICE_GROUP_count_facet_race.pdf)
plot <- ggplot(data=salary, aes(x=factor(SERVICE_GROUP))) +
  geom_bar(stat="count", position=position_dodge())
plot <- plot + facet_grid(race ~ .)
plot <- plot + theme(axis.text.x=element_text(angle=50, size=5, vjust=0.5))
plot

# Plot service group to count, gender bar. that will show how gender compares
# across all the service groups (SERVICE_GROUP_count_gender.pdf)
plot <- ggplot(data=salary, aes(x=factor(SERVICE_GROUP), y = mean(BasePay), fill = gender)) +
  geom_bar(stat="count", position=position_dodge())
#plot <- plot + facet_grid(race ~ .)
plot <- plot + theme(axis.text.x=element_text(angle=50, size=5, vjust=0.5))
plot

plot <- ggplot(salary, aes(x=factor(SERVICE_GROUP), y=BasePay, fill = gender)) + stat_summary(fun.y="mean", geom="bar",position=position_dodge())
plot <- plot + theme(axis.text.x=element_text(angle=50, size=5, vjust=0.5))
plot

plot <- ggplot(salary, aes(x=factor(SERVICE_GROUP), y=BasePay, fill = race)) + stat_summary(fun.y="mean", geom="bar",position=position_dodge())
plot <- plot + theme(axis.text.x=element_text(angle=50, size=5, vjust=0.5))
plot

# Plot service group to count, race bar. that will show how race compares
# across all the service groups (SERVICE_GROUP_count_race.pdf)
plot <- ggplot(data=salary, aes(x=factor(SERVICE_GROUP), fill = race)) +
  geom_bar(stat="count", position=position_dodge())
#plot <- plot + facet_grid(race ~ .)
plot <- plot + theme(axis.text.x=element_text(angle=50, size=5, vjust=0.5))
plot


# basePay against gender (+ faceted with year)
plot <- ggplot(data=salary, aes(x=BasePay, group = gender, colour = gender)) +
  geom_line(stat="count")
plot

plot_yf <- plot + facet_grid(Year ~ .)
plot_yf

plot_rf <- plot + facet_grid(race ~ .)
plot_rf

#basePay against service groups
plot <- ggplot(data=salary, aes(x=race, group = race, colour = race)) +
  geom_line(stat="count")
plot <- plot + facet_grid(Year ~ .)
plot


