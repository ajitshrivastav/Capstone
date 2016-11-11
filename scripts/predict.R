# prediction
library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(e1071)
library(caret)
library(caTools)
salary <- read.csv("data_sets/intermediate/salary.csv")

# linear model for BasePay (independent variable) to TotalPay (dependent variable)
lm_totalPay_basePay <- lm(TotalPay ~ BasePay, data = salary)
summary(lm_totalPay_basePay)

# split 70:30 train:test, remove all the other columns except BasePay and job groups
salary$X.4 = NULL
salary$X.3 = NULL
salary$X.2 = NULL
salary$X.1 = NULL
salary$X = NULL
salary$Id = NULL
salary$EmployeeName = NULL
salary$JobTitle = NULL
salary$BasePay = NULL
salary$OvertimePay = NULL
salary$OtherPay = NULL
salary$Benefits = NULL
salary$TotalPay = NULL
salary$TotalPayBenefits = NULL
salary$Year = NULL
salary$Notes = NULL
salary$Agency = NULL
salary$Status = NULL
salary$first_name = NULL
salary$last_name = NULL
salary$gender = NULL
salary$race = NULL
salary$SERVICE_GROUP = NULL
salary$basePayToBase20 = NULL
split = sample.split(salary$basePayToBase50, 0.7)
varNames <- names(train_set)
varNames <- varNames[!varNames %in% c("basePayToBase20")]
varNames1 <- paste(varNames, collapse = "+")

train_set= salary[split, 1:54]
test_set = salary[!split, 1:54]

# Decision tree based indep_var - job_group, dep_var - BasePay decision_tree_bp_jg_train.pdf
sal_tree <- rpart(basePayToBase20 ~ . , data = train_set)
prp(sal_tree, varlen=40, tweak=0.75)

# Decision tree based indep_var - job_group, dep_var - BasePay decision_tree_bp_jg_test.pdf
sal_tree <- rpart(basePayToBase20 ~ . , data = test_set)
prp(sal_tree, varlen=40, tweak=0.75)

pred = predict(sal_tree, type="class")
table(pred)
table(pred, test_set$basePayToBase30)

# Random Forest model
rf.form <- as.formula(paste("basePayToBase50", varNames1, sep = " ~ "))
salary.rf <- randomForest(rf.form,
                          train_set,
                          ntree=200)
predictForest <- predict(salary.rf, newdata = test_set)
confusion_matrix <- table(test_set$basePayToBase50, predictForest)
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
accuracy

varImpPlot(salary.rf,
           sort = T,
           main="Variable Importance",
           n.var=10)

# try to see if there is a relation between BasePay and race using linear model
lm_basePay_race <- lm(BasePay ~ race, data = salary)
summary(lm_basePay_race)

# try to see if there is a relation between BasePay and race using decision tree model
sal_tree <- rpart(basePayToBase50 ~ race , data = salary)
prp(sal_tree, varlen=40, tweak=0.75)

# try to see if there is a relation between BasePay and race using random forest  model
train_set= salary[split, 1:78]
test_set = salary[!split, 1:78]
basePay_race.rf <- randomForest(as.factor(basePayToBase50) ~ race,
                                train_set,
                                ntree=200)
predictForest <- predict(basePay_race.rf, newdata = test_set)
confusion_matrix <- table(test_set$basePayToBase50, predictForest)
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
accuracy