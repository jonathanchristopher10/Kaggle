library(dplyr)
library(ggplot2)
library(magrittr)
library(reshape2)
library(caret)
library(randomForest)

#INITIATION----

data_training1 <- read.csv("C:/Users/Jonathan-PC/Documents/Taralite/cs-training.csv", header = TRUE)
data_test <- read.csv("C:/Users/Jonathan-PC/Documents/Taralite/cs-test.csv", header = TRUE)

data_training <- data_training[, -1]
data_test <- data_test[, -1]

#MISSING VALUE----

data_training1 %>%
  is.na %>%
  melt %>%
  ggplot(aes(x=Var1, y=Var2)) + geom_raster(aes(fill = value)) + scale_fill_discrete(name = "Missing Value or Not", breaks = c("FALSE", "TRUE"), labels = c("No", "Missing Value"))

#1. AGE----

data_training %>%
  ggplot(aes(age)) + geom_histogram(col = 'black', fill = 'blue') + labs(title="Age Distribution")

summary(data_training$age)

#2. REVOLVING UTILIZATION----

boxplot(data_training$DebtRatio)
  #ada data dengan nilai ekstrim sebanyak 3321
  outliers <- data_training$RevolvingUtilizationOfUnsecuredLines>1
  sum(outliers)
  
summary(data_training$RevolvingUtilizationOfUnsecuredLines)

#ubah data outliers ke median
data_training$RevolvingUtilizationOfUnsecuredLines[outliers]=0.15

data_training %>%
  ggplot(aes(RevolvingUtilizationOfUnsecuredLines)) + geom_histogram(col = "black", fill = "green") + labs(title = "Revolving Utilization from 0 to 1 Ratio")


#3. DEBT RATIO----

boxplot(data_training$DebtRatio)
  #ada data outliers yang lebih dari 100000, dihapus
  data_training <- data_training[-which(data_training$DebtRatio>100000),]

summary(data_training$DebtRatio)


#4. MONTHLY INCOME----

sum(is.na(data_training$MonthlyIncome))
summary(data_training$MonthlyIncome)

#delete income > 300000
data_training <- data_training[-which(data_training$MonthlyIncome>300000),]
#missing value to median
data_training$MonthlyIncome[is.na(data_training$MonthlyIncome)]=5400

#5. NUMBER OPEN CREDIT LINES----

summary(data_training$NumberRealEstateLoansOrLines)

data_training %>%
  ggplot(aes(NumberOfOpenCreditLinesAndLoans)) + geom_histogram(col = "black", fill = "red") + labs(title = "Number of Open Credit Lines and Loans Distribution")

#6. NUMBER REAL ESTATE LOANS----

data_training %>%
  ggplot(aes(NumberRealEstateLoansOrLines)) + geom_histogram(col = "black") + labs(title = "Number of Real Estate Loans or Lines Distribution")
boxplot(data_training$NumberRealEstateLoansOrLines)

#remove pendapatan 54
data_training <- data_training[-which(data_training$NumberRealEstateLoansOrLines==54),]

#7. NUMBER OF DEPENDENTS--------------------
sum(is.na(data_training$NumberOfDependents))

#missing value to median
data_training$NumberOfDependents[is.na(data_training$NumberOfDependents)]=0

data_training %>%
  ggplot(aes(NumberOfDependents)) + geom_histogram()


#8. NUMBER OF TIME 30 - 59 PAST DUE NOT WORSE----

boxplot(data_training$NumberOfTime30.59DaysPastDueNotWorse)
#value over >96 to median
data_training$NumberOfTime30.59DaysPastDueNotWorse[data_training$NumberOfTime30.59DaysPastDueNotWorse>=96] = 0

#9. NUMBER OF TIME 60 - 89 PAST DUE NOT WORSE----

#value over >96 to median
data_training$NumberOfTime60.89DaysPastDueNotWorse[data_training$NumberOfTime60.89DaysPastDueNotWorse>=96] = 0

#10. NUMBER OF TIME 90 DAYS LATE-----

data_training %>%
  ggplot(aes(NumberOfTimes90DaysLate)) + geom_histogram(col = 'black')

#value over >96 to median
data_training$NumberOfTimes90DaysLate[data_training$NumberOfTimes90DaysLate>=96] = 0

#CS TEST MISSING VALUE----
data_test$MonthlyIncome[is.na(data_test$MonthlyIncome)] = 5400
data_test$NumberOfDependents[is.na(data_test$NumberOfDependents)] = 0

#SERIOUS DLQIN2YRS----

barplot(prop.table(table(data_training$SeriousDlqin2yrs)))

data_training$SeriousDlqin2yrs <- as.factor(data_training$SeriousDlqin2yrs)

#DOWNSAMPLING----

newTrain <- data_training[data_training$SeriousDlqin2yrs==1,]
DownSampleTrain <- data_training[data_training$SeriousDlqin2yrs==0,]
downsampleDat <- sample(1:139948,10500)

newTrainDat<-rbind(newTrain,DownSampleTrain[downsampleDat,])
newTrainDat<-newTrainDat[sample(nrow(newTrainDat)),]
rownames(newTrainDat)<-NULL


#### ######
#MODELLING#
#### ### ##

#RANDOM FOREST----
set.seed(1234)
test_rf <- randomForest(newTrainDat[,-1], as.factor(newTrainDat$SeriousDlqin2yrs)
                                ,do.trace = TRUE, ntree = 500
                                ,forest = TRUE)
print(test_rf)

prediksi <- data.frame(predict(test_rf, newdata = data_test[,-1], 'prob'))
SeriousDlqin2yrs <- prediksi[,2]

rf_data_test <- cbind(SeriousDlqin2yrs, data_test[,-1])

write.csv(rf_data_test, file = "C:/Users/Jonathan-PC/Documents/Taralite/data_test_rf.csv", quote = FALSE, row.names = FALSE)

#GLM----
glmControl <- trainControl(method = "cv", number = 10, repeats = 10)

set.seed(1234)
test_glm <- train(SeriousDlqin2yrs ~., data = newTrainDat, method = "glm",
                  trControl = glmControl)

print(test_glm)

prediksi_glm <- data.frame(predict(test_glm$finalModel, newdata = data_test[,-1], type = 'response'))
SeriousDlqin2yrs <- prediksi_glm

glm_data_test <- cbind(SeriousDlqin2yrs, data_test[,-1])

write.csv(glm_data_test, file = "C:/Users/Jonathan-PC/Documents/Taralite/data_test_glm.csv", quote = FALSE, row.names = FALSE)


#GBM----
set.seed(1234)
test_gbm <- train(SeriousDlqin2yrs ~., data = newTrainDat, method = "gbm")

print(test_gbm)

prediksi_gbm <- data.frame(predict(test_gbm$finalModel, data_test[,-1], 150))
gbm_data_set <- cbind(prediksi_gbm, data_test[,-1])

write.csv(gbm_data_set, file = "C:/Users/Jonathan-PC/Documents/Taralite/data_test_gbm.csv", quote = FALSE, row.names = FALSE)


