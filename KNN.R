
library(tidyverse)
library(class)
library(fastDummies)

MentalHealth = read_csv("MentalHealthDataSet.csv")

#Making a copy of MentalHealth to make numerical values from categorical to implement KNN
MentalHealth_copy <- MentalHealth
MentalHealth_copy$Gender <- ifelse(MentalHealth_copy$Gender == "Male", 1, 0)
MentalHealth_copy$Mental_Health_History <- ifelse(MentalHealth_copy$Mental_Health_History == "Yes", 1, 0)

#Creating numerical values from categorical and deleting categorical values
MentalHealth_copy$Age <- dummy_cols(MentalHealth_copy$Age)[-1]
MentalHealth_copy$Occupation <- dummy_cols(MentalHealth_copy$Occupation)[-1]
MentalHealth_copy$Days_Indoors <- dummy_cols(MentalHealth_copy$Days_Indoors)[-1]
MentalHealth_copy$Coping_Struggles <- as.integer(factor(MentalHealth_copy$Coping_Struggles, levels = c("No", "Yes")))

#Setting Age, Gender, Occupation, Days indoors and Mental Health as train and test data
data_train <- MentalHealth_copy[, c(1:4, 8)]
data_test <- MentalHealth_copy[, c(1:4, 8)]

#KNN
classification <- MentalHealth_copy$Coping_Struggles
pred <- knn(train = data_train, test = data_test, cl = classification, k = round(sqrt(ncol(MentalHealth))))

#Checking the success rate of KNN
sum(pred == classification) / nrow(MentalHealth)
