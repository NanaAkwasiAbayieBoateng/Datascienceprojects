# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(caret)
library(klaR)
# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")

# load the diabetes dataset
ddata <- read.csv("../input/diabetes.csv")
# define an 80%/20% train/test split of the dataset
split=0.80
set.seed(123)
trainIndex <- createDataPartition(ddata$Outcome, p=split, list=FALSE)
data_train <- ddata[ trainIndex,]
data_test <- ddata[-trainIndex,]
data_train$Outcome <- as.factor(data_train$Outcome)
data_test$Outcome <- as.factor(data_test$Outcome)
# train a naive bayes model
model <- NaiveBayes(Outcome~., data=data_train)
# make predictions
x_test <- data_test[,1:8]
y_test <- data_test[,9]
predictions <- predict(model, x_test)
# summarize results
confusionMatrix(predictions$class, y_test)