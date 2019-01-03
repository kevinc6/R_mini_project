library(randomForest)
library(readr)
library(ggplot2)
library(missForest)     # Imputing the missing values from dataset
library(doParallel)     # Parallel processing of missForest
library(caret)          # Using Caret package for Cross Validation
library(e1071)
library(elasticnet)   
train <- read.csv("train.csv",sep=",",stringsAsFactors = FALSE,header = TRUE)
test <- read.csv("test.csv",sep=",",stringsAsFactors = FALSE,header = TRUE)
summary(train$SalePrice)

SalePrice = train$SalePrice
train$SalePrice = NULL
fulldata <- rbind(train,test)

cat(sprintf("Percentage of missing values in the overall train dataset: %s%s\n", round(length(which(is.na(train) == TRUE)) * 100 / (nrow(train) * ncol(train)), 2), "%"))
cat(sprintf("Percentage of missing values in the overall test dataset: %s%s\n", round(length(which(is.na(test) == TRUE)) * 100 / (nrow(test) * ncol(test)), 2), "%"))

registerDoParallel(cores = 3)
set.seed(999)