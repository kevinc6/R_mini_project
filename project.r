library(ggplot2)
library(tidyverse)
library(corrplot)
train <- read.csv ("train.csv", header=TRUE, sep=",")
test <- read.csv("test.csv", header=TRUE, sep="," )
all<- rbind(within(train, rm("SalePrice")), test)
summary(train$SalePrice)
ggplot(data=train, aes(x=SalePrice/100)) + geom_histogram (binwidth = 20 )

num_vars <- train %>% select_if(is.numeric) %>% names()

# how many numerical variables?

df_num <- train[ ,num_vars]
cor_num <- cor(df_num, use="pairwise.complete.obs")
cor_sorted <- as.matrix(sort(cor_num[,'SalePrice'], decreasing = TRUE))
cor_high <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.2)))
cor_num <- cor_num[cor_high, cor_high]
cex.before <- par("cex")
par(cex = 0.5)
corrplot.mixed(cor_num, diag = "l", tl.col="black", tl.pos = "lt")
par(cex = cex.before)

train$LotShape[is.na(train$LotShape)]