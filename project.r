library(dplyr)
library(ggplot2)
library(tidyverse)
library(corrplot)
library(randomForest)

train <- read.csv ("train.csv", header=TRUE, sep=",")
SalePrice <- train$SalePrice
test <- read.csv("test.csv", header=TRUE, sep="," )
all<- rbind(within(train, rm("SalePrice")), test)
summary(train$SalePrice)
ggplot(data=train, aes(x=SalePrice/100)) + geom_histogram (binwidth = 20 )

####Data Cleaning####
all$LotFrontage[is.na(all$LotFrontage)] <- 0
all$MasVnrArea[is.na(all$MasVnrArea)] <- 0
all$BsmtFinSF1[is.na(all$BsmtFinSF1)] <- 0
all$BsmtFinSF2[is.na(all$BsmtFinSF2)] <- 0
all$BsmtUnfSF[is.na(all$BsmtUnfSF)] <- 0
all$TotalBsmtSF[is.na(all$TotalBsmtSF)] <- 0
all$BsmtFullBath[is.na(all$BsmtFullBath)] <- 0
all$BsmtHalfBath[is.na(all$BsmtHalfBath)] <- 0
all$GarageYrBlt[is.na(all$GarageYrBlt)] <- 0
all$GarageCars[is.na(all$GarageCars)] <- 0
all$GarageArea[is.na(all$GarageArea)] <- 0
all$KitchenQual[is.na(all$KitchenQual)] <-0
#Typos in the dataset.
all$GarageYrBlt[all$GarageYrBlt==2207] <- 2007
all[is.na(all)] <- "None"

all$ExterQual<- recode(all$ExterQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
all$ExterCond<- recode(all$ExterCond,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
all$BsmtQual<- recode(all$BsmtQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
all$BsmtCond<- recode(all$BsmtCond,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
all$BsmtExposure<- recode(all$BsmtExposure,"None"=0,"No"=1,"Mn"=2,"Av"=3,"Gd"=4)
all$BsmtFinType1<- recode(all$BsmtFinType1,"None"=0,"Unf"=1,"LwQ"=2,"Rec"=3,"BLQ"=4,"ALQ"=5,"GLQ"=6)
all$BsmtFinType2<- recode(all$BsmtFinType2,"None"=0,"Unf"=1,"LwQ"=2,"Rec"=3,"BLQ"=4,"ALQ"=5,"GLQ"=6)
all$HeatingQC<- recode(all$HeatingQC,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
all$KitchenQual<- recode(all$KitchenQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
all$Functional<- recode(all$Functional,"None"=0,"Sev"=1,"Maj2"=2,"Maj1"=3,"Mod"=4,"Min2"=5,"Min1"=6,"Typ"=7)
all$FireplaceQu<- recode(all$FireplaceQu,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
all$GarageFinish<- recode(all$GarageFinish,"None"=0,"Unf"=1,"RFn"=2,"Fin"=3)
all$GarageQual<- recode(all$GarageQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
all$GarageCond<- recode(all$GarageCond,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
all$PoolQC<- recode(all$PoolQC,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
all$Fence<- recode(all$Fence,"None"=0,"MnWw"=1,"GdWo"=2,"MnPrv"=3,"GdPrv"=4)
all$LotShape<-recode(all$LotShape,"Reg"=4,"IR1"=3,"IR2"=2,"IR3"=1)
#Adding features
all$TotalSF = all$TotalBsmtSF + all$X1stFlrSF + all$X2ndFlrSF
all$AllSf = all$TotalBsmtSF + all$GrLivArea

train <- all[1:1460,]
train$SalePrice = SalePrice
test <- all[1461:2919,]

#How many numerical variables?
num_vars <- train %>% select_if(is.numeric) %>% names()
non_num_var <- setdiff(names(train),num_vars)
df_num <- train[ ,num_vars]
df_non_num <- train[,non_num_var]

####Exploratory Visualization####

cor_num <- cor(df_num,use="pairwise.complete.obs")
cor_sorted <- as.matrix(sort(cor_num[,'SalePrice'], decreasing = TRUE))
cor_high <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.2)))
cor_num <- cor_num[cor_high, cor_high]
cex.before <- par("cex")
par(cex = 0.45)
corrplot.mixed(cor_num, diag = "l", tl.col="black", tl.pos = "lt")
par(cex = cex.before)

