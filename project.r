library(dplyr)
library(ggplot2)
library(tidyverse)
library(corrplot)
library(dplyr)
train <- read.csv ("train.csv", header=TRUE, sep=",")
SalePrice <- train$SalePrice
test <- read.csv("test.csv", header=TRUE, sep="," )

all<- rbind(within(train, rm("SalePrice")), test)
summary(train$SalePrice)
ggplot(data=train, aes(x=SalePrice/100)) + geom_histogram (binwidth = 20 )
<<<<<<< HEAD
# ####Data Cleaning####
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
#additional
all$LotShape<-recode(all$LotShape,"Reg"=4,"IR1"=3,"IR2"=2,"IR3"=1)

#Adding an important feature - Total area of basement
all$TotalSF = all$TotalBsmtSF + all$X1stFlrSF + all$X2ndFlrSF

train <- all[1:1460,]
train$SalePrice = SalePrice
test <- all[1461:2919,]


# cat(sprintf("Percentage of missing values in the overall train dataset: %s%s\n", round(length(which(is.na(train) == TRUE)) * 100 / (nrow(train) * ncol(train)), 2), "%"))
# cat(sprintf("Percentage of missing values in the overall test dataset: %s%s\n", round(length(which(is.na(test) == TRUE)) * 100 / (nrow(test) * ncol(test)), 2), "%"))

#how many numerical variables?
num_vars <- train %>% select_if(is.numeric) %>% names()
non_num_var <- setdiff(names(train),num_vars)
df_num <- train[ ,num_vars]
df_non_num <- train[,non_num_var]
=======

num_vars <- train %>% select_if(is.numeric) %>% names() 

####Exploratory Visualization####
# how many numerical variables?

df_num <- train[ ,num_vars]
#df_non_num <-train[,non_num_vars]
cor_num <- cor(df_num, use="pairwise.complete.obs") 

# num_vars <- train %>% select_if(is.numeric) %>% names()
# non_num_var=setdiff(names(train),num_vars)

# how many numerical variables?
# df_num <- train[ ,num_vars]
# df_non_num=train[,non_num_var]

# cor_num <- cor(df_num,use="pairwise.complete.obs")
# cor_sorted <- as.matrix(sort(cor_num[,'SalePrice'], decreasing = TRUE))
# cor_high <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.2)))
# cor_num <- cor_num[cor_high, cor_high]
# cex.before <- par("cex")
# par(cex = 0.7)
# corrplot.mixed(cor_num, diag = "l", tl.col="black", tl.pos = "lt")
# par(cex = cex.before)

# ####Data Cleaning####
# cat(sprintf("Percentage of missing values in the overall train dataset: %s%s\n", round(length(which(is.na(train) == TRUE)) * 100 / (nrow(train) * ncol(train)), 2), "%"))
# cat(sprintf("Percentage of missing values in the overall test dataset: %s%s\n", round(length(which(is.na(test) == TRUE)) * 100 / (nrow(test) * ncol(test)), 2), "%"))
# 
train$ExterQual<- recode(train$ExterQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
train$ExterCond<- recode(train$ExterCond,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
train$BsmtQual<- recode(train$BsmtQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
train$BsmtCond<- recode(train$BsmtCond,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
train$BsmtExposure<- recode(train$BsmtExposure,"None"=0,"No"=1,"Mn"=2,"Av"=3,"Gd"=4)
train$BsmtFinType1<- recode(train$BsmtFinType1,"None"=0,"Unf"=1,"LwQ"=2,"Rec"=3,"BLQ"=4,"ALQ"=5,"GLQ"=6)
train$BsmtFinType2<- recode(train$BsmtFinType2,"None"=0,"Unf"=1,"LwQ"=2,"Rec"=3,"BLQ"=4,"ALQ"=5,"GLQ"=6)
train$HeatingQC<- recode(train$HeatingQC,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
train$KitchenQual<- recode(train$KitchenQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
train$Functional<- recode(train$Functional,"None"=0,"Sev"=1,"Maj2"=2,"Maj1"=3,"Mod"=4,"Min2"=5,"Min1"=6,"Typ"=7)
train$FireplaceQu<- recode(train$FireplaceQu,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
train$GarageFinish<- recode(train$GarageFinish,"None"=0,"Unf"=1,"RFn"=2,"Fin"=3)
train$GarageQual<- recode(train$GarageQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
train$GarageCond<- recode(train$GarageCond,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
train$PoolQC<- recode(train$PoolQC,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
train$Fence<- recode(train$Fence,"None"=0,"MnWw"=1,"GdWo"=2,"MnPrv"=3,"GdPrv"=4)
#additional
train$LotShape<-recode(train$LotShape,"Reg"=4,"IR1"=3,"IR2"=2,"IR3"=1)
#all$<-recode(all$,)
#Adding an important feature - Total area of basement
train$TotalSF = train$TotalBsmtSF + train$X1stFlrSF + train$X2ndFlrSF

num_vars <- train %>% select_if(is.numeric) %>% names()
non_num_var=setdiff(names(train),num_vars)

df_num <- train[ ,num_vars]
df_non_num=train[,non_num_var]
>>>>>>> 00124ee4a3b4f04eabebd85c37a55be43e9b3144

cor_num <- cor(df_num,use="pairwise.complete.obs")
cor_sorted <- as.matrix(sort(cor_num[,'SalePrice'], decreasing = TRUE))
cor_high <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.2)))
cor_num <- cor_num[cor_high, cor_high]
cex.before <- par("cex")
par(cex = 0.45)
corrplot.mixed(cor_num, diag = "l", tl.col="black", tl.pos = "lt")
par(cex = cex.before)

<<<<<<< HEAD




=======
>>>>>>> 00124ee4a3b4f04eabebd85c37a55be43e9b3144
