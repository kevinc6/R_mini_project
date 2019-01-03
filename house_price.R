library(ggplot2)
train.data=read.csv("train.csv",sep=",",na.strings = "")
test.data=read.csv("test.csv",sep=",",na.strings = "")


ggplot(train.data,aes(x=SalePrice/1000))+geom_histogram(binwidth = 20)
sapply(train.data,function(x) sum(is.na(x)))

train.data$Alley[1]
is.na(train.data$Alley[1])
class(train.data$Alley[1])

train.data=train.data[,-7]

