





###################dplyr & ggplot2################
library(dplyr)
library(ggplot2)

dat<-read.csv("E:\\Nishanth\\Kaggle\\Datasets\\Titanic\\train.csv")

dat<-dat[,-c(1,4,9,11)]
str(dat)
head(dat)

dat <- dat[complete.cases(dat),]

table(dat$Sex, dat$Survived)

MaleSurvivors <- dat %>% filter(Survived == 1 & as.character(Sex) == "male")

total <- seq(1,nrow(dat),1)
samprows <- sample(total, 0.3*nrow(dat))
samp <- dat[samprows,]

ggplot(samp, aes(x=Fare,y=Age,col=factor(Survived),shape=Sex))+
  geom_point()


ggplot(dat1[dat1$Fare<300,], aes(x=Fare,y=tit,col=Survived)) + geom_point(size=4)