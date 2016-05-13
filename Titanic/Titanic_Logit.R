rm(list=ls(all=TRUE))

dat<-read.csv("E:\\Nishanth\\Kaggle\\Datasets\\Titanic\\train.csv")

str(dat)

str(dat)
dat<-dat[,-c(1,4,9,11)]
dat$Survived<-as.factor(as.character(dat$Survived))
dat$Pclass<-as.factor(as.character(dat$Pclass))
dat$SibSp<-as.factor(as.character(dat$SibSp))
dat$Parch<-as.factor(as.character(dat$Parch))

sum(is.na(dat))

library(DMwR)
dat1 <- centralImputation(dat)

set.seed(24)
rows<-seq(1,nrow(dat1),1)
train<-sample(rows,nrow(dat1)*.7)
trainset<-dat1[train,]
testset<-dat1[-train,]

plot(trainset$Fare,trainset$Survived)
table(trainset$Embarked,trainset$Survived)

reg<-glm(Survived~.,family="binomial",data=trainset)

reg2<-glm(Survived~Pclass+Sex+Age+SibSp,family="binomial",data=trainset)

library(car)
vif(reg2)
summary(reg2)

trainset$pred<-predict(reg2,type="response")
table(trainset$Survived, trainset$pred>=0.6)