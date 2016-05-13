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


####################Ridge#################
datreg<-read.csv("E:\\Nishanth\\Kaggle\\Datasets\\Titanic\\train.csv")
datreg<-datreg[,-c(1,4,9,11)]
str(datreg)

library(DMwR)
datreg1 <- centralImputation(datreg)

set.seed(24)
rows<-seq(1,nrow(datreg1),1)
train<-sample(rows,nrow(datreg1)*.7)
trainset<-datreg1[train,]
testset<-datreg1[-train,]

library(glmnet)
str(trainset)

dummies <- model.matrix(trainset$Survived~trainset$Sex+trainset$Embarked)[,-1]
tr <- data.frame(trainset[,c(2,4,5,6,7)], dummies)
tr.mat <- as.matrix(tr)

cv <- cv.glmnet(tr.mat, trainset$Survived)
cv$lambda.min

ridge <- glmnet(tr.mat, trainset$Survived, lambda=cv$lambda.min, alpha=0)
coef(ridge)

trainset$PredRidge <- predict(ridge, tr.mat, type = "response")
table(trainset$Survived, trainset$PredRidge>=0.6)

lasso <- glmnet(tr.mat, trainset$Survived, lambda=cv$lambda.min, alpha=1)
coef(lasso)

trainset$PredLasso <- predict(lasso, tr.mat, type = "response")
table(trainset$Survived, trainset$PredLasso>=0.56)

elastic <- glmnet(tr.mat, trainset$Survived, lambda=cv$lambda.min, alpha=0.5)
coef(elastic)

trainset$PredElastic <- predict(elastic, tr.mat, type = "response")
table(trainset$Survived, trainset$PredElastic>=0.6)

########################Decision Trees###############
#dat<-read.csv("D:\\Practice\\Titanic\\train.csv")
dat<-read.csv("E:\\Nishanth\\Kaggle\\Datasets\\Titanic\\train.csv")
#install.packages("KRLS")
str(dat)
dat<-dat[,-c(1,9)]
dat$Survived<-as.factor(as.character(dat$Survived))
dat$Pclass<-as.factor(as.character(dat$Pclass))
dat$SibSp<-as.factor(as.character(dat$SibSp))
dat$Parch<-as.factor(as.character(dat$Parch))

sum(is.na(dat))

library(DMwR)
#dat1 <- centralImputation(dat)
dat1 <- knnImputation(dat, k=5, scale=T)

levels(dat1$Embarked)
table(dat1$Embarked)[1]

levels(dat1$Cabin)
table(dat1$Cabin)[1]

levels(dat1$Embarked)[1] = "missing"
levels(dat1$Cabin)[1] = "missing"

library(stringr)
cabin <- as.character(dat1$Cabin)
cabin1 <- ifelse(cabin != "missing", substr(cabin,1,1), "M")
cabinmissing <- ifelse(cabin1 == "M", 1, 0)

cabin2 <- ifelse(cabin != "missing", substr(cabin, nchar(cabin)-3, nchar(cabin)), 0)
cabin2 <- ifelse(cabin2 != "0", substr(cabin2,2,nchar(cabin2)), cabin2)

check.numeric <- function(N){
  !length(grep("[^[:digit:]]", as.character(N)))
}

cabin2 <- ifelse(lapply(cabin2, check.numeric), cabin2, substr(cabin2,2,nchar(cabin2)))
cabin2 <- ifelse(cabin2 == "", 0, cabin2)

cabin2 <- as.numeric(cabin2)
sum(is.na(cabin2))

name <- as.character(dat1$Name)
sum(name == "")
tit <- substr(name, str_locate(name, ", ")+2, str_locate(name, ", ")+4)
tit <- ifelse(tit %in% c("Mr.","Mis", "Mrs", "Mas"), tit, ifelse(dat1$Sex=="Male", iflelse(dat1$Age >=18, "Mr.", "Mas"), ifelse(dat1$Age >=30, "Mrs", "Mis")))
tit <- as.factor(tit)

library(dplyr)
cabin1 <- as.factor(cabin1)
cabin2 <- as.factor(cabin2)
cabinmissing <- as.factor(cabinmissing)
dat1 <- cbind(dat1, cabin1, cabin2, tit,cabinmissing)

TopSurnames <- dat1 %>% filter(Survived == "1") %>% group_by(Surname=substr(Name,1,str_locate(Name, ",")-1)) %>% dplyr::summarise(Total =n()) %>% arrange(desc(Total)) %>% head(10)
valset %>% filter(substr(Name,1,str_locate(Name, ",")-1) %in% TopSurnames$Surname) %>% select(Error)

dat1 %>% group_by(tit) %>% dplyr::summarise(Tot = n()) %>% arrange(desc(Tot))

#dat1<- dat1 %>% select(-Name, -Cabin, -Sex)

library(ggplot2)
ggplot(dat1[dat1$Fare<300,], aes(x=Fare,y=tit,col=Survived)) + geom_point(size=4)

table(dat$Sex,dat1$cabin1,dat1$Survived)
#library(infotheo)
#dat1_Num_binned <- discretize(X=dat1[,c("Age","Fare")],disc="equalfreq", nbins=3)
#dat2 <- data.frame(dat1[,-c(4,7)], dat1_Num_binned)

set.seed(25)
rows<-seq(1,nrow(dat1),1)
train<-sample(rows,nrow(dat1)*.7)
trainset<-dat1[train,]
testset<-dat1[-train,]

library(C50)

DT_C50 <- C5.0(Survived~SibSp+Parch+Fare+Embarked+cabin1+tit,data=trainset,rules=T)
DT_C50 <- C5.0(Survived~SibSp+Parch+Fare+cabin1+tit,data=trainset,rules=T)
DT_C50 <- C5.0(Survived~SibSp+Parch+Fare+cabin1+tit+Age,data=trainset,rules=T)
DT_C50
summary(DT_C50)
C5imp(DT_C50, pct=TRUE)

set.seed(25)
val<-seq(1,nrow(testset),1)
valrows<-sample(val,nrow(testset)*.6)
valset<-testset[valrows,]
testvalset<-testset[-valrows,]

valset$pred <- predict(DT_C50, newdata = valset)
table(valset$Survived, valset$pred)

set.seed(670)
val<-seq(1,nrow(testset),1)
valrows<-sample(val,nrow(testset)*.6)
valset<-testset[valrows,]

valset$pred <- predict(DT_C50, newdata = valset)
table(valset$Survived, valset$pred)

valset$Error <- ifelse(valset$Survived == "1" & valset$pred == "0", "FN", ifelse(valset$Survived == "0" & valset$pred == "1", "FP", ""))
ErrorRows <- valset %>% filter(Error != "")

testset$pred <- predict(DT_C50, newdata = testset)
table(testset$Survived, testset$pred)

#Predict

dattest<-read.csv("E:\\Nishanth\\Kaggle\\Datasets\\Titanic\\test.csv")
str(dattest)
dattest$Pclass<-as.factor(as.character(dattest$Pclass))
dattest$SibSp<-as.factor(as.character(dattest$SibSp))
dattest$Parch<-as.factor(as.character(dattest$Parch))

sum(is.na(dattest))

library(DMwR)
#dat1 <- centralImputation(dat)
dattest <- knnImputation(dattest, k=5, scale=T)

levels(dattest$Embarked)
table(dattest$Embarked)[1]

levels(dattest$Cabin)
table(dattest$Cabin)[1]

levels(dattest$Parch)
table(dattest$Parch)
levels(dattest$Parch)[8] = 6

levels(dattest$Cabin)[1] = "missing"

library(stringr)
cabin <- as.character(dattest$Cabin)
cabin1 <- ifelse(cabin != "missing", substr(cabin,1,1), "M")
cabin2 <- ifelse(cabin != "missing", substr(cabin, nchar(cabin)-3, nchar(cabin)), 0)
cabin2 <- ifelse(cabin2 != "0", substr(cabin2,2,nchar(cabin2)), cabin2)

check.numeric <- function(N){
  !length(grep("[^[:digit:]]", as.character(N)))
}

cabin2 <- ifelse(lapply(cabin2, check.numeric), cabin2, substr(cabin2,2,nchar(cabin2)))
cabin2 <- ifelse(cabin2 == "", 0, cabin2)

cabin2 <- as.numeric(cabin2)
sum(is.na(cabin2))

name <- as.character(dattest$Name)
sum(name == "")
tit <- substr(name, str_locate(name, ", ")+2, str_locate(name, ", ")+4)
tit <- as.factor(tit)

library(dplyr)
cabin1 <- as.factor(cabin1)
dattest<- dattest %>% mutate(cabin1, cabin2, tit)

dattest<- dattest %>% select(-Name, -Cabin, -Sex)

dattest$Survived <- predict(DT_C50, newdata = dattest)

pred <- dattest %>% select(PassengerId, Survived)
write.csv(pred, "E:\\Nishanth\\Kaggle\\Datasets\\Titanic\\prediction.csv", row.names = F)

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
