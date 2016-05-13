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