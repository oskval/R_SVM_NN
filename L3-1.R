set.seed(1)
x=matrix(rnorm(20*2), ncol = 2)
y=c(rep(-1, 10), rep(1,10))
x[y==1,]=x[y==1,] + 1
plot(x, col=(3-y))

dat = data.frame(x=x, y=as.factor(y))
library(e1071)
svmfit=svm(y~., data=dat, kernel="linear",
           cost=10,scale=FALSE)

plot(svmfit , dat,grid=100,color.palette =
       terrain.colors,xlim=c(-2.1,2.5),ylim=c(-1.3,2.6))

svmfit$index

summary(svmfit)

svmfit=svm(y~., data=dat, kernel="linear", cost=0.1,
           scale=FALSE)
plot(svmfit , dat,grid=100,color.palette = terrain.colors)
svmfit$index

set.seed(1)
tune.out=tune(svm,y~.,data=dat,kernel="linear",
              ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out)

bestmod=tune.out$best.model
summary(bestmod)

xtest=matrix(rnorm(20*2), ncol=2)
ytest=sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,] + 1
testdat=data.frame(x=xtest, y=as.factor(ytest))

ypred=predict(bestmod ,testdat)
table(predict=ypred, truth=testdat$y)

svmfit=svm(y~., data=dat, kernel="linear", cost=.01,
           scale=FALSE)
ypred=predict(svmfit ,testdat)
table(predict=ypred, truth=testdat$y)

dat=data.frame(x=x, y=as.factor(y))
svmfit=svm(y~., data=dat, kernel="linear", cost=1e5)
summary(svmfit)
plot(svmfit , dat,grid=100,color.palette = terrain.colors)


svmfit=svm(y~., data=dat, kernel="linear", cost=1)
summary(svmfit)
plot(svmfit ,dat,grid=100,color.palette = terrain.colors)

set.seed(1)
x=matrix(rnorm(200*2), ncol=2)
x[1:100,]=x[1:100,] + 2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150), rep(2,50))
dat=data.frame(x=x, y=as.factor(y))

plot(x, col=y)

library(caret)
set.seed(1)
idx.train = createDataPartition(y = dat$y, p = 0.8,
                                list = FALSE)
train = dat[idx.train, ]
test = dat[-idx.train, ]

svmfit = svm(y~., data = train, kernel="radial", gamma=1, cost=1)
plot(svmfit, train)
plot(svmfit, train, grid=100, color.palette = terrain.colors)
summary(svmfit)

svmfit=svm(y~., data=train, kernel="radial", gamma=1, cost = 1e5)
plot(svmfit, train, grid=100, color.palette = terrain.colors)

set.seed(1)
tune.out = tune(svm, y~., data=train, kernel="radial",
                ranges=list(cost=c(0.1, 1, 10, 100, 1000),
                            gamma=c(0.5, 1, 2, 3, 4)))
summary(tune.out)
plot(tune.out, color.palette = terrain.colors)

pred=predict(tune.out$best.model, newdata=test)
table(predicted_labels=pred, true_labels=test$y)

## Multiclass Support Vector Classifier

set.seed(1)
x=rbind(x, matrix(rnorm(50*2), ncol=2))
y=c(y, rep(0, 50))
x[y==0,2]=x[y==0,2] + 2
dat=data.frame(x=x, y=as.factor(y))
plot(x, col=(y+1))

svmfit=svm(y~., data=dat, kernel = "radial", cost=1, gamma=1)
plot(svmfit, dat, grid=100, color.palette = terrain.colors)

## Application to gene expression data

library(ISLR)
names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)

table(Khan$ytrain )
table(Khan$ytest )

dat=data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))
out = svm(y~., data=dat, kernel="linear", cost=10)
summary(out)
table(out$fitted, dat$y)

dat.te=data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred.te=predict(out, newdata=dat.te)
table(pred.te, dat.te$y)

## START OF L3-1

# 1 Frequency calculation and plotting
train = read.csv("satellite_train.csv")
test = read.csv("satellite_test.csv")
train$V37 = as.factor(train$V37)
levels(train$V37)=c("red soil", "cotton crop",
                        "grey soil","damp grey soil","soil with vegetation",
                        "very damp grey soil")
test$V37 = as.factor(test$V37)
levels(test$V37)=c("red soil", "cotton crop",
                    "grey soil","damp grey soil","soil with vegetation",
                    "very damp grey soil")
library(MASS)
v37 = train$V37
v37.freq = table(v37)
barplot(v37.freq, las=2)

## 2 Two variable plotting
train1 = train[,c("V1", "V2", "V37")]
test1 = test[,c("V1", "V2", "V37")]

plot(train1$V1, train1$V2, col=train1$V37)

## 3 Tune function for optimal cost value

tune.out=tune(svm, V37~V1+V2, data=train1, kernel="linear",
          ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out)

bestmod=tune.out$best.model
summary(bestmod)
plot(tune.out$best.model,train1, color.palette = terrain.colors, las=1)

pred=predict(bestmod ,test1)
table(predicted_labels=pred, true_labels=test1$V37)
mean(test1$V37==pred)
table1 = table(predicted=pred,true_labels=test1$V37)
diag(table1)/colSums(table1)

## 4 Tune function with nonlinear SVM and hyperparameters

tune.out = tune(svm, V37~V1+V2, data=train1, kernel="radial",
                ranges=list(cost=c(0.1, 1, 10),
                            gamma=c(0.5, 1, 2)))

summary(tune.out)

bestmod=tune.out$best.model
summary(bestmod)
plot(tune.out$best.model,train1, color.palette = terrain.colors, las=1)

pred=predict(bestmod ,test1)
table(predicted_labels=pred, true_labels=test1$V37)
mean(test1$V37==pred)
table1 = table(predicted=pred,true_labels=test1$V37)
diag(table1)/colSums(table1)

# 5 Repeat step 3 and 4 with the whole dataset

tune.out=tune(svm, V37~., data=train, kernel="linear",
              ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out)

bestmod=tune.out$best.model
summary(bestmod)

pred=predict(bestmod ,test)
table(predicted_labels=pred, true_labels=test$V37)
mean(test$V37==pred)
table1 = table(predicted=pred,true_labels=test$V37)
diag(table1)/colSums(table1)

#####
tune.out = tune(svm, V37~., data=train, kernel="radial",
                ranges=list(cost=c(0.1, 1),
                            gamma=c(0.5, 1)))

summary(tune.out)

bestmod=tune.out$best.model
summary(bestmod)

pred=predict(bestmod ,test)
table(predicted_labels=pred, true_labels=test$V37)
mean(test$V37==pred)
table1 = table(predicted=pred,true_labels=test$V37)
diag(table1)/colSums(table1)

