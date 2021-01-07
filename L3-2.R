library(h2o)
h2o.init(nthreads = -1)

data=read.csv("customer_churn.csv", stringsAsFactors=TRUE)
head(data)
str(data)

plot(density(data$tenure[data$Churn=="Yes"]),
     col="firebrick1",lwd=3,main="", xlab="tenure variable, i.e.
number of months the customer is with the company")
lines(density(data$tenure[data$Churn=="No"]),
      col="dodgerblue1",lwd=3)
grid()
legend(40, 0.04, legend=c("Churned", "Stayed"),
       col=c("firebrick1", "dodgerblue1"),cex=1.2,lty=1)

plot(density(data$MonthlyCharges[data$Churn=="Yes"]),col="red",
     lwd=3,main="",xlab="MonthlyCharges variable",ylim=c(0,0.02))
lines(density(data$MonthlyCharges[data$Churn=="No"]),col="blue",lwd=3)
grid()
legend(100, 0.02, legend=c("Stayed", "Churned"),col=c("red",
                                                      "blue"),cex=0.8,lty=1)
data[,"customerID"]=NULL

library(caret)
idx.train = createDataPartition(y = data$Churn, p = 0.8, list =
                                  FALSE)

data$Churn = as.factor(data$Churn)
train = data[idx.train, ]
test = data[-idx.train, ]

h2o_train=as.h2o(train)
h2o_test=as.h2o(test)

dl_model = h2o.deeplearning(x = names(h2o_train[,1:19]),
                            y = "Churn",
                            training_frame = h2o_train,
                            activation="Tanh",
                            hidden = c(10),
                            loss = "CrossEntropy",
                            score_each_iteration=TRUE,
                            epochs = 10000,
                            rate=0.01,
                            balance_classes=F,
                            adaptive_rate = F,
                            stopping_rounds=0)

plot(dl_model)

prediction = as.data.frame(h2o.predict(object = dl_model,
                                       newdata = h2o_test))
confMatrix=table(true_labels=test$Churn, predicted_labels=prediction[,1])
confMatrix
diag(confMatrix)/rowSums(confMatrix)*100

dl_model = h2o.deeplearning(x = names(h2o_train[,1:19]),
                            y = "Churn",
                            training_frame = h2o_train,
                            activation="Tanh",
                            hidden = c(20, 20, 20),
                            loss = "CrossEntropy",
                            score_each_iteration=TRUE,
                            epochs = 10000,
                            rate=0.01,
                            balance_classes=F,
                            adaptive_rate = F,
                            stopping_rounds=0)

plot(dl_model)
prediction = as.data.frame(h2o.predict(object = dl_model,
                                       newdata = h2o_test))

confMatrix=table(true_labels=test$Churn, predicted_labels=prediction[,1])
confMatrix
diag(confMatrix)/rowSums(confMatrix)*100

dl_model = h2o.deeplearning(x = names(h2o_train[,1:19]),
                            y = "Churn",
                            training_frame = h2o_train,
                            activation="Tanh",
                            hidden = c(20, 20, 20),
                            loss = "CrossEntropy",
                            score_each_iteration=TRUE,
                            epochs = 10000,
                            rate=0.01,
                            balance_classes=F,
                            adaptive_rate = F,
                            stopping_rounds=0,
                            l1 = 0,
                            l2 = 0.01)
prediction = as.data.frame(h2o.predict(object = dl_model,
                                       newdata = h2o_test))
mean(prediction$predict==test$Churn)*100
confMatrix=table(predicted_labels=prediction$predict,
                 true_labels=test$Churn)
diag(confMatrix)/colSums(confMatrix)*100

## Start of assignment

train = read.csv("arrhythmia_train.csv")
test = read.csv("arrhythmia_test.csv")
levels(train$arrhythmia)=c("normal", "supraventricular", "ventricular",
                           "unknown")
levels(test$arrhythmia)=c("normal", "supraventricular", "ventricular",
                           "unknown")
## 10 heartbeats
x = train[train$arrhythmia == "0",]

windows()
par(mfrow=c(2,5))
for(i in 1:10)
  plot(unlist(x[i,]), type="l")
dev.off()

## Two features with class dependent colors

plot(train$X30, train$X50, col=train$arrhythmia)

# Distributions of classes
train$arrhythmia = as.factor(train$arrhythmia)
test$arrhythmia = as.factor(test$arrhythmia)
plot(train$arrhythmia, las=2)

# Training and predicting with different parameters

h2o_train=as.h2o(train)
h2o_test=as.h2o(test)

dl_model = h2o.deeplearning(x = names(h2o_train[,1:187]),
                            y = "arrhythmia",
                            training_frame = h2o_train,
                            activation="Tanh",
                            hidden = c(10),
                            loss = "CrossEntropy",
                            score_each_iteration=TRUE,
                            epochs = 100,
                            rate=0.01,
                            balance_classes=F,
                            adaptive_rate = F,
                            stopping_rounds=0)

plot(dl_model)
prediction = as.data.frame(h2o.predict(object = dl_model,
                                       newdata = h2o_test))
mean(prediction$predict==test$arrhythmia)*100
confMatrix=table(predicted_labels=prediction$predict,
                 true_labels=test$arrhythmia)
diag(confMatrix)/colSums(confMatrix)*100

## antas budas daugiau hidden ir epochs 
dl_model = h2o.deeplearning(x = names(h2o_train[,1:187]),
                            y = "arrhythmia",
                            training_frame = h2o_train,
                            activation="Tanh",
                            hidden = c(10,10),
                            loss = "CrossEntropy",
                            score_each_iteration=TRUE,
                            epochs = 1000,
                            rate=0.01,
                            balance_classes=F,
                            adaptive_rate = F,
                            stopping_rounds=0,
                            l1 = 0,
                            l2 = 0.01)

plot(dl_model)
prediction = as.data.frame(h2o.predict(object = dl_model,
                                       newdata = h2o_test))
mean(prediction$predict==test$arrhythmia)*100
confMatrix=table(predicted_labels=prediction$predict,
                 true_labels=test$arrhythmia)
diag(confMatrix)/colSums(confMatrix)*100

## Trecias bandymas'

dl_model = h2o.deeplearning(x = names(h2o_train[,1:187]),
                            y = "arrhythmia",
                            training_frame = h2o_train,
                            activation="Rectifier",
                            hidden = c(10),
                            loss = "CrossEntropy",
                            score_each_iteration=TRUE,
                            epochs = 100,
                            rate=0.01,
                            balance_classes=F,
                            adaptive_rate = F,
                            stopping_rounds=0,
                            l1 = 0,
                            l2 = 0.01)

plot(dl_model)
prediction = as.data.frame(h2o.predict(object = dl_model,
                                       newdata = h2o_test))
mean(prediction$predict==test$arrhythmia)*100
confMatrix=table(predicted_labels=prediction$predict,
                 true_labels=test$arrhythmia)
diag(confMatrix)/colSums(confMatrix)*100

## Ketvirtas bandymas

dl_model = h2o.deeplearning(x = names(h2o_train[,1:187]),
                            y = "arrhythmia",
                            training_frame = h2o_train,
                            activation="Rectifier",
                            hidden = c(10,10),
                            loss = "CrossEntropy",
                            score_each_iteration=TRUE,
                            epochs = 1000,
                            rate=0.01,
                            balance_classes=F,
                            adaptive_rate = F,
                            stopping_rounds=0,
                            l1 = 0,
                            l2 = 0.01)
plot(dl_model)
prediction = as.data.frame(h2o.predict(object = dl_model,
                                       newdata = h2o_test))
mean(prediction$predict==test$arrhythmia)*100
confMatrix=table(predicted_labels=prediction$predict,
                 true_labels=test$arrhythmia)
diag(confMatrix)/colSums(confMatrix)*100
