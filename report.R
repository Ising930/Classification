data <- read.csv("hotels.csv")
dim(data)

install.packages("tree")
install.packages("e1071")
install.packages("pROC")

library(tree)
library(e1071)
library(pROC)

#Data Analysis
data <- data[,c(2:4,6:12,17:19,22,24,26,28:30)]
data <- data[data$agent != "NULL",]
data$agent <- as.numeric(data$agent)
str(data)
dim(data)
summary(data)
hist(data$stays_in_week_nights)
hist(data$agent)
hist(data$arrival_date_day_of_month)

#split
set.seed(1234)
sub <- sample(nrow(data),nrow(data)*0.7)
train_data <- data[sub,]
test_data <- data[-sub,]

#BYS
model_BYS = naiveBayes(is_canceled ~ ., data=train_data)
summary(model_BYS)
pred_BYS <- predict(model_BYS, test_data, type = "class")  
table(pred_BYS,test_data$is_canceled)
mean(pred_BYS == test_data$is_canceled)

#logistic
model_logit = glm(is_canceled~.,data=train_data,family=binomial(link="logit"))
summary(model_logit)

#imporve
model_logit2 = glm(is_canceled~.,data=train_data[,-c(5,10,15,16)],family=binomial(link="logit"))
summary(model_logit2)

#class predict
real=test_data$is_canceled
predict_=predict.glm(model_logit2,type="response",newdata=test_data)
predict=ifelse(predict_>0.5,1,0)
test_data$predict_logit = predict
head(test_data)

#performance
#Accuracy of calculation model
error=test_data$predict_logit-test_data$is_canceled
#The proportion of correctly judged quantity to the total 
accuracy=(nrow(test_data)-sum(abs(error)))/nrow(test_data)
accuracy

predict_=predict.glm(logi,type="response",newdata=test_data)
predict=ifelse(predict_>0.5,1,0)
logistic_roc <- roc(test_data$is_canceled,as.numeric(predict),pi = TRUE)
plot(logistic_roc, print.auc=TRUE, print.thres=TRUE,main='ROC of logistic')

#decision tree
train_data$is_canceled <- as.factor(train_data$is_canceled)
model_tree = tree(is_canceled ~ ., data=train_data)
plot(model_tree)
text(model_tree, pretty=0)

#performance1
pred <- predict(model_tree, test_data, type = "class")  
mean(pred == test_data$is_canceled)

#imporve
cv=cv.tree(model_tree,FUN=prune.misclass)
names(cv)
par(mfrow=c(1,2))
plot(cv$size,cv$dev,type="b") #Error rate as a function of tree size
plot(cv$k,cv$dev,type="b")  #Error rate as a function of complexity K
par(mfrow=c(1,1))

prune=prune.misclass(model_tree,best=6)
plot(prune)
text(prune,pretty=0)
tree.pred=predict(prune,test_data,type="class")
table(tree.pred,test_data$is_canceled) 
#performance2
mean(tree.pred == test_data$is_canceled)

