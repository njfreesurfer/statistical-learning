#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# An Introduction to Statistical Learning
# with Applications in R
#
# Gareth James, Daniela Witten, Trevor Hastie, and Robert Tibshirani
# 
#  To get a free PDF of the book: http://www-bcf.usc.edu/~gareth/ISL/
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Author: Tony Jiang
# Check out my git: https://github.com/njfreesurfer/stat-learning
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


##################
# Chapter 8: Applied
##################

# Set your working directory here; uncomment here if necessary
#setwd("~/your_path/statistical-learning/ch08")
#-----------------------
# Exercise 7
#-----------------------
library(MASS)
library(tree)
set.seed(1)
train=sample(1:nrow(Boston),nrow(Boston)/2)

library(ggplot2)
geterror<-function(m)
{
  test.error=rep(NA,100)
  for (i in 1:100){
    rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=m,importance=TRUE,ntree=i*5)
    yhat.boost=predict(rf.boston,newdata=Boston[-train,])
    test.error[i]=mean((yhat.boost-Boston[-train,"medv"])^2)
  }
  return(test.error)
}

test.error<-matrix(0, 5, 100)

for (m in 6:10){
  for (i in 1:100){
    rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=m,importance=TRUE,ntree=i*5)
    yhat.boost=predict(rf.boston,newdata=Boston[-train,])
    test.error[m-5,i]=mean((yhat.boost-Boston[-train,"medv"])^2)
  }
  
  
}

#plot 
g<-ggplot()
g<-g+geom_line(aes(x=seq(1,100),y=test.error[1,],color="mtry=06"))+
  geom_line(aes(x=seq(1,100),y=test.error[2,],color="mtry=07"))+
  geom_line(aes(x=seq(1,100),y=test.error[3,],color="mtry=08"))+
  geom_line(aes(x=seq(1,100),y=test.error[4,],color="mtry=09"))+
  geom_line(aes(x=seq(1,100),y=test.error[5,],color="mtry=10"))+
  xlab("Tree Number")+ylab("Mean Test Error")+
  scale_colour_discrete(name="mtry")
g  

#-----------------------
# Exercise 8
#-----------------------

#---------
# a
#---------
library(ISLR)
set.seed(1)
train=sample(1:nrow(Carseats),nrow(Carseats)/2)
#---------
# b
#---------
library(tree)
tree.carseats=tree(Sales~.,data=Carseats,subset=train)
plot(tree.carseats)
text(tree.carseats,pretty=0)
yhat=predict(tree.carseats,newdata=Carseats[-train,])
mean((yhat-Carseats[-train,"Sales"])^2)

#---------
# c
#---------
cv.carseats=cv.tree(tree.carseats,FUN = prune.tree)
plot(cv.carseats$size,cv.carseats$dev,type="b")
which.min(cv.carseats$dev)
prune.carseats=prune.tree(tree.carseats,best=12)
yhat=predict(prune.carseats,newdata=Carseats[-train,])
mean((yhat-Carseats[-train,"Sales"])^2)
# the MSE increased from 4.148897 to 4.61

#---------
# d
#---------
library(randomForest)
bag.carseats = randomForest(Sales ~ ., data = Carseats,subset=train, mtry = 10, ntree = 500, 
                            importance = T)
bag.pred = predict(bag.carseats, newdata=Carseats[-train,])
mean((Carseats[-train,"Sales"] - bag.pred)^2)
importance(bag.carseats)     

#---------
# e
#---------
rf.carseats = randomForest(Sales ~ ., data = Carseats,subset=train, mtry = 6, ntree = 500, 
                            importance = T)
rf.pred = predict(rf.carseats, newdata=Carseats[-train,])
mean((Carseats[-train,"Sales"] - rf.pred)^2)
importance(rf.carseats)     


#-----------------------
# Exercise 9
#-----------------------

#---------
# a
#---------
set.seed(1)
train=sample(1:nrow(OJ),nrow(OJ)/2)

#---------
# b
#---------
tree.OJ=tree(Purchase~.,data=OJ,subset=train)
summary(tree.OJ)
#---------
# c
#---------
tree.OJ
#---------
# d
#---------
plot(tree.OJ)
text(tree.OJ,pretty=0)
#---------
# e
#---------
pred=predict(tree.OJ,newdata=OJ[-train,],type="class")
table(OJ[-train,]$Purchase, pred)
#---------
# f
#---------
cv.oj = cv.tree(tree.OJ, FUN = prune.tree)

#---------
# g
#---------
plot(cv.oj$size, cv.oj$dev, type = "b", xlab = "Tree Size", ylab = "Deviance")
#---------
# h
#---------
cv.oj$size[which.min(cv.oj$dev)]
#---------
# i
#---------
prune.oj=prune.tree(tree.OJ,best=6)

#---------
# j
#---------
summary(prune.oj)

#---------
# k
#---------

pred.unpruned = predict(tree.OJ,newdata=OJ[-train,],type="class")
misclass.unpruned = sum(OJ[-train,]$Purchase != pred.unpruned)
misclass.unpruned/length(pred.unpruned)
pred.pruned = predict(prune.oj,newdata=OJ[-train,],type="class")
misclass.pruned = sum(OJ[-train,]$Purchase != pred.pruned)
misclass.pruned/length(pred.pruned)
#error rate is same

#-----------------------
# Exercise 10
#-----------------------
#---------
# a
#---------
library(ISLR)
sum(is.na(Hitters$Salary))
Hitters = Hitters[-which(is.na(Hitters$Salary)), ]
sum(is.na(Hitters$Salary))
Hitters$Salary = log(Hitters$Salary)
#---------
# b
#---------
train = 1:200
Hitters.train = Hitters[train, ]
Hitters.test = Hitters[-train, ]

#---------
# c
#---------
library(gbm)
set.seed(103)
pows = seq(-10, -0.2, by = 0.1)
lambdas = 10^pows
length.lambdas = length(lambdas)
train.errors = rep(NA, length.lambdas)
test.errors = rep(NA, length.lambdas)
for (i in 1:length.lambdas) {
  boost.hitters = gbm(Salary ~ ., data = Hitters.train, distribution = "gaussian", 
                      n.trees = 1000, shrinkage = lambdas[i])
  train.pred = predict(boost.hitters, Hitters.train, n.trees = 1000)
  test.pred = predict(boost.hitters, Hitters.test, n.trees = 1000)
  train.errors[i] = mean((Hitters.train$Salary - train.pred)^2)
  test.errors[i] = mean((Hitters.test$Salary - test.pred)^2)
}

plot(lambdas, train.errors, type = "b", xlab = "Shrinkage", ylab = "Train MSE", 
     col = "blue", pch = 20)
#---------
# d
#---------
plot(lambdas, test.errors, type = "b", xlab = "Shrinkage", ylab = "Test MSE", 
     col = "red", pch = 20)
min(test.errors)
lambdas[which.min(test.errors)]
#---------
# e
#---------
lm.fit = lm(Salary ~ ., data = Hitters.train)
lm.pred = predict(lm.fit, Hitters.test)
mean((Hitters.test$Salary - lm.pred)^2)
library(glmnet)
set.seed(134)
x = model.matrix(Salary ~ ., data = Hitters.train)
y = Hitters.train$Salary
x.test = model.matrix(Salary ~ ., data = Hitters.test)
lasso.fit = glmnet(x, y, alpha = 1)
lasso.pred = predict(lasso.fit, s = 0.01, newx = x.test)
mean((Hitters.test$Salary - lasso.pred)^2)
#---------
# f
#---------
boost.best = gbm(Salary ~ ., data = Hitters.train, distribution = "gaussian", 
                 n.trees = 1000, shrinkage = lambdas[which.min(test.errors)])
summary(boost.best)

#---------
# g
#---------
library(randomForest)
set.seed(21)
rf.hitters = randomForest(Salary ~ ., data = Hitters.train, ntree = 500, mtry = 19)
rf.pred = predict(rf.hitters, Hitters.test)
mean((Hitters.test$Salary - rf.pred)^2)
#-----------------------
# Exercise 11
#-----------------------
#---------
# a
#---------
library(ISLR)
train = 1:1000
Caravan$Purchase = ifelse(Caravan$Purchase == "Yes", 1, 0)
Caravan.train = Caravan[train, ]
Caravan.test = Caravan[-train, ]

#---------
# b
#---------
library(gbm)
set.seed(342)
boost.caravan = gbm(Purchase ~ ., data = Caravan.train, n.trees = 1000, shrinkage = 0.01, 
                    distribution = "bernoulli")
summary(boost.caravan)
#---------
# c
#---------
boost.prob = predict(boost.caravan, Caravan.test, n.trees = 1000, type = "response")
boost.pred = ifelse(boost.prob > 0.2, 1, 0)
table(Caravan.test$Purchase, boost.pred)
lm.caravan = glm(Purchase ~ ., data = Caravan.train, family = binomial)
lm.prob = predict(lm.caravan, Caravan.test, type = "response")
lm.pred = ifelse(lm.prob > 0.2, 1, 0)
table(Caravan.test$Purchase, lm.pred)
#-----------------------
# Exercise 12
#-----------------------

set.seed(1)
library(ISLR)
summary(Weekly)
train = sample(nrow(Weekly), 2/3 * nrow(Weekly))
test = -train

#logistic
glm.fit = glm(Direction ~ . - Year - Today, data = Weekly[train, ], family = "binomial")
glm.probs = predict(glm.fit, newdata = Weekly[test, ], type = "response")
glm.pred = rep("Down", length(glm.probs))
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Weekly$Direction[test])

#Boosting
Weekly$BinomialDirection = ifelse(Weekly$Direction == "Up", 1, 0)
boost.weekly = gbm(BinomialDirection ~ . - Year - Today - Direction, data = Weekly[train, 
                                                                                   ], distribution = "bernoulli", n.trees = 5000)
yhat.boost = predict(boost.weekly, newdata = Weekly[test, ], n.trees = 5000)
yhat.pred = rep(0, length(yhat.boost))
yhat.pred[yhat.boost > 0.5] = 1
table(yhat.pred, Weekly$BinomialDirection[test])

#bagging
Weekly = Weekly[, !(names(Weekly) %in% c("BinomialDirection"))]
bag.weekly = randomForest(Direction ~ . - Year - Today, data = Weekly, subset = train, 
                          mtry = 6)
yhat.bag = predict(bag.weekly, newdata = Weekly[test, ])
table(yhat.bag, Weekly$Direction[test])

#Random forests
rf.weekly = randomForest(Direction ~ . - Year - Today, data = Weekly, subset = train, 
                         mtry = 2)
yhat.bag = predict(rf.weekly, newdata = Weekly[test, ])
table(yhat.bag, Weekly$Direction[test])

#Boosting resulted in the lowest validation set test error rate.