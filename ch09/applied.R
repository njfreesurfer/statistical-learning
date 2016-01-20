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
# Chapter 9: Applied
##################

# Set your working directory here; uncomment here if necessary
#setwd("~/your_path/statistical-learning/ch08")
#-----------------------
# Exercise 4
#-----------------------
set.seed(1)
x=matrix(rnorm(200*2),ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(2,50))
train=sample(200,100)

par(mfrow=c(1,1))
plot(x,col=y)
# construct SVM with linear and radial kernels
library(e1071)

svm.lin=svm(y~.,data=data.train,kernel="linear",gamma=1,cost=1)
svm.rad=svm(y~.,data=dat[train,],kernel="radial",gamma=1,cost=1)
plot(svm.lin,dat[train,])
plot(svm.rad,dat[train,])

#check accuracy on training dataypred.lin=predict(svm.lin,x)
ypred.rad=predict(svm.rad,dat[train,])
ypred.lin=predict(svm.lin,dat[train,])

table(ypred.lin,dat[train,"y"])
table(ypred.rad,dat[train,"y"])
#we can see the accuracy in radial model is much better than linearl kernel.
#check accuracy on test data
ypred.radial=predict(svm.rad,dat[-train,])
ypred.lin=predict(svm.lin,dat[-train,])

table(predict=ypred.lin,truth=dat[-train,"y"])
table(predict=ypred.radial,truth=dat[-train,"y"])
#Let's plot ROC Curves
library(ROCR)
rocplot=function(pred,truth,...)
{
  predob=prediction(pred,truth)
  perf=performance(predob,"tpr","fpr")
  plot(perf,...)
}

par(mfrow=c(1,2))
#turn on the option for decision values
svm.lin=svm(y~.,data=dat[train,],kernel="linear",gamma=2,cost=1,decision.values=T)
svm.rad=svm(y~.,data=dat[train,],kernel="radial",gamma=2,cost=1,decision.values=T)
plot(svm.lin,dat[train,])
plot(svm.rad,dat[train,])
fitted.lin=attributes(predict(svm.lin,dat[train,],decision.values=TRUE))$decision.values
fitted.rad=attributes(predict(svm.rad,dat[train,],decision.values=TRUE))$decision.values
rocplot(fitted.lin,dat[train,"y"],main="Training Data",col="blue")
rocplot(fitted.rad,dat[train,"y"],add=T,col="red")
legend(0.2,1,c("Linear Kernel SVM","Radial Kernel SVM"), lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(1,1),col=c("blue","red"))
fitted.lin=attributes(predict(svm.lin,dat[-train,],decision.values=TRUE))$decision.values
fitted.radial=attributes(predict(svm.rad,dat[-train,],decision.values=TRUE))$decision.values
rocplot(fitted.lin,dat[-train,"y"],main="Test Data",col="blue")
rocplot(fitted.radial,dat[-train,"y"],add=T,col="red")
legend(0,1,c("Linear Kernel SVM","Radial Kernel SVM"), lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(1,1),col=c("blue","red"))

# we can see radial SVM outpeforms linear SVM

#-----------------------
# Exercise 5
#-----------------------

#---------
# a
#---------
par(mfrow=c(1,1))
x1=runif(500)-0.5
x2=runif(500)-0.5
y=1*(x1^2-x2^2>0)
#---------
# b
#---------
plot(x1,x2,col=c("red","blue")[y+1],xlab="X1",ylab="X2",main="Data Plot")
legend(0.3,0.5,c("x1^2>x2^2","x1^2<X2^2"),pch=c(1,1),col=c("blue","red"))

#---------
# c
#---------
lm.fit = glm(y ~ x1 + x2, family = binomial)
summary(lm.fit)

#---------
# d
#---------
data = data.frame(x1 = x1, x2 = x2, y = y)
lm.prob = predict(lm.fit, data, type = "response")
lm.pred = ifelse(lm.prob > 0.52, 1, 0)
plot(x1,x2,col=c("red","blue")[lm.pred+1],xlab="X1",ylab="X2",main="Simple Logistic Model")
legend(0.3,0.5,c("x1^2>x2^2","x1^2<X2^2"),pch=c(1,1),col=c("blue","red"))

#---------
# e
#---------
lm.fit2 = glm(y ~ poly(x1, 2) + poly(x2, 2) + I(x1 * x2), data = data, family = binomial)
summary(lm.fit2)


#---------
# f
#---------
lm.prob = predict(lm.fit2, data, type = "response")
lm.pred = ifelse(lm.prob > 0.52, 1, 0)
plot(x1,x2,col=c("red","blue")[lm.pred+1],xlab="X1",ylab="X2",main="Polynomial Logistic Model")
legend(0.3,0.5,c("x1^2>x2^2","x1^2<X2^2"),pch=c(1,1),col=c("blue","red"))

#---------
# g
#---------
svm.fit=svm(as.factor(y+1)~x1+x2,kernel="linear",gamma=1,cost=1)
svm.pred = predict(svm.fit, data)
plot(x1,x2,col=c("red","blue")[strtoi(svm.pred)],xlab="X1",ylab="X2",main="Support Vector Classifier Model")
legend(0.3,0.5,c("x1^2>x2^2","x1^2<X2^2"),pch=c(1,1),col=c("blue","red"))
#---------
# h
#---------
svm.fit=svm(as.factor(y+1)~x1+x2,gamma=1,cost=1)
svm.pred = predict(svm.fit, data)
plot(x1,x2,col=c("red","blue")[strtoi(svm.pred)],xlab="X1",ylab="X2",main="Non-Linear SVM")
legend(0.3,0.5,c("x1^2>x2^2","x1^2<X2^2"),pch=c(1,1),col=c("blue","red"))

#---------
# i
#---------

#model with nonlinear boundary tends to outperform linear models.

#-----------------------
# Exercise 6
#-----------------------

#---------
# a
#---------
set.seed(3154)
# Class one
x.one = runif(500, 0, 90)
y.one = runif(500, x.one + 10, 100)
x.one.noise = runif(50, 20, 80)
y.one.noise = 5/4 * (x.one.noise - 10) + 0.1

# Class zero
x.zero = runif(500, 10, 100)
y.zero = runif(500, 0, x.zero - 10)
x.zero.noise = runif(50, 20, 80)
y.zero.noise = 5/4 * (x.zero.noise - 10) - 0.1

# Combine all
class.one = seq(1, 550)
x1 = c(x.one, x.one.noise, x.zero, x.zero.noise)
x2 = c(y.one, y.one.noise, y.zero, y.zero.noise)
y = rep(2, 1100)
y[class.one] = 1
#add some noises

plot(x1,x2,col=c("red","blue")[y],xlab="X1",ylab="X2",main="Data Plot",xlim=c(0,110))
legend(102,90,c("class 1","class 2"),pch=c(1,1),col=c("blue","red"))





#---------
# b
#---------
data = data.frame(x1 = x1, y = y, x2 = x2)
tune.out = tune(svm, as.factor(y) ~ ., data = data, kernel = "linear", ranges = list(cost = c(0.01, 
                                                                                              0.1, 1, 5, 10, 100, 1000, 10000)))
summary(tune.out)
data.frame(cost = tune.out$performances$cost, misclass = tune.out$performances$error * 
             1100)


#---------
# c
#---------
set.seed(2)
# Class one
x.one = runif(500, 0, 90)
y.one = runif(500, x.one + 10, 100)
x.one.noise = runif(50, 20, 80)
y.one.noise = 5/4 * (x.one.noise - 10) + 0.1

# Class zero
x.zero = runif(500, 10, 100)
y.zero = runif(500, 0, x.zero - 10)
x.zero.noise = runif(50, 20, 80)
y.zero.noise = 5/4 * (x.zero.noise - 10) - 0.1

# Combine all
class.one = seq(1, 550)
x1 = c(x.one, x.one.noise, x.zero, x.zero.noise)
x2 = c(y.one, y.one.noise, y.zero, y.zero.noise)
y = rep(2, 1100)
y[class.one] = 1
data.test=data.frame(x1=x1,x2=x2,y=y)
all.costs = c(0.01, 0.1, 1, 5, 10, 100, 1000, 10000)
test.errors = rep(NA, 8)
#add some noises
for (i in 1:length(all.costs)) {
  svm.fit = svm(as.factor(y) ~ ., data = dat, kernel = "linear", cost = all.costs[i])
  svm.predict = predict(svm.fit, dat.test)
  test.errors[i] = sum(svm.predict != dat.test$y)
}

data.frame(cost = all.costs, `test misclass` = test.errors)

#when cost=10, the test error is smallest. 

#---------
# d
#---------
#We again see an overfitting phenomenon for linear kernel. 
# A large cost tries to fit correctly classify noisy-points and hence overfits the train data. 
# A small cost, however, makes a few errors on the noisy test points and performs better on test data.

#-----------------------
# Exercise 7
#-----------------------

library(ISLR)
#---------
# a
#---------
Auto$milage=ifelse(Auto$mpg>median(Auto$mpg),1,0)
Auto$milage=as.factor(Auto$milage)

#---------
# b
#---------
tune.out = tune(svm, milage ~ ., data = Auto, kernel = "linear", ranges = list(cost = c(0.01, 
                                                                                              0.1, 1, 5, 10, 100, 1000)))
summary(tune.out)

#when cost =10, we get the smallest error
#---------
# b
#---------
tune.out = tune(svm, milage ~ ., data = Auto, kernel = "radial", ranges = list(cost = c(0.01, 
                                                                                                   0.1, 1, 5, 10, 100, 1000)))
summary(tune.out)

#---------
# c
#---------
svm.lin=svm(milage ~., data=Auto,kernel="linear",cost=10,gamma=2)
svm.poly=svm(milage~.,data=Auto,kernel="polynomial",cost=10,degree=2)
svm.rad=svm(milage~.,data=Auto,kernel="radial",cost=10,gamma=2)

plot(svm.lin,Auto,mpg~weight)
plot(svm.poly,Auto,mpg~weight)
plot(svm.rad,Auto,mpg~weight)

#-----------------------
# Exercise 8
#-----------------------
library(ISLR)
#---------
# a
#---------
set.seed(1)
train=sample(nrow(OJ),800)
OJ.train=OJ[train,]
OJ.test=OJ[-train,]
#---------
# b
#---------
svm.fit=svm(Purchase~.,data=OJ.train,kernel="linear",cost=0.01)
summary(svm.fit)
#---------
# c
#---------
pred.train=predict(svm.fit,OJ.train)
table(predict=pred.train,truth=OJ.train$Purchase)
(78+55)/(439+78+55+228)
pred.test=predict(svm.fit,OJ.test)
table(predict=pred.test,truth=OJ.test$Purchase)
(31+18)/(141+31+18+80)
#---------
# d
#---------
tune.out = tune(svm, Purchase~ ., data = OJ.train, kernel = "linear", ranges = list(cost = c(0.01, 
                                                                                              0.1, 1, 5, 10)))
summary(tune.out)
# best cost=1
#---------
# e
#---------
svm.fit=svm(Purchase~.,data=OJ.train,kernel="linear",cost=1)
summary(svm.fit)

pred.train=predict(svm.fit,OJ.train)
table(predict=pred.train,truth=OJ.train$Purchase)
(73+54)/(440+73+54+233)
pred.test=predict(svm.fit,OJ.test)
table(predict=pred.test,truth=OJ.test$Purchase)
(33+19)/(140+78+19+78)

#---------
# f
#---------

tune.out = tune(svm, Purchase ~ ., data = OJ.train, kernel = "radial", ranges = list(cost = 10^seq(-2, 
                                                                                                   1, by = 0.25)))

summary(tune.out)
svm.rad= svm(Purchase ~ ., data = OJ.train, kernel = "radial", cost = tune.out$best.parameters$cost)
train.pred = predict(svm.radial, OJ.train)
table(OJ.train$Purchase, train.pred)
red.train=predict(svm.rad,OJ.train)
table(predict=pred.train,truth=OJ.train$Purchase)
(73+54)/(440+73+54+233)
pred.test=predict(svm.rad,OJ.test)
table(predict=pred.test,truth=OJ.test$Purchase)
(31,20)/(141+83+28+18)

#---------
# g
#---------

tune.out = tune(svm, Purchase ~ ., data = OJ.train, kernel = "polynomial",degree=2, ranges = list(cost = 10^seq(-2, 
                                                                                                   1, by = 0.25)))
svm.poly=svm(Purchase~.,data=OJ.train,kernel="polynomial",cost = tune.out$best.parameters$cost,degree=2)
red.train=predict(svm.poly,OJ.train)
table(predict=pred.train,truth=OJ.train$Purchase)
(73+54)/(440+73+54+233)
pred.test=predict(svm.poly,OJ.test)
table(predict=pred.test,truth=OJ.test$Purchase)
(31+19)/(141+83+28+18)



#overall ,radial is the best (slighly better than others)