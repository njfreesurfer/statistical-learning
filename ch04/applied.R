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
# Chapter 4: Applied
##################
# Set your working directory here; uncomment here if necessary
#setwd("~/your_path/statistical-learning/ch04")

#-----------------------
# Exercise 10
#-----------------------
library(ISLR)
#
# (a) 
#
summary(Weekly)
#check pairwise correlation
cor(Weekly[,-9])
# plot pair-wise
pairs(Weekly)
#we can see a strong correlation between Volume and Year. 
#
# (b) 
#
attach(Weekly)
glm.fit<-glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,data=Weekly,family=binomial)
summary(glm.fit)
#lag2 appears to be a significant indicator in predicting direction.

#
# (c) 
#
glm.probs=predict(glm.fit,type="response")
glm.pred=rep("Down",nrow(Weekly))
glm.pred[glm.probs>0.5]="Up"
table(glm.pred,Direction)
mean(glm.pred==Direction)
#Direction
#glm.pred Down  Up
#Down   54  48
#Up    430 557
# Percentage of correct predictions: (54+557)/(54+557+48+430) = 56.1%. 
# False Positive:  48/(557+48) = 7.9%. 
# False Negative: 430/(430+54)= 88.8%
#
# (d) 
#
Weekly.training=subset(Weekly,Year>=1990 & Year<=2008)
Weekly.testing=subset(Weekly,Year<1990 | Year>2008)
glm.fit1<-glm(Direction ~ Lag2,data=Weekly.training,family=binomial)
glm.probs=predict(glm.fit1,Weekly.testing,type="response")
glm.pred=rep("Down",nrow(Weekly.testing))
glm.pred[glm.probs>0.5]="Up"
table(glm.pred,Weekly.testing$Direction)
mean(glm.pred==Weekly.testing$Direction)


#
# (e) 
#
library(MASS)
lda.fit = lda(Direction ~ Lag2, data = Weekly.training)
lda.pred = predict(lda.fit, Weekly.testing)
table(lda.pred$class, Weekly.testing$Direction)
mean(lda.pred$class == Weekly.testing$Direction)

#
# (f) 
#
library(MASS)
qda.fit = qda(Direction ~ Lag2, data = Weekly.training)
qda.pred = predict(qda.fit, Weekly.testing)
table(qda.pred$class, Weekly.testing$Direction)
mean(qda.pred$class == Weekly.testing$Direction)

#
# (g) 
#
library(class)
train = (Year < 2009 & Year>1989)
train.X = as.matrix(Lag2[train])
test.X = as.matrix(Lag2[!train])
train.Direction = Direction[train]
set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Direction.0910)
mean(knn.pred == Direction.0910)

#
# (g) 
#
#Logistic regression and LDA methods provide similar test error rates.

# i would skip this part as this is merely chaning the parameters used in previous exercises.

#-----------------------
# Exercise 11`
#-----------------------

#
# (a) 
#
Auto$mpg01 = rep(0, length(Auto$mpg))
Auto$mpg01[Auto$mpg > median(Auto$mpg)] = 1

#
# (b) 
#
pairs(Auto)
cor(Auto[, -9])

# cylinders, displacement, horsepower and weight show strong correlation to mpg01

#
# (c) 
#
train=row(Auto)[,1]%%2==0
test = !train
Auto.train = Auto[train, ]
Auto.test = Auto[test, ]
mpg01.test = Auto$mpg01[test]

#
# (d) 
#
library(MASS)
lda.fit = lda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, 
              subset = train)
lda.pred = predict(lda.fit, Auto.test)
mean(lda.pred$class != mpg01.test)


#
# (e) 
#
qda.fit = qda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, 
              subset = train)
qda.pred = predict(qda.fit, Auto.test)
mean(qda.pred$class != mpg01.test)

#
# (f) 
#
glm.fit = glm(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, 
              family = binomial, subset = train)
glm.probs = predict(glm.fit, Auto.test, type = "response")
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1
mean(glm.pred != mpg01.test)

#
# (g) 
#
library(class)
attach(Auto)
train.X = cbind(cylinders, weight, displacement, horsepower)[train, ]
test.X = cbind(cylinders, weight, displacement, horsepower)[test, ]
train.mpg01 = mpg01[train]
set.seed(1)
# KNN(k=1)
knn.pred = knn(train.X, test.X, train.mpg01, k = 1)
mean(knn.pred != mpg01.test)
knn.pred = knn(train.X, test.X, train.mpg01, k = 10)
mean(knn.pred != mpg01.test)

knn.pred = knn(train.X, test.X, train.mpg01, k = 100)
mean(knn.pred != mpg01.test)

#a large value of K yields smaller test error

#-----------------------
# Exercise 12
#-----------------------

#
# (a) 
#
Power=function(){
  print(2**3)
}
Power()

#
# (b) 
#

Power2=function(x,a){
  print(x**a)
}
#
# (c) 
#
Power2(10,3)
Power2(8,17)
Power2(131,3)

#
# (d) 
#
Power3=function(x,a){
  return(x**a)
}
#
# (e) 
#
x=seq(10)
plot(x,Power3(x,2),log = "xy", ylab =expression("log of y=x^2"), xlab = "Log of x", 
     main = "Log of x^2 versus Log of x")
#
# (f) 
#
PlotPower=function(x,a)
{
  plot(x,x^a)
}
PlotPower(1:10,3)

#-----------------------
# Exercise 13
#-----------------------
library(ISLR)
attach(Boston)
crime01 = rep(0, length(crim))
crime01[crim > median(crim)] = 1
Boston = data.frame(Boston, crime01)

train = 1:(dim(Boston)[1]/2)
test = (dim(Boston)[1]/2 + 1):dim(Boston)[1]
Boston.train = Boston[train, ]
Boston.test = Boston[test, ]
crime01.test = crime01[test]
# logistic regression
glm.fit = glm(crime01 ~ . - crime01 - crim, data = Boston, family = binomial, 
              subset = train)
glm.probs = predict(glm.fit, Boston.test, type = "response")
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1
mean(glm.pred != crime01.test)

# LDA
lda.fit = lda(crime01 ~ . - crime01 - crim, data = Boston, subset = train)
lda.pred = predict(lda.fit, Boston.test)
mean(lda.pred$class != crime01.test)

# KNN
library(class)
train.X = cbind(zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio, black, 
                lstat, medv)[train, ]
test.X = cbind(zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio, black, 
               lstat, medv)[test, ]
train.crime01 = crime01[train]
set.seed(1)
# KNN(k=1)
knn.pred = knn(train.X, test.X, train.crime01, k = 1)
mean(knn.pred != crime01.test)
