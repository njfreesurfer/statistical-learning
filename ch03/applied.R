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
# Chapter 3: Applied
##################

#-----------------------
# Exercise 8
#-----------------------




# Set your working directory here; uncomment here if necessary
#setwd("~/your_path/statistical-learning/ch03")
Auto = read.csv("../data/Auto.csv", header=T, na.strings="?")
Auto = na.omit(Auto)
summary(Auto)
#---------
# a
#---------
attach(Auto)
lm.fit = lm(mpg ~ horsepower)
summary(lm.fit)

# i. yes
# ii.a very significant relationship (exteremly small p-value)
# iii. negative. meaning that the greater the horsepower is the less is mpg.
# iv. 
#with prediction interval
predict(lm.fit,data.frame(horsepower=98),interval="prediction")
#with confidence interval
predict(lm.fit,data.frame(horsepower=98),interval="confidence")

#---------
# b
#---------
plot(horsepower,mpg)
abline(lm.fit,col="red")
#---------
# c
#---------
par(mfrow=c(2,2))
plot(lm.fit)

#-----------------------
# Exercise 9
#-----------------------
#---------
# a
#---------
pairs(Auto)
#---------
# b
#---------
cor(subset(Auto, select=-name))
#---------
# c
#---------
lm.model1<- lm(mpg~.-name, data=Auto)
summary(lm.model1)
#i. Yes. we hae a significnat F stat
#ii. displacement,weight, year,and origin.
#iii. the coefficient is 0.7>0. this suggests as technoglogy improves, the mpg
#      increaes accordingly
#---------
# d
#---------
par(mfrow=c(2,2))
plot(lm.model1)
# Residual plots suggest some curvilinear pattern. leverage plot shows row 14 as a high leverage entry
#---------
# e
#---------
lm.model2<- lm(mpg~cylinders*displacement+displacement*weight)
summary(lm.model2)

#---------
# f
#---------
lm.model3<-lm(log(mpg)~.-name,data=Auto)
summary(lm.model3)
plot(lm.model3)
# residual plot shows a better fit by log transform

#-----------------------
# Exercise 10
#-----------------------

#load dataset
library(ISLR)
attach(Carseats)

#---------
# a
#---------
lm.model<-lm(Sales~Price+Urban+US)
summary(lm.model)
#---------
# b
#---------
# Price

# The linear regression suggests a relationship between price and sales given the 
# low p-value of the t-statistic. The coefficient states a negative relationship 
# between Price and Sales: as Price increases, Sales decreases.

# UrbanYes

#The linear regression suggests that there isn't a relationship between the location
# of the store and the number of sales based on the high p-value of the t-statistic.

#USYes

#The linear regression suggests there is a relationship between whether the store is
# in the US or not and the amount of sales. The coefficient states a positive 
# relationship between USYes and Sales: if the store is in the US, the sales will 
# increase by approximately 1201 units.

#---------
# c
#---------
# Sales = 13.04  -0.05 Price  -0.02 UrbanYes + 1.20 USYes

#---------
# d
#---------

# based on a significance level of p=0.05, we can reject the H0 for Price and USYes

#---------
# e
#---------
lm.model2<-lm(Sales~Price+US)
summary(lm.model2)

#---------
# f
#---------
#this model fits slightly than the previous model. 

#---------
# g
#---------
confint(lm.model2)
#---------
# h
#---------
par(mfrow=c(2,2))
plot(lm.model2)
# a few points appear to be having high leverage

#-----------------------
# Exercise 11
#-----------------------

set.seed(1)
x=rnorm(100)
y=2*x+rnorm(100)
#---------
# a
#---------
lm.model1<-lm(y~x+0)
summary(lm.model1)
# of course, the result shows a strong effect of x on y. 
#---------
# b
#---------
lm.model2<-lm(x~y+0)
summary(lm.model2)
#---------
# c
#---------
# both model suggest a linear relationship between x and y. the only difference is the betas
# are difference because of the formula difference
#---------
# d
#---------
t=sum(x*y)*(sqrt(length(x)-1))/(sqrt(sum(x^2)*sum(y^2)-sum(x*y)^2))
print(t)
#this prints 18.72593 which is the same as the one reported in summary
#---------
# e
#---------
# if we switch the x and y in the formula,the formula remains the same, so the t-statistics
# should be same in both models.
#---------
# f
#---------
lm.model3<-lm(y~x)
summary(lm.model3)
lm.model4<-lm(x~y)
summary(lm.model4)
# check summary for the t-statistics

#-----------------------
# Exercise 12
#-----------------------
#---------
# a
#---------
# when the sum squares of x equals the sum squares of y.
#---------
# b
#---------
set.seed(1)
x = rnorm(100)
y = 2*x
lm.fit = lm(y~x+0)
lm.fit2 = lm(x~y+0)
coef(lm.fit)
coef(lm.fit2)

#---------
# c
#---------
set.seed(1)
x <- rnorm(100)
y <- -sample(x, 100)
sum(x^2)
sum(y^2)
lm.fit <- lm(y~x+0)
lm.fit2 <- lm(x~y+0)
coef(lm.fit)
coef(lm.fit2)

#-----------------------
# Exercise 13
#-----------------------
#---------
# a
#---------
set.seed(1)
x <- rnorm(100)
#---------
# b
#---------
eps<-rnorm(100,0,sqrt(0.25))
#---------
# c
#---------
y=-1+0.5*x+eps
length(y)
lm.model<-lm(y~x)
coef(lm.model)
#---------
# d
#---------
plot(x,y)
#linear relationship between x and y but very noisy. 
#---------
# e
#---------
lm.model<-lm(y~x)
coef(lm.model)
#our estimates are pretty close to the true values
#---------
# f
#---------
plot(x,y)

abline(lm.model, lwd=1, col=2)
abline(-1, 0.5, lwd=1, col=3)
legend(-1, legend = c("model fit", "pop. regression"), col=2:3, lwd=1)

#---------
# g
#---------
lm.model2<-lm(y~x+I(x^2))
summary(lm.model2)
# R square increaes with the additional x^2 term.
#---------
# h
#---------
set.seed(1)
x <- rnorm(100)
eps<-rnorm(100,0,sqrt(0.1))
y=-1+0.5*x+eps
length(y)
lm.model3<-lm(y~x)
summary(lm.model3)

# R square is higher because the model can explain the same amount of variance due to
# the decreased variance form the error term.

#---------
# i
#---------
set.seed(1)
x <- rnorm(100)
eps<-rnorm(100,0,sqrt(5))
y=-1+0.5*x+eps
length(y)
lm.model4<-lm(y~x)
summary(lm.model4)
# R square is lower because the model cannot explain the same amount of variance due to
# the increased variance form the error term.

#---------
# j
#---------
confint(lm.model)
confint(lm.model3)
confint(lm.model4)
#All intervals seem to be centered on approximately 0.5, with the second fit's interval
# being narrower than the first fit's interval and the last fit's interval being wider 
# than the first fit's interval.

#-----------------------
# Exercise 14
#-----------------------

#---------
# a
#---------
set.seed(1)
x1=runif(100)
x2=0.5*x1+rnorm(100)/10
y=2+2*x1+0.3*x2+rnorm(100)

#---------
# b
#---------
cor(x1,x2)
plot(x1,x2)

#---------
# c
#---------
lm.model<-lm(y~x1+x2)
summary(lm.model)
#close to true betas but a lot of errors! we can only reject beta_1 not beta_2

#---------
# d
#---------
lm.model2<-lm(y~x1)
summary(lm.model2)
# yes
#---------
# e
#---------
lm.model3<-lm(y~x2)
summary(lm.model3)
# yes
#---------
# f
#---------
#No, because x1 and x2 have collinearity, it is hard to distinguish their effects when 
# regressed upon together. When they are regressed upon separately, the linear 
# relationship between y and each predictor is indicated more clearly.
#---------
# g
#---------
x1 = c(x1, 0.1)
x2 = c(x2, 0.8)
y = c(y, 6)
lm.fit1 = lm(y~x1+x2)
summary(lm.fit1)
lm.fit2 = lm(y~x1)
summary(lm.fit2)
lm.fit3 = lm(y~x2)
summary(lm.fit3)
#In the first model, it shifts x1 to statistically insignificance and shifts x2 to 
# statistiscal significance from the 
# change in p-values between the two linear regressions.


#-----------------------
# Exercise 15
#-----------------------
#---------
# a
#---------
library(MASS)
summary(Boston)
Boston$chas <- factor(Boston$chas, labels = c("N","Y"))
summary(Boston)
attach(Boston)
lm.zn = lm(crim~zn)
summary(lm.zn) # yes
lm.indus = lm(crim~indus)
summary(lm.indus) # yes
lm.chas = lm(crim~chas) 
summary(lm.chas) # no
lm.nox = lm(crim~nox)
summary(lm.nox) # yes
lm.rm = lm(crim~rm)
summary(lm.rm) # yes
lm.age = lm(crim~age)
summary(lm.age) # yes
lm.dis = lm(crim~dis)
summary(lm.dis) # yes
lm.rad = lm(crim~rad)
summary(lm.rad) # yes
lm.tax = lm(crim~tax)
summary(lm.tax) # yes
lm.ptratio = lm(crim~ptratio)
summary(lm.ptratio) # yes
lm.black = lm(crim~black)
summary(lm.black) # yes
lm.lstat = lm(crim~lstat)
summary(lm.lstat) # yes
lm.medv = lm(crim~medv)
summary(lm.medv) # yes

#---------
# b
#---------
lm.all = lm(crim~., data=Boston)
summary(lm.all)
#we can reject H0 for zn,dis,rad,black and medv.


#---------
# c
#---------

x = c(coefficients(lm.zn)[2],
      coefficients(lm.indus)[2],
      coefficients(lm.chas)[2],
      coefficients(lm.nox)[2],
      coefficients(lm.rm)[2],
      coefficients(lm.age)[2],
      coefficients(lm.dis)[2],
      coefficients(lm.rad)[2],
      coefficients(lm.tax)[2],
      coefficients(lm.ptratio)[2],
      coefficients(lm.black)[2],
      coefficients(lm.lstat)[2],
      coefficients(lm.medv)[2])
y = coefficients(lm.all)[2:14]
plot(x, y)

#---------
# d
#---------
#fit a linear model up to order 3 for each predictor ,then check the p-values 
# associated with each beta. list if significance level is below the 0.05.
lm.zn = lm(crim~poly(zn,3))
summary(lm.zn) # 1, 2
lm.indus = lm(crim~poly(indus,3))
summary(lm.indus) # 1, 2, 3
# lm.chas = lm(crim~poly(chas,3)) : qualitative predictor
lm.nox = lm(crim~poly(nox,3))
summary(lm.nox) # 1, 2, 3
lm.rm = lm(crim~poly(rm,3))
summary(lm.rm) # 1, 2
lm.age = lm(crim~poly(age,3))
summary(lm.age) # 1, 2, 3
lm.dis = lm(crim~poly(dis,3))
summary(lm.dis) # 1, 2, 3
lm.rad = lm(crim~poly(rad,3))
summary(lm.rad) # 1, 2
lm.tax = lm(crim~poly(tax,3))
summary(lm.tax) # 1, 2
lm.ptratio = lm(crim~poly(ptratio,3))
summary(lm.ptratio) # 1, 2, 3
lm.black = lm(crim~poly(black,3))
summary(lm.black) # 1
lm.lstat = lm(crim~poly(lstat,3))
summary(lm.lstat) # 1, 2
lm.medv = lm(crim~poly(medv,3))
summary(lm.medv) # 1, 2, 3