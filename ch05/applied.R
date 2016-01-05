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
# Chapter 5: Applied
##################
library(ISLR)
# Set your working directory here; uncomment here if necessary
#setwd("~/your_path/statistical-learning/ch05")
#-----------------------
# Exercise 5
#-----------------------

#
# (a) 
#
set.seed(1)
glm.fit=glm(default~income+balance,data=Default,family =binomial)

#
# (b) 
#

# i
train=row(Default)[,1]%%2==0
Default.training=Default[train,]
Default.testing=Default[!train,]
# ii
glm.fit1=glm(default~income+balance,data=Default,subset=train,family =binomial)
# iii
glm.pred=rep("No",nrow(Default.testing))
glm.probs=predict(glm.fit1,Default.testing,type="response")
glm.pred[glm.probs>.5]="Yes"
table(glm.pred,Default.testing$default)
# iv
mean(glm.pred!=Default.testing$default)

#
# (c) 
#

# try 1
train=row(Default)[,1]%%2==1
Default.training=Default[train,]
Default.testing=Default[!train,]
# ii
glm.fit2=glm(default~income+balance,data=Default,subset=train,family =binomial)
# iii
glm.pred=rep("No",nrow(Default.testing))
glm.probs=predict(glm.fit2,Default.testing,type="response")
glm.pred[glm.probs>.5]="Yes"
table(glm.pred,Default.testing$default)
# iv
mean(glm.pred!=Default.testing$default)

# try 2
train=row(Default)[,1]>0.5*nrow(Default)
Default.training=Default[train,]
Default.testing=Default[!train,]
# ii
glm.fit2=glm(default~income+balance,data=Default,subset=train,family =binomial)
# iii
glm.pred=rep("No",nrow(Default.testing))
glm.probs=predict(glm.fit2,Default.testing,type="response")
glm.pred[glm.probs>.5]="Yes"
table(glm.pred,Default.testing$default)
# iv
mean(glm.pred!=Default.testing$default)

# try 3
train=row(Default)[,1]<0.5*nrow(Default)
Default.training=Default[train,]
Default.testing=Default[!train,]
# ii
glm.fit2=glm(default~income+balance,data=Default,subset=train,family =binomial)
# iii
glm.pred=rep("No",nrow(Default.testing))
glm.probs=predict(glm.fit2,Default.testing,type="response")
glm.pred[glm.probs>.5]="Yes"
table(glm.pred,Default.testing$default)
# iv
mean(glm.pred!=Default.testing$default)

#error rates are :  0.024, 0.0282 and 0.02579484 respectively. similar error with variations.

#
# (d) 
#
train=row(Default)[,1]%%2==0
Default.training=Default[train,]
Default.testing=Default[!train,]
# ii
glm.fit2=glm(default~income+balance+student,data=Default,subset=train,family =binomial)
# iii
glm.pred=rep("No",nrow(Default.testing))
glm.probs=predict(glm.fit2,Default.testing,type="response")
glm.pred[glm.probs>.5]="Yes"
table(glm.pred,Default.testing$default)
# iv
mean(glm.pred!=Default.testing$default)
# 0.0238
# not really improving the model prediction 


#-----------------------
# Exercise 6
#-----------------------

#
# (a) 
#

library(ISLR)
set.seed(1)
glm.fit=glm(default~income+balance,data=Default,family =binomial)
summary(glm.fit)
#
# (b) 
#

boot.fn=function(data,index)
{
  return (coef(glm(default~income+balance,data=data,subset=index,family=binomial)))
}
#
# (c) 
#
boot(Default,boot.fn,1000)
#ORDINARY NONPARAMETRIC BOOTSTRAP


#Call:
#  boot(data = Default, statistic = boot.fn, R = 1000)


#Bootstrap Statistics :
#  original        bias     std. error
#t1* -1.154047e+01 -2.623092e-02 4.343749e-01
#t2*  2.080898e-05  4.777330e-08 4.949036e-06
#t3*  5.647103e-03  1.366848e-05 2.307172e-04
#
# (d) 
#
summary(glm(default~income+balance,data=Default,family=binomial))$coef
#Estimate   Std. Error    z value      Pr(>|z|)
#(Intercept) -1.154047e+01 4.347564e-01 -26.544680 2.958355e-155
#income       2.080898e-05 4.985167e-06   4.174178  2.990638e-05
#balance      5.647103e-03 2.273731e-04  24.836280 3.638120e-136

#same parameter estimates and simliar error variance

#-----------------------
# Exercise 7
#-----------------------
library(ISLR)
#
# (a) 
#
pred.error=rep(0,)
for (i in 1:nrow(Weekly))
{
  train=row(Default)[,1]!=i
  glm.fit=glm(Direction~Lag1+Lag2,data=Weekly,subset=train,family=binomial)
  glm.pred=predict(glm.fit,Weekly[!train,],type="response")
  if glm.pred>.5
  {}
}
glm.fit=glm(Direction~Lag1+Lag2,data=Weekly,family=binomial)
summary(glm.fit)
#
# (b) 
#
train=row(Default)[,1]!=1
glm.fit2=glm(Direction~Lag1+Lag2,data=Weekly,subset=train,family=binomial)
summary(glm.fit2)

#-----------------------
# Exercise 8
#-----------------------

#
# (a) 
#
set.seed(1)
y=rnorm(100)
x=rnorm(100)
y=x-2*x^2+rnorm(100)

#n=100,p=1
# y=-2x^2+x+e

#
# (b) 
#
plot(x,y)

# it has a parabolic trend. 

#
# (c) 
#
library(boot)
set.seed(1)
y=rnorm(100)
x=rnorm(100)
y=2*x^2+rnorm(100,0,0.1)
df=data.frame(x,y)
for (i in 1:4)
{
  print(paste("order=",i))
  glm.fit=glm(y~poly(x,i),data=df)
  cv.err=cv.glm(df,glm.fit)
  print(cv.err$delta)
}
#
# (d) 
#
set.seed(10)

for (i in 1:4)
{
  print(paste("order=",i))
  glm.fit=glm(y~poly(x,i),data=df)
  print(summary(glm.fit))
  cv.err=cv.glm(df,glm.fit)
  print(cv.err$delta)
}
#exactly same as (c) as LOOCV is deterministic.
#
# (e) 
#
# model (ii) has the lowest LOOCV error. Yes. it is closest to the true model. 
#
# (f) 
#

for (i in 1:4)
{
  print(paste("order=",i))
  glm.fit=glm(y~poly(x,i),data=df)
  print(summary(glm.fit))
  cv.err=cv.glm(df,glm.fit)
  print(cv.err$delta)
}
# based on error estimate, both approaches show order 2 has the best fit. in fact, when using order=1
# the parameter doesn't show significance. It only reaches to significance when having order=2 items.
#-----------------------
# Exercise 9
#-----------------------

library(MASS)

#
# (a) 
#
mean(Boston$medv)

#
# (b) 
#
sd(Boston$medv)/sqrt(nrow(Boston))

#
# (c) 
#
boot.fn=function(data,index)
  return(mean(data[index]))
library(boot)
bstrap=boot(Boston$medv,boot.fn,1000)
bstrap
# 
##ORDINARY NONPARAMETRIC BOOTSTRAP


##Call:
 ## boot(data = Boston$medv, statistic = boot.fn, R = 1000)


##Bootstrap Statistics :
##  original      bias    std. error
##t1* 22.53281 0.002099605   0.4113565

#
# (d) 
#
t.test(Boston$medv)
## 95 percent confidence interval:
##  21.72953 23.33608

c(bstrap$t0 - 2 * 0.4113, bstrap$t0 + 2 * 0.4113)
##[1] 21.71021 23.35541

#
# (e) 
#
median(Boston$medv)

#
# (f) 
#
boot.fn1=function(data,index)
  return(median(data[index]))
bstrap=boot(Boston$medv,boot.fn1,1000)
##ORDINARY NONPARAMETRIC BOOTSTRAP


##Call:
##  boot(data = Boston$medv, statistic = boot.fn1, R = 1000)


##Bootstrap Statistics :
##  original   bias    std. error
##t1*     21.2 -0.00225   0.3746436

# small variation

#
# (f) 
#

quantile(Boston$medv,0.1)


boot.fn3 = function(data, index)
  return(quantile(data[index], 0.1))
boot(Boston$medv, boot.fn3, 1000)

# Tenth-percentile of 12.75 with SE of 0.4936215