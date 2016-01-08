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
# Chapter 6: Applied
##################
# Set your working directory here; uncomment here if necessary
#setwd("~/your_path/statistical-learning/ch06")
#-----------------------
# Exercise 8
#-----------------------

#---------
# a
#---------
set.seed(1)
n=100
X=rnorm(n)
e=rnorm(n)

#---------
# b
#---------
beta0=20
beta1=3.2
beta2=1.5
beta3=2.1
Y=beta0+beta1*X+beta2*X^2+beta3*X^3+e

#---------
# c
#---------
df=data.frame(Y,X)
library(leaps)
regfit.full=regsubsets(Y~poly(X,10,raw=T),df,nvmax=10)
reg.summary=summary(regfit.full)
reg.summary$cp
reg.summary$bic
reg.summary$adjr2
par(mfrow=c(3,1))
plot(reg.summary$cp, xlab="Subset Size", ylab="Cp", pch=20, type="l")
points(which.min(reg.summary$cp), reg.summary$cp[which.min(reg.summary$cp)], pch=4, col="red", lwd=7)
plot(reg.summary$bic, xlab="Subset Size", ylab="BIC", pch=20, type="l")
points(which.min(reg.summary$bic), reg.summary$bic[which.min(reg.summary$bic)], pch=4, col="red", lwd=7)
plot(reg.summary$adjr2, xlab="Subset Size", ylab="Adjusted R2", pch=20, type="l")
points(which.max(reg.summary$adjr2),reg.summary$adjr2[which.max(reg.summary$adjr2)], pch=4, col="red", lwd=7)

#---------
# d
#---------

#using forward stepwise selection
regfit.forward=regsubsets(Y~poly(X,10,raw=T),df,nvmax=10,method="forward")
reg.summary=summary(regfit.forward)
reg.summary$cp
reg.summary$bic
reg.summary$adjr2
par(mfrow=c(3,1))
plot(reg.summary$cp, xlab="Subset Size", ylab="Cp", pch=20, type="l")
points(which.min(reg.summary$cp), reg.summary$cp[which.min(reg.summary$cp)], pch=4, col="red", lwd=7)
plot(reg.summary$bic, xlab="Subset Size", ylab="BIC", pch=20, type="l")
points(which.min(reg.summary$bic), reg.summary$bic[which.min(reg.summary$bic)], pch=4, col="red", lwd=7)
plot(reg.summary$adjr2, xlab="Subset Size", ylab="Adjusted R2", pch=20, type="l")
points(which.max(reg.summary$adjr2),reg.summary$adjr2[which.max(reg.summary$adjr2)], pch=4, col="red", lwd=7)
#using backward stepwise selection
regfit.backward=regsubsets(Y~poly(X,10,raw=T),df,nvmax=10,method="backward")
reg.summary=summary(regfit.backward)
reg.summary$cp
reg.summary$bic
reg.summary$adjr2
par(mfrow=c(3,1))
plot(reg.summary$cp, xlab="Subset Size", ylab="Cp", pch=20, type="l")
points(which.min(reg.summary$cp), reg.summary$cp[which.min(reg.summary$cp)], pch=4, col="red", lwd=7)
plot(reg.summary$bic, xlab="Subset Size", ylab="BIC", pch=20, type="l")
points(which.min(reg.summary$bic), reg.summary$bic[which.min(reg.summary$bic)], pch=4, col="red", lwd=7)
plot(reg.summary$adjr2, xlab="Subset Size", ylab="Adjusted R2", pch=20, type="l")
points(which.max(reg.summary$adjr2),reg.summary$adjr2[which.max(reg.summary$adjr2)], pch=4, col="red", lwd=7)

#---------
# e
#---------
library(glmnet)
xmat = model.matrix(y~poly(x, 10, raw=T), data=data.full)[, -1]
mod.lasso = cv.glmnet(xmat, Y, alpha=1)
best.lambda = mod.lasso$lambda.min
best.lambda
plot(mod.lasso)
# Next fit the model on entire data using best lambda
best.model = glmnet(xmat, Y, alpha=1)
predict(best.model, s=best.lambda, type="coefficients")

#---------
# f
#---------
Y=beta0+1.23*X^7+e
data.full = data.frame("y" = Y, "x" = X)
mod.full = regsubsets(y~poly(x, 10, raw=T), data=data.full, nvmax=10)
mod.summary = summary(mod.full)

# Find the model size for best cp, BIC and adjr2
which.min(mod.summary$cp)
which.min(mod.summary$bic)
which.max(mod.summary$adjr2)
coefficients(mod.full, id=1)
coefficients(mod.full, id=2)
coefficients(mod.full, id=4)

#bic picks the best model

xmat = model.matrix(Y~poly(X, 10, raw=T), data=data.full)[, -1]
mod.lasso = cv.glmnet(xmat, Y, alpha=1)
best.lambda = mod.lasso$lambda.min
best.lambda
best.model = glmnet(xmat, Y, alpha=1)
predict(best.model, s=best.lambda, type="coefficients")

# lasso correctly finds the best model and yields good estimate of intercept as well. 


#-----------------------
# Exercise 9
#-----------------------
#---------
# a
#---------
library(ISLR)
names(College)
set.seed(1)
train.size = dim(College)[1] / 2
train = sample(1:dim(College)[1], train.size)
test = -train
College.train = College[train, ]
College.test = College[test, ]

#---------
# b
#---------
lm.fit=lm(Apps~., data=College.train)
lm.pred = predict(lm.fit, College.test)
mean((College.test$Apps - lm.pred)^2)
#1108531

#---------
# c
#---------
#ridge regression
library(glmnet)
train.mat = model.matrix(Apps~., data=College.train)
test.mat = model.matrix(Apps~., data=College.test)
grid = 10 ^ seq(4, -2, length=100)
mod.ridge = cv.glmnet(train.mat, College.train$Apps, alpha=0, lambda=grid, thresh=1e-12)
lambda.best = mod.ridge$lambda.min
lambda.best
ridge.pred = predict(mod.ridge, newx=test.mat, s=lambda.best)
mean((College.test$Apps - ridge.pred)^2)
#1108512
#---------
# d
#---------
#lasso
mod.lasso = cv.glmnet(train.mat, College.train$Apps, alpha=1, lambda=grid, thresh=1e-12)
lambda.best = mod.lasso$lambda.min
lambda.best
lasso.pred = predict(mod.lasso, newx=test.mat, s=lambda.best)
mean((College.test$Apps - lasso.pred)^2)
#1028718
#---------
# e
#---------
library(pls)
pcr.fit = pcr(Apps~., data=College.train, scale=T, validation="CV")
validationplot(pcr.fit, val.type="MSEP")
pcr.pred = predict(pcr.fit, College.test, ncomp=10)
mean((College.test$Apps - data.frame(pcr.pred))^2)
#1505718

#---------
# f
#---------
pls.fit = plsr(Apps~., data=College.train, scale=T, validation="CV")
validationplot(pls.fit, val.type="MSEP")
pls.pred = predict(pls.fit, College.test, ncomp=10)
mean((College.test$Apps - data.frame(pls.pred))^2)
#1134531

#---------
# g
#---------
test.avg = mean(College.test$Apps)
lm.test.r2 = 1 - mean((College.test$Apps - lm.pred)^2) /mean((College.test$Apps - test.avg)^2)
ridge.test.r2 = 1 - mean((College.test$Apps - ridge.pred)^2) /mean((College.test$Apps - test.avg)^2)
lasso.test.r2 = 1 - mean((College.test$Apps - lasso.pred)^2) /mean((College.test$Apps - test.avg)^2)
pcr.test.r2 = 1 - mean((College.test$Apps - data.frame(pcr.pred))^2) /mean((College.test$Apps - test.avg)^2)
pls.test.r2 = 1 - mean((College.test$Apps - data.frame(pls.pred))^2) /mean((College.test$Apps - test.avg)^2)
barplot(c(lm.test.r2, ridge.test.r2, lasso.test.r2, pcr.test.r2, pls.test.r2), col="red", names.arg=c("OLS", "Ridge", "Lasso", "PCR", "PLS"), main="Test R-squared")


#-----------------------
# Exercise 10
#-----------------------

#---------
# a
#---------
set.seed(1)
p = 20
n = 1000
x = matrix(rnorm(n*p), n, p)
B = rnorm(p)
B[3] = 0
B[4] = 0
B[9] = 0
B[19] = 0
B[10] = 0
eps = rnorm(p)
y = x %*% B + eps
#---------
# b
#---------
train = sample(seq(1000), 100, replace = FALSE)

y.train = y[train,]
y.test = y[-train,]
x.train = x[train,]
x.test = x[-train,]

#---------
# c
#---------
df=data.frame(x=x,y=y)
best.fit=regsubsets(y~.,data=df[train,],nvmax=p)
x.mat=model.matrix(y.train~.,data=df[train,])
val.errors=rep(NA,p)
for (i in 1:p){
  coefi=coef(best.fit,id=i)
  pred=x.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((y.train-pred)^2)
}
plot(val.errors, ylab="Training MSE", pch=19, type="b")
#---------
# d
#---------
df=data.frame(x=x,y=y)
best.fit=regsubsets(y~.,data=df[train,],nvmax=p)
x.mat=model.matrix(y~.,data=df[-train,])
val.errors=rep(NA,p)
for (i in 1:p){
  coefi=coef(best.fit,id=i)
  pred=x.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((y.test-pred)^2)
}
plot(val.errors, ylab="Training MSE", pch=19, type="b")
#---------
# e
#---------
which.min(val.errors)

#---------
# f
#---------
coef(best.fit,id=which.min(val.errors))
#all predictors are included in the model while some of them actually didn't contribute to the signal 

#---------
# g
#---------
x_cols = colnames(x, do.NULL=FALSE, prefix="x.")
val.errors = rep(NA, p)
a = rep(NA, p)
b = rep(NA, p)
for (i in 1:p) {
  coefi = coef(best.fit, id=i)
  a[i] = length(coefi)-1
  b[i] = sqrt(
    sum((B[x_cols %in% names(coefi)] - coefi[names(coefi) %in% x_cols])^2) +
      sum(B[!(x_cols %in% names(coefi))])^2)
}
plot(x=a, y=b, xlab="number of coefficients",
     ylab="error between estimated and true coefficients")
which.min(b)


#-----------------------
# Exercise 11
#-----------------------
set.seed(1)
library(MASS)
library(leaps)
library(glmnet)
#---------
# a
#---------

# best subset
predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}

k = 10
p = ncol(Boston)-1
folds = sample(rep(1:k, length=nrow(Boston)))
cv.errors = matrix(NA, k, p)
for (i in 1:k) {
  best.fit = regsubsets(crim~., data=Boston[folds!=i,], nvmax=p)
  for (j in 1:p) {
    pred = predict(best.fit, Boston[folds==i, ], id=j)
    cv.errors[i,j] = mean((Boston$crim[folds==i] - pred)^2)
  }
}
rmse.cv = sqrt(apply(cv.errors, 2, mean))
plot(rmse.cv, pch=19, type="b")
which.min(rmse.cv)
rmse.cv[which.min(rmse.cv)]
#lasso
x = model.matrix(crim~.-1, data=Boston)
y = Boston$crim
cv.lasso = cv.glmnet(x, y, type.measure="mse")
plot(cv.lasso)
coef(cv.lasso)
sqrt(cv.lasso$cvm[cv.lasso$lambda == cv.lasso$lambda.1se])

#ridge
x = model.matrix(crim~.-1, data=Boston)
y = Boston$crim
cv.ridge = cv.glmnet(x, y, type.measure="mse", alpha=0)
plot(cv.ridge)
coef(cv.ridge)
sqrt(cv.ridge$cvm[cv.ridge$lambda == cv.ridge$lambda.1se])

#pcr
library(pls)
pcr.fit = pcr(crim~., data=Boston, scale=TRUE, validation="CV")
summary(pcr.fit)
#---------
# b
#---------
#bset subset provides the best model in terms of cross validation
#---------
# c
#---------
#no. the best model has 9 features. 