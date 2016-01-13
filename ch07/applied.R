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
# Chapter 7: Applied
##################
library(ISLR)
# Set your working directory here; uncomment here if necessary
#setwd("~/your_path/statistical-learning/ch05")
#-----------------------
# Exercise 6
#-----------------------

#
# (a) 
#
library(boot)
#set seed 
set.seed(11)
# run cross-validation
N=15
cv.error=rep(0,N)
for (i in 1:N)
{
  glm.fit=glm(wage~poly(age,i),data=Wage)
  cv.error[i]=cv.glm(Wage,glm.fit,K=10)$delta[1]
}
#plot the cross-validation error
plot(cv.error,type="l")
points(which.min(cv.error),cv.error[which.min(cv.error)],pch=4,col='red',lwd=7)

# degree 6 has the minimal error. 
d=which.min(cv.error)
best.fit=lm(wage~poly(age,d),data=Wage)
agelims = range(Wage$age)
age.grid = seq(from=agelims[1], to=agelims[2],length=100)
pred = predict(best.fit, data.frame(age=age.grid))
plot(wage~age, data=Wage, col="darkgrey")
lines(age.grid, pred, col="blue", lwd=2)
#it is probably easier to plot with ggplot
#library(ggplot2)
#g=ggplot()+geom_point(aes(x=age,y=wage),data=Wage)
#g+geom_smooth(aes(x=age,y=wage),data=Wage,se=TRUE,method="lm",formula=y~poly(x,d))+
 # labs(title="polynomial fit to Wage (order=6)")
# the result does not necessarily agree with the ANOVA because ANOVA computes the RSS on the entire 
# set and here we are using cross-validation.
#
# (b) 
#

N=10
cv.error=rep(NA,N)
for (i in 2:N)
{
  Wage$agecut=cut(Wage$age,i)
  glm.fit=glm(wage~agecut,data=Wage)
  cv.error[i]=cv.glm(Wage,glm.fit,K=10)$delta[1]
}
#plot the cross-validation error
plot(2:10,cv.error[-1],type="l",xlab="Number of cuts", ylab="CV error")
points(which.min(cv.error),cv.error[which.min(cv.error)],pch=4,col='red',lwd=7)

lm.fit = glm(wage~cut(age, 8), data=Wage)
agelims = range(Wage$age)
age.grid = seq(from=agelims[1], to=agelims[2],length=100)
lm.pred = predict(lm.fit, data.frame(age=age.grid))
plot(wage~age, data=Wage, col="darkgrey")
lines(age.grid, lm.pred, col="red", lwd=2)

#-----------------------
# Exercise 7
#-----------------------

# marital status and wage
plot(Wage$maritl,Wage$wage)
# jobclass and wage
plot(Wage$jobclass,Wage$wage)

# Fit polynomial model
model.1=lm(wage~maritl,data=Wage)
model.2=lm(wage~jobclass,data=Wage)
model.3=lm(wage~maritl+jobclass,data=Wage)
summary(model.1)$r.squared
summary(model.2)$r.squared
summary(model.3)$r.squared
deviance(model.1)
deviance(model.2)
deviance(model.3)

# Fit spline model
# not possible on categorical vairables

# Fit General Additive Models.
library(gam)
model.gam = gam(wage ~ maritl + jobclass + s(age, 4), data = Wage)
deviance(model.gam)

#-----------------------
# Exercise 8
#-----------------------

library(ISLR)
set.seed(1)
pairs(Auto)

#lets construct a polynomial model to see how displacement predicts the mpg
models=list()
RSS=rep(NA,10)
for (d in 1:10)
{
  models[[d]]=lm(mpg~poly(displacement,d),data=Auto)
  RSS[d]=deviance(models[[d]])
}
plot(RSS)
#points(which.min(RSS),RSS[which.min(RSS)],pch="x",cex=2,col="red")

anova(models[[1]],models[[2]],models[[3]],models[[4]],models[[5]])
# step functions
cv.errs = rep(NA, 10)
for (c in 2:10) {
  Auto$dis.cut = cut(Auto$displacement, c)
  fit = glm(mpg ~ dis.cut, data = Auto)
  cv.errs[c] = cv.glm(Auto, fit, K = 10)$delta[2]
}
which.min(cv.errs)
# splines
ibrary(splines)
cv.errs = rep(NA, 10)
for (df in 3:10) {
  fit = glm(mpg ~ ns(displacement, df = df), data = Auto)
  cv.errs[df] = cv.glm(Auto, fit, K = 10)$delta[2]
}
which.min(cv.errs)
#GAM

fit = gam(mpg ~ s(displacement, 4) + s(horsepower, 4), data = Auto)
summary(fit)


#-----------------------
# Exercise 9
#-----------------------
library(MASS)

#
# (a) 
#
fit=lm(nox~poly(dis,3),data=Boston)
dislim=range(Boston$dis)
dis.grid=seq(dislim[1],dislim[2],0.1)
pred=predict(fit,list(dis=dis.grid))
summary(fit)
plot(Boston$dis,Boston$nox)
lines(dis.grid,pred,col="red",lwd=2)
#
# (b) 
#
library(ggplot2)
lwd=1
g<-ggplot()+geom_point(aes(x=dis,y=nox),data=Boston,alpha=2/10,size=6)
g+geom_smooth(aes(x=dis,y=nox,color="0-bias"),method="lm",formula=y~poly(x,1),se=FALSE,size=lwd,data=Boston)+
  geom_smooth(aes(x=dis,y=nox,color="1-bias"),method="lm",formula=y~poly(x,2),se=FALSE,size=lwd,data=Boston)+
  geom_smooth(aes(x=dis,y=nox,color="2-bias"),method="lm",formula=y~poly(x,3),se=FALSE,size=lwd,data=Boston)+
  geom_smooth(aes(x=dis,y=nox,color="3-bias"),method="lm",formula=y~poly(x,4),se=FALSE,size=lwd,data=Boston)+
  geom_smooth(aes(x=dis,y=nox,color="4-bias"),method="lm",formula=y~poly(x,5),se=FALSE,size=lwd,data=Boston)+
  scale_colour_manual(name="Polynomial Fit",values = c("blue","pink","gray","black","gold"),
                      labels=c('order=1','order=2','order=3','order=4','order=5'),
                      guide = guide_legend(override.aes = list(size=c(0.5,0.5,0.5,0.5,0.5),shape=c(1,1,1,1,1),linetype = c( "solid","solid","solid","solid","solid"))))+
  labs (title = "Boston Data")+ 
  theme (legend.title = element_text (size = 15)) +
  ylab("Nox")+xlab("Dis")

#
# (c) 
#     

library(boot)
cv.error=rep(NA,15)
set.seed(1)
for (d in 1:15)
{
  model=glm(nox~poly(dis,d),data=Boston)
  cv.error[d] = cv.glm(Boston, model, K = 10)$delta[2]
}
plot(1:15, cv.error, xlab = "Degree",                                 ylab = "CV error", type = "l", pch = 20,                                                           
     lwd = 2)

#                                                                                                                    
# (d)                          
#
library(ISLR)
library(splines) 
library(MASS)
dislim=range(Boston$dis)
dis.grid=seq(dislim[1],dislim[2],0.1)
model=lm(nox~bs(dis,df=4,knots=c(4,7,11)),data=Boston)
pred = predict(model, list(dis = dis.grid))
plot(nox ~ dis, data = Boston, col = "darkgrey")
lines(dis.grid, pred, col = "red", lwd = 2)

#                                                                                                                    
# (d)                          
#
library(boot)
cv.error=rep(NA,10)
for (d in 1:10)
{
  lm.fit=glm(nox~poly(dis,d),data=Boston)
  cv.error[d]=cv.glm(Boston,lm.fit,K=10)$delta[2]
}
plot(cv.error)
which.min(cv.error)
                      
#-----------------------
# Exercise 10
#-----------------------

#                                                                                                                    
# (a)                          
#
library(ISLR)
library(leaps)
set.seed(1)
train = sample(length(Outstate), length(Outstate)/2)
test = -train

College.train = College[train, ]
College.test = College[test, ]
reg.fit = regsubsets(Outstate ~ ., data = College.train, nvmax = 17, method = "forward")
reg.summary = summary(reg.fit)
par(mfrow = c(1, 3))
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
min.cp = min(reg.summary$cp)
std.cp = sd(reg.summary$cp)
abline(h = min.cp + 0.2 * std.cp, col = "red", lty = 2)
abline(h = min.cp - 0.2 * std.cp, col = "red", lty = 2)
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
min.bic = min(reg.summary$bic)
std.bic = sd(reg.summary$bic)
abline(h = min.bic + 0.2 * std.bic, col = "red", lty = 2)
abline(h = min.bic - 0.2 * std.bic, col = "red", lty = 2)
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R2", 
     type = "l", ylim = c(0.4, 0.84))
max.adjr2 = max(reg.summary$adjr2)
std.adjr2 = sd(reg.summary$adjr2)
abline(h = max.adjr2 + 0.2 * std.adjr2, col = "red", lty = 2)
abline(h = max.adjr2 - 0.2 * std.adjr2, col = "red", lty = 2)


reg.fit = regsubsets(Outstate ~ ., data = College, method = "forward")
coefi = coef(reg.fit, id = 6)
names(coefi)

#                                                                                                                    
# (b)                          
#
library(gam)

gam.fit = gam(Outstate ~ Private + s(Room.Board, df = 2) + s(PhD, df = 2) + 
                s(perc.alumni, df = 2) + s(Expend, df = 5) + s(Grad.Rate, df = 2), data = College.train)
par(mfrow = c(2, 3))
plot(gam.fit, se = T, col = "blue")


#                                                                                                                    
# (c)                          
#

gam.pred = predict(gam.fit, College.test)
gam.err = mean((College.test$Outstate - gam.pred)^2)
gam.err
gam.tss = mean((College.test$Outstate - mean(College.test$Outstate))^2)
test.rss = 1 - gam.err/gam.tss
test.rss
#                                                                                                                    
# (d)                          
#

summary(gam.fit)


#-----------------------
# Exercise 11
#-----------------------

#                                                                                                                    
# (a)                          
#

set.seed(1)
beta0=2.1
beta1=1.2
beta2=0.6
x1=rnorm(100)
x2=rnorm(100)
e=rnorm(100,sd=0.1)
y=beta0+beta1*x1+beta2*x2+e
par(mfrow = c(1,1))

#                                                                                                                    
# (b)                          
#
b1=0

#                                                                                                                    
# (c)                          
#
a=y-b1*x1
b2=lm(a~x2)$coef[2]


#                                                                                                                    
# (d)                          
#
a=y-b2*x2
b1=lm(a~x1)$coef[2]

#                                                                                                                    
# (e)                          
#
N=1000
b1=rep(NA,N)
b0=rep(NA,N)
b2=rep(NA,N)
b1[1]=0
for (i in 1:(N-1))
{
  a=y-b1[i]*x1
  b2[i+1]=lm(a~x2)$coef[2]
  a=y-b2[i+1]*x2
  b1[i+1]=lm(a~x1)$coef[2]
  b0[i+1]=lm(a~x1)$coef[1]
}


#                                                                                                                    
# (f)                          
#

lm.fit=lm(y~x1+x2)
lm.fit$coefficients
#                                                                                                                    
# (g)                          
#

#one iteration is enough


#-----------------------
# Exercise 12
#-----------------------

set.seed(1)
p = 100
n = 1000
x = matrix(ncol = p, nrow = n)
coefi = rep(NA, p)
for (i in 1:p) {
  x[, i] = rnorm(n)
  coefi[i] = rnorm(1) * 100
}
y = x %*% coefi + rnorm(n)
beta = rep(0, p)
max_iterations = 1000
errors = rep(NA, max_iterations + 1)
iter = 2
errors[1] = Inf
errors[2] = sum((y - x %*% beta)^2)
threshold = 1e-04
while (iter < max_iterations && errors[iter - 1] -  errors[iter] > threshold) {
  for (i in 1:p) {
    a = y - x %*% beta + beta[i] * x[, i]
    beta[i] = lm(a ~ x[, i])$coef[2]
  }
  iter = iter + 1
  errors[iter] = sum((y - x %*% beta)^2)
  print(c(iter - 2, errors[iter - 1], errors[iter]))
}