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
# Chapter 2: Applied
##################

#-----------------------
# Exercise 8
#-----------------------

#
# 8(a) 
#

# Set your working directory here; uncomment here if necessary
#setwd("~/your_path/statistical-learning/ch02")
college=read.csv("../data/College.csv",header=T,na.strings="?")

#
# 8(b) 
#
#assign row names based on the first column
rownames(college) = college[,1]
fix(college)
# remove the first column
college = college[,-1]
fix(college)

#
# 8(c)
#
#---------
# i
#---------
summary(college)

#---------
# ii
#---------
pairs(college[,1:10])

#---------
# iii
#---------
plot(college$Private,college$Outstate)
title(main="Side-by-side boxplots of Outstate versus Private", sub="",
     xlab="Private School", ylab="Outstate tutition($)") 

# alternatively you can attach the dataframe 
# attach(college)
# plot(Private,Outstate)
#---------

#---------
# iv
#---------
Elite=rep("No",nrow(college))
Elite[college$Top10perc>50]="Yes"
Elite=as.factor(Elite)
college=data.frame(college,Elite)
plot(college$Elite,college$Outstate)
title(main="Side-by-side boxplots of Outstate versus Elite", sub="",
      xlab="Elite", ylab="Outstate tutition($)")
#---------
# alternatively you can attach the dataframe 
#attach(college)
#plot(Elite,Outstate)
#title(main="Side-by-side boxplots of Outstate versus Elite", sub="",
#      xlab="Elite", ylab="Outstate tutition($)") 
#detach(college)
#---------

#---------
#v
#---------
par(mfrow=c(2,2))

hist(college$F.Undergrad,main = "Undergrad histogram with 10 bins",breaks=10)
hist(college$F.Undergrad,main = "Undergrad histogram with 20 bins",breaks=20)
hist(college$PhD,main = "PhD histogram with 10 bins",breaks=10)
hist(college$PhD,main = "PhD histogram with 20 bins",breaks=20)

# alternatively you can attach the dataframe 
#attach(college)
#hist(F.Undergrad,main = "Undergrad histogram with 10 bins",breaks=10)
#hist(F.Undergrad,main = "Undergrad histogram with 20 bins",breaks=20)
#hist(PhD,main = "PhD histogram with 10 bins",breaks=10)
#hist(PhD,main = "PhD histogram with 20 bins",breaks=20)
detach(college)


#---------
# vi
#---------
par(mfrow=c(1,1))
# High tuition correlates to high graduation rate.
plot(college$Outstate, college$Grad.Rate,main = "High tuition correlates to high graduation rate")
# Colleges with low acceptance rate tend to have low S:F ratio.
plot(college$Accept / college$Apps, college$S.F.Ratio,main=" Colleges with low acceptance rate tend to have low S:F ratio.")
#Colleges with the most students from top 10% perc don't necessarily 
# have the highest graduation rate. Also, rate > 100 is questionable!
plot(college$Top10perc, college$Grad.Rate,main="Colleges with the most students from top 10% perc don't necessarily 
      have the highest graduation rate. Also, rate > 100 is erroneous!")


#-----------------------
# Exercise 9
#-----------------------
#load the data from CSV file
auto=read.csv("../data/Auto.csv",header=T,na.strings="?")
auto=na.omit(auto)
#---------
# (a)
#---------
#take a look at the first 5 rows of the dataframe
pairs(auto)
# quantitative predictors: mpg,displacement, horsepower, weight, acceleration
# quanlitative predictors: cylinders,year,origin,name
#---------
# (b)
#---------
sapply(auto[,c(1,3,4,5,6)], range, na.rm=TRUE)
#---------
# (c)
#---------
sapply(auto[,c(1,3,4,5,6)], mean, na.rm=TRUE)
sapply(auto[,c(1,3,4,5,6)], sd, na.rm=TRUE)
#---------
# (d)
#---------
sapply(auto[c(1:9,86:392),c(1,3,4,5,6)], range, na.rm=TRUE)
sapply(auto[c(1:9,86:392),c(1,3,4,5,6)], mean, na.rm=TRUE)
sapply(auto[c(1:9,86:392),c(1,3,4,5,6)], sd, na.rm=TRUE)

#---------
# (e)
#---------
pairs(auto)

#---------
# (f)
#---------

# identify predictors which have linear relationship with mpg. 
# we found horsepower, displacement,weight are good predictors in this sense.

#-----------------------
# Exercise 10
#-----------------------

#---------
# (a)
#---------
library(MASS)
# how many rows?
nrow(Boston)
# how many columns?
ncol(Boston)
#---------
# (b)
#---------
pairs(Boston)
#---------
# (c)
#---------
# in areas with lower median value of owner-occupied homes,crime rates tend to be higher.
plot(Boston$medv,Boston$crim)

#---------
# (d)
#---------
#the closer to the employement center, the higher the crime rate.
plot(Boston$indus,Boston$crim)
#lower tax rate is associated with lower crime rate. 
plot(Boston$tax,Boston$crim)
#lower pupil-teacher ratio is also associated with lower crime rate
plot(Boston$ptratio,Boston$crim)
#---------
# (e)
#---------
sum(Boston$chas)
#---------
# (f)
#---------
median(Boston$ptratio)
#---------
# (g)
#---------
which.min(Boston$medv)
#this gives us the suburb 399
Boston[399,]
# the low medv is closely related to the fact that 100% of the owner-occupied units were
# built before 1940 indicating no signs of developing in this suburb.
#---------
# (h)
#---------
#how many of the suburbs average more than seven rooms per dwelling?
sum(Boston$rm>7)
#how many of the suburbs average more than seven rooms per dwelling?
sum(Boston$rm>8)
# These suburbs have relatively lower crime rate and lower pupil-teacher ratio.


