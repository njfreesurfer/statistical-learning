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
