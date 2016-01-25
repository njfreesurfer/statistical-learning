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
# Chapter 10: Applied
##################

# Set your working directory here; uncomment here if necessary
#setwd("~/your_path/statistical-learning/ch10")
#-----------------------
# Exercise 7
#-----------------------
dsc = t(scale(t(USArrests)))
heatmap(data.matrix(dist(dsc)^2),Rowv=NA,Colv=NA)
heatmap(data.matrix(as.dist(1-cor(t(dsc)))),Rowv=NA,Colv=NA)
#will be two identical maps
#---------

#-----------------------
# Exercise 8
#-----------------------
pr.out=prcomp(USArrests,scale=TRUE)
#---------
# a
#---------
pr.var=pr.out$sdev^2
pve=pr.var/sum(pr.var)
#---------
# b
#---------

#directly compute the pve
pve2=apply(x^2,2,sum)/sum(scale(USArrests)^2)

#-----------------------
# Exercise 9
#-----------------------

#---------
# a
#---------
set.seed(2)
hc.complete=hclust(dist(USArrests),method="complete")
#---------
# b
#---------
cutree(hc.complete,3)
#---------
# c
#---------
hc.complete2=hclust(dist(scale(USArrests)),method="complete")

#---------
# d
#---------
plot(hc.complete2)
#should standadize the variables as different units are being used!

#-----------------------
# Exercise 10
#-----------------------

#---------
# a
#---------
set.seed(1)
x1 = matrix(rnorm(20*50, mean=0, sd=0.3), ncol=50)
x2 = matrix(rnorm(20*50, mean=1, sd=0.3), ncol=50)
x3 = matrix(rnorm(20*50, mean=2, sd=0.3), ncol=50)
x=rbind(x1,x2,x3)
L=rep(1,60)
L[21:40]=2
L[41:60]=3

#---------
# b
#---------
pc.out=prcomp(x,scale=FALSE)
plot(pc.out$x[,1],pc.out$x[,2],col=L)
plot(x[,c(1,2)],col=L)

#---------
# c
#---------
km.out=kmeans(x,3,nstart=20)
km.out$cluster
table(km.out$cluster,L)

#---------
# d
#---------
km.out=kmeans(x,2,nstart=20)
km.out$cluster
table(km.out$cluster,L)

#---------
# e
#---------
km.out=kmeans(x,e,nstart=20)
km.out$cluster
table(km.out$cluster,L)

#---------
# f
#---------
km.out=kmeans(pc.out$x[,c(1,2)],3,nstart=20)
km.out$cluster
table(km.out$cluster,L)

#---------
# g
#---------
km.out=kmeans(scale(x),3,nstart=20)
km.out$cluster
table(km.out$cluster,L)


#-----------------------
# Exercise 11
#-----------------------


#---------
# a
#---------
data=read.csv("../data/Ch10Ex11.csv",header=F)


#---------
# b
#---------
hc.out1=hclust(as.dist(1-cor(data)),method="complete")
plot(hc.out1)
hc.out2=hclust(as.dist(1-cor(data)),method="single")
plot(hc.out2)
hc.out3=hclust(as.dist(1-cor(data)),method="average")
plot(hc.out3)

#---------
# c
#---------
pr.out = prcomp(t(data))
total_load = apply(pr.out$rotation, 1, sum)
which.max(abs(pr.out$rotation[,1]))
          
