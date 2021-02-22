## Setting working directory ####
setwd("~/Desktop/Intoduction to ML and DM (02450)/Project 1/student datasets")
## Loading packages ####
library(markdown)
library(tidyverse)
library(caret)
library(dplyr)
library(doFuture)
library(doParallel)
library(earth)
library(gbm)
library(gam)
library(ggplot2)
library(glmnet)
library(grid)
library(gridExtra)
library(hexbin)
library(ipred)
library(labeling)
library(MASS)
library(neuralnet)
library(NeuralNetTools)
library(NeuralNetworkVisualization)
library(nnet)
library(pdp)
library(plotmo)
library(randomForest)
library(ranger)
library(reshape2)
library(rlang)
library(rpart.plot)
library(rsample)
library(shape)
library(splines)
library(xgboost)
library(pROC)
library(caTools)
library(adabag)

## Creating data sets and Merging the two datasets ####
math=read.table("student-mat.csv",sep=";",header=TRUE)
port=read.table("student-por.csv",sep=";",header=TRUE)
mergedata=merge(math,port,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(mergedata)) # 382 students
math=math[,match(colnames(port),colnames(math))] #now the columns in the two datasets are in the same order




## Structure of the variables####
str(math)
str(port)
################ Variable transformation ###############################
## Changing cont. to factors####
math<-transform(math,
                studytime=factor(studytime,labels=c('<2 h','2-5 h','5-10 h','>10 h'))
                ,traveltime=factor(traveltime,labels=c('<15 min','15-30 min','30-60 min','>60 min')),
                Fedu=factor(Fedu,labels=c('none','1st-4th grade','5th-9th grade','10th-12th grade','higher')),
                Medu=factor(Medu,labels=c('none','1st-4th grade','5th-9th grade','10th-12th grade','higher'))
                )
port<-transform(port,
                studytime=factor(studytime,labels=c('<2 h','2-5 h','5-10 h','>10 h')),
                traveltime=factor(traveltime,labels=c('<15 min','15-30 min','30-60 min','>60 min')),
                Fedu=factor(Fedu,labels=c('none','1st-4th grade','5th-9th grade','10th-12th grade','higher')),
                Medu=factor(Medu,labels=c('none','1st-4th grade','5th-9th grade','10th-12th grade','higher')))


str(math)
str(port)
## Check to see if there are any missing values ####
sum(is.na(math))
sum(is.na(port)) # no missing values in each data set
###############Exploratory data analysis #################################
## Summary statistics ####

## Correlation matrix####
corr_matrix<-function(data){
  cp<-cor(data.matrix(na.omit(data)),method="spearman")
  ord<-rev(hclust(as.dist(1-abs(cp)))$order)
  colPal<-colorRampPalette(c("blue","yellow"),space="rgb")(100)
  x <- seq(pi/4, 5*pi, length.out=10)
  y <- seq(pi/4, 5*pi, length.out=10)
  grid <- expand.grid(X=x, Y=y)
  grid$Z <- runif(100, -1, 1)
  
  ## Write a panel function (after examining 'args(panel.levelplot) to see what
  ## will be being passed on to the panel function by levelplot())
  myPanel <- function(x, y, z, ...) {
    panel.levelplot(x,y,z,...)
    panel.text(x, y, round(z,1))
  }
  levelplot(cp[ord,ord],xlab="",ylab="",col.regions=colPal,at=seq(-1,1,length.out=100),colorkey=list(space="top",labels=list(cex=1)),scales=list(x=list(rot=45),y=list(draw=FALSE),cex=1),panel=myPanel)
}
## Correlation:Math####
corr_math<-corr_matrix(math)

## Correlation:Port ####
corr_port<-corr_matrix(port)






##One hot encoding (For use in the linear regression models remove redundants-when using tree based methods use the redundants) ####
is.fact<-function(x){sapply(x,is.factor)}
is.int<-function(x){sapply(x,is.integer)}
math_fac<-math[,is.fact(math)]
port_fac<-port[,is.fact(port)]
math_int<-math[,is.int(math)]
port_int<-port[,is.int(port)]
dmy_math<-dummyVars("~.",data=math_fac,fullRank = T)
dmy_port<-dummyVars("~.",data=port_fac,fullRank=T)
trsf_math <- data.frame(predict(dmy_math, newdata =math_fac ))
trsf_port<-data.frame(predict(dmy_port, newdata =port_fac ))
math<-cbind(trsf_math,math_int)
port<-cbind(trsf_port,port_int)

#trying to add a comment again
