# script for playing with data and functions from "practical machine learning" course

# install used packages
install.packages("caret")   # includes ggplot2!
install.packages("kernlab") # for spam and other data sets
install.package("ISLR")

library(caret)
library(kernlab)
library(ISLR)

# prepare training and test sets
data(Wage)
inTrain <- createDataPartition(y=Wage$wage,p=0.7,list=FALSE)
train_wage <- Wage[inTrain,]; test_wage <-Wage[-inTrain,]
featurePlot(x=train_wage[,c("age","education","jobclass")], y=train_wage$wage,plot="pairs")

# some data exploration to see which variables to use in the model
qplot(age,wage,color=jobclass,data=train_wage)
qplot(age,wage,color=maritl,data=train_wage)

# train the model
glm_wage <- train(wage~age+education+jobclass,method="lm",data=train_wage)
# use "wage ~ ." to train a model using all covariates

# some diagnostics how good the model is
# 1. fitted values against residuals: should be line near zero
#    In this case, it is falling off to the right, suggesting we should use another variable
plot(glm_wage$finalModel,1,pch=19,cex=0.5)

# 2. color this plot by another variable to see whether it explains the outliers
finMod <- glm_wage$finalModel
qplot(finMod$fitted,finMod$residuals,color=race)

# 3. plot against index: should be randomly clustered around 0
plot(finMod$residuals)

# 4. predicted versus truth in test set: want data clustered around diagonal up the middle
pred <- predict(glm_wage,test_wage)
qplot(wage,pred,color=year,data=test_wage)