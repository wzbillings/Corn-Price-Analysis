######################
# Project 3 R Script #
# Hoops Billings     #
######################
#This script is used to perform a multiple linear regression analysis on the USDA
#Feed grains database cleaned data, with farmer received price as the response variable.

#Start by setting working directory, random seed, and loading needed packages
setwd("/Users/zanebillings/Documents/Academic/2019 Spring/MATH 375/Project3/WorkingDirectory")
set.seed(863)
library(tidyverse)
library(car)

#import data, rename columns, create dummy var for tracking time
CornFull <- read_csv("Feed_Grains_CornData_Clean.csv")
n <- nrow(CornFull)
CornFull$index <- seq(from = 0, to = (n - 1), by = 1)

#set up the training dataset and the testing dataset
CornTraining <- sample_n(tbl = CornFull,
                       size = floor(0.8*nrow(CornFull)))
CornTesting <- setdiff(CornFull,CornTraining)

#Set up the reduced model
ModelFullReduced <- lm(formula = price~1,
                      data = CornTraining)
summary(ModelFullReduced)

#Set up the model with time only
ModelTimeOnly <- lm(formula = price~index,
                   data = CornTraining)
summary(ModelTimeOnly)

#Model with only longest-running parameters
Model1866All <- lm(formula = price~index+harvest+production+yield,
               data = CornTraining)
summary(Model1866All)

#Matrix of pairwise correlations for the 1866 full model
CornPairs = cbind(CornFull[,2],CornFull[,4:6],CornFull[,23])
pairs(CornPairs)

#Stepwise Regression for the model using the 1866 parameters
##Forward stepwise regression for the 1866 model
Model1866ForwardStep <-
  step(object = ModelFullReduced,
       scope = price~index+harvest+production+yield,
       data = CornTraining,
       direction = "forward")
##Backward stepwise regression for the 1866 model
Model1866BackwardStep <-
  step(object = Model1866All,
       scope = price~1,
       data = CornTraining,
       direction = "backward")

#Analysis of Residuals for full 1866 model
acf(Model1866All$residuals,lag.max = 150)
pacf(Model1866All$residuals,lag.max = 150)

#Test for residual homoscedasticity and normality
par(mfrow=c(2,2)) 
plot(Model1866All)
shapiro.test(Model1866All$residuals)
ncvTest(Model1866All)

#Find Influential poitns for the 1866 model
influence.measures(Model1866All)







