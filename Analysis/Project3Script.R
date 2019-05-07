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
library(nlme)
library(olsrr)

#import data, rename columns, create dummy var for tracking time
CornFull <- read_csv("Feed_Grains_CornData_Clean.csv")
n <- nrow(CornFull)
CornFull$t <- seq(from = 0, to = (n - 1), by = 1)

#set up the training dataset and the testing dataset
CornTraining <- CornFull[1:floor(0.8*n), ]
CornTesting <- setdiff(CornFull,CornTraining)

#Set up the reduced model
Model1866Reduced <- lm(formula = price~1,
                      data = CornTraining)
summary(Model1866Reduced)

#Model with only longest-running parameters
Model1866Full <- lm(formula = price~t+harvest+production+yield,
               data = CornTraining)
summary(Model1866Full)

#Matrix of pairwise correlations for the 1866 full model
CornPairs = cbind(CornFull[,2],CornFull[,4:6],CornFull[,23])
pairs(CornPairs)

#Stepwise Regression for the model using the 1866 parameters
##Forward stepwise regression for the 1866 model
Model1866ForwardStep <-
  step(object = ModelFullReduced,
       scope = price~t+harvest+production+yield,
       data = CornTraining,
       direction = "forward")
##Backward stepwise regression for the 1866 model
Model1866BackwardStep <-
  step(object = Model1866Full,
       scope = price~1,
       data = CornTraining[,],
       direction = "backward")

#Analysis of Residuals for full 1866 model
par(mfrow=c(2,1))
FullModelResACF <- acf(Model1866Full$residuals)
FullModelResPACF <- pacf(Model1866Full$residuals)

#Test for residual homoscedasticity and normality
par(mfrow=c(2,2)) 
plot(Model1866Full)
shapiro.test(Model1866Full$residuals)
ols_test_breusch_pagan(Model1866Full)

#Find Influential poitns for the 1866 model
#influence.measures(Model1866Full)
ols_plot_cooksd_bar(Model1866Full)
ols_plot_dffits(Model1866Full)
#ols_plot_dfbetas(Model1866Full)

#GLS model with AR1 correlations
AR1Term <- FullModelResPACF$acf[1]
ModelAR1Res <- gls(model = price~harvest+production+yield,
                  data = CornTraining[,],
                  correlation = corAR1(AR1Term,~t,fixed=FALSE))
summary(ModelAR1Res)
plot(ModelAR1Res,main = "price~harvest+production+yield, AR1Res")

#GLS model without production, based on previous results.
ModelAR1ResNoProd <- gls(model = price~harvest+yield,
                          data = CornTraining,
                          correlation = corAR1(AR1Term,~t,fixed=FALSE))
summary(ModelAR1ResNoProd)
plot(ModelAR1ResNoProd,main = "price~harvest+yield, AR1Res")

#examine after some cutoff point in the 1960's-ish.
Corn1952 <- CornFull[87:152,]
Corn1952Training <- Corn1952[1:floor(0.8*nrow(Corn1952)),]
Corn1952Testing <- setdiff(Corn1952,Corn1952Training)

Model1952Full <- lm(formula = price ~ t+acreage+harvest+production+yield+loan+heat,
                    data = Corn1952Training)
summary(Model1952Full)
Model1952Reduced <- lm(formula = price~1,
                       data = Corn1952Training)

#Pairwise scatterplots of variables
CornPairs1952 = cbind(Corn1952[,2],Corn1952[,3:7],Corn1952[,14],Corn1952[,23])
pairs(CornPairs1952)

##1952 Forward stepwise
Model1952ForwardStep <-
  step(object = Model1952Reduced,
       scope = price~t+harvest+production+yield+acreage+loan+heat,
       data = CornTraining,
       direction = "forward")
##1952 Backward stepwise
Model1952BackwardStep <-
  step(object = Model1952Full,
       scope = price~1,
       data = CornTraining[,],
       direction = "backward")
summary(Model1952ForwardStep)
summary(Model1952BackwardStep)

Model1952Working <- Model1952ForwardStep
Model1952WorkingResid <- Model1952ForwardStep$residuals
par(mfrow=c(2,1))
Model1952ResidACF <- acf(Model1952WorkingResid)
Model1952ResidPACF <- pacf(Model1952WorkingResid)

##1952 GLS model
AR1Term1952 <- Model1952ResidPACF$acf[1]
Model1952AR1Res <- gls(model = price~t+loan+yield+heat,
                   data = Corn1952Training,
                   correlation = corAR1(AR1Term1952,~t,fixed=FALSE))
summary(Model1952AR1Res)

Model1952AR1ResStep2 <- gls(model = price~t+loan+yield,
                       data = Corn1952Training,
                       correlation = corAR1(AR1Term1952,~t,fixed=FALSE))
summary(Model1952AR1ResStep2)

Model1952AR1ResStep3 <- gls(model = price~t+yield,
                            data = Corn1952Training,
                            correlation = corAR1(AR1Term1952,~t,fixed=FALSE))
summary(Model1952AR1ResStep3)
phi = Model1952AR1ResStep3$modelStruct$corStruct;phi
  
#getting uncorrelated e's
z = Model1952AR1ResStep3$residuals
zn = length(z)
par(mfrow=c(1,1)) 
plot(z)

e = z[2:zn] - 0.8879908*z[1:(zn-1)]
CornPairsReduced = cbind(Corn1952[,2],Corn1952[,5],Corn1952[,23])
pairs(CornPairsReduced)

Model1952AR1ResLog <- gls(model = log10(price)~t+yield,
                            data = Corn1952Training,
                            correlation = corAR1(AR1Term1952,~t,fixed=FALSE))
summary(Model1952AR1ResLog)
r = Model1952AR1ResLog$residuals
plot(r,main =  "Residuals: Log(price), AR1 correlation")
elog = r[2:zn] - 0.9156077*r[1:(zn-1)]
plot(r,Corn1952Training$price,main = "Residuals: Log(price), AR1 correlation, corrected")

#construct variance estimates to correct for heteroskedasticity 
ResidualVarEstimate <- lm(log10(Corn1952Training$price)~r)
summary(ResidualVarEstimate)
ResCfs <- coef(ResidualVarEstimate)
VarEstimates <- 1/(ResidualVarEstimate$fitted.values)^2

#weighted gls model w/ log transform price
Model1952AR1ResLogWeighted <- gls(model = log10(price)~t+yield,
                          data = Corn1952Training,
                          correlation = corAR1(AR1Term1952,~t,fixed=FALSE),
                          weights = varFixed(~VarEstimates))
rw = Model1952AR1ResLogWeighted$residuals
rwn = length(rw)
plot(rw, main = "Residuals: Log(price), AR1 correlation, weighted")
CorrectedResiduals = rw[2:rwn] - 0.9975218*rw[1:(rwn-1)]
plot(CorrectedResiduals)
GoodModel <- Model1952AR1ResLogWeighted
BadModel <- Model1952AR1ResLog
summary(GoodModel)
#check influential measures, remove outliers, refit
par(mfrow = c(1,2))
plot(elog,main = "Residuals: Log(price), AR1 correlation, corrected")
plot(CorrectedResiduals, main = "Residuals: Log(price), AR1 correlation, weighted, corrected")


par(mfrow=c(2,1))
pacf(CorrectedResiduals)
plot(GoodModel$fitted[-1],CorrectedResiduals)
text(CorrectedResiduals~GoodModel$fitted[-1], labels = Corn1952Training$t[-1],pos=4)

exclude = c(105,106,120)
ExcludedData = Corn1952Training[-exclude,]
ExcludedVariance = VarEstimates[-exclude]
ExcludeModel <- gls(model = log10(price)~t+yield,
                                  data = ExcludedData,
                                  correlation = corAR1(AR1Term1952,~t,fixed=FALSE),
                                  weights = varFixed(~ExcludedVariance))
summary(ExcludeModel)
