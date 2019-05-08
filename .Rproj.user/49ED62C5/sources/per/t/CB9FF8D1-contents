######################
# Project 3 R Script #
# Hoops Billings     #
######################
#This script is used to perform a multiple linear regression analysis on the USDA
#Feed grains database cleaned data, with farmer received price as the response variable.

#Start by setting working directory, random seed, and loading needed packages
setwd("/Users/zanebillings/Documents/Academic/2019 Spring/MATH 375/Project3/WorkingDirectory")

library(tidyverse) #used for data manipulation and graphing
library(car) #used for linear model stuff
library(nlme) #used for linear model stuff
library(olsrr) #used for linear model stuff

#import data, rename columns, create dummy var for tracking time
CornFull <- read_csv("Feed_Grains_CornData_Clean.csv")
n <- nrow(CornFull) #count number of rows
CornFull$t <- seq(from = 0, to = (n - 1), by = 1) #create index variable

#set up the training dataset and the testing dataset
CornTraining <- CornFull[1:floor(0.8*n), ] #first 80% is training
CornTesting <- setdiff(CornFull,CornTraining) #everything else is testing

#Set up the reduced model...only intercept
Model1866Reduced <- lm(formula = price~1,
                      data = CornTraining)
summary(Model1866Reduced)

#Model with only longest-running parameters
Model1866Full <- lm(formula = price~t+harvest+production+yield,
               data = CornTraining)
summary(Model1866Full)

#Matrix of pairwise correlations for the 1866 full model
CornPairs = cbind(CornFull[,2],CornFull[,4:6],CornFull[,23]) #takes the vars I want to fit only
pairs(CornPairs) #pairwise scatterplots for data in CornPairs

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
par(mfrow=c(2,1)) #plotting option
FullModelResACF <- acf(Model1866Full$residuals) #ACF of residuals
FullModelResPACF <- pacf(Model1866Full$residuals) #PACF of residuals

#Test for residual homoscedasticity and normality
par(mfrow=c(2,2)) #plotting option
plot(Model1866Full) #autoplot of diagnostics

#Find Influential poitns for the 1866 model
ols_plot_cooksd_bar(Model1866Full)
#cook's d plot shows which vars may be outliers.

#examine after 1952 to ignore weird trends at beginning
Corn1952 <- CornFull[87:152,] #data from 1952 onward
Corn1952Training <- Corn1952[1:floor(0.8*nrow(Corn1952)),] #first 80% is training
Corn1952Testing <- setdiff(Corn1952,Corn1952Training) #rest is testing

#full model with everything
Model1952Full <- lm(formula = price ~ t+acreage+harvest+production+yield+loan+heat,
                    data = Corn1952Training)
summary(Model1952Full)
#reduced model intercept only
Model1952Reduced <- lm(formula = price~1,
                       data = Corn1952Training)

#Pairwise scatterplots of variables I want to use
CornPairs1952 = cbind(Corn1952[,2],Corn1952[,3:7],Corn1952[,14],Corn1952[,23])
pairs(CornPairs1952)

##1952 Forward stepwise regression
Model1952ForwardStep <-
  step(object = Model1952Reduced,
       scope = price~t+harvest+production+yield+acreage+loan+heat,
       data = CornTraining,
       direction = "forward")
##1952 Backward stepwise regression
Model1952BackwardStep <-
  step(object = Model1952Full,
       scope = price~1,
       data = CornTraining[,],
       direction = "backward")
summary(Model1952ForwardStep)
summary(Model1952BackwardStep)

#rename things to make it easier
Model1952Working <- Model1952ForwardStep
#extract residuals
Model1952WorkingResid <- Model1952ForwardStep$residuals
par(mfrow=c(2,1)) #plotting option
#check to see if residuals are correlated
Model1952ResidACF <- acf(Model1952WorkingResid)
Model1952ResidPACF <- pacf(Model1952WorkingResid)

##1952 GLS model...resdiuals are AR1
AR1Term1952 <- Model1952ResidPACF$acf[1] #AR1 coeff estimate
#fit the GLS model
Model1952AR1Res <- gls(model = price~t+loan+yield+heat,
                   data = Corn1952Training,
                   correlation = corAR1(AR1Term1952,~t,fixed=FALSE))
summary(Model1952AR1Res)

#manual stepwise selection, remove highest p-value
Model1952AR1ResStep2 <- gls(model = price~t+loan+yield,
                       data = Corn1952Training,
                       correlation = corAR1(AR1Term1952,~t,fixed=FALSE))
summary(Model1952AR1ResStep2)

#manual stepwise selection, remove highest p again
Model1952AR1ResStep3 <- gls(model = price~t+yield,
                            data = Corn1952Training,
                            correlation = corAR1(AR1Term1952,~t,fixed=FALSE))
summary(Model1952AR1ResStep3)

#getting uncorrelated e's
z = Model1952AR1ResStep3$residuals #extract correlated residuals
zf = Model1952AR1ResStep3$fitted #extract fitted values
zn = length(z) #length of residuals
par(mfrow=c(1,1)) #plotting option
plot(zf,z) #correlated residuals vs fitted

e = z[2:zn] - 0.8879908*z[1:(zn-1)] #correct for correlation in resid
plot(zf,e) #plot corrected res vs Fitted
pacf(e) #plot PACF of corrected Residuals

#generated more pairwise comparisons
CornPairsReduced = cbind(Corn1952[,2],Corn1952[,5],Corn1952[,23]) #scatterplots
pairs(CornPairsReduced) #pairwsie scatterplots

#take log transform of price and refit GLS
Model1952AR1ResLog <- gls(model = log10(price)~t+yield,
                            data = Corn1952Training,
                            correlation = corAR1(AR1Term1952,~t,fixed=FALSE))
summary(Model1952AR1ResLog)
r = Model1952AR1ResLog$residuals #residuals
elog = r[2:zn] - 0.9156077*r[1:(zn-1)] #correlation corrected residuals
flog = Model1952AR1ResLog$fitted[-1] #fitted values
plot(flog,elog) #plotted corrected residuals vs Fitted
pacf(elog) #check PACF of corrected Residuals

#construct variance estimates to correct for heteroskedasticity
#this regression estimates the variance of the residuals
ResidualVarEstimate <- lm(log10(Corn1952Training$price)~r)
summary(ResidualVarEstimate)
#take the values of the estimate variances
VarEstimates <- 1/(ResidualVarEstimate$fitted.values)^2

#weighted gls model w/ log transform price
Model1952AR1ResLogWeighted <- gls(model = log10(price)~t+yield,
                          data = Corn1952Training,
                          correlation = corAR1(AR1Term1952,~t,fixed=FALSE),
                          weights = varFixed(~VarEstimates))
rw = Model1952AR1ResLogWeighted$residuals #extract residuals
rwn = length(rw) #length of residuals
CorrectedResiduals = rw[2:rwn] - 0.9975218*rw[1:(rwn-1)] #correlation corrected residuals
rfw = Model1952AR1ResLogWeighted$fitted
plot(rfw,CorrectedResiduals) #plotted res vs fitted
pacf(CorrectedResiduals) #plot PACF of corrected residuals

#validate original GLS model...no weights or log tf with test data
#for presentation since it has to be 5-7 minutes
TestModel <- gls(model = price~t+yield,
                            data = Corn1952Testing,
                            correlation = corAR1(AR1Term1952,~t,fixed=FALSE))
summary(TestModel)
tmres <- TestModel$residuals #extract residuals
resn <- length(TestModel$residuals) #length of residuals
tmcr <- tmres[2:resn] - 0.9975218*tmres[1:(resn-1)] #corrected residuals
tmf <- TestModel$fitted #fitted values
#plot correct residuals vs. fitted and view corrected res. PACF
plot(tmf[-1],tmcr,xlab = "Fitted",ylab = "Corrected residuals",main = "Test residuals")
pacf(tmcr,main = "Testing Data: Corrected residual PACF")
