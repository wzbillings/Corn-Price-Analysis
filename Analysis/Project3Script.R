######################
# Project 3 R Script #
# Hoops Billings     #
######################
#This script is used to perform a multiple linear regression analysis on the USDA
#Feed grains database cleaned data, with farmer recieved price as the response variable.
#Data names are easily understandable in .csv file, will be changed here for convinience.
#price = Price recieved by farmers in current market year in $/bushel
#acreage = planted acreage of corn in market year, in millions of acres
#harvest = millions of acres of corn harvested for grain in current MY
#production = total production of corn in millions of bushels in current MY
#yield = yield of corn per harvested acre, in bushels/acre for current MY
#loan = loan rate of corn in current MY in $/bushel
#bf = broiler:feed ratio, (NEEDS EXPLANATION)
#ef = egg:feed ratio,
#hc = hog:corn ratio,
#mf = milk:feed ratio,
#shc = steer-heifer:corn ratio,
#tf = turkey:feed ratio,
#heat = MY Mean global heat anomaly estimated by NASA.
#export = 1000s of bushels of corn exported from the US in current MY.
#import = 1000s of bushels of corn imported into the US in current MY.
#railrate = rail rate price index, relative price of railway shipping, in current MY.
#railcar = 1000s of rail cars loaded with grain in current MY.
#supply = total supply of corn in US for current MY in millions of bushels.
#disapp = total disappearance of corn in US for current MY in millions of bushels.
#feed = amount of corn feed to livestock in current MY, in 1000s of metric tons.
#fai = total feed animal index, relative number of GCAUs (Grain consuming animal units).

#Start by setting working directory, random seed, and loading needed packages
setwd("/Users/zanebillings/Documents/Academic/2019 Spring/MATH 375/Project3/WorkingDirectory")
set.seed(863)
library(tidyverse)

#import data, rename columns, create dummy var for tracking time
CornFull <- read_csv("Feed_Grains_CornData_Clean.csv")
colnames(CornFull) <- c("year","price","acreage","harvest","production",
                   "yield","loan","bf","ef","hc","mf","shc","tf",
                   "heat","export","import","railrate","railcar","supply",
                   "disapp","feed","fai")
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
plot(y = Model1866All$residuals, x = CornTraining$price)
acf(Model1866All$residuals,lag.max = 150)
pacf(Model1866All$residuals,lag.max = 150)

#Find Influential poitns for the 1866 model
influence.measures(Model1866All)

# ##Adding in a few more variables...1926 model includes heat anomaly and acreage
# Corn1926 = data.frame(CornFull$year,CornFull$price,CornFull$acreage,
#                       CornFull$harvest,CornFull$yield,CornFull$heat,
#                       CornFull$production)[-(1:60),]
# colnames(Corn1926) = c("year","price","acreage","harvest","yield","heat",
#                        "production")
# Corn1926$index = seq(from = 0, to = nrow(Corn1926) - 1, by = 1)
# 
# #Set up 1926 full model
# Model1926Full = lm(formula = price~index+acreage+harvest+yield+heat+production,
#                    data = Corn1926)
# summary(Model1926Full)
# step(object = ModelFullReduced,
#      scope = price~index+acreage+harvest+yield+heat+production,
#      data = Corn1926)