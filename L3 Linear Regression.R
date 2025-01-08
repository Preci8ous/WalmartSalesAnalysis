setwd("C:/Users/metremblay/Dropbox/W&M/Teaching WM/BUAD5072 - Machine Learning I/Fall 2024/datasets")
rm(list=ls())
#ndnndndnd
#########################################################
######## Advertising Model#########################################################
#########################################################
Ad<-read.csv("Advertising.csv")
# The Advertising data set.Sales, in thousands of units, as a function of TV, radio, 
# and newspaper budgets, in thousands of dollars, for 200 diﬀerent markets.
str(Ad)

summary(Ad) #note that sales is in the 1000s (units), budget in the 1000s (dollars)
pairs(Ad) #Can subset out if there are two many variables
attach(Ad) #we are interested at correlation between sales and tv


## make regression object
options(scipen=999) #so we can see the zeros
lmAd <- lm(sales~TV)
summary(lmAd) #string effect
#sales increase by 47 units for each 1000 dollar increase in advertising 
#sales will be within 3259 units
plot(sales~TV)
abline(lmAd,col="purple")

coef(lmAd)

tv<-150
7.03259355 + 0.04753664*tv #regression equation

confint(lmAd)
6.12971927 + 0.04223072*tv #lower bound 
7.93546783 + 0.05284256*tv #upper bound
# Give TV at 150 our 95% confidence interval is [12.46433,15.86185]
#check out actual data - we fall w/in CI

predict(lmAd, data.frame(TV=150))

#Looking at the assumptions, normality of errors
#test for linearity - we ploted, and ANOVA was significant

#history of the residuals
hist(lmAd$residuals)
#some skew - but passes normality
mean(lmAd$residuals)
par(mfrow=(c(2,2)))
plot(lmAd)

# residual plot we are following that red line instead of having a pattern 
# we do see fitted values are fanning out to the right, we have perhaps heteroskedacticity
# Q-Q plot tests if linearity,looks ok
# next chart, similar to first but just standardized  - random
# Nothing alarming in Leverage plot

# Check out error point

install.packages("lmtest")

lmtest::bptest(lmAd) #:: knows to pull from that library
# violates assumption of homoskedasticity, lowers accuracy, found heteroskedasticity (want not significant) 
# split dataset and look at MSE


#########################################################
###################Corolla ##############################
##########################################################

# VARIABLES IN THE TOYOTA COROLLA
# Variable Description
# Price         Offer price in Euros
# Model         Model of Corolla
# Age           Age in months as of August 2004
# KM            Accumulated kilometers on odometer
# Fuel_Type     Type Fuel type (Petrol, Diesel, CNG)
# HP            Horsepower
# Metallic      Metallic color? (Yes = 1, No = 0)
# Automatic     Automatic (Yes = 1, No = 0)
# cc            Cylinder volume in cubic centimeters
# Doors         Number of doors
# QuartTax      Quarterly road tax in Euros
# Weight        Weight in kilograms
par(mfrow=(c(1,1)))
corolla<- read.csv("corolla.csv")
summary(corolla)
corolla <- read.csv("corolla.csv",stringsAsFactors = TRUE) # what else could I do?coerce
class(corolla$Model)
attach(corolla)
hist(Price) #positive skew/right skew
pairs(corolla)
plot(Price~Age) #linear, negatively
plot(Price~KM) # not linear - probably need to transform, but we can check with regression
plot(Price~Fuel_Type) #different because it is a factor, box plot - outside box and whiskers outliers/3SD from mean
plot(Price~cc) #something is up = data point way off, look at record 81/verify and fix

fix(corolla)
summary(cc) #why still showing?  because of attach
detach(corolla)
attach(corolla)
summary(cc)
plot(Price~cc) #looks like an interval

corolla[81,"cc"] #another way
corolla[81,"cc"]<-1600
#####lets run a regression
lm.corolla<-lm(Price~Age)
summary(lm.corolla)

####test assumptions
hist(lm.corolla$residuals)#pass bell curve
mean(lm.corolla$residuals) #pass close to zero
lmtest::bptest(lm.corolla) #significant, we reject the null hypotheses of data being homoskedactic  which is bad!

##Generate an example
?bptest  #scroll to bottom
## generate a regressor
x <- rep(c(-1,1), 50)
## generate heteroskedastic and homoskedastic disturbances
err1 <- rnorm(100, sd=rep(c(1,2), 50)) #repeats so there is a pattern heteroskedastic
err2 <- rnorm(100) #random, should create a homoskedastic response
## generate a linear relationship
y1 <- 1 + x + err1
y2 <- 1 + x + err2
## perform Breusch-Pagan test
library(lmtest)
bptest(y1 ~ x) #heteroskedastic bad - reject the null p<.05
bptest(y2 ~ x) # homoskedastic good -fail to reject the null good#
#collapse code using Edit->folding-Collapse or Alt L

par(mfrow=(c(2,2)))
plot(lm.corolla) #Q-Q Plot calls out three, take a look in dataset by sorting by price , but they were the three that compared with the X in age, had the most unusual values.
par(mfrow=(c(1,1)))
plot(Price~Age) #Three really high priced cars? Makes it almost look curvilinear.  Outliers? When do you stop? Lets split instead?

overall<-mltools::mse(Price,lm.corolla$fitted.values) #actual vs predicted
overall

#split dataset
n <- length(corolla$Price)
n1 <- 900

set.seed(100)
trainobsnum <- sample(1:n, n1)
train <- corolla[trainobsnum,]test <- corolla[-trainobsnum,]

lm.train <- lm(Price~Age, data=train)
summary(lm.train)

predictions <- predict(lm.train, newdata=test)
head(predictions)

overall
mltools::mse(predictions, test$Price) #why is higher? 

cor(test$Price, predictions)
