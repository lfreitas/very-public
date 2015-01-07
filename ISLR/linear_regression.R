library(ISLR)
library(plyr)
library(MASS)
library(car)      # companion to applied regresion -- diagnostic functions
library(robustHD) # used for winsorizing data




#
#
#
#   LINEAR REGRESSION
#
#
#


names(Boston)
attach(Boston)


#
# Univariate regression
#
lm.fit <- lm(medv~lstat,data=Boston)  # fit simple linear model
summary(lm.fit)                    # summary of regression results
names(lm.fit)                      # results can also be accessed individually
coef(lm.fit)                       # coefficient estimates
confint(lm.fit)                    # confidence intervals for estimates
newdata <- data.frame(lstat=(c(5,10,15))) 
predict(lm.fit,newdata,interval ="confidence") 
predict(lm.fit,newdata,interval ="prediction") 


plot(lstat,medv)  
abline(lm.fit)                  # plot the fit
abline(lm.fit,lwd=3)            # customize the line, explore some symbols
abline(lm.fit,lwd=3,col="red")
plot(lstat,medv,col="red")
plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")
plot(1:20,1:20,pch=1:20)

# split plot area into 2x2 and print diagnostic plots
par(mfrow=c(2,2))           
plot(lm.fit)
par(mfrow=c(1,1))
plot(predict(lm.fit), residuals(lm.fit)) # make some diagnostics bigger
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues (lm.fit))                 # plot leverage (influential predictors)
which.max(hatvalues(lm.fit))             # return index of highest-leverage obs

#
# Basic multivariate linear model
#
lm.fit<-lm(medv~.,data=Boston) # regression of medv on all other vars
summary(lm.fit)
vif(lm.fit)          # VIFs for the predictors, part of car package
round(cor(Boston),2) # pairwise correlations, rounded to nearest hundredth

# regression of medv on all other vars except age
lm.fit1<-lm(medv~.-age,data=Boston)
summary(lm.fit1)

#
# Linear regression with interactions
#
# Note that the interaction automatically includes the linear terms
summary(lm(medv~lstat*age,data=Boston))

#
# Linear regression with polynomial terms
#
# poly() is a flexible way of including polynomial terms
summary(lm(medv~poly(lstat,5),data=Boston))
detach(Boston)

#
# Linear regression with qualitative predictors
#

attach(Carseats)
names(Carseats)
str(Carseats)

# R automatically turns factor vars into dummies
lm.fit <- lm(Sales~ShelveLoc)
summary(lm.fit)

# constrasts() lays out how the dummies are being generated
contrasts(ShelveLoc)

# more generally, this function subsets the df to vars that are factors and 
# displays all of the constrasts
x<-sapply(Carseats[ , sapply(Carseats, is.factor)], contrasts)


# fit a model with all of the vars in linear form plus a couple of interactions
lm.fit <- lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)


# use the aggregate function to average the numeric vars in the df by ShelveLoc
aggregate(Carseats[,sapply(Carseats,is.numeric)],
          by=list(ShelveLoc),
          FUN=mean,
          na.rm=TRUE)

detach(Carseats)
