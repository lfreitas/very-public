library(ISLR)
library(plyr)
library(MASS)
library(car)      # companion to applied regresion -- diagnostic functions
library(robustHD) # used for winsorizing data

#
#
#
#   EDA
#
#
#




college <- read.csv("http://www-bcf.usc.edu/~gareth/ISL/College.csv")

head(college)         #look at the first 10 obs
str(college)          #summary of the variable types and values


rownames(college) <- college[,1] #assign college names to the row names
college <- college[,-1]          #eliminate college name as a variable
head(college)

summary(college)      #statistical summary of all vars
pairs(college)        #pairwise scatterplots for entire data set -- takes a bit to produce

plot(college$Private,college$Outstate) # boxplot of Outstate using Private as binning var

#divide universities into two groups based on whether or not the proportion of students coming 
#from the top 10% of their high school classes exceeds 50 %
college$Elite <- rep("No",nrow(college))          #set default to No
college$Elite[college$Top10perc > 50] <- "Yes"    #apply conditional logic to convert some to Yes
college$Elite <- as.factor(college$Elite)

plot(college$Elite,college$Outstate)  #generate a boxplot of the new var and out-of-state attendance

#plot two histograms side-by-side
split.screen(c(1,2))
screen(1)
hist(college$Outstate)
screen(2)
hist(college$Apps)
close.screen(all=TRUE)

#generate a heatmap
image(college$Outstate,college$Apps,college$expend)


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
lm.fit=lm(medv~lstat,data=Boston)  #fit simple linear model
summary(lm.fit)                    #summary of regressions results
names(lm.fit)                      #results can also be accessed individually
confint(lm.fit)                    #confidence intervals for estimates
newdata <- data.frame(lstat=(c(5,10,15))) 
predict(lm.fit,newdata,interval ="confidence") 
predict(lm.fit,newdata,interval ="prediction") 


plot(lstat,medv)  
abline(lm.fit)                  #plot the fit
abline(lm.fit,lwd=3)            #customize the line, explore some symbols
abline(lm.fit,lwd=3,col="red")
plot(lstat,medv,col="red")
plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")
plot(1:20,1:20,pch=1:20)

par(mfrow=c(2,2))           #split plot area into 2x2 and print diagnostic plots
plot(lm.fit)
par(mfrow=c(1,1))
plot(predict(lm.fit), residuals(lm.fit)) #make some diagnostics bigger
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues (lm.fit))                 #plot leverage (influential predictors)
which.max(hatvalues(lm.fit))             #return index of highest-leverage obs

#
# Basic multivariate linear model
#
lm.fit<-lm(medv~.,data=Boston) #regression of medv on all other vars
summary(lm.fit)
vif(lm.fit)

lm.fit1<-lm(medv~.-age,data=Boston) #regression of medv on all other vars except age
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
summary(lm(medv~poly(lstat,2),data=Boston))
detach(Boston)



#
# Linear regression with dummies
#
attach(Carseats)
names(Carseats)
str(Carseats)

# R automatically turns factor vars into dummies
lm.fit <- lm(Sales~ShelveLoc)
summary(lm.fit)

# constrasts() lays out how the dummies are being generated
contrasts(ShelveLoc)

#more generally, this function subsets the df to vars that are factors and displays all of the constrasts
x<-sapply(Carseats[,sapply(Carseats,is.factor)],contrasts)


#fit a model with all of the vars in linear form plus a couple of interactions
lm.fit <- lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)


#use the aggregate function to average the numeric vars in the df by ShelveLoc
aggregate(Carseats[,sapply(Carseats,is.numeric)],by=list(ShelveLoc),FUN=mean,na.rm=TRUE)

detach(Carseats)



#
# Classification
#
attach(Smarket)
names(Smarket)
str(Smarket)
pairs(Smarket)

#run a correlation matrix for the numeric vars a couple of ways
cor(Smarket[,-9])
x<-cor(Smarket[,sapply(Smarket,is.numeric)])

split.screen(c(1,5))
screen(1)
hist(subset(Smarket,Year==2001)$Today)
screen(2)
hist(subset(Smarket,Year==2002)$Today)
screen(3)
hist(subset(Smarket,Year==2003)$Today)
screen(4)
hist(subset(Smarket,Year==2004)$Today)
screen(5)
hist(subset(Smarket,Year==2005)$Today)
close.screen(all=TRUE)

#produce a couple of conditional plots. Note that using Year as a number allows bleed across panels, while turning it into a factor makes the plot exclusive by year
coplot(Volume~Today | Year, data=Smarket, rows=1, overlap=0)
coplot(Volume~Today | as.factor(Year), data=Smarket, rows=1, overlap=0)

#testing out a winsorization routine
summary(Smarket[,2:8])
wins <- sapply(Smarket[,2:8],winsorize)
summary(wins)