library(ISLR)
library(plyr)
library(dplyr)
library(MASS)
library(car)      # companion to applied regresion -- diagnostic functions
library(robustHD) # used for winsorizing data
library(sm)       # for overlayed density plots



#
# Logistic Regression
#

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

# produce a couple of conditional plots. Note that using Year as a number allows
# bleed across panels, while turning it into a factor makes the plot exclusive 
# by year
coplot(Volume~Today | Year, data=Smarket, rows=1, overlap=0)
coplot(Volume~Today | as.factor(Year), data=Smarket, rows=1, overlap=0)

#testing out a winsorization routine
summary(Smarket[,2:8])
wins <- sapply(Smarket[,2:8],winsorize)
summary(wins)

# look at volume over time
split.screen(c(1,2))
screen(1)
plot(Smarket$Volume)
screen(2)
boxplot(Smarket$Volume~Smarket$Year)
close.screen(all=TRUE)

# logistic regression of direction on other vars
glm.fit <- glm(Direction∼Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
               data=Smarket,
               family=binomial)
# glm() returns an object that summarizescaptures many aspects of the regression 
# a few of the more helpful ones are below
summary(glm.fit) # summary of results focused on fit and coeff significance
coef(glm.fit) # vector of coeffs
hist(glm.fit$fitted.values) # histogram of predicted probs
# generate predicted probs for the training data (if type is not given, returns
# the logit)
glm.probs <- predict(glm.fit, type="response")
head(glm.probs) 

# next few steps produce a confusion matrix of predictions vs. actual values
contrasts(Smarket$Direction)
glm.pred <- rep("Down", length(glm.probs))
glm.pred[glm.probs > 0.5] <- "Up"
# using a different label for the predictions makes it easier to see that they
# appear as the rows
# glm.pred <- rep("Pr_Down", length(glm.probs))
# glm.pred[glm.probs > 0.5] <- "Pr_Up"
table(glm.pred, Smarket$Direction)
mean(glm.pred==Smarket$Direction) # correct ~52% of time based on training


# build a model that uses historical data to predict the future
train <- (Smarket$Year < 2005)
Smarket2005 <- Smarket[!train, ]
Direction2005 <- Smarket$Direction[!train]
glm.fit <- glm(Direction∼Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
               data=Smarket,
               family=binomial,
               subset=train)
glm.probs <- predict(glm.fit, Smarket2005, type="response")
glm.pred <- rep("Down", length(glm.probs))
glm.pred[glm.probs > 0.5] <- "Up"
table(glm.pred, Smarket2005$Direction)
mean(glm.pred==Smarket2005$Direction) # correct ~48% of time based on validation



#
# Linear Discriminant Analysis
#

lda.fit <- lda(Direction∼Lag1+Lag2, data=Smarket, subset=train)
lda.fit

# produce plots of the discriminants vs. actual classes
plot(lda.fit)
lda.pred <- predict(lda.fit, Smarket2005)
names(lda.pred)
head(data.frame(lda.pred))

# show the range of the discriminant for each class
range(lda.pred$x[lda.pred$class=="Down"])
range(lda.pred$x[lda.pred$class=="Up"])
# can do the same for the probability of market going up
# range(lda.pred$posterior[,2][lda.pred$class=="Down"])
# range(lda.pred$posterior[,2][lda.pred$class=="Up"])

# generate confusion matrix vs. actuals
lda.class <- lda.pred$class
table(lda.class, Direction2005)
sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)

# compare class assignments of LDA vs. logistic regression
table(lda.class, glm.pred)

# compare prob densit\ies of LDA with logistic regression
probs <- data.frame(lda=lda.pred$posterior[,2], 
                   logistic=glm.probs)
plotlda <- data.frame(prob=lda.pred$posterior[,2], 
                      lab = "lda")
plotlog <- data.frame(prob=glm.probs, 
                      lab = "log")
plotdat <- rbind(plotlda, plotlog)

sm.density.compare(plotdat$prob,plotdat$lab)
colfill<-c(2:(2+length(levels(factor(plotdat$lab))))) 
legend(x="topleft", levels(plotdat$lab), fill=colfill)

