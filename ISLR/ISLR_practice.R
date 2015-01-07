library(ISLR)
library(plyr)
library(MASS)
library(car)      # companion to applied regresion -- diagnostic functions
library(robustHD) # used for winsorizing data




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