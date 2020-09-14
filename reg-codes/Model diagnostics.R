library(faraway)

data("savings")
head(savings)
?savings

fit1 <- lm(sr~. , data=savings)
summary(fit1)

## We will be talking about all 4 of the following plots,
## but for now we focus on the first plot
par(mfrow=c(2,2))
plot(fit1)
par(mfrow=c(1,1))

## Tukey-Anscombe plot
plot(fit1,which=1)

## or
plot(resid(fit1)~fitted(fit1), main="Residuals vs. Fitted")
abline(h=0)


## plotting residuals against predictors
par(mfrow=c(2,2))
plot(resid(fit1)~savings$pop15)
abline(h=0)
plot(resid(fit1)~savings$pop75)
abline(h=0)
plot(resid(fit1)~savings$dpi)
abline(h=0)
plot(resid(fit1)~savings$ddpi)
abline(h=0)


## another plot for checking constant variance
par(mfrow=c(1,1))
plot(resid(fit1)^2~fitted(fit1), main="Squared Residuals vs. Fitted")

## consider also the slope in the following model:
fit.res <- lm(resid(fit1)^2~fitted(fit1))
## is the slope significant?
summary(fit.res)


## From Faraway's book. Examples of: 
par(mfrow=c(3,3))
## Constant Variance
for(i in 1:9) plot(1:50,rnorm(50))
## Strong non-constant variance
for(i in 1:9) plot(1:50,(1:50)*rnorm(50))
## Mild non-constant variance
for(i in 1:9) plot(1:50,sqrt((1:50))*rnorm(50))
## Non-linearity
for(i in 1:9) plot(1:50,cos((1:50)*pi/25)+rnorm(50))


## Residuals against index
par(mfrow=c(1,1))
plot(resid(fit1)~c(1:length(resid(fit1))), main="Residuals vs. Index", 
     xlab="Index", ylab="Residuals")
abline(h=0)


## standardized residuals vs. fitted
plot(rstandard(fit1)~fitted(fit1), main="Standardized residuals vs. fitted", 
     xlab = "Fitted values", ylab="Standardized residuals")
abline(h=0)
## studentized residuals vs. fitted
plot(rstudent(fit1)~fitted(fit1), main="Studentized residuals vs. fitted", 
     xlab = "Fitted values", ylab="Studentized residuals")
abline(h=0)

## Square root of the Standardized residuals vs fitted values
plot(fit1, which=3)

## examples for the normal QQ plot
set.seed(1)
par(mfrow=c(2,2))
qqnorm(rnorm(50), main="Standard Normal")
abline(a=0,b=1)
qqnorm(exp(rnorm(50)), main = "Lognormal (a skewed distribution)")
abline(a=0,b=1)
qqnorm(rcauchy(50), main = "Cauchy (long-tailed distribution)")
abline(a=0,b=1)
qqnorm(runif(50), main = "Uniform (short-tailed distribution)")
abline(a=0,b=1)


## some more examples
par(mfrow=c(3,3))
## Standard normal
for(i in 1:9){ 
  qqnorm(rnorm(50)) 
  abline(a=0,b=1)}
## Log-normal
for(i in 1:9){
  qqnorm(exp(rnorm(50)))
  abline(a=0,b=1)}
## Cauchy
for(i in 1:9){ 
  qqnorm(rcauchy(50))
  abline(a=0,b=1)}
## Uniform
for(i in 1:9){
  qqnorm(runif(50))
  abline(a=0,b=1)}
par(oldpar)


## histogram of the standardized residuals
## with a normal curve
par(mfrow=c(1,2))
hist(rstandard(fit1),freq=F)
curve(dnorm(x,0,1), xlim=c(-3,3),add=TRUE,lty=1,col=1,lwd=1) 

qqnorm(rstandard(fit1))
abline(a=0,b=1)


## the residual normal QQ plot is also available through:
par(mfrow=c(1,1))
plot(fit1, which = 2)


## artificial data
x1 <- rnorm(1000)
eps <- rnorm(1000)
y <- 2+ 3*x1 + 1.5*x1^2 + eps

fit.artificial <- lm(y~x1)

par(mfrow=c(1,2)) 
plot(fit.artificial,which=1 )
plot(resid(fit.artificial)~x1,main="Residuals vs. x1")

## the relationship does not appear to be linear
par(mfrow=c(1,1)) 
plot(y~x1)

## appears is a bit "more" linear
par(mfrow=c(1,1)) 
plot(x1^2+x1,y)

## add a quadratic term
fit.artificial2 <- lm(y~x1 + I(x1^2))
summary(fit.artificial2)

## residual plots look ok now
par(mfrow=c(1,2)) 
plot(fit.artificial2,which=1)                     
plot(resid(fit.artificial2),x1)


## Transformations
library(alr4)
?brains

## first fit a simple linear regression
fit.brain <- lm(BrainWt~BodyWt, data=brains)

## the residual plots are not too convincing
par(mfrow=c(1,2)) 
plot(resid(fit.brain)~brains$BodyWt, main= "Residuals of BrainWt ~ BodyWt vs. BodyWt")
plot(fit.brain,which=1)

## data comes from a right-skewed distribution
par(mfrow=c(1,3)) 
hist(brains$BodyWt)
hist(brains$BrainWt)
plot(BrainWt~BodyWt,data=brains, main="Brain Weight vs Body Weight")

## the skeweness of the data is obvious
par(mfrow=c(1,1)) 
plot(BrainWt~BodyWt,data=brains, main="Brain Weight vs Body Weight")

## which relationship appears the 'most' linear?
par(mfrow=c(2,2))
plot((1/brains$BodyWt),(1/brains$BrainWt), xlab="1/BodyWt", ylab="1/BrainWt")
plot(sqrt(brains$BodyWt),sqrt(brains$BrainWt), xlab="sqrt(BodyWt)", ylab="sqrt(BrainWt)")
plot(brains$BodyWt^(1/3),brains$BrainWt^(1/3), xlab="(BodyWt)^(1/3)", ylab="(BrainWt)^(1/3)")
plot(log(brains$BodyWt),log(brains$BrainWt), xlab="log(BodyWt)", ylab="log(BrainWt)")
par(mfrow=c(2,2))


## Box-Cox and Yeo-Johnson
library(MASS)

par(mfrow=c(1,1))

## Savings example
boxcox(fit1,plotit=T)
title(main = "95% CI for the Box-Cox transform.")

library(car)
boxCox(fit1)

## Brains example
boxcox(fit.brain)
title(main = "95% CI for the Box-Cox transform.")

boxcox(lm(BrainWt~log(BodyWt),data=brains))

boxCox(fit.brain,family="yjPower")

## for transforming the data once you obtain lambda
?bcPower


##Side note: the F test comparing two models that  only differ 
##in a single effect of a predictor will yield the same result 
## as the t-test of that predictor effect in the bigger model
fit1 <- lm(sr~. , data=savings)

summary(fit1)
fit1prime <- lm(sr~pop75+dpi+ddpi,data=savings)
anova(fit1,fit1prime)


