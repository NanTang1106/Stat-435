## Most of the data we will be relying in this course 
## comes from packages alr4 and alr3
library(alr4)
library(alr3)


### Slides part 1

## slide 10
?Forbes
xbar <- mean(Forbes$bp)
ybar <- mean(Forbes$pres)
sxx <- sum((Forbes$bp - xbar)^2)
sxy <- sum((Forbes$bp - xbar)*(Forbes$pres - ybar))

beta1.hat <- sxy/sxx
beta0.hat <- ybar-beta1.hat*xbar
cat(beta0.hat,beta1.hat)

## compare with the output of the regression fit
summary(lm(pres~bp,data=Forbes))

## slide 21
plot(Forbes$bp,Forbes$pres)
abline(lm(pres~bp,data=Forbes))

## slide 20
photo <- snowgeese$photo[which(snowgeese$photo <55)]
obs <- snowgeese$obs2[which(snowgeese$photo <55)]

plot(lm(photo~obs),which=c(1))

##or
lm.geese <- lm(photo~obs)
plot(lm.geese$fitted.values, lm.geese$residuals, xlab="Fitted values",
     ylab="Residuals", main="Snow geese data: Residuals versus fitted")
abline(0,0)

## slide 21
plot(lm(pres~bp,data=Forbes),which=c(1))

## or alternatively
lm.forbes <- lm(pres~bp,data=Forbes)
plot(lm.forbes$fitted.values, lm.forbes$residuals, xlab="Fitted values",
     ylab="Residuals", main="Forbes data: Residuals versus fitted")
abline(0,0)


## slide 22
plot(lm(dheight~mheight, data=Heights),which=c(1))

##or
lm.heights <- lm(dheight~mheight, data=Heights)
plot(lm.heights$fitted.values, lm.heights$residuals, xlab="Fitted values",
     ylab="Residuals", main="Heights data: Residuals versus fitted")
abline(0,0)


## slide 26
summary(lm(pres~bp,data=Forbes))
RSS <- sum(lm(pres~bp,data=Forbes)$residuals^2)
n <- 17
sigma.hat.sq <- RSS/(n-2)

## compare the square root of sigma.hat.sq with
## the residual standard error from the summary of the lm output
sqrt(sigma.hat.sq)
summary(lm(pres~bp,data=Forbes))

## slide 32
beta0.hat
se.b0.hat <- sqrt(sigma.hat.sq)*sqrt(1/n + xbar^2/sxx)
t.05.15 <- qt(0.95,15)

beta0.hat - t.05.15*se.b0.hat
beta0.hat + t.05.15*se.b0.hat

## compare with 
confint(lm(pres~bp,data=Forbes),level=.9)

