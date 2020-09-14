
cars.data <- readRDS("carsdata.RDS")
head(cars.data)

cars.data1 <- cars.data[which(cars.data$cylinders %in% c(6,8)),] 
cars.data1$cylinders <- as.factor(as.numeric(cars.data1$cylinders)-1)
  
fit.cars1 <- lm(acceleration~weight+cylinders, data=cars.data1)
summary(fit.cars1)

predict(fit.cars1,data.frame(cylinders=as.factor(0),weight=c(3000)))

plot(acceleration~weight,data=cars.data1,col=c("red","blue")[cylinders], 
     main="Acceleration ~ Weight + Cylinders")
abline(a = fit.cars1$coefficients[1], b= fit.cars1$coefficients[2], col="red")
abline(a=fit.cars1$coefficients[1]+fit.cars1$coefficients[3],b=fit.cars1$coefficients[2], col="blue")
legend(4300,22.5,col=c("red","blue"),legend = c("cylinders = 6","cylinders = 8"),lty=c(1,1))


## categorical variable with 3 levels
fit.cars2 <- lm(acceleration~weight+cylinders,data=cars.data)
summary(fit.cars2)

## if cylinders was not encoded as factor
summary(lm(acceleration~weight+as.numeric(cylinders),data=cars.data))

## fitted values for weight =3000 and cylinders 6, 8
predict(fit.cars2,data.frame(cylinders=as.factor(6),weight=c(3000)))
16.118 +0.988 -4.965
predict(fit.cars2,data.frame(cylinders=as.factor(8),weight=c(3000)))

## plot the different fitted regression lines
plot(acceleration~weight,data=cars.data, col=c("green","red","blue")[cylinders], 
     main="Acceleration ~ Weight + Cylinders")
abline(a = fit.cars2$coefficients[1], b= fit.cars2$coefficients[2], col="green")
abline(a=fit.cars2$coefficients[1]+fit.cars2$coefficients[3],
       b=fit.cars2$coefficients[2], col="red")
abline(a=fit.cars2$coefficients[1]+fit.cars2$coefficients[4],
       b=fit.cars2$coefficients[2], col="blue")
legend(4000,25,col=c("green","red","blue"),legend = c("cylinders = 4","cylinders = 6","cylinders = 8"),lty=c(1,1))


## choose a different ref. level with function relevel()
cars.data2 <- within(cars.data, cylinders <- relevel(cylinders, ref = 2))
fit.cars3 <-  lm(acceleration~weight+cylinders,data=cars.data2)

## The coefficients and the p-values are different!
summary(fit.cars3)

## fit without the categorical predictor
fit.w <- lm(acceleration~weight,data=cars.data)
## test for the significance of the categorical predictor:
anova(fit.w,fit.cars2)
anova(fit.w,fit.cars3)
## the result is the same regardless of reference level

## doing the F-test (anova test) by hand
f <- (2406.7-1967)/1967*(387/2)
1-pf(f,2,387)

## including the interaction of weight and cylinders
fit.cars4 <- lm(acceleration~weight*cylinders,data=cars.data)
summary(fit.cars4)
## or
summary(lm(acceleration~weight+cylinders+weight:cylinders,data=cars.data))

## plotting the different regression lines
plot(acceleration~weight,data=cars.data, col=c("green","red","blue")[cylinders], 
     main="Acceleration ~ Weight * Cylinders")
abline(a = fit.cars4$coefficients[1], b= fit.cars4$coefficients[2], col="green")
abline(a=fit.cars4$coefficients[1]+fit.cars4$coefficients[3],
       b=fit.cars4$coefficients[2]+fit.cars4$coefficients[5], col="red")
abline(a=fit.cars4$coefficients[1]+fit.cars4$coefficients[4],
       b=fit.cars4$coefficients[2]+fit.cars4$coefficients[6], col="blue")
legend(4000,25,col=c("green","red","blue"),legend = c("cylinders = 4","cylinders = 6","cylinders = 8"),lty=c(1,1))


## fit with interaction and different ref. level:
fit.cars4.relev <- lm(acceleration~weight*cylinders,data=cars.data2)
## Now all p-values are significant at the .5 level!
summary(fit.cars4.relev)

anova(fit.cars3, fit.cars4)

## including some other predictors and interactions
fit.cars5 <- lm(acceleration~weight+cylinders+horsepower+mpg +
                  weight:cylinders + weight:horsepower + weight:mpg+
                  cylinders:horsepower + cylinders:mpg+ 
                  horsepower:mpg + weight:horsepower:mpg ,data=cars.data)
summary(fit.cars5)


## including polynomials of degree 2 and 3 for predictor weight
fit.cars6 <- lm(acceleration~weight+cylinders+horsepower+mpg +
                  weight:cylinders + weight:horsepower + weight:mpg+
                  cylinders:horsepower + cylinders:mpg+ 
                  horsepower:mpg + weight:horsepower:mpg + 
                  I(weight^2) + I(weight^3),data=cars.data)
summary(fit.cars6)

## Consider using the function poly to add polynomials
?poly
dat1 <- as.matrix(data.frame(mpg=cars.data$mpg, horsepower=cars.data$horsepower,
                   weight=cars.data$weight))
fit.cars.star <-  lm(acceleration~poly(dat1,3),data=cars.data)
summary(fit.cars.star)

## Problems with correlated polynomials
dat2 <- as.matrix(data.frame(mpg=cars.data$mpg, mpg2 =cars.data$mpg^2, mpg3=cars.data$mpg^3,
                             horsepower=cars.data$horsepower, horsepower2=cars.data$horsepower^2,
                             horsepower3=cars.data$horsepower^2,
                             weight=cars.data$weight, weight2=cars.data$weight^2, weight3=cars.data$weight^2))
cor(dat2)

## Center the data
dat3 <- as.matrix(data.frame(mpg=scale(cars.data$mpg,scale=F), mpg2 =scale(cars.data$mpg,scale=F)^2, 
                             mpg3=scale(cars.data$mpg,scale=F)^3,
                             horsepower=scale(cars.data$horsepower,scale=F), 
                             horsepower2=scale(cars.data$horsepower,scale=F)^2,
                             horsepower3=scale(cars.data$horsepower,scale=F)^3,
                             weight=scale(cars.data$weight,scale=F),
                             weight2=scale(cars.data$weight,scale=F)^2,
                             weight3=scale(cars.data$weight,scale=F)^3))

cor(dat3)


## By contrast the correlations after using the poly function:
cor(poly(dat1,3))

library(corrgram)
corrgram(poly(dat1,3))

library(corrplot)
corrplot(cor(poly(dat1,3)))

## Comment on R-squared
## only weight included
summary(fit.w)
## weight and cylinders
summary(fit.cars2)
summary(fit.cars4)
## further including horsepower and mpg
summary(fit.cars5)
summary(fit.cars6)
## Multiple R-squared alway increases as you add more predictors
## What about the residual standard error?

## See what happens if we perform a simple regression for each level of cylinders:
cars4 <- subset(cars.data,cars.data$cylinders=="4")
cars6 <- subset(cars.data,cars.data$cylinders=="6")
cars8 <- subset(cars.data,cars.data$cylinders=="8")

## intercepts from the simple LRs
coef(lm(acceleration~weight,data=cars4))[1]
coef(lm(acceleration~weight,data=cars6))[1]
coef(lm(acceleration~weight,data=cars8))[1]

coef(lm(acceleration~weight + cylinders,data=cars.data))

## individual intercepts from the MLR
coef(lm(acceleration~weight + cylinders,data=cars.data))[1]
(coef(lm(acceleration~weight + cylinders,data=cars.data))[1]
 +coef(lm(acceleration~weight + cylinders,data=cars.data))[3])
(coef(lm(acceleration~weight + cylinders,data=cars.data))[1]
+coef(lm(acceleration~weight + cylinders,data=cars.data))[4])

## What if we had included the interaction term in the multiple linear regression?
