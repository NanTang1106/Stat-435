# Simulation: two independent Gaussians
set.seed(123)
x1 <- rnorm(100, mean=70, sd=15)
x2 <- rnorm(100, mean=70, sd=15)

# Add in a linear combination of X1 and X2 
x3 <- (x1+x2)/2


pairs(cbind(x1,x2,x3))
cor(cbind(x1,x2,x3))

library(faraway)
?seatpos

fit.carseat <- lm(hipcenter~.,data=seatpos)
summary(fit.carseat)


library(corrplot)

par(mfrow=c(1,1))
corrplot(cor(seatpos[,-9]))


vif(fit.carseat)

fit.carseat2 <- lm(hipcenter~Age + Weight +Ht, data=seatpos)
summary(fit.carseat2)
vif(fit.carseat2)

age    <- seatpos$Age
bmi    <- (seatpos$Weight*0.454)/(seatpos$Ht/100)^2
shoes  <- seatpos$HtShoes-seatpos$Ht
seated <- seatpos$Seated/seatpos$Ht
arm    <- seatpos$Arm/seatpos$Ht
thigh  <- seatpos$Thigh/seatpos$Ht
leg    <- seatpos$Leg/seatpos$Ht

seatpos.new <- data.frame(age=age,bmi=bmi,height=seatpos$Ht,
                          shoes=shoes,seated=seated,
                          arm=arm,thigh=thigh,leg=leg,hipcenter=seatpos$hipcenter)
fit.carseat3 <- lm(hipcenter~.,data=seatpos.new)
summary(fit.carseat3)
vif(fit.carseat3)
pdf("/Users/ema/uwdrive/stat 504/Slides/img/correl3.pdf")
corrplot(cor(seatpos.new[,-9]))
?cor
?corrplot
dev.off()

library(faraway)
?star
data(star)
pdf("/Users/ema/uwdrive/stat 504/Slides/img/star1.pdf")
plot(light~temp,data=star)
dev.off()
pdf("/Users/ema/uwdrive/stat 504/Slides/img/star2.pdf")
hist(star$temp)
dev.off()


pdf("/Users/ema/uwdrive/stat 504/Slides/img/star3.pdf",height=4,width=8)
par(mfrow=c(1,3))
plot(light~temp,data=star)
hist(star$temp,main="Temperature")
boxplot(star$temp,main="Temperature")
dev.off()
pdf("/Users/ema/uwdrive/stat 504/Slides/img/star5.pdf")
par(mfrow=c(1,1))
plot(light~temp,data=star)
fit.star1 <- lm(light ~ temp, data=star)
abline(fit.star1, lwd=2,lty=1,col="red")
fit.star2 <- lm(light ~temp, data=star, subset=(temp>3.6))
abline(fit.star2,lwd=2,lty=2,col="blue")
legend(4.15,6.3,legend=c("all points","excluding outliers"),lty=c(1,2), 
       col=c("red","blue"),lwd=c(2,2))
dev.off()

sort(abs(resid(fit.star1)))
sort(abs(rstudent(fit.star1)))

## note that R already labels the concerning points
plot(fit.star1,which=1)

## let's look at a diagnostic plot
plot(rstudent(fit.star1)~fitted(fit.star1))
abline(h=0)
s1 <- smooth.spline(fitted(fit.star1),rstudent(fit.star1),df=5)
lines(s1, col = "gray")

plot(fit.star1,which=4,main="Star data")

qf(0.5,2,45)
qf(0.5,2,3450)
qf(0.5,20,3450)

cooks.distance(fit.star1)

plot(fit.star1,which=5, main="Star data")


?savings
data("savings")

fit1 <- lm(sr~. , data=savings)


head(savings)
X <- as.matrix(savings[,-1])
H <- X%*%solve((t(X)%*%X))%*%t(X)
## leverage
diag(H)

par(mfrow=c(1,2))
plot(fit1,which=1)
## studentized residuals vs. fitted
plot(rstudent(fit1)~fitted(fit1), main="Studentized residuals vs. fitted", 
     xlab = "Fitted values", ylab="Studentized residuals")
abline(h=0)
s2 <- smooth.spline(fitted(fit1),rstudent(fit1),df=5)
lines(s2, col = "gray")


##Cook's distance

cooks.distance(fit1)
par(mfrow=c(1,2))
plot(fit1,which=4)
plot(fit1,which=5)


library(alr4)
?brains


fit.brain <- lm(BrainWt~BodyWt, data=brains)

par(mfrow=c(1,3))
plot(BrainWt~BodyWt, data=brains)
abline(fit.brain)
plot(fit.brain,which=c(4,5))


fit.brain2 <- lm(log(BrainWt)~log(BodyWt), data=brains)

par(mfrow=c(1,3))
plot((log(BrainWt))~(log(BodyWt)), data=brains)
abline(fit.brain2)
plot(fit.brain2,which=c(4,5))
