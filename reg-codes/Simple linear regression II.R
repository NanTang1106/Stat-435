######################################################
library(alr4)
library(alr3)
## For the fire damage dataset:
#install.packages("s20x")
library(s20x)

### Slides part 2

## slides 3, 6-7
?ftcollinssnow
plot(Late~Early, data=ftcollinssnow)
summary(lm(Late~Early,data=ftcollinssnow))

## slides 11,12
anova(lm(Late~Early,data=ftcollinssnow))


## Snow geese data, slides 19-23
## Weisberg
?snowgeese
photo <- snowgeese$photo[which(snowgeese$photo <55)]
obs <- snowgeese$obs2[which(snowgeese$photo <55)]

lm.geese <- lm(photo~obs)
summary(lm.geese)

qt(0.975,16)

?confint
confint(lm.geese)

?confidenceEllipse
confidenceEllipse(lm.geese,grid=TRUE,xlab="Intercept",ylab="Slope",
                  main="Snow geese: 95% confidence region and intervals")

abline(h=confint(lm.geese)[2,1],lty=2,lwd=2,col="red")
abline(h=confint(lm.geese)[2,2],lty=2,lwd=2,col="red")

abline(v=confint(lm.geese)[1,1],lty=2,lwd=2,col="red")
abline(v=confint(lm.geese)[1,2],lty=2,lwd=2,col="red")

points(0,1,col="red",lwd=3)

## slide 30
#install.packages("HH")
library(HH)
ci.plot(lm.geese) 

## slide 32
anova(lm.geese)

## slide 34
plot(photo~obs, xlab="Observer's estimate", ylab="Photographic count")
abline(lm(photo~obs))

summary(lm.geese)

## slide 35
plot(Forbes$bp,Forbes$pres)
lm.forbes <- lm(pres~bp,data=Forbes)
abline(lm.forbes)

summary(lm.forbes)


## slide 36
plot(Heights)
lm.heights <- lm(dheight~mheight, data=Heights)
abline(lm.heights)

summary(lm.heights)


## slides 37-40
?fire.df
data(fire.df)

plot(fire.df)
lm.fire <- lm(damage~distance, data=fire.df)
abline(lm.fire)

summary(lm.fire)


