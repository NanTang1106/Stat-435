## 
getwd()
setwd("/Users/ema/uwdrive/stat 504/Slides/logistic regression/")

dat = read.table("anesthetic.txt", header = T)
str(dat)
dat$movement

fit.raw = glm(movement ~ conc, data=dat, family=binomial)
summary(fit.raw)

predict(fit.raw, type="link")
predict(fit.raw, type="response")

## plot the logit curve
layout(matrix(1:2,2,1))
my.etas = seq(-8,8, by=.01)
my.prob = 1/(1+exp(-my.etas))
plot(my.etas, my.prob, type="l", bty="n",
     xlab="linear predictor: log-odds eta",
     ylab="probability of ’success’")
abline(h=0); abline(h=1);
lines(c(-10,0),c(.5,.5), lty=2)
lines(c(0,0),c(0,.5), lty=2)
my.conc = seq(0,2.5,by=.05)
my.etas = -6.469  + 5.567 * my.conc
my.prob = 1/(1+exp(-my.etas))
plot(my.conc, my.prob, type="l", bty="n", adj=1,
     xlab="", ylab="prob. no movement")
mtext("concentration", side=1, line=0.4)
mtext("eta", side=1, line=2.4)
mtext("-6.5\n(intercept)",side=1,at=0, line=4)
mtext("-0.9\n(-6.5+5.6)",side=1,at=1, line=4)
conc.5 = (0-(-6.469))/5.567
mtext("0",side=1,at=conc.5, line=3)
mtext("4.7\n(-6.5+2*5.6)",side=1,at=2, line=4)
lines(c(-1,conc.5),c(.5,.5), lty=2)
lines(c(conc.5,conc.5),c(0,.5), lty=2)

## Plot of movement probability versus concentration
plot(movement ~ conc, data=dat)
plot(movement ~ as.factor(conc), data=dat)
plot(nomove~ conc, data=dat)
plot(jitter(nomove)~ conc, data=dat)
plot(jitter(nomove,amount=.02)~ conc, data=dat)
myconc = seq(0.8,2.5,by=.05)
lines(myconc, predict(fit.raw, type="response",
                      list(conc = myconc)))


with(dat, table(movement, conc))


dat2 = data.frame( conc = c(.8,1,1.2,1.4,1.6,2.5),
                   total = c(7,5,6,6,4,2),
                   prop = c(1/7,1/5,4/6,4/6,4/4,2/2))
fit.tot = glm(prop~ conc, data=dat2, weights=total,  family=binomial)
predict(fit.tot, type="link")
predict(fit.tot, type="response")

summary(fit.tot)


plot(prop ~ conc, data=dat2)
lines(myconc, predict(fit.raw, type="response", list(conc=myconc)))


### Runoff
runoff = read.table("runoff.txt",header=T)
plot( RunoffEvent ~ Precip, data=runoff)
plot(jitter(RunoffEvent,amount=.02) ~ Precip, data=runoff)
library(lattice)
 
densityplot(~ StormDuration,groups=factor(RunoffEvent),
            data=runoff,auto.key=list(columns=2))
densityplot(~ Precip,groups=factor(RunoffEvent),
            data=runoff,auto.key=list(columns=2))



fit1 <-  glm(RunoffEvent~ Precip,data=runoff, family=binomial)
summary(fit1)


plot(jitter(RunoffEvent, amount=.02)~ Precip, data=runoff)
myprecip = seq(0,5, by=.01)
lines(myprecip, predict(fit1,  list(Precip = myprecip),type="response"))


newdat <-  data.frame(Precip=c(0, 0.25, 0.5, 0.75, 1.0, 1.1,
                             1.25, 1.5, 1.75, 2.0, 4.0))
predict(fit1, newdat)
predict(fit1, newdat, type="response")

runoff$MaxIntensity10
fit2 <-  glm(RunoffEvent~ Precip + MaxIntensity10,
                      data=runoff, family=binomial)
summary(fit2)



fit3 <-  glm(RunoffEvent~ Precip * MaxIntensity10,
                         data=runoff, family=binomial)
summary(fit3)



plot(jitter(RunoffEvent,amount=.02) ~ Precip, data=runoff,
     ylab="Probability of runoff event")
legend("right",pch=1,col=c("blue","red","green"),
       legend=c("1.0","0.8","0.24"),title="MaxIntensity10")
myprecip = seq(0,5,0.02)             # calculate predictions
prob1 = predict(fit2,type="response",
                data.frame(Precip=myprecip, MaxIntensity10=0.24))
prob2 = predict(fit2,type="response",
                data.frame(Precip=myprecip, MaxIntensity10=0.80))
prob3 = predict(fit2,type="response",
                data.frame(Precip=myprecip, MaxIntensity10=1.00))
lines(myprecip, prob1, col="green")  # draw prediction curves
lines(myprecip, prob2, col="red")
lines(myprecip, prob3, col="blue")
abline(h=0,lty=2)                    # Add horizontal lines
abline(h=1,lty=2)


### with interaction

plot(jitter(RunoffEvent,amount=.02) ~ Precip, data=runoff,
     ylab="Probability of runoff event")
legend("right",pch=1,col=c("blue","darkblue","black"),
       legend=c("1.0","0.8","0.24"),title="MaxIntensity10")
myprecip = seq(0,5,0.02)             # calculate predictions
prob1 = predict(fit3,type="response",
                data.frame(Precip=myprecip, MaxIntensity10=0.24))
prob2 = predict(fit3,type="response",
                data.frame(Precip=myprecip, MaxIntensity10=0.80))
prob3 = predict(fit3,type="response",
                data.frame(Precip=myprecip, MaxIntensity10=1.00))
lines(myprecip, prob1, col="black")  # draw prediction curves
lines(myprecip, prob2, col="darkblue")
lines(myprecip, prob3, col="blue")
abline(h=0,lty=2)                    # Add horizontal lines
abline(h=1,lty=2)


babyfood = read.table("babyfood.txt", header=T)
# re-ordering the food levels, non-alphabetically:
babyfood$food = factor(babyfood$food,  levels = c("bottle","mixed","breast"))
babyfood$nondisease = with(babyfood, total - disease)
xtabs(disease/total ~ sex+food, babyfood) 


plot(xtabs(disease/total~ sex+food, babyfood),
     main="Respiratory disease incidence in 1st year")
plot(xtabs(disease/total~ food+sex, babyfood), 
     main="Respiratory disease incidence in 1st year")


l1 = with(babyfood, tapply(disease,    food, sum))
l2 = with(babyfood, tapply(nondisease, food, sum))
l1
l2
cbind(l1, l2)
chisq.test(cbind(l1,l2))



fit <-  glm(disease/total ~ sex + food, weight=total,
          family=binomial, data=babyfood)
fit <-  glm(cbind(disease, nondisease)~ sex + food, family=binomial, data=babyfood)
summary(fit)
pchisq(0.72192, df=2, lower.tail=F)
babyfood$total             


summary(fit.raw)
summary(fit.tot)
pchisq(27.754, df=28, lower.tail=F)  # don’t trust this one
pchisq(1.7321, df=4, lower.tail=F)


residuals(fit.raw, type="response")[1:4]
residuals(fit.tot, type="response")


residuals(fit.raw, type="pearson")
residuals(fit.tot, type="pearson")


residuals(fit.raw) 
residuals(fit.tot)


layout(matrix(1:4,2,2))
plot(fit.raw)
plot(fit.tot)
plot(fit2)


summary(fit1)
summary(fit2)
pchisq(148.13-116.11, df=229-228, lower.tail=F)

anova(fit1, fit2, test="Chisq")

anova(fit2, test="Chisq")
drop1(fit2, test="Chisq") 


anova(fit1, fit2, fit3, test="Chisq")



AIC(fit1)
AIC(fit2)
AIC(fit3)


summary(fit2)

summary(fit)
# CI for breastfeeding effect:
c(-0.6693 - 2*0.1530, -0.6693 + 2*0.1530)
# CI for change in odds due to breastfeeding:
exp(c(-0.6693 - 2*0.1530, -0.6693 + 2*0.1530))


library(MASS)
confint(fit)
exp(confint(fit))




rte = read.table("rte.txt", header=T)
fit.ph = glm(growth~ ph, family=binomial, data=rte)
summary(fit.ph)

plot(growth ~ ph, data=rte)
mypH = seq(4,7,by=.05)
lines(mypH, predict(fit.ph, type="response", list(ph=mypH)))



fit.aw = glm(growth~ aw, family=binomial, data=rte)
summary(fit.aw)

plot(growth~ aw, data=rte)
myaw = seq(.65,1,by=.005)
lines(myaw, predict(fit.aw, type="response", list(aw=myaw)))

fit.awph = glm(growth~ aw+ph, family=binomial, data=rte)

growthcolor = rep(NA, 68)
growthcolor[rte$growth==0] = "black"
growthcolor[rte$growth==1] = "orangered"
plot(aw~ph, data=rte, col=growthcolor)

fit.awph = glm(growth~ aw+ph, family=binomial, data=rte)
summary(fit.awph)
drop1(fit.awph, test="Chisq")

#install.packages("brglm")
library(brglm)
fit.awph = brglm(growth ~ aw+ph, family=binomial, data=rte)
summary(fit.awph)


co <- coef(fit.awph)
co

b = -co["ph"]/co["aw"]    # slope of line on a aw ̃ph plot
a50 = -co[1]/co["aw"]     # intercept of line with 1:1 odds
a80 = ( log(4) -co[1])/co["aw"] # intercept
a20 = (-log(4) -co[1])/co["aw"] # intercept
plot(aw~ph, data=rte, col=growthcolor)
abline(a80,b, col="orangered", lty=3)
abline(a50,b, col="orangered4")
abline(a20,b, col="black", lty=3)

legend("bottomleft", lty=c(3,1,3),title="odds of growth",
                 col=c("orangered","orangered4","black"),
                 legend=c("4:1","1:1","1:4"))

