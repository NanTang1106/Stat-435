## Fuel consumption example
## Weisberg p15-18
?fuel2001
new.fuel <- data.frame(Fuel=1000*fuel2001$FuelC/fuel2001$Pop, Tax=fuel2001$Tax, 
                       Dlic=1000*fuel2001$Drivers/fuel2001$Pop,
                       Income=fuel2001$Income, logMiles=log(fuel2001$Miles))
plot(new.fuel)

## Multiple linear regression fit
fit1 <- lm(Fuel~Tax + Dlic + Income + logMiles,data=new.fuel)
summary(fit1)


## See the coefficent estimate for Tax changes:
fit2 <-  lm(Fuel~Tax ,data=new.fuel)
summary(fit2)

fit3 <-  lm(Fuel~Tax+ Dlic ,data=new.fuel)
summary(fit3)

fit4 <-  lm(Fuel~Income + logMiles + Tax + Dlic, data=new.fuel)
summary(fit4)


####### Testing and confidence intervals

#####
anova(fit3,fit1)

fit.empty <- lm(Fuel~1,data=new.fuel)
anova(fit.empty,fit1)

summary(fit1)

### Extract p-values of fit1
p.vals <- summary(fit1)$coefficients[2:5,4]

## Bonferroni
bf <- 0.05/rep(4,4)
bf

p.vals

which(p.vals<bf)

## Alternatively:
p.adjust(p.vals, method = "bonferroni")

which(p.adjust(p.vals, method = "bonferroni")<.05)

## Holm
hm <- 0.05/c(4,3,2,1)
hm

sort(p.vals)

(sort(p.vals)<hm)

## Alternatively:
p.adjust(p.vals, method = "holm")

(sort(p.adjust(p.vals, method = "holm"))<.05)


## FDR control 0.05
fdr <- 0.05/4*c(1,2,3,4)
fdr

sort(p.vals)

(sort(p.vals)<fdr)

## Alternatively:
p.adjust(p.vals, method = "fdr")

(sort(p.adjust(p.vals, method = "fdr"))<.05)


## suppose you wanted to control FWER at .05 level
## and you obtained p-values 0.01, 0.028, 0.029

pvals <- c(0.01,0.028,0.029)

## Bonferroni
bf <- 0.05/rep(3,3)
bf

(pvals<bf)

## According to the Bonferroni procedure: 
## Which p-values indicate that the null hypothesis should be rejected?

## Answer: 0.01

## Holm 
hm <- 0.05/c(3,2,1)
hm

sort(pvals)

(sort(pvals)<hm)

## According to the Holm procedure: 
## Should you reject the null hypothesis corresponding to p-value: 0.029?

## Answer: No.

