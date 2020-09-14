# From Cosma Shalizi's lecture notes.
# Calculate Mallow's Cp for a list of linear models
# Input: List of models, all fit by lm
# Output: Vector of Cp statistics
# Presumes: All models are nested inside the largest model; all models
# fit on a common data set
Cp.lm <- function(mdl.list) {
  # How many samples do we have?
  # Presumes all models fit to the same data
  n <- nobs(mdl.list[[1]])
  # Extract the number of degrees of freedom for each model
  DoFs <- sapply(mdl.list, function(mdl) { sum(hatvalues(mdl)) })
  # Extract the MSEs of each model
  MSEs <- sapply(mdl.list, function(mdl) { mean(residuals(mdl)^2) })
  # Which model had the most parameters?
  # Presuming that model includes all the others as special cases
  biggest <- which.max(DoFs)
  # Use the nesting model's MSE to estimate sigma^2
  sigma2.hat <- MSEs[[biggest]]*n/(n-DoFs[[biggest]])
  Cp <- MSEs + 2*sigma2.hat*DoFs/n
  return(Cp)
}




## Example of usage:
## Cp.lm(list(mdl1, mdl2, mdl3))

# Calculate LOOCV score for a linear model
# Input: a model as fit by lm()
# Output: leave-one-out CV score
loocv.lm <- function(mdl) {
  return(mean((residuals(mdl)/(1-hatvalues(mdl)))^2))
}



?state.x77
data(state)
statedata <- data.frame(state.x77,row.names = state.abb)
head(statedata)

fit1 <- lm(Life.Exp~., data=statedata) ## all predictors
fit2 <- lm(Life.Exp~Population + Income + Murder,data=statedata)
fit3 <- lm(Life.Exp~Population + Income,data=statedata)

## Let us compare the Mallow's C_p of these three models
Cp.lm(list(fit1,fit2,fit3))
## We prefer the biggest model, as it has a smallest Mallow's C_p
## Among the two smaller models, we prefer the model that includes the predictor Murder

summary(fit1)$r.sq
summary(fit2)$r.sq
summary(fit3)$r.sq

summary(fit1)$adj.r.sq
summary(fit2)$adj.r.sq
summary(fit3)$adj.r.sq

AIC(fit1,fit2,fit3)
## The AIC results agree with the Mallow's C_p

## Compare with by hand calculation
AIC(fit1)
2*(8+1) + 50*(1+ log(2*pi)) + 50*log(sum(resid(fit1)^2)/50)


BIC(fit1,fit2,fit3)
## The BIC prefers fit2, that is model that includes only 
## Population, Income and Murder as Predictors

loocv.lm(fit1)
loocv.lm(fit2)
loocv.lm(fit3)
## LOOCV also prefers fit2

## for k-fold cross-validation
# install.packages("lmvar")
library(lmvar)
?cv.lm

fit1 <- lm(Life.Exp~., data=statedata, x=TRUE, y=TRUE) ## all predictors
fit2 <- lm(Life.Exp~Population + Income + Murder,data=statedata, x=TRUE, y=TRUE)
fit3 <- lm(Life.Exp~Population + Income,data=statedata, x=TRUE, y=TRUE)

fit1.cv5 <- cv.lm(fit1,5)
fit2.cv5 <- cv.lm(fit2,5)
fit3.cv5 <- cv.lm(fit3,5)

fit1.cv5
fit2.cv5
fit3.cv5
## 5-fold cross validation also prefers the second model


### Model selection procedures:
?step

#fit1 <- lm(Life.Exp ~ (Population + Income +Illiteracy +Murder + HS.Grad + Frost + Area)^2, data=statedata, x=TRUE, y=TRUE) ## all predictors

step(fit1, dir="backward", k=2)
step(fit1, dir="backward", k=log(50)) ## for BIC since there are 50 samples

fit.empty <- lm(Life.Exp~1,data=statedata)
step(fit.empty, dir="forward", k=log(50),scope=list(upper=fit1,lowwer=fit.empty))
step(fit2, dir="both", k=2) 


## All subsets
library(leaps)
regsubsets.out <- regsubsets(Life.Exp ~ (Population + Income +Illiteracy +Murder + HS.Grad + Frost + Area)^2,data = statedata)
summary(regsubsets.out)

summary(regsubsets.out)$cp
summary(regsubsets.out)$bic
summary(regsubsets.out)$adjr2


## The greedy methods are not always optimal
set.seed(123)
e1 <- rnorm(100)
e2 <- rnorm(100)
e3 <- rnorm(100)
e4 <- rnorm(100)
e5 <- rnorm(100)
x1 <- e1
x2 <- - 0.8*x1^2 + e2
x3 <- 0.5*x1^2 + e3 
y <-   - 0.2*abs(x3) + 0.1*x2 + 0.58*x1 + e4
x4  <- 0.47*y + x3 + e5 
dataset <- data.frame(x1=x1,x2=x2,x3=x3,x4=x4,y=y)
library(leaps)
?regsubsets
out <- regsubsets(y~.,data=dataset,nbest=15)
inclusion.mat <- summary(out)$which
all.var <- c('X1','X2','X3','X4')
var.idx <- lapply(1:nrow(inclusion.mat),function(x) all.var[inclusion.mat[x,-1]])
var.idx <- unlist(lapply(var.idx,function(x) paste(x,collapse = ',')))
idx <- order(summary(out)$bic,decreasing=T)
#pdf("bic_scores.pdf",width=9,height=6)
plot(1:16,c(13.5,summary(out)$bic[idx]),xaxt = 'n',xlab="",ylab="BIC",main="BIC scores (in decreasing order)")
axis(1, at=1:16, labels=FALSE)
text(x = 1:16, par("usr")[3] - 3, labels = c('None',var.idx[idx]), srt = 60, pos = 1, xpd = TRUE)




