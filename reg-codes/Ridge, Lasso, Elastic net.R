## Example from Ryan Tibshirani:
## Example for OLS prediction error
set.seed(0)
n = 50
p = 30
x = matrix(rnorm(n*p),nrow=n)

bstar = c(runif(10,0.5,1),runif(20,0,0.3))
bstar = c(runif(10,0.5,1),rep(0,20))
bstar = c(runif(30,0.5,1))
mu = as.numeric(x%*%bstar)

par(mar=c(4.5,4.5,0.5,0.5))

hist(bstar,breaks=30,col="gray",main="",
     xlab="True coefficients")


set.seed(1)
R = 100

fit = matrix(0,R,n)
err = numeric(R)

for (i in 1:R) {
  cat(c(i,", "))
  y = mu + rnorm(n)
  ynew = mu + rnorm(n)
  
  a = lm(y~x+0)
  bls = coef(a)
  fit[i,] = x%*%bls
  err[i] = mean((ynew-fit[i,])^2)
}

prederr = mean(err)

bias = sum((colMeans(fit)-mu)^2)/n
var = sum(apply(fit,2,var))/n

bias
var
(p+1)/n
1 + bias + var
prederr

##########################

library(MASS)

set.seed(1)
R = 100
nlam = 60
lam = seq(0,25,length=nlam)

fit.ls = matrix(0,R,n)
fit.rid  = array(0,dim=c(R,nlam,n))
err.ls = numeric(R)
err.rid = matrix(0,R,nlam)

for (i in 1:R) {
  cat(c(i,", "))
  y = mu + rnorm(n)
  ynew = mu + rnorm(n)
  
  a = lm(y~x+0)
  bls = coef(a)
  fit.ls[i,] = x%*%bls
  err.ls[i] = mean((ynew-fit.ls[i,])^2)
  
  aa = lm.ridge(y~x+0,lambda=lam)
  brid = coef(aa)
  fit.rid[i,,] = brid%*%t(x)
  err.rid[i,] = rowMeans(scale(fit.rid[i,,],center=ynew,scale=F)^2)
}

aveerr.ls = mean(err.ls)
aveerr.rid = colMeans(err.rid)

bias.ls = sum((colMeans(fit.ls)-mu)^2)/n
var.ls = sum(apply(fit.ls,2,var))/n

bias.rid = rowSums(scale(apply(fit.rid,2:3,mean),center=mu,scale=F)^2)/n
var.rid = rowSums(apply(fit.rid,2:3,var))/n

mse.ls = bias.ls + var.ls
mse.rid = bias.rid + var.rid
prederr.ls = mse.ls + 1
prederr.rid = mse.rid + 1

bias.ls
var.ls
p/n

prederr.ls
aveerr.ls

cbind(prederr.rid,aveerr.rid)

par(mar=c(4.5,4.5,0.5,0.5))
plot(lam,prederr.rid,type="l",
     xlab="Amount of shrinkage",ylab="Prediction error")
abline(h=prederr.ls,lty=2)
text(c(1,24),c(1.48,1.48),c("Low","High"))
legend("topleft",lty=c(2,1),
       legend=c("Linear regression","Ridge regression"))

par(mar=c(4.5,4.5,0.5,0.5))
plot(lam,mse.rid,type="l",ylim=c(0,max(mse.rid)),
     xlab=expression(paste(lambda)),ylab="")
lines(lam,bias.rid,col="red")
lines(lam,var.rid,col="blue")
abline(h=mse.ls,lty=2)
legend("bottomright",lty=c(2,1,1,1),
       legend=c("Linear MSE","Ridge MSE","Ridge Bias^2","Ridge Var"),
       col=c("black","black","red","blue"))

############################################


## Example Ridge, ozone
library(gss)
?ozone

data(ozone, package = "gss")


##Data pre-processing:
pairs(ozone)

## check for a transformation of response
boxcox(lm(upo3~., data=ozone))

ozone$logupo3 <- log(ozone$upo3)
d.ozone <- subset(ozone, select=-upo3)


## outlier detecrtion
pairs(d.ozone, pch = ".",gap = 0.1)
plot(vdht~wdsp,data=d.ozone)

which.max(hat(d.ozone[,-10]))
(9+1)/330
(out <- which.max(d.ozone[,"wdsp"])) 
out
d.ozone[out,]
summary(d.ozone)

## we will remove this outlier
d.ozone.e <- d.ozone[-out,]



## Linear models ----------------------------
## fit 1 
fit1 <- lm(scale(d.ozone$logupo3,scale=F)~scale(as.matrix(d.ozone[,-10]))+0, data = d.ozone.e,x=TRUE,y=TRUE)
coef(fit1)

#loocv.lm(fit1)

library(lmvar)
?cv.lm
cv.lm(fit1)

#######


### Ridge regression with ozone data

require(glmnet) 
?glmnet

ridge <- glmnet(x=scale(as.matrix(d.ozone[,-10])),y=scale(d.ozone$logupo3,scale=F), alpha=0,intercept=F) 
plot(ridge, xvar="lambda", main="Ridge Regression")

ridge.cv <- cv.glmnet(x=scale(as.matrix(d.ozone[,-10])),y=scale(d.ozone$logupo3,scale=F), alpha=0,intercept=F) 
plot(ridge.cv)


## how to choose lambda
## lambda that gives the min CV error
ridge.cv$lambda.min
## largest lambda (most regularized regression) that is within 1 SE of the min CV error
ridge.cv$lambda.1se

## compare their CV errors
min(ridge.cv$cvm)
ridge.cv$cvm[which(ridge.cv$lambda == ridge.cv$lambda.1se)]

## To see that the CV error corresponding to lambda.1se is within 
## 1 SE of the CV error corresponding to lambda.min see:
ridge.cv$cvsd[which(ridge.cv$lambda == ridge.cv$lambda.min)] 
ridge.cv$cvsd[which(ridge.cv$lambda == ridge.cv$lambda.min)] + min(ridge.cv$cvm)

## the next larger lambda after lamda.1se has an error that is larger than min + 1se
ridge.cv$cvm[which(ridge.cv$lambda == ridge.cv$lambda.1se)-1]

## we can also look at the coefficients
coef(ridge.cv)
coef(glmnet(x=scale(as.matrix(d.ozone[,-10])),y=scale(d.ozone$logupo3,scale=F), alpha=0,intercept=F,
            lambda=ridge.cv$lambda.1se))
coef(glmnet(x=scale(as.matrix(d.ozone[,-10])),y=scale(d.ozone$logupo3,scale=F), alpha=0,intercept=F,
            lambda=ridge.cv$lambda.min))


## Model diagnostics
library(plotmo) # for plotres
## consider which residual plots are relevant (e.g., QQ plot is not informative in this case)
?plotres

plotres(ridge,predict.s=ridge.cv$lambda.1se,which=c(1,2,3,6))
plotres(ridge,predict.s=ridge.cv$lambda.min,which=c(1,2,3,6))


## Same procedure as  above for LASSO regression:
lasso <- glmnet(x=scale(as.matrix(d.ozone[,-10])),y=scale(d.ozone$logupo3,scale=F), alpha=1,intercept=F)  
?plot.glmnet
plot(lasso, xvar="lambda", main="Lasso Regression") 

lasso.cv <- cv.glmnet(x=scale(as.matrix(d.ozone[,-10])),y=scale(d.ozone$logupo3,scale=F), alpha=1,intercept=F) 
plot(lasso.cv)

min(lasso.cv$cvm)

## how to choose lambda
## lambda that gives the min CV error
lasso.cv$lambda.min
## largest lambda (most regularized regression) that is within 1 SE of the min CV error
lasso.cv$lambda.1se

## compare their CV errors
min(lasso.cv$cvm)
lasso.cv$cvm[which(lasso.cv$lambda == lasso.cv$lambda.1se)]

## Model diagnostics
plotres(lasso,predict.s=lasso.cv$lambda.1se,which=c(1,2,3,6))
plotres(lasso,predict.s=lasso.cv$lambda.min,which=c(1,2,3,6))

## again we can consider the coef
coef(lasso.cv)
coef(glmnet(x=scale(as.matrix(d.ozone[,-10])),y=scale(d.ozone$logupo3,scale=F), alpha=1,intercept=F,
            lambda=lasso.cv$lambda.min))


## elastic net

elastic.net <- glmnet(x=scale(as.matrix(d.ozone[,-10])),y=scale(d.ozone$logupo3,scale=F), alpha=.5,intercept=F) 
plot(elastic.net , xvar="lambda", main="Elastic Net Regression") 

elastic.net.cv <- cv.glmnet(x=scale(as.matrix(d.ozone[,-10])),y=scale(d.ozone$logupo3,scale=F), alpha=.5,intercept=F) 
plot(elastic.net.cv)

min(elastic.net.cv$cvm)
elastic.net.cv$cvm[which(elastic.net.cv$lambda == elastic.net.cv$lambda.1se)]

plotres(elastic.net,predict.s=elastic.net.cv$lambda.min)
plotres(elastic.net,predict.s=elastic.net.cv$lambda.1se)

coef(elastic.net.cv)
coef(glmnet(x=scale(as.matrix(d.ozone[,-10])),y=scale(d.ozone$logupo3,scale=F), alpha=.5,intercept=F,
            lambda=elastic.net.cv$lambda.min))

## 2-dim CV to find the best alpha and lambda at the same time
# ELASTIC NET search WITH 0 < ALPHA < 1
## we can use lambda.min or lambda.1se 
## for this example we will output lambda.min
a <- seq(0.1, 0.9, 0.05)
search <- foreach(i = a, .combine = rbind) %dopar% {
  cv <- cv.glmnet(x=scale(as.matrix(d.ozone[,-10])), y=scale(d.ozone$logupo3,scale=F), intercept=F, nfold = 10,  alpha = i)
  data.frame(cvm = cv$cvm[cv$lambda == cv$lambda.min], lambda.min = cv$lambda.min, alpha = i)
}
cv3 <- search[search$cvm == min(search$cvm), ]
md3 <- glmnet(x=scale(as.matrix(d.ozone[,-10])),y=scale(d.ozone$logupo3,scale=F),intercept=F, lambda = cv3$lambda.min, alpha = cv3$alpha)



coef(md3)


