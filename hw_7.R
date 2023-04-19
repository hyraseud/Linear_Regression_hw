#hw7 512

#q1

#set up X1, X2, X3

x1 <- BikeProject$tempeture
x2 <- BikeProject$humidity
x3 <- BikeProject$windspeed

y <- BikeProject$count

#weighted LM with simulated heteroscedastic data

x<-rep(seq(1:10),100)
y<-10*x+rnorm(10,mean=0, sd=5*x)
het<-data.frame(x1,x2,x3,y)
plot(lm(y~x1+x2+x3,het))

het.mod<-lm(y~x1+x2+x3, het)


het.mod2<-lm((y)~x1+x2+x3, data=het)

plot(het.mod2)
summary(het.mod2)
confint(het.mod2)

summary(het.mod)

plot(y~x1+x2+x3, data=het, main="Model Comparison")
lines(het$x, predict(het.mod), col="red", lwd=2, lty=1)
lines(het$x, predict(het.mod2), col="blue", lwd=2, lty=2)
legend(2, 150, legend=c("OLS","WLS"), col=c("red","blue"),lty=1:2)




#Compare the standarized residuals (fitted, residual) for OLS and WLS

plot(fitted(het.mod), rstandard(het.mod))
plot(fitted(het.mod2), rstandard(het.mod2))

library(boot)
library(lmridge)
library(MASS)
# return coefficients
data<-het
boot.wlscoef <- function(data, indices, maxit=100) {
  data <- data[indices,]
  colnames(data)<-c("x1","x2","x3","y")
  data.mod<-lm(y~x1+x2+x3, data)
  wts1<-1/fitted(lm(abs(residuals(data.mod))~x1+x2+x3, data))^2
  data.mod2<-lm(y~x1+x2+x3, data)
  return(coef(data.mod2))
}

wls_model_bootcoeff <- boot(data=het, statistic = boot.wlscoef, R=1000, maxit=100)
wls_model_bootcoeff
boot.ci(wls_model_bootcoeff, type="perc", index=2)
ols.mod<-lm(y~x1+x2+x3, data=het)

boot.olscoef <- function(data, indices) {
  data <- data[indices,]
  ols.mod <- lm(y ~ x1 + x2 + x3, data = data)
  return(coef(ols.mod))
}

ols_model_bootcoeff <- boot(data = het, statistic = boot.olscoef, R = 1000)
ols_model_bootcoeff

boot.ci(ols_model_bootcoeff, type = "basic")

mathpro<-het

plot(mathpro)
summary(lm(y~x1+x2+x3, data=mathpro))
anova(lm(y~x1+x2+x3, data=mathpro))

residualPlots(lm(y~x1+x2+x3, data=mathpro))


library(car)
influencePlot(lm(y~x1+x2+x3, data=mathpro))

plot(lm(y~x1+x2+x3, data=mathpro), pch=18, col="red", which=c(4))

dfbetasPlots(lm(y~x1+x2+x3, data=mathpro))



qf(0.2, 4,17379)

plot(mathpro)
plot(mathpro$x3, mathpro$y)


# robust regression analysis 

library(MASS)
rbMod<-rlm(y~x2+x3+x4, data=mathpro, psi=psi.huber)
olsMod<-lm(y~x2+x3+x4, data=mathpro)
summary(rbMod)
summary(olsMod)
cooks_dist<-cooks.distance(olsMod)
ifpoint<- which(cooks_dist > 0.91)
ifpoint
residRb<-resid(rbMod)
residualPlots(rbMod)

confint.default(rbMod)
summary(pres.mod)
wts1<-1/fitted(lm(abs(residuals(het.mod))~x1+x2+x3, het))^2
pres.mod2<-lm(y~x1+x2+x3, weight=wts1, data=het)
summary(pres.mod2)
confint(pres.mod2)
plot(y~x1+x2+x3 data=het, main="Model Comparison")

library(MASS)

boot.robcoef <- function(data, indices) {
  data <- data[indices,]
  rob.mod <- rlm(y ~ x1 + x2 + x3, data = data)
  return(coef(rob.mod))
}

rob_model_bootcoeff <- boot(data = het, statistic = boot.robcoef, R = 1000)
rob_model_bootcoeff


boot.ci(rob_model_bootcoeff, type = "basic")



wts1
