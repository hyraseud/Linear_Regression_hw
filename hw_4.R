
y<-as.matrix(BikeProject$count)
colnames(y)<-c("count")

mod <- lm(BikeProject$count ~ BikeProject$tempeture)
xh=c(.7)
summary(mod)
sigma(mod) * sqrt(1/17377 + (xh-mean(x))^2/(var(x) *17378))

qt(1-(0.1/6),17377)
confint(mod, level=0.95)
sqrt(3*qf(0.9, 3, 17377))
coef(mod) + sqrt(2 * qf(0.9, 2, 17377) * diag(vcov(mod)))

coef(mod) - sqrt(2 * qf(0.9, 2, 17377) * diag(vcov(mod)))

library(ALSM)

ci.reg(mod, new, type='n', alpha=0.1)

new<-data.frame(temp=0.6)
summary(predict(mod,new, se.fit=TRUE)) ##return the standard error of prediction 
predict(mod, new, interval="confidence")
ci.reg(mod, new, type='m',alpha=0.1) 
        
#set up X1, X2, X3

temp <- BikeProject$tempeture
humidity <- BikeProject$humidity
windspeed <- BikeProject$windspeed

Intercept<-rep(1, 17379) 

x<-cbind(Intercept,temp, humidity, windspeed)


bikedata<-data.frame(BikeProject$count, temp, humidity, windspeed)

xty<-t(x)%*%y
xty
xtx<-t(x)%*%x
xtxinv<-solve(xtx)
xtxinv

class(xty)
class(xtxinv)

betahat<-xtxinv %*% xty
betahat

#hat matrix 
x
xtxinv
xty

hat<-x%*%xtxinv%*%t(x)
ide<-diag(17379)
I_H<-as.matrix(ide-hat)



#Compute SST, SSE and SSR
y
J<-matrix(1,nrow=17379,ncol=17379)
SST<-t(y)%*%y-(1/17379)*t(y)%*%J%*%y
cat("SST=",SST[1,1],"\n")

resid<-y-x%*%betahat
cat("resid=",resid,"\n")

dim(resid)
SSE<-t(resid)%*%resid
cat("SSE=",SSE,"\n")

SSR<-SST-SSE
cat("SSR=",SSR,"\n")

MSE<-SSE/(17379-4)
cat("MSE=",MSE,"\n")
cat("residual s=",sqrt(MSE),"\n")

#diag(MSE[1,1]*I_H)

tempMod<-lm(BikeProject$count~temp + humidity + windspeed, bikedata)
summary(tempMod)
anova(tempMod)
confint(tempMod,level=0.95)
93677759 + 49874585 + 164674

#vcov matrix 
dim(xtxinv)
dim(MSE)
sigmaBeta<-MSE[1,1]*solve(t(x)%*%x)
sigmaBeta

sqrt(diag(sigmaBeta))
