#hw6 512

senic <- senic_homework6

y <- senic$length

x1 <- senic$infection

x2 <- senic$region


factor(x2, 
       levels = c(1, 2, 3, 4),
       labels = c(0, 0, 1, 1))
#cool

model1 <- lm(y ~ x1)

model2 <- lm(y ~ x1 + x2 + x1*x2)

summary(model1)
summary(model2)

qf(0.95, 1, 113)
red <- lm(y ~ x1 + x2)
full <- lm(y ~ x1 + x2 + x1*x2)
anova(red, full)


#q2 

x1 <- senic_homework6$infection - mean(x1)
senic$region <- senic_homework6$region
modelFull<-lm(y~x1+as.factor(senic$region) + x1*as.factor(senic$region), data=senic)
modelRed<-lm(y~x1)
anova(modelRed, modelFull)

qf(0.95, 4, 109)


#set up X1, X2, X3

x1 <- BikeProject$tempeture
x2 <- BikeProject$humidity
x3 <- BikeProject$windspeed

y <- BikeProject$count

bikedata<-data.frame(count, temp, humidity, windspeed)




require("lattice")
xyplot(count~temp + humidity + windspeed, data=bikedata)

bike <- lm(count~temp + humidity + windspeed)
anova(bike)

library(car)
residualPlots(bike)


influencePlot(model)
library(ALSM)
BikeProject[,2:4]
sub<-BestSub(BikeProject[,2:4],BikeProject$count,method=c('r2','r2adj','sse','cp','press','aic','sbc'),num=3)
sum<-summary(sub)
plot(sub)
sum
sub

dfbetasPlots(lm(y~x1+x2+x3))
summary(cooks.distance(lm(y~x1+x2+x3)))

model<-lm(y~x1+x2+x3)
oCooks<-sort(cooks.distance(model))
oCooks
summary(oCooks)
library(fmsb)
VIF(model)
dfbetasPlots(lm(y~x1+x2+x3))
