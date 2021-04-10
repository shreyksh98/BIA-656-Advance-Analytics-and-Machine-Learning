# Question 1

library(TSA)
library(tseries)
library(fGarch)
setwd("C:\\Users\\shrey\\OneDrive\\Documents\\Stevens\\SIT Sem 3\\BIA 656\\BIA 656 R codes")

q1=read.table("d-spy-0111.txt",header=T)
q1
inta=log(q1$rtn+1) #transforming simple returns to log returns

head(inta)

#Q1.a
t.test(inta) #to check if there is significant difference
Box.test(inta, lag=12, type ='Ljung') #using 12 lags for monthly series

#to check for ARCH effect
acf((inta), lag=10)
acf(abs(inta), lag=10)
Box.test(inta,lag=12,type="Ljung")
Box.test(inta^2,lag=12,type="Ljung")

#Q1.b

model2=garchFit(~arma(1,1)+garch(2,1),data=inta,trace=F) #fitting the model
summary(model2)

sresi=model2@residuals/model2@sigma.t  ## For model checking
acf(sresi)
#for residual checking
Box.test(sresi,lag=12,type="Ljung")
Box.test(sresi^2,lag=12,type="Ljung") 

plot(model2) #use selection 13 for QQ plots

#Q1.c
model3=garchFit(~arma(1,1) + garch(2,1),data=inta,trace=F,cond.dist="std")
summary(model3)
sresi2=model3@residuals/model3@sigma.t  ## For model checking
acf(sresi2)
Box.test(sresi2,lag=12,type="Ljung")
Box.test(sresi2^2,lag=12,type="Ljung")

#Q1.d
#fiting the arma-aparch model
require(rugarch)
spec1=ugarchspec(variance.model=list(model="apARCH",garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,1)))
summary(spec1)
model4=ugarchfit(spec=spec1,data=inta)
model4
p1 <- ugarchforecast(model4,n.ahead=5)
p1
sigma(p1) #volatality 

#Q2
rm(list=ls())
setwd("C:\\Users\\shrey\\OneDrive\\Documents\\Stevens\\SIT Sem 3\\BIA 656\\BIA 656 R codes")
install.packages(fGarch)
library(fGarch)
q2=read.table("m-ko-6111.txt", header = T)
q2
intb=log(q2$ko+1)
intb

#Q2.a
t.test(intb) #to check if there is significant difference
Box.test(intb, lag=12, type ='Ljung') #using 12 lags for monthly series

#to check for ARCH effect
acf((intb), lag=12)
acf(abs(intb), lag=12)
Box.test(intb,lag=12,type="Ljung")
Box.test(intb^2,lag=12,type="Ljung")

#Q2.b
model5=garchFit(~garch(1,1),data=intb,trace=F)
summary(model5)

sresi3=model6@residuals/model6@sigma.t  ## For model checking
acf(sresi3)
Box.test(sresi3,lag=12,type="Ljung")
Box.test(sresi3^2,lag=12,type="Ljung")

#Q2.c
model7=garchFit(~garch(1,1),data=intb,trace=F,cond.dist="std")
summary(model7)
sresi4=model7@residuals/model7@sigma.t  ## For model checking
acf(sresi4)
Box.test(sresi4,lag=12,type="Ljung")
Box.test(sresi4^2,lag=12,type="Ljung")
plot(model7)

require(rugarch)
spec2=ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)))
summary(spec2)
model8=ugarchfit(spec=spec2,data=intb)
p2 <- ugarchforecast(model8,n.ahead=5)
p2
sigma(p2)

#Q3
setwd("C:\\Users\\shrey\\OneDrive\\Documents\\Stevens\\SIT Sem 3\\BIA 656\\BIA 656 R codes")
library(fGarch)
q3=read.table("m-ko-6111.txt", header = T)
q3
intc=log(q3$ko+1)
intc
intc = intc * 100
intc
require(rugarch)

model9=garchFit(~garch(1,1),submodel = "TGARCH",data=intb,trace=F,cond.dist="std")
summary(model9)
sresi5=model9@residuals/model9@sigma.t  ## For model checking
acf(sresi5)
Box.test(sresi5,lag=12,type="Ljung")
Box.test(sresi5^2,lag=12,type="Ljung")

spec3=ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)))
model10=ugarchfit(spec=spec3,data=intg)
model10
Box.test(intc,lag=12,type="Ljung")
Box.test(intc^2,lag=12,type="Ljung")

#Q3.b
variance.model=list(model="fGARCH",garchOrder=c(1,1), submodel="GARCH")
mean.model=list(armaOrder=c(0,0),include.mean=TRUE,archm=T,archpow=2)
spec4=ugarchspec(variance.model=variance.model,mean.model=mean.model, distribution.model="norm")
m2=ugarchfit(spec=spec4,data=intc)
m2

model11=garchFit(~garch(1,1),submodel = "gjrGARCH",data=intb,trace=F,cond.dist="std")
summary(model11)
sresi6=model11@residuals/model11@sigma.t  ## For model checking
acf(sresi6)
Box.test(sresi6,lag=12,type="Ljung")
Box.test(sresi6^2,lag=12,type="Ljung")

model12=ugarchfit(spec=spec4,data=intg)
model12
                    