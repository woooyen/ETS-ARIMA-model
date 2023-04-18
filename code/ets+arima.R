library(smooth)

a31 <- read.csv("서울시 월별 평균 대기오염도 정보 중구.csv",header=F)

a31 <- ts(a31,end=c(2022,6),freq=12)
forecast::autoplot(a31,ylab="")

train <- window(a31,end=c(2020,6))
test <- window(a31,start=c(2020,7),end=c(2022,6))

library(ggplot2)
forecast::ggseasonplot(a31,main="")
forecast::BoxCox.lambda(train) #-0.36
train2 <- train^(-0.36)




ets <- adam(model="ZZZ",train2)

arima <- adam(train2,model="NNN",orders=list(ar=c(15,10),i=c(2,1),ma=c(15,10),select=T))

tbats <- forecast::tbats(train)


plot(ets,c(10,11))
adam <- adam(train2,model="ANM",orders=list(ar=c(0,0),i=c(0,0),ma=c(16,0)),initial="back")
adam2 <- adam(train2,model="ANM",orders=list(ar=c(20,0),i=c(0,0),ma=c(0,0)),initial="back")
plot(adam2,c(10,11))


set.seed(123)
forecast::accuracy((forecast(arima,h=24)$mean)^(1/(-0.36)),test)
forecast::accuracy((forecast(ets,h=24)$mean)^(1/(-0.36)),test)
forecast::accuracy((forecast::forecast(tbats,h=24)$mean),test)
forecast::accuracy((forecast(adam,h=24)$mean)^(1/(-0.36)),test)
forecast::accuracy((forecast(adam2,h=24)$mean)^(1/(-0.36)),test)


plot(window(a31,start=c(2018,1)),ylab="",)
lines(test,col=2)
lines((forecast(adam,h=24)$mean)^(1/(-0.36)),col=3)
lines((forecast(adam2,h=24)$mean)^(1/(-0.36)),col=4)
lines((forecast(ets,h=24)$mean)^(1/(-0.36)),col=5)
lines((forecast(arima,h=24)$mean)^(1/(-0.36)),col=6)
lines((forecast::forecast(tbats,h=24)$mean),col=7)
legend("topleft",c("ETS(A,N,M)+ARIMA(0,0,16)","ETS(A,N,M)+ARIMA(20,0,0)","ETS","ARIMA","TBATS"),cex=0.8,col=3:7,lty=1,lwd=1.5)



###################################################################################
#sunspot.year 
forecast::autoplot(sun,ylab="")
plot(sunspot.year)
sun <- sunspot.year
train <- window(sunspot.year,end=c(1968))
test <- window(sunspot.year,start=c(1969))
forecast::BoxCox.lambda(train)

train2 <- train^(0.27)

ets <- adam(train2,model="ZZZ")

arima <- adam(train2,model="NNN",order=list(ar=c(20,10),i=c(2,1),ma=c(20,10),select=T))

tbats <- forecast::tbats(train)

plot(ets,c(10,11))
adam5 <- adam(train2,model="ANN",order=list(i=c(1,0)))
adam2 <- adam(train2,model="ANN",order=list(ar=c(8,0),i=c(1,0)))
plot(adam5,c(10,11))

adam3 <- adam(train2,model="ANN",order=list(ar=c(13,0),i=c(1,0)))
plot(adam3,c(10,11))

adam4 <- adam(train2,model="ZZZ",order=list(ar=c(20,10),i=c(2,1),ma=c(20,10),select=T)) #ar6
smooth::auto.ssarima(ets$residuals)

set.seed(123)
plot(window(sun,start=c(1940)),ylab="")
lines(test,col=2)
lines((forecast(adam2,h=20)$mean)^(1/0.27),col=3)
lines((forecast(adam3,h=20)$mean)^(1/0.27),col=4)
lines((forecast(adam4,h=20)$mean)^(1/0.27),col=5)
lines((forecast(ets,h=20)$mean)^(1/0.27),col=6)
lines((forecast(arima,h=20)$mean)^(1/0.27),col=7)
lines((forecast::forecast(tbats,h=20)$mean),col=8)
legend("topleft",c("ETS(A,N,N)+ARIMA(8,1,0)","ETS(A,N,N)+ARIMA(13,1,0)","ETS(A,N,N)+ARIMA(6,0,0)","ETS","ARIMA","TBATS"),cex=0.8,col=3:8,lty=1,lwd=1.5)


set.seed(123)
forecast::accuracy((forecast(arima,h=20)$mean)^(1/0.27),test)
forecast::accuracy((forecast(ets,h=20)$mean)^(1/0.27),test)
forecast::accuracy((forecast(adam2,h=20)$mean)^(1/0.27),test)
forecast::accuracy((forecast(adam3,h=20)$mean)^(1/0.27),test)
forecast::accuracy((forecast(adam4,h=20)$mean)^(1/0.27),test)
forecast::accuracy((forecast::forecast(tbats,h=20)$mean),test)
forecast::accuracy((forecast(adam5,h=20)$mean)^(1/0.27),test)
###################################################################################

plot(lynx)
forecast::autoplot(lynx,ylab="")
lynx <- lynx
train <- window(lynx,end=(1924))
test <- window(lynx,start=c(1925))
forecast::BoxCox.lambda(train)
train2 <- train^(0.14)



ets <- adam(train2,model="ZZZ")

arima <- adam(train2,model="NNN",order=list(ar=c(5,3),i=c(2,1),ma=c(5,3),select=T))

tbats <- forecast::tbats(train)


plot(ets,c(10,11))

adam <- adam(train2,model="MNN",order=list(ar=c(5,0)))
plot(adam,c(10,11))

adam2 <- adam(train2,order=list(ar=c(5,3),i=c(2,1),ma=c(5,3),select=T))



set.seed(123)
plot(window(lynx,start=c(1880)),ylab="",xlab="")
lines(test,col=2)
lines(forecast(adam,h=10)$mean^(1/0.14),col=3)
lines(forecast(adam2,h=10)$mean^(1/0.14),col=4)
lines(forecast(ets,h=10)$mean^(1/0.14),col=5)
lines(forecast(arima,h=10)$mean^(1/0.14),col=6)
lines(forecast::forecast(tbats,h=10)$mean,col=7)
legend("topleft",c("ETS(M,N,N)+ARIMA(5,0,0)","ETS(M,N,N)+ARIMA(3,0,0)","ETS","ARIMA","TBATS"),cex=0.8,col=3:7,lty=1,lwd=1.5)

set.seed(123)
forecast::accuracy(forecast(ets,h=10)$mean^(1/0.14),test)
forecast::accuracy(forecast(arima,h=10)$mean^(1/0.14),test)
forecast::accuracy(forecast::forecast(tbats,h=10)$mean,test)
forecast::accuracy(forecast(adam2,h=10)$mean^(1/0.14),test)
forecast::accuracy(forecast(adam,h=10)$mean^(1/0.14),test)

########################################################################################
