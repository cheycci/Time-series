library(readxl)
stock <- read_excel("/Users/sunshaoyang/Desktop/NASDAQAMZN.xlsx")

library(psych)
describe(stock)


nasd=ts(stock$NASDAQ,start=1998.1,freq=12)
amzn=ts(stock$AMZN,start=1998.1,freq=12)
plot(nasd, main="NASDAQ")
plot(amzn, main="Amazon")


par(mfrow=c(3,1))
acf(nasd, type = "covariance", main="Autocovariance", lag.max=30, ylab="NASDAQ COV")
acf(nasd, type = "correlation", main="Autocorrelation", lag.max=30, ylab= "NASDAQ ACF")
acf(nasd, type = "partial",main="Partial Autocorrelation",lag.max=30, ylab="NASDAQ PACF")


par(mfrow=c(3,1))
acf(amzn, type = "covariance", main="Autocovariance", lag.max=30, ylab="Amazon COV")
acf(amzn, type = "correlation", main="Autocorrelation", lag.max=30, ylab= "Amazon ACF")
acf(amzn, type = "partial",main="Partial Autocorrelation",lag.max=30, ylab="Amazon PACF")

library(forecast)
library(tseries)
auto.arima(nasd)
auto.arima(amzn)

model_nasd=Arima(nasd, order=c(0,1,0), seasonal=list(order=c(0,0,1)),include.drift = TRUE)
summary(model_nasd)


library(lmtest)
coeftest(model_nasd)


model_amzn=Arima(amzn, order=c(0,2,2), seasonal=list(order=c(0,0,1)))
summary(model_amzn)

coeftest(model_amzn)


plot(model_nasd$fitted, model_nasd$residuals)
plot(model_amzn$fitted, model_amzn$residuals)


par(mfrow=c(3,1))
acf(model_nasd$residuals, type = "covariance", main="Autocovariance", lag.max=30, ylab="model_nasd residuals COV")
acf(model_nasd$residuals, type = "correlation", main="Autocorrelation", lag.max=30, ylab= "model_nasd residuals ACF")
acf(model_nasd$residuals, type = "partial",main="Partial Autocorrelation",lag.max=30, ylab="model_nasd residuals PACF")


par(mfrow=c(3,1))
acf(model_amzn$residuals, type = "covariance", main="Autocovariance", lag.max=30, ylab="model_amzn residuals COV")
acf(model_amzn$residuals, type = "correlation", main="Autocorrelation", lag.max=30, ylab= "model_amzn residuals ACF")
acf(model_amzn$residuals, type = "partial",main="Partial Autocorrelation",lag.max=30, ylab="model_amzn residuals PACF")


plot(model_amzn$residuals)


library(strucchange)
y=recresid(model_nasd$residuals~1)
plot(efp(model_nasd$residuals~1, type = "Rec-CUSUM"))


plot(y, pch=16,ylab="Recursive Residuals")


y=recresid(model_amzn$res~1)
plot(efp(model_amzn$res~1, type = "Rec-CUSUM"))


plot(y, pch=16,ylab="Recursive Residuals")


accuracy(model_nasd)


accuracy(model_amzn)


plot(forecast(model_nasd,h=12))


plot(forecast(model_amzn,h=12))


library(vars) 
y=cbind(nasd,amzn)
y_tot=data.frame(y)
VARselect(y_tot, 10)


var_model=VAR(y_tot,p=8)
summary(var_model)


irf(var_model)
plot(irf(var_model, n.ahead=12))


grangertest(nasd ~ amzn, order = 8)


grangertest(amzn ~ nasd, order = 8)


var.predict = predict(object=var_model, n.ahead=12)
plot(var.predict)
