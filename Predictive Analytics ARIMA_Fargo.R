library(readxl)
setwd("PATH")
options(scipen = 999)

dt<-read_xlsx("Fargo Health - Data Supplement.xlsx",sheet = 2)
str(dt)

dt.nw<-dt
dt.nw$`Incoming Examinations`<-as.numeric(dt.nw$`Incoming Examinations`)
dt.nw$`Incoming Examinations`[dt.nw$`Incoming Examinations`>9999]<-NA

ts.fargo <- ts(dt.nw$`Incoming Examinations`, frequency=12, start=c(2006,1))
plot.ts(ts.fargo)


# Removing Na -------------------------------------------------------------

dtna<-na.omit(dt.nw)
tsna.fargo <- ts(dtna$`Incoming Examinations`, frequency=12, start=c(2006,1))
plot.ts(tsna.fargo)


# TS & Decomposing --------------------------------------------------------

library(TTR)
fargo.dec.ts<-decompose(tsna.fargo)
fargo.dec.ts$seasonal
plot(fargo.dec.ts)

fargo.dec.ts.adjusted<-tsna.fargo -fargo.dec.ts$seasonal
plot(fargo.dec.ts.adjusted)

library(forecast)

# ARIMA -------------------------------------------------------------------
tsna.fargo.diff<-diff(tsna.fargo,differences = 1) #
plot.ts(tsna.fargo.diff)
plot.ts(tsna.fargo)

auto.arima(tsna.fargo,ic="bic",trace=TRUE)

tsna.fargo.arima <- arima(tsna.fargo, order=c(0,1,1),method = "ML") # 
tsna.fargo.arima

tsna.fargo.arima.forecast <- forecast:::forecast.Arima(tsna.fargo.arima, h=12)
tsna.fargo.arima.forecast
plot(tsna.fargo.arima.forecast)

plot.ts(tsna.fargo.arima.forecast$residuals)
hist(tsna.fargo.arima.forecast$residuals, col="red", freq=FALSE, breaks=20)
lines(density(tsna.fargo.arima.forecast$residuals,na.rm = T),col='blue')
