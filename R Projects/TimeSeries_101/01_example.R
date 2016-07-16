sales_data <- c(18,
                33,
                41,
                7,
                34,
                35,
                24,
                25,
                24,
                21,
                25,
                20,
                22,
                31,
                40,
                29,
                25,
                21,
                22,
                54,
                31,
                25,
                26,
                35)
tsales <- ts(sales_data, start = c(2016, 1), frequency = 12)
plot(tsales, type = 'o', pch = 20)
tsales
################################
# install.packages('forecast')
library(forecast)
opar <- par(no.readonly = T)
par(mfrow = c(2, 2))
ylim <- c(min(Nile), max(Nile))
plot(Nile, main = 'Raw time series')
plot(ma(Nile, order = 3), main = 'k=3', ylim = ylim)
plot(ma(Nile, order = 5), main = 'k=5', ylim = ylim)
plot(ma(Nile, order = 15), main = 'k=15', ylim = ylim)
######################################################################################
plot(AirPassengers)
lAirPassengers <- log(AirPassengers)
plot(lAirPassengers,ylab = 'log(AirPassengers)')
library(forecast)
fit <- stl(lAirPassengers,s.window = 'periodic')
fit$time.series
plot(fit$time.series)
# plot(fit)
exp(fit$time.series)
monthplot(AirPassengers)
seasonplot(AirPassengers,year.labels = T)
######################################################################################
######################################################################################
##
library(forecast)
fit <- ets(nhtemp,model = 'ANN')
fit
forecast(fit,3)
