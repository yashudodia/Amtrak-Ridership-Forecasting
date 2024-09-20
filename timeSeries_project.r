# Time Series Analysis
# Group Project

# using libraries
library(forecast)
library(zoo)

# setting up working directory
setwd("~/Desktop/BAN673_TSA/Project/")

# creating data frame
amtrek_data.ts <- read.csv("amtrak_ridership_3.csv")

# display summary of the data
summary(amtrek_data.ts)

# display first 6 data of the frame.
head(amtrek_data.ts)

# display last 6 data of the frame.
tail(amtrek_data.ts)



Amtrek.ts <- ts(amtrek_data.ts$Number_of_Passengers, 
                    start = c(1991, 1), end = c(2013, 12), freq = 12)

Amtrek.ts

max(amtrek_data.ts$Number_of_Passengers)

plot(Amtrek.ts, 
     xlab = "Time", ylab = "Passengers", 
     ylim = c(1360852, 2890763), main = "Amtrek Passengers", col = "blue")
axis(1, at = seq(1991, 2013))


nValid <- 36
nTrain <- length(Amtrek.ts) - nValid
train.ts <- window(Amtrek.ts, start = c(1991, 1), end = c(2013, nTrain))
valid.ts <- window(Amtrek.ts, start = c(1991, nTrain + 1), end = c(1991, nTrain + nValid))
train.ts
valid.ts

autocor <- Acf(Amtrek.ts, lag.max = 12, main = "Autocorrelation for Passengers")
Lag <- round(autocor$lag, 0)
ACF <- round(autocor$acf, 3)
data.frame(Lag, ACF)



autoplot(Amtrek.ts, ylab = "Passengers", 
         main = "Amtrek Passenger Data", col = "blue", lwd = 1)

c.stl <- stl(Amtrek.ts, s.window = "periodic")
autoplot(c.stl, main = "Amtrek Passengers Time Series Components")

## Use plot() to plot time series data
plot(Amtrek.ts, 
     xlab = "Time", ylab = "Passengers (in 000s)", 
     ylim = c(1000, 7000), xlim = c(1991, 2022), 
     main = "Amtrek Passengers", lwd = 2, col = "blue") 
axis(1, at = seq(1991, 2022, 1), labels = format(seq(1991, 2022, 1)))


# Use stl() function to plot times series components of the original data. 
# The plot includes original data, trend, seasonal, and reminder 
# (level and noise component).
passenger.stl <- stl(Amtrek.ts, s.window = "periodic")
autoplot(passenger.stl, main = "Amtrek Passengers Time Series Components")

# Use Acf() function to identify autocorrelation and plot autocorrelation
# for different lags (up to maximum of 12).
Acf(Amtrek.ts, lag.max = 12, main = "Autocorrelation for SFO Air Passengers Data")

length(Amtrek.ts)

# Developing data partition with the validation partition of 20 periods and 
# the rest for the training partition
validation_period <- 36
training_period <- length(Amtrek.ts) - validation_period
training.ts <- window(Amtrek.ts, start = c(1991, 01), end = c(1991, training_period))
validation.ts <- window(Amtrek.ts, start = c(1991, training_period + 1), 
                        end = c(1991, training_period + validation_period))

validation.ts

# Use Acf() function to identify autocorrelation for training and validation
# data sets, and plot autocorrelation for different lags (up to maximum of 12)
Acf(training.ts, lag.max = 12, main = "Autocorrelation for Training Data Set")
Acf(validation.ts, lag.max = 12, main = "Autocorrelation for Validation Data Set")


# Plotting the time series data and visualize partitions. 
plot(training.ts, 
     xlab = "Time", ylab = "Passengers", ylim = c(1000, 7000), bty = "l",
     xaxt = "n", xlim = c(2005, 2022), main = "SFO Air Passengers", lwd = 2) 
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)))
lines(validation.ts, col = "black", lty = 1, lwd = 2)
# Plotting on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2017, 2017), c(0, 7000))
lines(c(2019.93, 2019.93), c(0, 7000))
text(2011, 7000, "Training")
text(2018.5, 7000, "Validation")
text(2021, 7000, "Future")
arrows(2005, 6500, 2016.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.2, 6500, 2019.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 6500, 2022, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)

## ----- LINEAR TREND --------
# developing a regression model with linear trend (training data)
training.lin.trend <- tslm(training.ts ~ trend)
summary(training.lin.trend)
# forecasting in the validation period
training.lin.trend.pred <- forecast(training.lin.trend, 
                                    h = validation_period, level = 0)
training.lin.trend.pred

# Plot predictions for linear trend forecast.
plot(training.lin.trend.pred$mean, 
     xlab = "Time", ylab = "Passengers (in 000s)", ylim = c(1000, 7000), bty = "l",
     xlim = c(2005, 2022), main = "Linear Trend Forecast", 
     col = "blue", lwd =2) 
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)) )
lines(training.lin.trend$fitted, col = "blue", lwd = 2)
lines(training.ts, col = "black", lty = 1)
lines(validation.ts, col = "black", lty = 1)
# Plotting on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2017, 2017), c(0, 7000))
lines(c(2019.93, 2019.93), c(0, 7000))
text(2011, 7000, "Training")
text(2018.5, 7000, "Validation")
text(2021, 7000, "Future")
arrows(2005, 6500, 2016.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.2, 6500, 2019.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 6500, 2022, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)


## ----- LINEAR TREND AND SEASONALITY--------
# developing a regression model with linear trend and seasonality (training data)
training.lin.trend.seas <- tslm(training.ts ~ trend + season)
summary(training.lin.trend.seas)
# forecasting in the validation period
training.lin.trend.seas.pred <- forecast(training.lin.trend.seas, 
                                         h = validation_period, level = 0)
training.lin.trend.seas.pred

# Plot predictions for linear trend and seasonality forecast.
plot(training.lin.trend.seas.pred$mean, 
     xlab = "Time", ylab = "Passengers (in 000s)", ylim = c(1000, 7000), bty = "l",
     xlim = c(2005, 2022), main = "Linear Trend and Seasonality Forecast", 
     col = "blue", lwd =2) 
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)) )
lines(training.lin.trend.seas$fitted, col = "blue", lwd = 2)
lines(training.ts, col = "black", lty = 1)
lines(validation.ts, col = "black", lty = 1)
# Plotting on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2017, 2017), c(0, 7000))
lines(c(2019.93, 2019.93), c(0, 7000))
text(2011, 7000, "Training")
text(2018.5, 7000, "Validation")
text(2021, 7000, "Future")
arrows(2005, 6500, 2016.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.2, 6500, 2019.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 6500, 2022, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)

## ----- QUADRATIC TREND--------
# developing a regression model with quadratic trend (training data)
training.quad.trend <- tslm(training.ts ~ trend + I(trend^2))
summary(training.quad.trend)
# forecasting in the validation period
training.quad.trend.pred <- forecast(training.quad.trend, 
                                     h = validation_period, level = 0)
training.quad.trend.pred

# Plot predictions for quadratic trend forecast.
plot(training.quad.trend.pred$mean, 
     xlab = "Time", ylab = "Passengers (in 0000s)", ylim = c(1000, 7000), bty = "l",
     xlim = c(2005, 2022), main = "Quadratic Trend Forecast", 
     col = "blue", lwd =2) 
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)) )
lines(training.quad.trend$fitted, col = "blue", lwd = 2)
lines(training.ts, col = "black", lty = 1)
lines(validation.ts, col = "black", lty = 1)
# Plotting on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2017, 2017), c(0, 7000))
lines(c(2019.93, 2019.93), c(0, 7000))
text(2011, 7000, "Training")
text(2018.5, 7000, "Validation")
text(2021, 7000, "Future")
arrows(2005, 6500, 2016.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.2, 6500, 2019.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 6500, 2022, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)


## ----- QUADRATIC TREND AND SEASONALITY--------
# developing a regression model with quadratic trend (training data)
training.quad.trend.seas <- tslm(training.ts ~ trend + I(trend^2) + season)
summary(training.quad.trend.seas)
# forecasting in the validation period
training.quad.trend.seas.pred <- forecast(training.quad.trend.seas, 
                                          h = validation_period, level = 0)
training.quad.trend.seas.pred

# Plot predictions for quadratic trend and seasonality forecast.
plot(training.quad.trend.seas.pred$mean, 
     xlab = "Time", ylab = "passengers (in 0000s)", ylim = c(1000, 7000), bty = "l",
     xlim = c(2005, 2022), main = "Quadratic Trend Forecast", 
     col = "blue", lwd =2) 
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)) )
lines(training.quad.trend.seas$fitted, col = "blue", lwd = 2)
lines(training.ts, col = "black", lty = 1)
lines(validation.ts, col = "black", lty = 1)
# Plotting on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2017, 2017), c(0, 7000))
lines(c(2019.93, 2019.93), c(0, 7000))
text(2011, 7000, "Training")
text(2018.5, 7000, "Validation")
text(2021, 7000, "Future")
arrows(2005, 6500, 2016.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.2, 6500, 2019.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 6500, 2022, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)



# Accuracy of regression model with linear trend (training data)
round(accuracy(training.lin.trend.pred$mean, validation.ts), 3)
# Accuracy of regression model with linear trend and seasonality (training data)
round(accuracy(training.lin.trend.seas.pred$mean, validation.ts), 3)
# Accuracy of regression model with quadratic trend (training data)
round(accuracy(training.quad.trend.pred$mean, validation.ts), 3)
# Accuracy of regression model with quadratic trend and seasonality (training data)
round(accuracy(training.quad.trend.seas.pred$mean, validation.ts), 3)
# Accuracy of Naive forecast model (training data)
round(accuracy(training.naive.pred$mean, validation.ts), 3)
# Accuracy of Seasonal Naive forecast model (training data)
round(accuracy(training.snaive.pred$mean, validation.ts), 3)


## -------- ENHANCING REGRESSION MODEL (Developing two-level model) -------
## ------- LINEAR TREND AND SEASONALITY & TRAILING MA OF RESIDUALS ------

# Plot residuals of the predictions with trend and seasonality.
plot(training.lin.trend.pred$residuals, 
     xlab = "Time", ylab = "Residuals", ylim = c(-1000, 1500), bty = "l",
     xaxt = "n", xlim = c(2005, 2022), 
     main = "Regresssion Residuals for Training and Validation Data", 
     col = "brown", lwd = 2) 
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)))
lines(validation.ts - training.lin.trend.pred$mean, col = "brown", lwd = 2, lty = 1)
# Plotting on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2017, 2017), c(-1500, 1500))
lines(c(2019.93, 2019.93), c(-1500, 1500))
text(2011, 1500, "Training")
text(2018.5, 1500, "Validation")
text(2021, 1500, "Future")
arrows(2005, 1300, 2016.8, 1300, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.2, 1300, 2019.8, 1300, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 1300, 2022, 1300, code = 3, length = 0.1,
       lwd = 1, angle = 30)

## ------ TWO-LEVEL MODEL (regression model with linear trend and seasonality & trailing MA of residuals) ------
# Identify and display residuals based on the regression model with linear trend and seasonality (training model)
training.lin.trend.seas.res <- training.lin.trend.seas.pred$residuals
training.lin.trend.seas.res
# Apply trailing MA for residuals with window width k = 2. 
ma.trail.lin.trend.seas.res <- rollmean(training.lin.trend.seas.res, k = 2, align = "right")
ma.trail.lin.trend.seas.res
# Create residuals forecast for validation period.
ma.trail.lin.trend.seas.res.pred <- forecast(ma.trail.lin.trend.seas.res, h = validation_period, level = 0)
ma.trail.lin.trend.seas.res.pred
# Regression residuals in validation period.
training.lin.trend.seas.res.valid <- validation.ts - training.lin.trend.seas.pred$mean
training.lin.trend.seas.res.valid
# To develop real forecast for validation period, 
# combine regression forecast and trailing MA forecast for residuals.
valid.forecast.2level.linTS.ma <- training.lin.trend.seas.pred$mean + ma.trail.lin.trend.seas.res.pred$mean
valid.forecast.2level.linTS.ma

# Plot the predictions for trailing MA.
plot(training.ts, 
     xlab = "Time", ylab = "Passengers (in 0000s)", ylim = c(1000, 7000), bty = "l",
     xlim = c(2005, 2022), main = "Linear Trend and Seasonality & Trailing MA") 
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)) )
lines(train.ma.trailing, col = "brown", lwd = 2)
lines(valid.forecast.2level.linTS.ma, col = "brown", lwd = 2, lty = 2)
lines(validation.ts)
legend(2005,6500, legend = c("Passengers", "Training MA, k=2",
                             "Validation MA, k= 2"), 
       col = c("black", "brown", "brown"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")
# Plotting on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2017, 2017), c(0, 7000))
lines(c(2019.93, 2019.93), c(0, 7000))
text(2011, 7000, "Training")
text(2018.5, 7000, "Validation")
text(2021, 7000, "Future")
arrows(2005, 6500, 2016.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.2, 6500, 2019.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 6500, 2022, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)



## ------ TWO-LEVEL MODEL (regression model with quadratic trend and seasonality & trailing MA of residuals) ------
# Identify and display residuals based on the regression model with linear trend and seasonality (training model)
training.quad.trend.seas.res <- training.quad.trend.seas.pred$residuals
training.quad.trend.seas.res
# Apply trailing MA for residuals with window width k = 2. 
ma.trail.quad.trend.seas.res <- rollmean(training.quad.trend.seas.res, k = 2, align = "right")
ma.trail.quad.trend.seas.res
# Create residuals forecast for validation period.
ma.trail.quad.trend.seas.res.pred <- forecast(ma.trail.quad.trend.seas.res, h = validation_period, level = 0)
ma.trail.quad.trend.seas.res.pred
# Regression residuals in validation period.
training.quad.trend.seas.res.valid <- validation.ts - training.quad.trend.seas.pred$mean
training.quad.trend.seas.res.valid
# To develop real forecast for validation period, 
# combine regression forecast and trailing MA forecast for residuals.
valid.forecast.2level.quadTS.ma <- training.quad.trend.seas.pred$mean + ma.trail.quad.trend.seas.res.pred$mean
valid.forecast.2level.quadTS.ma

# Plot the predictions for trailing MA.
plot(training.ts, 
     xlab = "Time", ylab = "Passengers (in 0000s)", ylim = c(1000, 7000), bty = "l",
     xlim = c(2005, 2022), main = "Quadratic Trend and Seasonality & Trailing MA") 
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)) )
lines(train.ma.trailing, col = "brown", lwd = 2)
lines(valid.forecast.2level.quadTS.ma, col = "brown", lwd = 2, lty = 2)
lines(validation.ts)
legend(2005,6500, legend = c("Passengers", "Training MA, k=2",
                             "Validation MA, k= 2"), 
       col = c("black", "brown", "brown"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")
# Plotting on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2017, 2017), c(0, 7000))
lines(c(2019.93, 2019.93), c(0, 7000))
text(2011, 7000, "Training")
text(2018.5, 7000, "Validation")
text(2021, 7000, "Future")
arrows(2005, 6500, 2016.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.2, 6500, 2019.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 6500, 2022, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)




# Accuracy of regression model with linear trend and seasonality (training data)
round(accuracy(training.lin.trend.seas.pred$mean, validation.ts), 3)
# Accuracy of two-level Model
# regression model with linear trend & seasonality and trailing MA for residuals (training data)
round(accuracy(valid.forecast.2level.linTS.ma, validation.ts), 3)
# Accuracy of regression model with quadratic trend and seasonality (training data)
round(accuracy(training.quad.trend.seas.pred$mean, validation.ts), 3)
# Accuracy of two-level Model
# regression model with quadratic trend & seasonality and trailing MA for residuals (training data)
round(accuracy(valid.forecast.2level.quadTS.ma, validation.ts), 3)
# Accuracy of Naive forecast model (training data)



# Use Acf() function to identify autocorrelation for the model residuals 
# (training and validation sets), and plot autocorrelation for different 
# lags (up to maximum of 12).
Acf(training.lin.trend.seas.pred$residuals, lag.max = 12, 
    main = "Autocorrelation for Training Residuals")
Acf(validation.ts - training.lin.trend.seas.pred$mean, lag.max = 12, 
    main = "Autocorrelation for Validation Residuals")



## -------- ENHANCING REGRESSION MODEL (Developing two-level model) -------
## ------- LINEAR TREND AND SEASONALITY & AUTOREGRESSIVE MODEL OF RESIDUALS ------

# Using Arima() function to fit AR(1) model for training residuals of regression model with linear trend
# The Arima model of order = c(1,0,0) gives an AR(1) model
lin.trend.seas.res.ar1 <- Arima(training.lin.trend.seas$residuals, order = c(1,0,0))
summary(lin.trend.seas.res.ar1)
z.stat <- (0.7889 - 1)/0.0546
p.val <- pnorm(z.stat)
p.val

# The Arima model of order = c(2,0,0) gives an AR(2) model
lin.trend.seas.res.ar2 <- Arima(training.lin.trend.seas$residuals, order = c(2,0,0))
summary(lin.trend.seas.res.ar2)
z.stat <- (0.6619 - 1)/0.0853
p.val <- pnorm(z.stat)
p.val

# Develop a data frame to demonstrate the training AR model results 
# vs. original training series, training regression model, 
# and its residuals.  
train.df <- data.frame(training.ts, training.lin.trend$fitted, 
                       training.lin.trend.seas$residuals, lin.trend.seas.res.ar1$fitted, 
                       lin.trend.seas.res.ar1$residuals, lin.trend.seas.res.ar2$fitted, 
                       lin.trend.seas.res.ar2$residuals)
names(train.df) <- c("train.data", "Regression.linearTS", "Regression.Residuals",
                     "AR(1).Model", "AR(1).Model.Residuals",
                     "AR(2).Model", "AR(2).Model.Residuals")
train.df


# Use forecast() function to make prediction of residuals in validation set.
lin.trend.seas.res.ar1.pred <- forecast(lin.trend.seas.res.ar1, h = validation_period, level = 0)
lin.trend.seas.res.ar1.pred

lin.trend.seas.res.ar2.pred <- forecast(lin.trend.seas.res.ar2, h = validation_period, level = 0)
lin.trend.seas.res.ar2.pred

# Create data table with validation data, regression forecast
# for validation period, AR(1) residuals for validation, and 
# two level model results. 
valid.2level.linTS.ar1.pred <- training.lin.trend.seas.pred$mean + lin.trend.seas.res.ar1.pred$mean
valid.2level.linTS.ar2.pred <- training.lin.trend.seas.pred$mean + lin.trend.seas.res.ar2.pred$mean

valid.df <- data.frame(validation.ts, training.lin.trend.seas.pred$mean,
                       valid.2level.linTS.ar2.pred)
names(valid.df) <- c("Passenger.Valid", "Reg.LinTS.Forecast", "Combined.Forecast.AR(2)")
valid.df




## ------- QUADRATIC TREND AND SEASONALITY & AUTOREGRESSIVE MODEL OF RESIDUALS ------

# The Arima model of order = c(1,0,0) gives an AR(1) model
quad.trend.seas.res.ar1 <- Arima(training.quad.trend.seas$residuals, order = c(1,0,0))
summary(quad.trend.seas.res.ar1)
z.stat <- (0.6700 - 1)/0.0639
p.val <- pnorm(z.stat)
p.val

# The Arima model of order = c(2,0,0) gives an AR(2) model
quad.trend.seas.res.ar2 <- Arima(training.quad.trend.seas$residuals, order = c(2,0,0))
summary(quad.trend.seas.res.ar2)
z.stat <- (0.5901 - 1)/0.0859
p.val <- pnorm(z.stat)
p.val

# Develop a data frame to demonstrate the training AR model results 
# vs. original training series, training regression model, 
# and its residuals.  
quad_train.df <- data.frame(training.ts, training.quad.trend$fitted, 
                            training.quad.trend.seas$residuals, quad.trend.seas.res.ar1$fitted, 
                            quad.trend.seas.res.ar1$residuals)
names(quad_train.df) <- c("train.data", "Regression.quadTS", "Regression.Residuals",
                          "AR(1).Model", "AR(1).Model.Residuals")
quad_train.df


# Use forecast() function to make prediction of residuals in validation set.
quad.trend.seas.res.ar1.pred <- forecast(quad.trend.seas.res.ar1, h = validation_period, level = 0)
quad.trend.seas.res.ar1.pred

# Create data table with validation data, regression forecast
# for validation period, AR(1) residuals for validation, and 
# two level model results. 
quad_valid.two.level.pred <- training.quad.trend.seas.pred$mean + quad.trend.seas.res.ar1.pred$mean

quad_valid.df <- data.frame(validation.ts, training.quad.trend.seas.pred$mean, 
                            quad_valid.two.level.pred)
names(quad_valid.df) <- c("Passenger.Valid", "Reg.QuadTS.Forecast", "Combined.Forecast.(AR1)")
quad_valid.df



##--------- NAIVE FORECAST--------
# Use naive() to make naive forecast (training.naive.pred) 
# for validation data.
training.naive.pred <- naive(training.ts, h = validation_period)

# Plot predictions for naive forecast.
plot(training.naive.pred$mean, 
     xlab = "Time", ylab = "Passengers (in 0000s)", ylim = c(1000, 7000), bty = "l",
     xlim = c(2005, 2022), main = "Naive Forecast", 
     col = "blue", lwd =2) 
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)) )
lines(training.naive.pred$fitted, col = "blue", lwd = 2)
lines(training.ts, col = "black", lty = 1)
lines(validation.ts, col = "black", lty = 1)
# Plotting on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2017, 2017), c(0, 7000))
lines(c(2019.93, 2019.93), c(0, 7000))
text(2011, 7000, "Training")
text(2018.5, 7000, "Validation")
text(2021, 7000, "Future")
arrows(2005, 6500, 2016.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.2, 6500, 2019.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 6500, 2022, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)



##--------- SEASONAL NAIVE FORECAST--------
# Use snaive() to make naive forecast (training.snaive.pred) 
# for validation data.
training.snaive.pred <- snaive(training.ts, h = validation_period)

# Plot predictions for seasonal naive forecast.
plot(training.snaive.pred$mean, 
     xlab = "Time", ylab = "Passengers (in 0000s)", ylim = c(1000, 7000), bty = "l",
     xlim = c(2005, 2022), main = "Seasonal Naive Forecast", 
     col = "blue", lwd =2) 
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)) )
lines(training.snaive.pred$fitted, col = "blue", lwd = 2)
lines(training.ts, col = "black", lty = 1)
lines(validation.ts, col = "black", lty = 1)
# Plotting on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2017, 2017), c(0, 7000))
lines(c(2019.93, 2019.93), c(0, 7000))
text(2011, 7000, "Training")
text(2018.5, 7000, "Validation")
text(2021, 7000, "Future")
arrows(2005, 6500, 2016.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.2, 6500, 2019.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 6500, 2022, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)




# Accuracy of regression model with linear trend and seasonality (training data)
round(accuracy(training.lin.trend.seas.pred$mean, validation.ts), 3)
# Accuracy of two-level regression model with linear trend & seasonality and 
# trailing MA for residuals (training data)
round(accuracy(valid.forecast.2level.linTS.ma, validation.ts), 3)
# Accuracy of regression model with linear trend (training data) and 
# AR(2) model for residuals (training data)
round(accuracy(valid.2level.linTS.ar2.pred, validation.ts), 3)
# Accuracy of regression model with quadratic trend and seasonality (training data)
round(accuracy(training.quad.trend.seas.pred$mean, validation.ts), 3)
# Accuracy of regression model with quadratic trend (training data) and 
# AR(1) model for residuals (training data)
round(accuracy(quad_valid.two.level.pred, validation.ts), 3)
# Accuracy of Seasonal Naive forecast model (training data)
round(accuracy(training.snaive.pred$mean, validation.ts), 3)



Acf(training.lin.trend.seas.pred$residuals, lag.max = 12, 
    main = "Autocorrelation for Training Residuals")
Acf(lin.trend.seas.res.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for Residuals of Training Residuals")
Acf(lin.trend.seas.res.ar2$residuals, lag.max = 12, 
    main = "Autocorrelation for Residuals of Training Residuals")

Acf(training.quad.trend.seas.pred$residuals, lag.max = 12, 
    main = "Autocorrelation for Training Residuals")
Acf(quad.trend.seas.res.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for Residuals of Training Residuals")



#### ENTIRE DATASET ####

## ---- LINEAR TREND AND SEASONALITY -------
# developing a regression model with linear trend ans seasonality (training data)
passenger.lin.trend.seas <- tslm(Amtrek.ts ~ trend + season)
summary(passenger.lin.trend.seas)
# forecasting in the validation period
passenger.lin.trend.seas.pred <- forecast(passenger.lin.trend.seas, 
                                          h = 12, level = 0)
passenger.lin.trend.seas.pred

# Use plot() function to create plot with linear trend and seasonality 
plot(Amtrek.ts, 
     xlab = "Time", ylab = "Passengers", 
     ylim = c(1000, 7000), xlim = c(2005, 2025), 
     main = "Linear Trend and Seasonality Regression Model - SFO Air Passengers")
lines(passenger.lin.trend.seas$fitted, col = "blue", lwd = 2)
lines(passenger.lin.trend.seas.pred$mean, col = "blue", lwd = 2, lty = 2)


## -------- ENHANCING REGRESSION MODEL (Developing two-level model) -------
## ------- LINEAR TREND AND SEASONALITY & TRAILING MA OF RESIDUALS ------
# Identify and display residuals based on the regression model with linear trend and seasonality (training model)
passenger.lin.trend.seas.res <- passenger.lin.trend.seas.pred$residuals
passenger.lin.trend.seas.res
# Apply trailing MA for residuals with window width k = 2. 
ap.ma.trail.lin.trend.seas.res <- rollmean(passenger.lin.trend.seas.res, k = 2, align = "right")
ap.ma.trail.lin.trend.seas.res
# Create residuals forecast for future periods
ap.ma.trail.lin.trend.seas.res.pred <- forecast(ap.ma.trail.lin.trend.seas.res, h = 12, level = 0)
ap.ma.trail.lin.trend.seas.res.pred 
# combine regression forecast and trailing MA forecast for residuals.
ap.forecast.2level.linTS.ma <- passenger.lin.trend.seas.pred$mean + ap.ma.trail.lin.trend.seas.res.pred$mean
ap.forecast.2level.linTS.ma


# Use plot() function to create plot 
plot(Amtrek.ts, 
     xlab = "Time", ylab = "Passengers", 
     ylim = c(1000, 7000), xlim = c(2005, 2025), 
     main = "Linear Trend and Seasonality Regression Model + Trailing MA (width = 2)")
lines(passenger.lin.trend.seas$fitted, col = "blue", lwd = 2)
lines(ap.forecast.2level.linTS.ma, col = "blue", lwd = 2, lty = 2)


## ---- TWO-LEVEL MODEL (Linear T&S and AR(2) of Residuals) -------
# The Arima model of order = c(2,0,0) gives an AR(2) model
passenger.lin.trend.seas.res.ar2 <- Arima(passenger.lin.trend.seas.pred$residuals, order = c(2,0,0))
summary(passenger.lin.trend.seas.res.ar2)
z.stat <- (0.6601 - 1)/0.0770
p.val <- pnorm(z.stat)
p.val
# Use forecast() function to make prediction of residuals
passenger.lin.trend.seas.res.ar2.pred <- forecast(passenger.lin.trend.seas.res.ar2, h = 12, level = 0)
passenger.lin.trend.seas.res.ar2.pred
# two level model results
passenger.two.level.linTS.ar2.pred <- passenger.lin.trend.seas.pred$mean + passenger.lin.trend.seas.res.ar2.pred$mean
passenger.two.level.linTS.ar2.pred


# Use plot() function to create plot 
plot(Amtrek.ts, 
     xlab = "Time", ylab = "Passengers", 
     ylim = c(1000, 7000), xlim = c(2005, 2025), 
     main = "Linear Trend and Seasonality Regression Model + Autoregressive (2)")
lines(passenger.lin.trend.seas$fitted, col = "blue", lwd = 2)
lines(passenger.two.level.linTS.ar2.pred, col = "blue", lwd = 2, lty = 2)



# Accuracy of regression model with linear trend and seasonality(entire data)
round(accuracy(passenger.lin.trend.seas.pred$fitted, Amtrek.ts), 3)
# Accuracy of regression model with linear trend and seasonality (entire data) and 
# trailing MA for residuals (entire data)
round(accuracy(passenger.lin.trend.seas.pred$fitted + 
                   ap.ma.trail.lin.trend.seas.res.pred$fitted, Amtrek.ts), 3)
# Accuracy of regression model with linear trend and seasonality (entire data) and 
# AR(2) model for residuals (entire data)
round(accuracy(passenger.lin.trend.seas.pred$fitted + 
                   passenger.lin.trend.seas.res.ar2.pred$fitted, Amtrek.ts), 3)
# Accuracy of seasonal naive forecast (baseline model)
round(accuracy((snaive(Amtrek.ts))$fitted, Amtrek.ts), 3)



## TEST PREDICTABILITY

# Use Arima() function to fit AR(1) model for passenger
# The ARIMA model of order = c(1,0,0) gives an AR(1) model.
passenger.data.ar1<- Arima(Amtrek.ts, order = c(1,0,0))
summary(passenger.data.ar1)

# The ARIMA model of order = c(2,0,0) gives an AR(2) model.
passenger.data.ar2<- Arima(Amtrek.ts, order = c(2,0,0))
summary(passenger.data.ar2)

# Create first difference of ClosePrice data using diff() function.
diff.passenger.data <- diff(Amtrek.ts, lag = 1)
diff.passenger.data

Acf(diff.passenger.data, lag.max = 12, 
    main = "Autocorrelation for First Differencing - SFO Air Passengers")



## 
# Function ts() takes three arguments: start, end, and freq.
# With monthly data, frequency (freq) of periods in a season (year) is 12. 
# With quarterly data, frequency in a season (year) is equal to 4.
# Arguments start and end are pairs: (season number, period number).
pass.ts <- ts(project.data$Passengers_in000s, 
              start = c(2006,1), end = c(2019, 12), freq = 12)

head(pass.ts)
tail(pass.ts)

## 
# Plot the time series data. 
plot(pass.ts, 
     xlab = "Time", ylab = "SFO Air Passengers(in 1000)", ylim = c(100, 6000), bty = "l",
     xaxt = "n", xlim = c(2006, 2020.25), main = "SFO Air Passengers", lwd = 3, col="blue") 
axis(1, at = seq(2006, 2020, 1), labels = format(seq(2006, 2020, 1)))



# The plot includes original data, trend, seasonal, and reminder (level and noise component).
pass.stl <- stl(pass.ts, s.window = "periodic")
autoplot(pass.stl, main = "SFO Air passengers Time Series Components")

# Use Acf() function to identify autocorrelation and plot autocorrelation
# for different lags (up to maximum of 12).
autocor <- Acf(pass.ts, lag.max = 12, main = "Autocorrelation for SFO Air Passengers")

# Display autocorrelation coefficients for various lags.
Lag <- round(autocor$lag, 0)
ACF <- round(autocor$acf, 3)
data.frame(Lag, ACF)

#ACF for Training and validation periods 
Acf(train.ts, lag.max = 12, main = "Autocorrelation for Air Passenger Training Data Set")
Acf(valid.ts, lag.max = 12, main = "Autocorrelation for Air Passenger Validation Data Set")

pass.ar1<- Arima(pass.ts, order = c(1,0,0))
summary(pass.ar1)


z.statistic <- (0.9193  -1)/0.0312

#P-Value for z.statistic
p.value <-pnorm(z.statistic)
p.value 

# Using the first referencing (lag1) of the historical data and Acf() function

diff.pass <- diff(pass.ts, lag = 1)

#Acf() function with maximum of 8 lags

Acf(diff.pass, lag.max = 8, 
    main = "Autocorrelation SFO Air Passengers for First Differencing (lag1)")


# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.
# nTrain and nValid, respectively.
nValid <- 36
nTrain <- length(pass.ts) - nValid
train.ts <- window(pass.ts, start = c(2006, 1), end = c(2006, nTrain))
valid.ts <- window(pass.ts, start = c(2006, nTrain + 1), 
                   end = c(2006, nTrain + nValid))

head(train.ts)
tail(train.ts)

head(valid.ts)
tail(valid.ts)
valid.ts


##1. Automatic Holt-Winter's
## HOLT-WINTER'S (HW) EXPONENTIAL SMOOTHING WITH PARTITIONED DATA, AUTOMATED
# Use optimal alpha, beta, & gamma to fit HW over the training period.
train.hw.ZZZ <- ets(train.ts, model = "ZZZ")
train.hw.ZZZ 

# Use forecast() function to make predictions using this HW model with validation period (nValid). 
# Show predictions in tabular format.
train.hw.ZZZ.pred <- forecast(train.hw.ZZZ, h = nValid, level = 0)
train.hw.ZZZ.pred

##Naive and Seasonal naive for comparison

training.naive.pred <- naive(train.ts, h = validation_period)
training.snaive.pred <- snaive(train.ts, h = validation_period)

##Accuracy using the the validation period
# Accuracy of HW(ZZZ) (training data)
round(accuracy(train.hw.ZZZ.pred$mean, valid.ts), 3)
# Accuracy of Naive forecast model (training data)
round(accuracy(training.naive.pred$mean, valid.ts), 3)
# Accuracy of Seasonal Naive forecast model (training data)
round(accuracy(training.snaive.pred$mean, valid.ts), 3)

#####

# Plot HW predictions for original data, automated selection of the 
# model and optimal smoothing parameters.
plot(train.hw.ZZZ.pred$mean, 
     xlab = "Time", ylab = "Passengers (in 000s)", ylim = c(1000, 7000), bty = "l",
     xaxt = "n", xlim = c(2006, 2019.25), 
     main = "Holt-Winter's Model with Automated Selection of Model", 
     lty = 5, col = "blue", lwd = 2) 
axis(1, at = seq(2006, 2019, 1), labels = format(seq(2006, 2019, 1)))
lines(train.hw.ZZZ$fitted, col = "blue", lwd = 2)
lines(pass.ts)
legend(2006,5000, 
       legend = c("Passengers", 
                  "Holt-Winter's Automated Model for Training Partition",
                  "Holt-Winter's Automated Model for Validation Partition"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2017, 2017), c(0, 7000))
lines(c(2019.93, 2019.93), c(0, 7000))
text(2011, 7000, "Training")
text(2018.5, 7000, "Validation")
arrows(2006, 6500, 2016.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.2, 6500, 2019.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)

########
## FORECAST WITH HOLT-WINTER'S MODEL USING ENTIRE DATA SET INTO
## THE FUTURE FOR 12 PERIODS.

# Create Holt-Winter's (HW) exponential smoothing for full Air Passengers data set. 
# Use ets() function with model = "ZZZ", to identify the best HW option
# and optimal alpha, beta, & gamma to fit HW for the entire data period.
HW.ZZZ <- ets(pass.ts, model = "ZZZ")
HW.ZZZ #.

# Use forecast() function to make predictions using this HW model for
# 12 month into the future.
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 12 , level = 0)
HW.ZZZ.pred

####
# plot HW predictions for original data, optimal smoothing parameters.
plot(HW.ZZZ.pred$mean, 
     xlab = "Time", ylab = "Passengers (in 000s)", ylim = c(1000, 7000), bty = "l",
     xaxt = "n", xlim = c(2006, 2021.25),
     main = "Holt-Winter's Automated Model for Entire Data Set and Forecast for Future 12 Periods", 
     lty = 2, col = "blue", lwd = 2) 
axis(1, at = seq(2006, 2021, 1), labels = format(seq(2006, 2021, 1)))
lines(HW.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(pass.ts)
legend(2006,5000, 
       legend = c("Passengers", 
                  "Holt-Winter's Automated Model for Entire Data Set",
                  "Holt-Winter's Automated Model's Forecast, Future 12 Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
# lines(c(2004.25 - 3, 2004.25 - 3), c(0, 2600))
lines(c(2020, 2020), c(0, 7000))
lines(c(2019.25, 20.25), c(0, 7000))
#lines(c(2019.93, 2019.93), c(0, 7000))
text(2012.25, 7000, "Entire Data Set")
text(2020.75, 7000, "Future")
arrows(2006.5 - 0.5, 6500, 2019.75, 6550, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 6550, 2021.75, 6550, code = 3, length = 0.1,
       lwd = 1, angle = 30)

###
##Accuracy for HW(ZZ) on the entire data set

round(accuracy(HW.ZZZ.pred$fitted, pass.ts), 3) 
round(accuracy((snaive(pass.ts))$fitted, pass.ts), 3)


##Holt-Winter's automated model with AR(1) model for the regression


## Residual of the Holt-Winter's (ZZZ) Model

train.hw.ZZZ <- ets(train.ts, model = "ZZZ")
train.hw.ZZZ 

# Use forecast() function to make predictions using this HW model with validation period (nValid). 
# Show predictions in tabular format.
train.hw.ZZZ.pred <- forecast(train.hw.ZZZ, h = nValid, level = 0)
train.hw.ZZZ.pred
train.hw.ZZZ.pred$residuals


#######

# Use Acf() function to identify autocorrelation for the model residuals 
# (training and validation sets), and plot autocorrelation for different 
# lags (up to maximum of 12).
Acf(train.hw.ZZZ.pred$residuals, lag.max = 12, 
    main = "Autocorrelation for Air Passengers Training Residuals")
Acf(valid.ts - train.hw.ZZZ.pred$mean, lag.max = 12, 
    main = "Autocorrelation for Air Passengers Validation Residuals")


## USE Arima() FUNCTION TO CREATE AR(1) MODEL FOR TRAINING RESIDUALS.
## CREATE TWO-LEVEL MODEL WITH Holt-Winter's Optimal 

# Use summary() to identify parameters of AR(1) model. 
res.ar1 <- Arima(train.hw.ZZZ.pred$residuals, order = c(1,0,0))
summary(res.ar1)
res.ar1$fitted

####


# Use forecast() function to make prediction of residuals in validation set.
res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)
res.ar1.pred


# Develop a data frame to demonstrate the training AR model results 
# vs. original training series, training HW(ZZZ) model, 
# and its residuals.  
train.df <- data.frame(train.ts, train.hw.ZZZ$fitted, 
                       train.hw.ZZZ$residuals, res.ar1$fitted, res.ar1$residuals)
names(train.df) <- c("Passengers", "HW(ZZZ)", "Residuals",
                     "AR.Model", "AR.Model.Residuals")
train.df

Acf(res.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for Air Passengers Training Residuals of Residuals")

######

# Create data table with validation data, HW(ZZZ) forecast
# for validation period, AR(1) residuals for validation, and 
# two level model results. 
valid.two.level.pred <- train.hw.ZZZ.pred$mean + res.ar1.pred$mean

valid.df <- data.frame(valid.ts, train.hw.ZZZ.pred$mean, 
                       res.ar1.pred$mean, valid.two.level.pred)
names(valid.df) <- c("Passengers", "HW.Forecast", 
                     "AR(1)Forecast", "Combined.Forecast")
valid.df

# Use accuracy() function to identify common accuracy measures for validation period forecast:
# (1) two-level model (HW(ZZZ) + AR(1) model for residuals),
# (2) Holt-Winter's Optimal only.
round(accuracy(valid.two.level.pred, valid.ts), 3)
round(accuracy(train.hw.ZZZ.pred$mean, valid.ts), 3)




## HW.ZZZ and AR1 on the Entire Data set  

#From Above- (HW ZZZ) on the Entire Data set
HW.ZZZ <- ets(pass.ts, model = "ZZZ")
HW.ZZZ 

# Use forecast() function to make predictions using this HW model for
# 12 month into the future.
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 12 , level = 0)
HW.ZZZ.pred

# Use Arima() function to fit AR(1) model for HW(ZZZ) residuals.
# The ARIMA model order of order = c(1,0,0) gives an AR(1) model.
# Use forecast() function to make prediction of residuals into the future 12 months.
#residual.ar6 <- Arima(HW.ZZZ$residuals, order = c(6,0,0))
#residual.ar6.pred <- forecast(residual.ar6, h = 12, level = 0)


residual.ar1 <- Arima(HW.ZZZ$residuals, order = c(1,0,0))
residual.ar1.pred <- forecast(residual.ar1, h = 12, level = 0)

# Use summary() to identify parameters of AR(1) model.
summary(residual.ar1)

# Use Acf() function to identify autocorrealtion for the residual of residuals 
# and plot autocorrelation for different lags (up to maximum of 12).
Acf(residual.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for Air Passengers Residuals of Residuals for Entire Data Set")


# Identify forecast for the future 12 periods as sum of HW(ZZZ)
# and AR(1) model for residuals.
HW.ar1.pred <- HW.ZZZ.pred$mean + residual.ar1.pred$mean
HW.ar1.pred


# Create a data table with HW(ZZZ) forecast for 12 future periods,
# AR(1) model for residuals for 12 future periods, and combined two-level forecast for
# 12 future periods. 
table.df <- data.frame(HW.ZZZ.pred$mean, 
                       residual.ar1.pred$mean, HW.ar1.pred)
names(table.df) <- c("HW.Forecast", "AR(1)Forecast","Combined.Forecast")
table.df



# Use accuracy() function to identify common accuracy measures for:
# (1) two-level model (HW(ZZZ) model + AR(1) model for residuals),
# (2) Holt-Winter's Optimal only and
# (3) seasonal naive forecast. 
round(accuracy(HW.ZZZ.pred$fitted + residual.ar1$fitted, pass.ts), 3)
round(accuracy(HW.ZZZ.pred$fitted, pass.ts), 3) 
round(accuracy((snaive(pass.ts))$fitted, pass.ts), 3)


#Two level with Holt-Winter's autoselection and Trailing Moving Average for the residuals of the Holt's Model


train.hw.ZZZ <- ets(train.ts, model = "ZZZ")
train.hw.ZZZ 

# Use forecast() function to make predictions using this HW model with validation period (nValid). 
# Show predictions in tabular format.
train.hw.ZZZ.pred <- forecast(train.hw.ZZZ, h = nValid, level = 0)
train.hw.ZZZ.pred
train.hw.ZZZ.pred$residuals

# Identify and display residuals based on HW(ZZZ) model (training model)
training.HW.ZZZ.res <- train.hw.ZZZ.pred$residuals
training.HW.ZZZ.res
# Apply trailing MA for residuals with window width k = 2. 
ma.trail.HW.ZZZ.res <- rollmean(training.HW.ZZZ.res, k = 2, align = "right")
ma.trail.HW.ZZZ.res
# Create residuals forecast for validation period.
ma.trail.HW.ZZZ.res.pred <- forecast(ma.trail.HW.ZZZ.res, h = validation_period, level = 0)
ma.trail.HW.ZZZ.res.pred
# HW(ZZZ) residuals in validation period.
training.HW.ZZZ.res.valid <- valid.ts - train.hw.ZZZ.pred$mean
training.HW.ZZZ.res.valid
# To develop real forecast for validation period, 
# combine HW(ZZZ) forecast and trailing MA forecast for residuals.
valid.forecast.2level.HW.ma <- train.hw.ZZZ.pred$mean + ma.trail.HW.ZZZ.res.pred$mean
valid.forecast.2level.HW.ma


# Use accuracy() function to identify common accuracy measures for:
# (1) two-level model (HW(ZZZ) model + AR(1) model for residuals),
# (2) Holt-Winter's Optimal only and
# (3) seasonal naive forecast. 


# Accuracy HW(ZZZ) (training data)
round(accuracy(train.hw.ZZZ.pred$mean, validation.ts), 3)
# Accuracy of two-level HW(ZZZ)model and trailing MA for residuals (training data)
round(accuracy(valid.forecast.2level.HW.ma, validation.ts), 3)



# Fit a HW with auto selection model
# entire data set.

HW.ZZZ <- ets(pass.ts, model = "ZZZ")
HW.ZZZ

# Use forecast() function to make predictions using this HW model for
# 12 month into the future.
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 12 , level = 95)
HW.ZZZ.pred


# Identify and display HW(ZZZ) residuals for entire data set.
tot.HW.res <- HW.ZZZ$residuals
tot.HW.res

# Use trailing MA to forecast residuals for entire data set.
tot.ma.trail.res <- rollmean(tot.HW.res, k = 4, align = "right")
tot.ma.trail.res

# Create forecast for trailing MA residuals for future 12 periods.
tot.ma.trail.res.pred <- forecast(tot.ma.trail.res, h = 12, level = 0)
tot.ma.trail.res.pred

# Develop 2-level forecast for future 12 periods by combining 
# HW(ZZZ) forecast and trailing MA for residuals for future
# 12 periods.
tot.fst.2level <- HW.ZZZ.pred$mean + tot.ma.trail.res.pred$mean
tot.fst.2level

# Create a table with HW(ZZZ) forecast, trailing MA for residuals,
# and total forecast for future 12 periods.
future12.df <- data.frame(HW.ZZZ.pred$mean, tot.ma.trail.res.pred$mean, 
                          tot.fst.2level)
names(future12.df) <- c("HW.Fst", "MA.Residuals.Fst", "Combined.Fst")
future12.df

# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.

round(accuracy(HW.ZZZ.pred$fitted + residual.ar1$fitted, pass.ts), 3)
round(accuracy(HW.ZZZ.pred$fitted, pass.ts), 3) 
round(accuracy(HW.ZZZ.pred$fitted+tot.ma.trail.res, pass.ts), 3)
round(accuracy((snaive(pass.ts))$fitted, pass.ts), 3)


## USE ts() FUNCTION TO CREATE TIME SERIES DATA SET.
## USE Acf() FUNCTION TO IDENTIFY AUTOCORRELATION.

# Function ts() takes three arguments: start, end, and freq.
# With monthly data, frequency (freq) of periods in a season (year) is 12. 
# With quarterly data, frequency in a season (year) is equal to 4.
# Arguments start and end are pairs: (season number, period number).
airpass.ts <- ts(project.data$Passengers_in000s, 
                 start = c(2006, 1), end = c(2019, 12), freq = 12)
airpass.ts
# Use Acf() function to identify autocorrelation and plot autocorrelation
# for different lags (up to maximum of 12).
Acf(airpass.ts, lag.max = 12, main = "Autocorrelation for Airpass Passengers")

## CREATE TIME SERIES PARTITION.

# Define the numbers of months in the training and validation sets,
nValid <- 36
nTrain <- length(airpass.ts) - nValid
train.ts <- window(airpass.ts, start = c(2006, 1), end = c(2006, nTrain))
valid.ts <- window(airpass.ts, start = c(2006, nTrain + 1), 
                   end = c(2006, nTrain + nValid))
train.ts
valid.ts

## FIT AR(2) MODEL.

# Use Arima() function to fit AR(2) model.
# The ARIMA model of order = c(2,0,0) gives an AR(2) model.
# Use summary() to show AR(2) model and its parameters.
train.ar2 <- Arima(train.ts, order = c(2,0,0))
summary(train.ar2)

# Apply forecast() function to make predictions for ts with 
# AR model in validation set.   
train.ar2.pred <- forecast(train.ar2, h = nValid, level = 0)
train.ar2.pred

# Use Acf() function to create autocorrelation chart of AR(2) model residuals.
Acf(train.ar2$residuals, lag.max = 12, 
    main = "Autocorrelations of AR(2) Model Residuals")

# Plot ts data, AR model, and predictions for validation period.
plot(train.ar2.pred$mean, 
     xlab = "Time", ylab = "Air Passengers (in 0000s)", ylim = c(1000, 7000), bty = "l",
     xlim = c(2005, 2022), main = "AR(2) Model",
     col = "blue", lwd =2) 
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)) )
lines(train.ar2.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lwd = 2, lty = 1)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend(2005,7000, legend = c("Airpass Passengers Time Series", 
                             "AR(2) Forecast for Training Period",
                             "AR(2) Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and hori
# describing training, validation, and future prediction intervals.
lines(c(2017, 2017), c(0, 7000))
lines(c(2019.93, 2019.93), c(0, 7000))
text(2011, 7000, "Training")
text(2018.5, 7000, "Validation")
text(2021, 7000, "Future")
arrows(2005, 6500, 2016.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.2, 6500, 2019.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 6500, 2022, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)


## FIT MA(2) MODEL.

# Use Arima() function to fit MA(2) model.
# The ARIMA model of order = c(0,0,2) gives an MA(2) model.
# Use summary() to show MA(2) model and its parameters.
train.ma2<- Arima(train.ts, order = c(0,0,2))
summary(train.ma2)

# Apply forecast() function to make predictions for ts with 
# MA model in validation set.    
train.ma2.pred <- forecast(train.ma2, h = nValid, level = 0)
train.ma2.pred

# Use Acf() function to create autocorrelation chart of MA(2) model residuals.
Acf(train.ma2$residuals, lag.max = 12, 
    main = "Autocorrelations of MA(2) Model Residuals")


# Plot ts data, MA model, and predictions for validation period.
plot(train.ma2.pred$mean, 
     xlab = "Time", ylab = "Air Passengers (in 0000s)", ylim = c(1000, 7000), bty = "l",
     xlim = c(2005, 2022), main = "MA Model",
     col = "blue", lwd =2) 
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)) )
lines(train.ma2.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend(2005,7000, legend = c("Air Passesngers", 
                             "MA(2) Forecast for Training Period",
                             "MA(2) Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2017, 2017), c(0, 7000))
lines(c(2019.93, 2019.93), c(0, 7000))
text(2011, 7000, "Training")
text(2018.5, 7000, "Validation")
text(2021, 7000, "Future")
arrows(2005, 6500, 2016.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.2, 6500, 2019.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 6500, 2022, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)


## FIT ARMA(2,2) MODEL.

# Use Arima() function to fit ARMA(2,2) model.
# The ARIMA model of order = c(2,0,2) gives an ARMA(2,2) model.
# Use summary() to show ARMA model and its parameters.
train.arma2 <- Arima(train.ts, order = c(2,0,2))
summary(train.arma2)

# Apply forecast() function to make predictions for ts with 
# ARMA model in validation set.    
train.arma2.pred <- forecast(train.arma2, h = nValid, level = 0)
train.arma2.pred

# Use Acf() function to create autocorrelation chart of ARMA(2,2) model residuals.
Acf(train.arma2$residuals, lag.max = 12, 
    main = "Autocorrelations of ARMA(2,2) Model Residuals")

# Plot ts data, ARMA model, and predictions for validation period.
plot(train.arma2.pred, 
     xlab = "Time", ylab = "Air Passengers (in 0000s)", ylim = c(1000, 7000), bty = "l",
     xlim = c(2005, 2022), main = "ARMA(2,2) Model",
     col = "blue", lwd =2) 
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)) )
lines(train.arma2.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend(2005,7000, legend = c("Air Passesngers Time Series", 
                             "ARMA(2,2) Forecast for Training Period",
                             "ARMA(2,2) Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2017, 2017), c(0, 7000))
lines(c(2019.93, 2019.93), c(0, 7000))
text(2011, 7000, "Training")
text(2018.5, 7000, "Validation")
text(2021, 7000, "Future")
arrows(2005, 6500, 2016.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.2, 6500, 2019.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 6500, 2022, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)


## FIT ARIMA(2,1,2) MODEL.

# Use Arima() function to fit ARIMA(2,1,2) model.
# Use summary() to show ARIMA model and its parameters.
train.arima <- Arima(train.ts, order = c(2,1,2)) 
summary(train.arima)

# Apply forecast() function to make predictions for ts with 
# ARIMA model in validation set.    
train.arima.pred <- forecast(train.arima, h = nValid, level = 0)
train.arima.pred

# Using Acf() function, create autocorrelation chart of ARIMA(2,1,2) model residuals.
Acf(train.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of ARIMA(2,1,2) Model Residuals")

# Plot ts data, ARIMA model, and predictions for validation period.
plot(train.arima.pred, 
     xlab = "Time", ylab = "Air Passengers (in 0000s)", ylim = c(1000, 8000), bty = "l",
     xlim = c(2005, 2022), main = "ARIMA(2,1,2) model",
     col = "blue", lwd =2) 
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)) )
lines(train.arima.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend(2005,7500, legend = c("RAir Passesngers Time Series", 
                             "ARIMA Forecast for Training Period",
                             "ARIMA Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2017, 2017), c(0, 7000))
lines(c(2019.93, 2019.93), c(0, 7000))
text(2011, 8000, "Training")
text(2018.5, 8000, "Validation")
text(2021, 8000, "Future")
arrows(2005, 7800, 2016.8, 7800, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.2, 7800, 2019.8, 7800, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 7800, 2022, 7800, code = 3, length = 0.1,
       lwd = 1, angle = 30)


## FIT ARIMA(2,1,2)(1,1,2) MODEL.

# Use Arima() function to fit ARIMA(2,1,2)(1,1,2) model for 
# trend and seasonality.
# Use summary() to show ARIMA model and its parameters.
train.arima.seas <- Arima(train.ts, order = c(2,1,2), 
                          seasonal = c(1,1,2)) 
summary(train.arima.seas)

# Apply forecast() function to make predictions for ts with 
# ARIMA model in validation set.    
train.arima.seas.pred <- forecast(train.arima.seas, h = nValid, level = 0)
train.arima.seas.pred

# Use Acf() function to create autocorrelation chart of ARIMA(2,1,2)(1,1,2) 
# model residuals.
Acf(train.arima.seas$residuals, lag.max = 12, 
    main = "Autocorrelations of ARIMA(2,1,2)(1,1,2) Model Residuals")

# Plot ts data, ARIMA model, and predictions for validation period.
plot(train.arima.seas.pred, 
     xlab = "Time", ylab = "Air Passengers (in 0000s)", ylim = c(1000, 8000), bty = "l",
     xlim = c(2005, 2022), main = "ARIMA(2,1,2)(1,1,2) model",
     col = "blue", lwd =2) 
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)) )
lines(train.arima.seas.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend(2005,7000, legend = c("Air Passesngers Time Series", 
                             "Seasonal ARIMA Forecast for Training Period",
                             "Seasonal ARIMA Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2017, 2017), c(0, 7000))
lines(c(2019.93, 2019.93), c(0, 7000))
text(2011, 8000, "Training")
text(2018.5, 8000, "Validation")
text(2021, 8000, "Future")
arrows(2005, 7700, 2016.8, 7700, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.2, 7700, 2019.8, 7700, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 7700, 2022, 7700, code = 3, length = 0.1,
       lwd = 1, angle = 30)


## FIT AUTO ARIMA MODEL.

# Use auto.arima() function to fit ARIMA model.
# Use summary() to show auto ARIMA model and its parameters.
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.  
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred

# Using Acf() function, create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(train.auto.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")

# Plot ts data, trend and seasonality data, and predictions for validation period.
plot(train.auto.arima.pred$mean, 
     xlab = "Time", ylab = "Passengers (in 0000s)", ylim = c(1000, 8000), bty = "l",
     xlim = c(2005, 2022), main = "MA Model",
     col = "blue", lwd =2) 
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)) )
lines(train.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend(2004,7500, legend = c("Air Passesngers Time Series", 
                             "Auto ARIMA Forecast for Training Period",
                             "Auto ARIMA Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2017, 2017), c(0, 8000))
lines(c(2019.93, 2019.93), c(0, 8000))
text(2011, 8000, "Training")
text(2018.5, 8000, "Validation")
text(2021, 8000, "Future")
arrows(2005, 7700, 2016.8, 7700, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.2, 7700, 2019.8, 7700, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 7700, 2022, 7700, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use accuracy() function to identify common accuracy measures 
# for validation period forecast:
# (1) AR(2) model; 
# (2) MA(2) model; 
# (3) ARMA(2,2) model; 
# (4) ARIMA(2,1,2) model; 
# (5) ARIMA(2,1,2)(1,1,2) model; and 
# (6) Auto ARIMA model.
round(accuracy(train.ar2.pred, valid.ts), 3)
round(accuracy(train.ma2.pred, valid.ts), 3)
round(accuracy(train.arma2.pred, valid.ts), 3)
round(accuracy(train.arima.pred, valid.ts), 3)
round(accuracy(train.arima.seas.pred, valid.ts), 3)
round(accuracy(train.auto.arima.pred, valid.ts), 3)


## FIT SEASONAL ARIMA AND AUTO ARIMA MODELS FOR ENTIRE DATA SET. 
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.


## FIT SEASONAL ARIMA FOR ENTIRE DATA SET. 

# Use arima() function to fit seasonal ARIMA(2,1,2)(1,1,2) model 
# for entire data set.
# use summary() to show auto ARIMA model and its parameters for entire data set.
arima.seas <- Arima(airpass.ts, order = c(2,1,2), 
                    seasonal = c(1,1,2)) 
summary(arima.seas)

# Apply forecast() function to make predictions for ts with 
# seasonal ARIMA model for the future 12 periods. 
arima.seas.pred <- forecast(arima.seas, h = 12, level = 0)
arima.seas.pred

# Use Acf() function to create autocorrelation chart of seasonal ARIMA 
# model residuals.
Acf(arima.seas$residuals, lag.max = 12, 
    main = "Autocorrelations of Seasonal ARIMA Model Residuals")

# Plot historical data, predictions for historical data, and seasonal 
# ARIMA forecast for 12 future periods.
plot(airpass.ts, 
     xlab = "Time", ylab = "Air Passengers (in 0000s)", ylim = c(1000, 7000), bty = "l",
     xlim = c(2005, 2022), main = "MA Model",
     col = "blue", lwd =2) 
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)) )
lines(arima.seas$fitted, col = "blue", lwd = 2)
lines(arima.seas.pred$mean, col = "blue", lty = 5, lwd = 2)
legend(2005,7000, legend = c("Air Passesngers Series", 
                             "Seasonal ARIMA Forecast", 
                             "Seasonal ARIMA Forecast for 12 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# plot on the chart vertical lines and horizontal arrows
# describing training and future prediction intervals.
# lines(c(2004.25 - 3, 2004.25 - 3), c(0, 2600))
lines(c(2017, 2017), c(0, 7000))
lines(c(2019.93, 2019.93), c(0, 7000))
text(2011, 7000, "Training")
text(2018.5, 7000, "Validation")
text(2021, 7000, "Future")
arrows(2005, 6500, 2016.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.2, 6500, 2019.8, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 6500, 2022, 6500, code = 3, length = 0.1,
       lwd = 1, angle = 30)


## FIT AUTO ARIMA MODELS FOR ENTIRE DATA SET. 

# Use auto.arima() function to fit ARIMA model for entire data set.
# use summary() to show auto ARIMA model and its parameters for entire data set.
auto.arima <- auto.arima(airpass.ts)
summary(auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model for the future 12 periods. 
auto.arima.pred <- forecast(auto.arima, h = 12, level = 0)
auto.arima.pred

# Use Acf() function to create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(auto.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")

# Plot historical data, predictions for historical data, and Auto ARIMA 
# forecast for 12 future periods.
plot(airpass.ts, 
     xlab = "Time", ylab = "Air Passengers (in 0000s)", ylim = c(1000, 8000), bty = "l",
     xlim = c(2005, 2022), main = "Auto ARIMA Model",
     col = "blue", lwd =2) 
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1) ))
lines(auto.arima$fitted, col = "blue", lwd = 2)
lines(auto.arima.pred$mean, col = "blue", lty = 5, lwd = 2)
legend(2005,7000, legend = c("Air Passesngers Series", 
                             "Auto ARIMA Forecast", 
                             "Auto ARIMA Forecast for 12 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")
# plot on the chart vertical lines and horizontal arrows
# describing training and future prediction intervals.
# lines(c(2004.25 - 3, 2004.25 - 3), c(0, 2600))
lines(c(2017, 2017), c(0, 8000))
lines(c(2019.93, 2019.93), c(0, 8000))
text(2011, 8000, "Training")
text(2018.5, 8000, "Validation")
text(2021, 8000, "Future")
arrows(2005, 7700, 2016.8, 7700, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.2, 7700, 2019.8, 7700, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.1, 7700, 2022, 7700, code = 3, length = 0.1,
       lwd = 1, angle = 30)


# MEASURE FORECAST ACCURACY FOR ENTIRE DATA SET.

# Use accuracy() function to identify common accuracy measures for:

# (1) Seasonal ARIMA (2,1,2)(1,1,2) Model
round(accuracy(arima.seas.pred$fitted, airpass.ts), 3)
# (2) Auto ARIMA Model
round(accuracy(auto.arima.pred$fitted, airpass.ts), 3)
# (3) Seasonal naive forecast
round(accuracy((snaive(airpass.ts))$fitted, airpass.ts), 3)
# (4) Naive forecast
round(accuracy((naive(airpass.ts))$fitted, airpass.ts), 3)

