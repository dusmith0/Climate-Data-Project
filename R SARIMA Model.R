## Created by Dustin Smith
## 2/2/2026
##
##### Initial SARMIA model for Climate Data  ######


## Naming Conventions
#- Climate = the original data set (146 observations with 19 parameters)
#- J.D = Jan - Dec (Yearly temp)
#- N.D = Dec - Nov (Meteorological Year)
#- DJF = Dec, Jan, Feb
#- MAM = Mar, Apr, May
#- JJA = June, July, Aug
#- SON = Sep, Oct, Nov
#-
#-
#-



##Libraries
library("dplyr") #filter(), select(), mutate(), group_by(), summarize(), arrange()
library("astsa") #General Times Series Plot
library("ggplot2") #Better Graphics
library("zoo")
library("forecast")
library("tseries")
library("purrr")

## Reading in the data.
climate <- read.csv("C:\\Users\\dustin.smith\\OneDrive - Lubbock Independent School District\\Pictures\\Documents\\Codes\\Python\\Climate data project\\Data\\Global means.csv")
View(climate)

str(climate)

## J.D. Data ##
####--------------------------------------------------------------####
## to get an early analysis 
auto.arima(climate$J.D) #Suggests a (1,1,3) with drift


## Checking stationary and difference effect for stationary. 
adf.test((climate$J.D))
adf.test(diff(climate$J.D))

tsplot((climate$J.D)) 
tsplot(diff(climate$J.D))

## Checking model selection with ACF and PACF
acf(diff(climate$J.D)) ## Used for the moving average lag
pacf(diff(climate$J.D)) ## Used for the autocorrelation lag
acf2(diff(climate$J.D))

## generating model 
## Fit is Auto Regression, Differencing, Moving Average
# 0,1,0 = AIC = -217.56, BIC = -211.61
# 1,1,1 = AIC=-235.65,  BIC=-223.75 #Maybe this one for simplicity
# 1,1,3 = AIC=-240.57,  BIC=-222.71  
fit <- forecast::Arima(climate$J.D,
                       order = c(1,1,3),
                       seasonal = c(0,0,0),
                       include.drift = TRUE)

summary(fit)

model <- fitted(fit)
plot(forecast(fit))
lines(model)



## Monthly Data ##
####--------------------------------------------------------------####
## The data causes margin errors if not coerced to a single list. 
months <- (climate[,2:13])
colnames(months) <- NULL

months <- c(unlist(t(months)))
head(months)

tsplot(months[1:length(months)])
tsplot((diff(months)))

## Dickey-Fuller test does not suggest differencing 
adf.test(months) # Test statistics = -4
adf.test(diff(months)) # Test Statistic = -14

## early modeling
auto.arima((months), seasonal = TRUE)
## Suggests a (3,1,3) with drift. It does not suggest seasonlaity. 

## acf2 ACF plot diverges, and indicates differencing in very needed. 
acf2(diff(months))
acf2((months))
  ## Maybe a (2,1,2)x(1,0,1)x12 ??
fit <- forecast::Arima(months, order = c(3,1,3),
                       seasonal = list(order = c(1, 0, 1), period = 12))

summary(fit)
fitted <- fitted(fit)
plot(forecast(fit, h = 120))
lines(fitted)

0.1064843^2
