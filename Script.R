# Forecasting Assignment
# Hina Hussain 

#########################################################################
######################### Exercise 1 ####################################
#########################################################################

library(readxl)
library(fpp2)

setwd("C:/Users/hhussain1/Desktop/Forecasting/Assignment")
data <- read_excel("DataSets2022.xlsx", sheet="Airpass_BE")
air <- ts(data[,2], frequency = 12, start = c(2003,1), end = c(2021,10))


### Train Test Split ###
air_train <- window(air, start=c(2003,1), end=c(2017,12))
air_test <- window(air, start=c(2018,1), end=c(2020,2))

# relevant full set #
air_data <- window(air, end=c(2020,2))

h <- length(air_test)

##################
### Question 1 ###
##################

#time series plot

plot(air_data, main="Time Series Plot: Belgium - EU Air Passengers",
     ylab="Passengers", xlab="Year")

#seasonal plot

seasonplot(air_data, year.labels=TRUE, year.labels.left=TRUE,
          main="Seasonal plot: Belgium - EU Air Passengers",
          ylab="Passengers",col=rainbow(20), pch=19)



#seasonal subseries plot

monthplot(air_data,ylab="Passengers",xlab="Month", xaxt="n",
          main="Seasonal subseries plot: Belgium - EU Air Passengers",
          type="l")
axis(1, at=1:12, labels=month.abb, cex=0.8)


#P ACF plot
acf(air_data, main="ACF Plot")
tsdisplay(air_data)

##################
### Question 2 ###
##################

#l <- BoxCox.lambda(air_data)
#0.0146
l <- 0

plot(BoxCox(air_data,lambda=0))


#check to see if lambda improves results


etslambda <- ets(air_train, lambda = l, biasadj = TRUE) 
fetslambda <- forecast(etslambda, h=h)


etssimple <- ets(air_train) 
fetssimple <- forecast(etssimple, h=h)


accuracy(fetslambda,air_test)[,c(2,3,5,6)]
accuracy(fetssimple,air_test)[,c(2,3,5,6)]

checkresiduals(fetslambda)

checkresiduals(fetssimple)


##################
### Question 3 ###
##################

h <- length(air_test)

fit <- snaive(air_train, lambda=l, h = h, biasadj = TRUE)
plot(fit, include=120)
lines(air_test, col="red")
#red is the real and blue is the forecast

#accuracy
accuracy(fit,air_test)[,c(2,3,5,6)]


#check residuals

checkresiduals(fit)

res <- residuals(fit)
tsdisplay(res)


Box.test(res,lag=24, fitdf=0, type="Lj") #lag = 2m for seasonal data (where m is the period of seasonality).



##################
### Question 4 ###
##################



#robust for outliers, 

stl_naive <- stlf(air_train, method="naive", h=h, lambda = 0, biasadj = TRUE, robust = TRUE)
stl_rwdrift <- stlf(air_train, method="rwdrift", h=h, lambda = 0, biasadj = TRUE, robust = TRUE)
stl_ets <- stlf(air_train, method="ets", h=h, lambda = 0, biasadj = TRUE, robust = TRUE)
stl_arima <- stlf(air_train, method="arima", h=h, lambda = 0, biasadj = TRUE, robust = TRUE)


#accuracy
accuracy(stl_naive,air_test)[,c(2,3,5,6)]
accuracy(stl_rwdrift,air_test)[,c(2,3,5,6)]
accuracy(stl_ets,air_test)[,c(2,3,5,6)]
accuracy(stl_arima,air_test)[,c(2,3,5,6)]


#check residuals
checkresiduals(stl_naive)
checkresiduals(stl_rwdrift)
checkresiduals(stl_ets)
checkresiduals(stl_arima)

plot(stl_naive, ylab = "Passengers")
lines(air_test, col="red")
 


##################
### Question 5 ###
##################


#ETS(A,A,A): Additive Holt-Winters' method with additive errors

ets1 <- ets(air_train, model = "AAA", damped = FALSE, lambda = l, biasadj = TRUE)
fets1 <- forecast(ets1, h=h)     
ets2 <- ets(air_train, model = "AAA", damped = TRUE, lambda = l, biasadj = TRUE)
fets2 <- forecast(ets2, h=h)



#automated ets
ets3 <- ets(air_train, lambda = l) #when lambda is specified, additive only is set to true
fets3 <- forecast(ets3, h=h)


#compare all 
ets1
ets2
ets3


plot(ets1)
plot(ets2)


#accuracy
accuracy(fets1,air_test)[,c(2,3,5,6)]
accuracy(fets3,air_test)[,c(2,3,5,6)]



#check residuals
checkresiduals(fets1)
checkresiduals(fets3)


#forecast graph of final

plot(fets3, ylab = "Passengers")
lines(air_test, col="red")


##################
### Question 6 ###
##################



aa1 <- auto.arima(air_train, stepwise = FALSE, approximation = FALSE, lambda = l, biasadj = TRUE)
faa1 <- forecast(aa1, h=h)
aa2 <- auto.arima(air_train, stepwise = FALSE, approximation = FALSE, lambda = l, biasadj = TRUE, d=0, D=1, max.order=9)
faa2 <- forecast(aa2, h=h)
aa3 <- auto.arima(air_train, stepwise = FALSE, approximation = FALSE, lambda = l, biasadj = TRUE, allowdrift = FALSE)
faa3 <- forecast(aa3, h=h)


aa1
aa2
aa3

#Other Arima Models

aa4 <- Arima(air_train, order=c(3,0,0), seasonal=c(2,1,3), lambda = l, biasadj = TRUE)
faa4 <- forecast(aa4, h=h)
aa5 <- Arima(air_train, order=c(3,0,0), seasonal=c(3,1,0), lambda = l, biasadj = TRUE)
faa5 <- forecast(aa5, h=h)

aa4
aa5


#accuracy 
accuracy(faa1,air_test)[,c(2,3,5,6)]
accuracy(faa2,air_test)[,c(2,3,5,6)]
accuracy(faa3,air_test)[,c(2,3,5,6)]
accuracy(faa4,air_test)[,c(2,3,5,6)]
accuracy(faa5,air_test)[,c(2,3,5,6)]


#residuals of the fit
checkresiduals(aa1)
checkresiduals(aa2)
checkresiduals(aa3)
checkresiduals(aa4)
checkresiduals(aa5)

tsdisplay(aa1$residuals)
tsdisplay(aa2$residuals)
tsdisplay(aa3$residuals)
tsdisplay(aa4$residuals)
tsdisplay(aa5$residuals)

#forecast graph of final

plot(faa5, ylab = "Passengers")
lines(air_test, col="red")


##################
### Question 8 ###
##################



etsfinal <- ets(air_data, model = "AAA", damped = TRUE, lambda = l, biasadj = TRUE)
fetsfinal <- forecast(etsfinal, h= 34)

plot(fetsfinal, main = "Out of Sample Forecasts", ylab = "Passengers")
lines(window(air,start=c(2020,2)), col="red")

summary(fetsfinal)


##################
### Question 9 ###
##################

#convert summary to dataframe
dataframe <- data.frame(summary(fetsfinal))

#obtain obs in period of interest
dataframe <- dataframe[1:20,]


avg <- mean(dataframe$Point.Forecast)
avg

#get avg from covid data

covid <- window(air,start=c(2020,3))

dataframe <- data.frame(covid)

dataframe

avg <- mean(dataframe$Airpass_BE)
avg

#########################################################################
######################### Exercise 2 ####################################
#########################################################################



data2 <- read_excel("UNRATENSA.xlsx", sheet="Sheet1")
emp <- ts(data2[,2], frequency = 12, start = c(2000,1), end = c(2022,3))


### Train Test Split ###
emp_train <- window(emp, start=c(2000,1), end=c(2017,12))
emp_test <- window(emp, start=c(2018,1), end=c(2020,2))

# full set #
emp_tt <- window(emp, end=c(2020,2))

h <- length(emp_test)


###################
### Exploration ###
###################

#time series plot

plot(emp_tt, main="Time Series Plot: Monthly Unemployment Rate",
     ylab="Unemployment Rate", xlab="Year")

#seasonal plot

seasonplot(emp_tt, year.labels=TRUE, year.labels.left=TRUE,
           main="Seasonal plot: Monthly Unemployment Rate",
           ylab="Unemployment Rate",col=rainbow(20), pch=19)



#seasonal subseries plot

monthplot(emp_tt,ylab="Unemployment Rate",xlab="Month", xaxt="n",
          main="Seasonal subseries plot:  Monthly Unemployment Rate",
          type="l")
axis(1, at=1:12, labels=month.abb, cex=0.8)


#P ACF plot
acf(emp_tt, main="ACF Plot")
tsdisplay(emp_tt)


#####################
### Decomposition ###
#####################


plot(stl(ts(emp_tt[1:242], frequency=12), s.window = "periodic", robust = TRUE), main = "STL Decomposition")


stl_naive2 <- stlf(emp_train, method="naive", h=h, robust = TRUE)
stl_ets2 <- stlf(emp_train, method="ets", h=h, robust = TRUE)



#accuracy
accuracy(stl_naive2,emp_test)[,c(2,3,5,6)]
accuracy(stl_ets2,emp_test)[,c(2,3,5,6)]



#check residuals
checkresiduals(stl_naive2)
checkresiduals(stl_ets2)


#plot selected
plot(stl_ets2, ylab = "Unemployment Rate")
lines(emp_test, col="red")


#####################
### ARIMA         ###
#####################

tsdisplay(diff(emp_train,12), main="Seasonally differenced Unemployment Rate")


tsdisplay(diff(diff(emp_train,12)),
          main="Double differenced Unemployment Rate")


##MA - 011 011 arima 

fit <- Arima(emp_train, order=c(0,1,1), seasonal=c(0,1,1))
tsdisplay(residuals(fit))

##MA - 0110 013 arima 

fit2 <- Arima(emp_train, order=c(0,1,10), seasonal=c(0,1,3))
tsdisplay(residuals(fit2))

fit2

#LB test

res2 <- residuals(fit2)
Box.test(res2, lag=24, fitdf=13, type="Ljung")

#accuracy
ffit2 <- forecast(fit2, h=h)
accuracy(ffit2,emp_test)[,c(2,3,5,6)]


##Auto arima

aa <- auto.arima(emp_train, stepwise = FALSE, approximation = FALSE, max.order=10)
faa <- forecast(aa, h=h)

aa

#LB test

res3 <- residuals(aa)
Box.test(res3, lag=24, fitdf=8, type="Ljung")

#residuals
checkresiduals(aa)

#accuracy
accuracy(faa,emp_test)[,c(2,3,5,6)]

##plot selected
plot(faa, ylab = "Unemployment Rate")
lines(emp_test, col="red")


#####################
### Out of sample forecast ##
#####################


final <- Arima(emp_tt, order=c(2,1,5), seasonal=c(0,1,1))
finalforecast <- forecast(final, h=34)

plot(finalforecast, main = "Out of Sample Forecasts", ylab = "Unemployment Rate")
lines(window(emp,start=c(2020,2)), col="red")



covidunemployment <- window(emp,start=c(2020,3))

covidunemployment
max(covidunemployment)

summary(finalforecast)

