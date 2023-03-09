############################################################ 
#Data collected from Stats Canada 
#Data set: Consumer Price Index (CPI)
#Created by Alicia Mckeough Feb 25th, 2023
############################################################ 
#loading ggplot and forecasting packages 
library(ggplot2) 
install.packages("fpp2")
library(fpp2) 
#loading data  
library(readxl)
Health_CAN_CPI_2019_2023 <- read_excel("Desktop/MMASc./Spring_semester/Consulting/Health_CAN_CPI_2019-2023.xlsx", 
                                         +     col_types = c("date", "numeric"))
View(Health_CAN_CPI_2019_2023)                                                                                                                          

#Convert data class to time series data 
Y<-ts(Health_CAN_CPI_2019_2023[,2],start=c(2019),frequency=12) 


# data parameters [,2] selecting from second column 
# Starting data 2013, frequency is collected 12 times per year  
############################################################# 
# Preform preliminary analysis   
############################################################# 
autoplot(Y) + 
  ggtitle("Five Year Time Plot of CPI in Canada Health Care") +  
  ylab("Consumer Price Index (CPI)") +
  xlab("Year")
# Data showing positive trend  

DY <-diff(Y)

# time plot of difference data - removing trend 
autoplot(DY) + 
  ggtitle("Five Year Time Plot of CPI in Canada Health Care") +  
  ylab("CPI") 

ggseasonplot(DY) + 
  ggtitle("Seasonal Plot: change in monthly CPI") +
  ylab("CPI")
############################################################## 
#Fit exponential smoothing model (ETS) - function determines best ETS model 
############################################################## 
fit_ets<-ets(DY) 
print(summary(fit_ets)) 
checkresiduals(fit_ets) 
# ETS A,N,N model fits best with residual SD of 1261.411 
############################################################### 
# Fit Arima methods and returns best Arima model   
fit_arima<-auto.arima(DY,d=1,stepwise = FALSE,approximation = FALSE,trace = TRUE) 
# y=data d=1 regular difference in data to ensure data is stationary 
# Approx and Stepwise = FALSE since we only have one data series  
# Trace = TRUE will print out all the models it runs  
#  Best model: ARIMA(0,1,0) with drift 
print(summary(fit_arima)) 
checkresiduals(fit_arima) 
sqrt(393246)  
# Residual SD 627.0933 (this model fits the best, has lower SD than ETS) 
############################################################## 
# Create a forecast with Arima model  
############################################################## 
FC<-forecast(fit_arima,h=5) #forecasting 5 years 
# Graphing plot 
autoplot(FC, include = 10)+  
  ggtitle("Forecast of 5 years in Canada") +  
  ylab("CPI") 
my_autoplot<-autoplot(FC)+ ggtitle("Forecast of 2023 CPI in Canada's Health Care System") +  
  ylab(" Consumer Price Index (CPI)") +  
  xlab("Year")   
my_autoplot + theme(text = element_text(size = 6)) +  
  scale_x_continuous(breaks=seq(2019,2023,1)) 
############################################################## 
# ONTARIO GRAPH  
############################################################## 
#Convert data class to time series data 
Y<-ts(canadaplasticD1[,3],start=c(2012),frequency=1) 
# data parameters [,3] selecting from second column 
# Starting data 2012, frequency is collected once per year  
############################################################# 
# Preform preliminary analysis   
############################################################# 
autoplot(Y) + ggtitle("Forecast of 5 years in Ontario") +  
  ylab("Tons of Plastic leaked permanently into the environment") 
# Data showing positive trend  
############################################################## 
#Fit exponential smoothing model (ETS) - function determines best ETS model 
############################################################## 
fit_ets<-ets(Y) 
print(summary(fit_ets)) 
checkresiduals(fit_ets) 
# ETS A,N,N model fits best with residual SD of 1261.411 
############################################################### 
# Fit Arima methods and returns best Arima model   
fit_arima<-auto.arima(Y,d=1,stepwise = FALSE,approximation = FALSE,trace = TRUE) 
# y=data d=1 regular difference in data to ensure data is stationary 
# Approx and Stepwise = FALSE since we only have one data series  
# Trace = TRUE will print out all the models it runs  
#  Best model: ARIMA(0,1,0) with drift 
print(summary(fit_arima)) 
checkresiduals(fit_arima) 
sqrt(393246)  
# Residual SD 627.0933 (this model fits the best, has lower SD than ETS) 
############################################################## 
# Create a forecast with Arima model  
############################################################## 
FC<-forecast(fit_arima,h=5) #forecasting 5 years 
# Graphing plot 
autoplot(FC, include = 5)+  
  ggtitle("Forecast of 5 years in Ontario") +  
  ylab("Tons of Plastic leaked into environment") 
my_autoplot<-autoplot(FC)+ ggtitle("Forecast of 5 years in Ontario") +  
  ylab("Tons of Plastic leaked into environment") +  
  xlab("Year")   
my_autoplot + theme(text = element_text(size = 7)) +  
  scale_x_continuous(breaks=seq(2012,2023,1)) 
############################################################## 
