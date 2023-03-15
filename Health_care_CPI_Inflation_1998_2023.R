############################################################
# CPI - Canadian Health Care System
# Data collected from Stats Canada 
# Created by Alicia Mckeough Feb 25th, 2023
############################################################ 
# loading ggplot and forecasting packages 
library(ggplot2) 
install.packages("fpp2")
library(fpp2) 
# loading data  

library(readxl)
Health_Care_CAN1998_2023 <- read_excel("Desktop/MMASc./Spring_semester/Consulting/Health_Care_CAN_Inflation.xlsx")
View(Health_Care_CAN1998_2023) 

# Convert data class to time series data 
Y<-ts(Health_Care_CAN1998_2023[,2],start=c(1998),frequency=12) 


# data parameters [,2] selecting from second column 
# Starting data 2019, frequency is collected 12 times per year  
############################################################# 
# Preform preliminary analysis   
############################################################# 
autoplot(Y) + 
  ggtitle("Time Plot of CPI in Canada Health Care 25 Years") +  
  ylab("Consumer Price Index (CPI)") +
  xlab("Year") + 
  theme(text = element_text(size = 8)) +  
  scale_x_continuous(breaks=seq(1998,2023,5))


# Data showing positive trend  

DY <-diff(Y)

# time plot of difference data - removing trend 
autoplot(DY) + 
  ggtitle("Removed Trend - Time Plot of CPI in Canada Health Care 25 Years") +  
  ylab("CPI") + 
  theme(text = element_text(size = 8)) +  
  scale_x_continuous(breaks=seq(1998,2023,5))

ggseasonplot(DY) + 
  ggtitle("Seasonal Plot: change in monthly CPI in Canadian Health Care 25 Years") +
  ylab("CPI")
############################################################## 
# Fit exponential smoothing model (ETS) - function determines best ETS model 
############################################################## 
fit_ets<-ets(Y) 
print(summary(fit_ets)) 
checkresiduals(fit_ets) 
# ETS M,Ad,A model fits best with residual SD of 0.0027 
############################################################### 
# Trying Arima method to determine best model   
fit_arima<-auto.arima(Y,d=1,D=1,stepwise = FALSE,approximation = FALSE,trace = TRUE) 
# y=data d=1 regular and D=1 seasonal difference in data to ensure data is stationary 
# Approx and Stepwise = FALSE since we only have one data series  
# Trace = TRUE will print out all the models it runs  
#  Best model: ARIMA(1,1,2)(0,1,1)[12] 
print(summary(fit_arima)) 
checkresiduals(fit_arima) 
sqrt(0.1053)  
# Residual SD 0.3244996 (this model is not the best, as it has a higher SD than ETS) 
############################################################## 
# Create a forecast with ETS model  
############################################################## 
FC<-forecast(fit_ets,h=48) #forecasting 4 years 
# Graphing plot - overview in the last 25 years
my_autoplot<-autoplot(FC)+ ggtitle("Forecast of 4 years in Canada's Health Care System with 25 years of Historical Data") +  
  ylab(" Consumer Price Index (CPI)") +  
  xlab("Year")   
my_autoplot + theme(text = element_text(size = 5)) +  
  scale_x_continuous(breaks=seq(1998,2027,1)) 

# Zoom in closer to forecasting data - last four years "48 months" 
my_autoplot_zoom <-autoplot(FC, include = 24) +  
  ggtitle("Forecast of 4 years in Canada's Health Care System with 25 years of Historical Data") +  
  ylab(" Consumer Price Index (CPI)") + 
  xlab("Year") 
my_autoplot_zoom + theme(text = element_text(size = 8)) 
############################################################## 
