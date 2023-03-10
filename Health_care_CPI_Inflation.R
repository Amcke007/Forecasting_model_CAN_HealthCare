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
Health_CAN_CPI_2019_2023 <- read_excel("Desktop/MMASc./Spring_semester/Consulting/Health_CAN_CPI_2019-2023.xlsx", 
                                         +     col_types = c("date", "numeric"))
View(Health_CAN_CPI_2019_2023)                                                                                                                          

# Convert data class to time series data 
Y<-ts(Health_CAN_CPI_2019_2023[,2],start=c(2019),frequency=12) 


# data parameters [,2] selecting from second column 
# Starting data 2019, frequency is collected 12 times per year  
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
  ggtitle("Seasonal Plot: change in monthly CPI in Canadian Health Care") +
  ylab("CPI")
############################################################## 
# Fit exponential smoothing model (ETS) - function determines best ETS model 
############################################################## 
fit_ets<-ets(Y) 
print(summary(fit_ets)) 
checkresiduals(fit_ets) 
# ETS A,A,A model fits best with residual SD of 0.4282 
############################################################### 
# Trying Arima method to determine best model   
fit_arima<-auto.arima(Y,d=1,D=1,stepwise = FALSE,approximation = FALSE,trace = TRUE) 
# y=data d=1 regular and D=1 seasonal difference in data to ensure data is stationary 
# Approx and Stepwise = FALSE since we only have one data series  
# Trace = TRUE will print out all the models it runs  
#  Best model: ARIMA(0,1,0)(0,1,0)[12] 
print(summary(fit_arima)) 
checkresiduals(fit_arima) 
sqrt(0.2419)  
# Residual SD 0.4918333 (this model is not the best, as it has a higher SD than ETS) 
############################################################## 
# Create a forecast with ETS model  
############################################################## 
FC<-forecast(fit_ets,h=24) #forecasting 2 years 
# Graphing plot - overview in the last 4 years
my_autoplot<-autoplot(FC)+ ggtitle("Forecast of 2 years in Canada's Health Care System") +  
  ylab(" Consumer Price Index (CPI)") +  
  xlab("Year")   
my_autoplot + theme(text = element_text(size = 10)) +  
  scale_x_continuous(breaks=seq(2019,2025,1)) 

# Zoom in closer to forecasting data - last two years "24 months" 
# and 2 year forecast 
autoplot(FC, include = 24)+  
  ggtitle("Forecast of 2 years in Canada's Health Care System") +  
  ylab(" Consumer Price Index (CPI)") + 
  xlab("Year") 
############################################################## 
