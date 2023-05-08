############################################################
# CPI - Canadian Health Care System
# Predicted 2018 vs. actual data for health care for 2018
# Created by Alicia Mckeough Feb 25th, 2023
############################################################ 
library(readxl)
Health_Care_CAN1998_2017_onward <- read_excel("Desktop/MMASc./Spring_semester/Consulting/Forecasting_model_CAN_HealthCare/CPI_CAN_HEALTH_1998_2023/Health_Care_CAN1998_2017_onward.xlsx")
View(Health_Care_CAN1998_2017_onward) 

Health_Care_CAN1998_2018_onward <- read_excel("Desktop/MMASc./Spring_semester/Consulting/Forecasting_model_CAN_HealthCare/CPI_CAN_HEALTH_1998_2023/Health_Care_CAN1998_2018_onward.xlsx")
View(Health_Care_CAN1998_2018_onward) 


# loading ggplot and forecasting packages 
library(ggplot2) 
install.packages("fpp2")
library(fpp2) 
# loading data  

###############################################################################

Y_predict_2018<-ts(Health_Care_CAN1998_2017_onward[,2],start=c(1998),frequency=12) 

Actual_ts_2018 <- ts(Health_Care_CAN1998_2018_onward$CPI, frequency = 12, start = c(1998, 1))
autoplot(Actual_ts_2018)

Actual_ts_2018

fit_ets_2018 <- ets(Y_predict_2018) 

FC_2018 <- forecast(fit_ets_2018,h=12)

autoplot(Actual_ts_2018) + autolayer(FC_2018, series = "FORECAST", PI = TRUE)

CPI_Actual_vs_Prediction <- ts(Health_Care_CAN1998_2018_onward[168:251,]$CPI, frequency = 12, start = c(2012, 1))

autoplot(CPI_Actual_vs_Prediction) + autolayer(FC_2018, series = "FORECAST", PI = FALSE)

############################################################## ############################################################## 
