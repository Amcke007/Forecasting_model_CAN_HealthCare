############################################################
# CPI - Canadian Health Care System
# Predicted 2022 vs. actual data for health care for 2022
# Created by Alicia Mckeough Feb 25th, 2023
############################################################ 
library(readxl)
Health_Care_CAN1998_2021_onward <- read_excel("Desktop/MMASc./Spring_semester/Consulting/Forecasting_model_CAN_HealthCare/CPI_CAN_HEALTH_1998_2023/Health_Care_CAN1998_2021_onward.xlsx")
View(Health_Care_CAN1998_2021_onward) 


# loading ggplot and forecasting packages 
library(ggplot2) 
install.packages("fpp2")
library(fpp2) 
# loading data  


Y_predict<-ts(Health_Care_CAN1998_2021_onward[,2],start=c(1998),frequency=12) 


###############################################################################
Y_predict<-ts(Health_Care_CAN1998_2021_onward[,2],start=c(1998),frequency=12) 
View(Health_Care_CAN1998_2021_onward)


fit_ets_2022<-ets(Y_predict) 

FC_2022<-forecast(fit_ets_2022,h=12)

Actual_ts_2022 <- ts(Health_Care_CAN1998_2021_onward$CPI, frequency = 12, start = c(1998, 1))
autoplot(Actual_ts_test)

autoplot(Actual_ts_test, include = 48) + autolayer(FC_2022, series = "FORECAST", PI = TRUE)


CPI_Actual_vs_Prediction <- ts(Health_Care_CAN1998_2023[252:299,]$CPI, frequency = 12, start = c(2019, 1))

autoplot(CPI_Actual_vs_Prediction) + autolayer(FC_2022, series = "FORECAST", PI = TRUE)

############################################################## 
