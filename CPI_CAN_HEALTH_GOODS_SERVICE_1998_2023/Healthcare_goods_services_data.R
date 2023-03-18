############################################################
# CPI - Canadian Health Care System
# Data collected from Stats Canada 
# Created by Alicia Mckeough March 18th, 2023
############################################################ 
# loading ggplot and forecasting packages 
Yes
library(ggplot2) 
install.packages("fpp2")
library(fpp2) 
# loading data  

library(readxl)
Health_Care_CAN_Inflation <- read_excel("Desktop/MMASc./Spring_semester/Consulting/Health_Care_CAN_Inflation.xlsx")
View(HealthGoods_Services) 

# Convert data class to time series data  


goods_data <- HealthGoods_Services[HealthGoods_Services$Products_and_product_groups == "Health care goods",]
services_data <- HealthGoods_Services[HealthGoods_Services$Products_and_product_groups == "Health care services",]

goods_ts <- ts(goods_data$VALUE, start = c(1998, 1), end = c(2023, 1), frequency = 12)
services_ts <- ts(services_data$VALUE, start = c(1998, 1), end = c(2023, 1), frequency = 12)




# data parameters [,2] selecting from second column 
# Starting data 2019, frequency is collected 12 times per year  
############################################################# 
# Preform preliminary analysis   
############################################################# 
# Health care goods difference data 

autoplot(goods_ts) + 
  ggtitle("Five Year Time Plot of CPI in Canada Health Care") +  
  ylab("Consumer Price Index (CPI)") +
  xlab("Year")
# Data showing positive trend  

DYG <-diff(goods_ts)

# Health care services difference data 

autoplot(services_ts) + 
  ggtitle("Five Year Time Plot of CPI in Canada Health Care") +  
  ylab("Consumer Price Index (CPI)") +
  xlab("Year")
# Data showing positive trend  

DYS <-diff(services_ts)

# time plot of difference data for Health care goods - removing trend 
autoplot(DYG) + 
  ggtitle("Five Year Time Plot of CPI in Canada Health Care") +  
  ylab("CPI") 

ggseasonplot(DYG) + 
  ggtitle("Seasonal Plot: change in monthly CPI in Canadian Health Care 25 Years") +
  ylab("CPI")


# time plot of difference data for Health care services - removing trend 
autoplot(DYS) + 
  ggtitle("Five Year Time Plot of CPI in Canada Health Care") +  
  ylab("CPI") 

ggseasonplot(DYS) + 
  ggtitle("Seasonal Plot: change in monthly CPI in Canadian Health Care 25 Years") +
  ylab("CPI")

############################################################## 
# Fit exponential smoothing model (ETS) - function determines best ETS model 
############################################################## 
goods_model_ets <- ets(goods_ts)
services_model_ets <- ets(services_ts)

# ETS model checking residuals for health care goods 
print(summary(goods_model_ets)) 
checkresiduals(goods_model_ets) 

# ETS M,A,N model fits best with residual SD of 0.0035 

# ETS model checking residuals for health care services  
print(summary(services_model_ets)) 
checkresiduals(services_model_ets) 

# ETS  model fits best with residual SD of 0.0023 
############################################################### 
# Trying Arima method to determine best model for health care goods
fit_arima_goods_ts<-auto.arima(goods_ts,d=1,D=1,stepwise = FALSE,approximation = FALSE,trace = TRUE) 
# y=data d=1 regular and D=1 seasonal difference in data to ensure data is stationary 
# Approx and Stepwise = FALSE since we only have one data series  
# Trace = TRUE will print out all the models it runs  
#  Best model for health care goods: ARIMA(1,1,0)(2,1,0)[12] 
print(summary(fit_arima_goods_ts)) 
checkresiduals(fit_arima_goods_ts) 
sqrt(0.17)  
# Residual SD 0.4123106 (the ETS model is not providing an accurate prediction) 
################################################################

# Trying Arima method to determine best model for health care goods
fit_arima_services_ts<-auto.arima(services_ts,d=1,D=1,stepwise = FALSE,approximation = FALSE,trace = TRUE) 
# y=data d=1 regular and D=1 seasonal difference in data to ensure data is stationary 
# Approx and Stepwise = FALSE since we only have one data series  
# Trace = TRUE will print out all the models it runs  
#  Best model for health care services: ARIMA(0,1,0)(2,1,2)[12]  
print(summary(fit_arima_services_ts)) 
checkresiduals(fit_arima_services_ts) 
sqrt(0.05008)

# Residual SD 0.2237856 (this model is not the best, as it has a higher SD than ETS) 
############################################################## 
# Create a forecast with ETS model  
############################################################## 

goods_forecast_arima<-forecast(fit_arima_goods_ts,h=48) #forecasting 4 years 
# Graphing plot - overview in the last 25 years


services_forecast<-forecast(services_model_ets,h=48) #forecasting 4 years 
# Graphing plot - overview in the last 25 years

goods_df <- data.frame(REF_DATE = time(goods_forecast_arima$mean),
                       Type = "Healthcare Goods",
                       Forecast = as.numeric(goods_forecast_arima$mean))

services_df <- data.frame(REF_DATE = time(services_forecast$mean),
                          Type = "Healthcare Services",
                          Forecast = as.numeric(services_forecast$mean))

goods_df$REF_DATE <- as.numeric(goods_df$REF_DATE)
services_df$REF_DATE <- as.numeric(services_df$REF_DATE)
forecast_df <- rbind(goods_df, services_df)



forecast_df <- rbind(goods_df, services_df)


# forecasting four years both health goods and services 

ggplot(forecast_df, aes(x = REF_DATE, y = Forecast, color = Type)) +
  geom_line() +
  labs(title = "Forecast of Healthcare Goods and Services (4 Years)",
       x = "Year",
       y = "Forecasted Value",
       color = "Type") +
  theme_minimal()


# plot Health care goods forecasting using arima model 
my_autoplot<-autoplot(goods_forecast_arima)+ ggtitle("Forecast of 4 years in Canada's Health Care Services with 25 years of Historical Data") +  
  ylab(" Consumer Price Index (CPI)") +  
  xlab("Year")   
my_autoplot + theme(text = element_text(size = 5)) +  
  scale_x_continuous(breaks=seq(1998,2027,1)) 

# Zoom in closer to forecasting Healthcare goods data - last four years "48 months" 
my_autoplot_zoom <-autoplot(goods_forecast_arima, include = 24) +  
  ggtitle("Forecast of 4 years in Canada's Health Care Goods with 25 years of Historical Data") +  
  ylab(" Consumer Price Index (CPI)") + 
  xlab("Year") 
my_autoplot_zoom + theme(text = element_text(size = 8)) 

################################################################################

# plot Health care services forecasting using ETS

my_autoplot<-autoplot(services_forecast)+ ggtitle("Forecast of 4 years in Canada's Health Care Services with 25 years of Historical Data") +  
  ylab(" Consumer Price Index (CPI)") +  
  xlab("Year")   
my_autoplot + theme(text = element_text(size = 5)) +  
  scale_x_continuous(breaks=seq(1998,2027,1)) 

# Zoom in closer to forecasting Healthcare services data - last four years "48 months" 
my_autoplot_zoom <-autoplot(services_forecast, include = 24) +  
  ggtitle("Forecast of 4 years in Canada's Health Care Services with 25 years of Historical Data") +  
  ylab(" Consumer Price Index (CPI)") + 
  xlab("Year") 
my_autoplot_zoom + theme(text = element_text(size = 8)) 


############################################################## 
