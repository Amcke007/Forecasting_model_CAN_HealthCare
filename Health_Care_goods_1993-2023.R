###############################################################################
# Created by ALicia Mckeough 
# March 12 2023

###############################################################################

library(fpp2) 

bquxjob_2016fdf7_186d2e4e025 <- read.csv("~/Desktop/MMASc./Spring_semester/Consulting/Forecasting_model_CAN_HealthCare/bquxjob_2016fdf7_186d2e4e025.csv")

# Convert data class to time series data 
Y<-ts(Health_Care_goods[,3],start=c(1993),frequency=12) 



# data parameters [,2] selecting from second column 
# Starting data 2019, frequency is collected 12 times per year  
############################################################# 
# Preform preliminary analysis   
############################################################# 
ViewChart <-autoplot(Y) + 
    ggtitle("Five Year Time Plot of CPI in Canada Health Care") +  
    ylab("Consumer Price Index (CPI)") +
    xlab("Year")

ViewChart + theme(text = element_text(size = 5)) +  
  scale_x_continuous(breaks=seq(1993,2027,1)) 

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