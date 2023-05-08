Health_Care_CAN1998_2023 <- read_excel("Desktop/MMASc./Spring_semester/Consulting/Forecasting_model_CAN_HealthCare/CPI_CAN_HEALTH_1998_2023/Health_Care_CAN1998_2023.xlsx")

D_1998_2018 <- Health_Care_CAN1998_2023[0:252,]
D_1998_2019 <- Health_Care_CAN1998_2023[0:264,]

Y_predict_2019<-ts(D_1998_2018[,2],start=c(1998),frequency=12) 
ets_2019 <- ets(Y_predict_2019)
FC_2019 <- forecast(ets_2019,h=12)

Actual_vs_Predict_2019 <- ts(D_1998_2019[193:264,]$CPI, frequency = 12, start = c(2014, 1))

autoplot(Actual_vs_Predict_2019) + autolayer(FC_2019, series = "FORECAST", PI = TRUE)
