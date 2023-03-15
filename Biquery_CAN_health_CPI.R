################################################################################
# Health Care CPI Data using the following variables:

# Health care goods
# Medicinal and pharmaceutical products
# Prescribed medicines (excluding medicinal cannabis)
# Non-prescribed medicines

# Created by Alicia Mckeough March 11, 2023
# Source Stats Canada
# Using Google bigquery
################################################################################

# Install Google bigquery to obtained SQL results 
install.packages("bigrquery")
library(bigrquery)

install.packages('devtools')

install.packages(c("DBI", "RSQLite"))

library(dplyr)

# connecting R to Bigquery 
ConnectBQ <- dbConnect(
  bigquery(),
  project = 'mmasc-inflation-can-health',
  dataset = 'CPI_all_goods_CAN_1993_2023'
)
View(ConnectBQ)

dbListTables(ConnectBQ)

SQL1<- paste("SELECT VALUE, REF_DATE, GEO, Products_and_product_groups
            FROM `mmasc-inflation-can-health.CPI_all_goods_CAN_1993_2023.CPI_CANADA_1993_2023` 
            WHERE Products_and_product_groups='Health care goods' OR Products_and_product_groups='Medicinal and pharmaceutical products' 
            OR Products_and_product_groups='Prescribed medicines (excluding medicinal cannabis)' 
            OR Products_and_product_groups='Non-prescribed medicines' AND GEO='Canada'")

# Now the data is here: https://bigquery.cloud.google.com

res <- query_exec(SQL1, project = "mmasc-inflation-can-health", use_legacy_sql = FALSE)

res

# Select rows with year = 2020 and columns 1, 3, and 5
mydata <- res[res$REF_DATE %in% 2020:2023, c(1, 2, 3, 4)]

mydata

View(mydata)

print(mydata)




