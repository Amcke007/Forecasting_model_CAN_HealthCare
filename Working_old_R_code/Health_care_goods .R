
library(tidyverse)
cpi_data <- read_csv("https://www150.statcan.gc.ca/n1/en/tbl/csv/18100005-eng.zip")

health_care_data <- HealthCare_data %>% 
  filter(str_detect("North American Industry Classification System (NAICS)", "Health"))

health_care_annual <- health_care_data %>%
  mutate(date = ymd("REF_DATE")) %>% 
  group_by(`North American Industry Classification System (NAICS)`, year) %>% 
  summarize(avg_cpi = mean("VALUE"))

library(lubridate)

health_care_annual <- health_care_data %>%
  mutate(date = ymd(REF_DATE)) %>% 
  group_by(`North American Industry Classification System (NAICS)`, year(date)) %>% 
  summarize(avg_cpi = mean(VALUE))

library(dplyr)
library(lubridate)

health_care_data %>% 
  mutate(date = ymd(REF_DATE))


library(lubridate)

library(dplyr)
install.packages("zoo")
library(zoo)
