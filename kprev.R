library(readxl)
library(tidyverse)
library(lubridate)
library(fable)
library(forecast)
library(fpp3)
kprev<-read_excel("KPRA.xlsx")
kprev<-as.data.frame(kprev)
glimpse(kprev)
kprev$year <- as.Date(as.character(kprev$Year), format = "%Y")
#kprev$yr<-year(kprev$year)
bank<-kprev %>% filter(Sector=="Banking")
naive(bank$value)
ets(bank$value)
ETS(bank$value)
