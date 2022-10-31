library(tidyverse)
library(dplyr)
library(ggplot2)
library(rddtools)
library(rugarch)
library(gridExtra)
library(forecast)
library(TSstudio)
library(tseries)
library(plotly)
library(fpp2)
library(xts)
library(lubridate)
library(readr)
library(bayesforecast)

# load the data
fiji <- read_csv("Desktop/PO-Tourism/data/ts/FJ_ts.csv")
fiji_ts <- ts(fiji$flights_number_7days, start=decimal_date(as.Date("2020-01-01")), frequency = 365.25)

# ACF and PACF
ggAcf(fiji_ts, lag.max = 20)
ggPacf(fiji_ts, lag.max = 20)

## first-order diffferencing
ggAcf(diff(fiji_ts), lag.max = 30)
ggPacf(diff(fiji_ts), lag.max = 40)

## diffferencing after cutoff
fiji_covid <- ts(fiji_ts[86:866], start=decimal_date(as.Date("2020-03-25")), frequency = 365.25)
ggAcf(diff(fiji_covid), lag.max = 40)
ggPacf(diff(fiji_covid), lag.max = 40)

### manually search 
d = 1
i = 1
temp = data.frame()
ls = matrix(rep(NA,6*64), nrow=64)

for (p in 1:8) 
{
  for(q in 1:8) 
  {
    if(p+q-1<=15)
    {
      model<- Arima(log(fiji_covid), order=c(p-1,1,q-1), include.drift=TRUE)
      ls[i,]= c(p-1,1,q-1,model$aic,model$bic,model$aicc)
      i=i+1
      print(i)
      }
  }
}
colnames(ls) <- c("p", "d", "q", "AIC", "BIC", "AICC")


### ARIMA model (5,1,3)
arima.auto <- auto.arima(fiji_covid, seasonal = T)
summary(arima.auto)
arima1.fitted <- arima.auto$fitted

plot(fiji_covid, col="blue", type = "p")
lines(arima1.fitted, col="red")
title("ARIMA(5,1,3)")

### SARMIA(2,1,4)(5,0,1)[7]
fiji_covid %>%
  diff(lag=7) %>%
  diff() %>% 
  ggtsdisplay(lag.max=50)

sarima1 <- Arima(fiji_covid, order=c(2,1,4), seasonal=c(5,0,1))
summary(sarima1)
sarima1$fitted

### VAR



## RDiT with
rdd_mod <- rdd_data(y = fiji$flights_number_7days, 
         x = fiji$week, 
         cutpoint = 11) %>% 
  rdd_reg_lm(slope = "separate", order = 2) 

rdd_mod

plot(fiji$flights_number_7days, col="green")
lines(rdd_mod$fitted.values, col="red")



## Loess regression for fitting separate 
fiji %>%
  select(flights_number_7days, Idx, week) %>%
  mutate(threshold = as.factor(ifelse(Idx >= 83, 1, 0))) %>%
  ggplot(aes(x = week, y = flights_number_7days, color = threshold)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  scale_color_brewer(palette = "Accent") +
  guides(color = FALSE) +
  geom_vline(xintercept = 11, color = "red",
             size = 1, linetype = "dashed") +
  labs(y = "Flights Number in 7 Days",
       x = "Dates")



