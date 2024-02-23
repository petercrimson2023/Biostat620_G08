library(here)
setwd(here())


Sys.setenv(LANGUAGE = "en")
Sys.setlocale("LC_TIME", "en_US.UTF-8")

library(tidyverse)
library(readxl)


load("./FederatedLR2/beta.RData")


#-------Data Reading---------
data_xin_temp = read_excel("./data/xinweiw_baseline_data.xlsx")



data_xin_temp %>% head()
data_xin_temp$Date = data_xin_temp$Date %>% as.Date(.,format="%m/%d/%Y")
data_xin_temp = data_xin_temp %>% mutate(
  Duration.per.use = Total.ST.min/Pickups
)
data_xin_temp$Weekday = (weekdays(data_xin_temp$Date)  %in% c( "Monday", 
                                                               "Tuesday", 
                                                               "Wednesday", 
                                                               "Thursday", 
                                                               "Friday")) %>% as.numeric()
data_xin_temp$Semester = (data_xin_temp$Date > as.Date("2024-01-09",format="%Y-%m-%d")) %>% as.numeric()
data_xin_temp$Semester.weekday = data_xin_temp$Semester * data_xin_temp$Weekday

data_xin_temp$Pickup.1st = data_xin_temp$Pickup.1st %>% 
  strptime(., format = "%Y-%m-%d %H:%M:%S", tz = "UTC") %>% 
  format(., format = "%H:%M") %>%
  replace_na(., "00:00")


#----------Data Extract---------

source("EDA_func.R")


data_xin_matrix = data_select_lag(data_xin_temp,"Xinwei Wang")

X = data_xin_matrix$x
Y = data_xin_matrix$y

res = Y - X %*% beta

res%>%mean()
res %>% sum()


#10.30014
#391.4055

RtR_xin = (t(res) %*% res) %>% as.numeric()

save(RtR_xin,file="./FederatedLR2/RtR_xin.RData")


#213730.4

