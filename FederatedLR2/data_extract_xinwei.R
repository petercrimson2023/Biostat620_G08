library(here)
setwd(here())

library(tidyverse)
library(readxl)

Sys.setenv(LANGUAGE = "en")
Sys.setlocale("LC_TIME", "en_US.UTF-8")



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


#---------Summary Statistics----------

XtX = (t(X)%*%X)
XtY = (t(X)%*%Y)
Y_mean = mean(Y)
count = length(Y)
YtY = (t(Y)%*%Y) %>% as.numeric()


data_xinwei_list = list(XtX=XtX,
                       XtY=XtY,
                       Y_mean=Y_mean,
                       count=count,
                       YtY=YtY)

save(data_xinwei_list,file="./FederatedLR2/XinweiWang_summary.RData")







