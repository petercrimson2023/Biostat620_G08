library(here)
setwd(here())

Sys.setenv(LANGUAGE = "en")
Sys.setlocale("LC_TIME", "en_US.UTF-8")

library(tidyverse)
library(readxl)


load("./FederatedLR2/beta.RData")


#----------------Reading data-------------------------


data_zhang_temp = read_excel("./data/data.csv") %>% tibble::tibble() 
data_zhang_temp %>% head()
data_zhang_temp %>% names()
names(data_zhang_temp) = data_zhang_temp %>% names() %>% tools::toTitleCase()
data_zhang_temp$Date = data_zhang_temp$Date %>% as.Date(.,format="%Y-%m-%d")

data_zhang_temp = data_zhang_temp %>% rename("Non-academic"="Non-Academic")

data_zhang_temp$Pickup.1st = data_zhang_temp$Pickup.1st %>% 
  strptime(., format = "%Y-%m-%d %H:%M:%S", tz = "UTC") %>% 
  format(., format = "%H:%M") %>%
  replace_na(., "00:00")

data_zhang_temp = data_zhang_temp %>% 
  mutate(Social.Time.Ratio = Social.ST.min/Total.ST.min, 
         Duration.per.use = Total.ST.min/Pickups,
         Weekday = as.numeric(weekdays(Date)  %in% c( "Monday", 
                                                      "Tuesday", 
                                                      "Wednesday", 
                                                      "Thursday", 
                                                      "Friday")),
         Semester = as.numeric(Date > as.Date("2024-01-09",format="%Y-%m-%d")),
         Semester.weekday = Semester * Weekday
  )


#-----------------------Data Extract-------------------------



source("EDA_func.R")

data_zhang_matrix = data_select_lag(data_zhang_temp,"Xinyu Zhang")

X = data_zhang_matrix$x
Y = data_zhang_matrix$y


res = Y - X %*% beta

res %>% mean()
res %>% sum()

# 16.12421
# 612.7199
#-----------------------Data Extract-------------------------

RtR_zhang = (t(res) %*% res) %>% as.numeric()

save(RtR_zhang,file="./FederatedLR2/RtR_zhang.RData")




