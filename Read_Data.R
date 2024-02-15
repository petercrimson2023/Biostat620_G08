if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}
library(here)
setwd(here())

#library(ggplot2)
library(dplyr)
library(readxl)

data_bulun = read.csv("BulunTe_IphoneMonitor.csv") %>% tibble::tibble()
data_bulun %>% head()
data_bulun %>% names()
data_bulun$Date = data_bulun$Date %>% as.Date(.,format="%Y/%m/%d")
data_bulun = data_bulun %>% rename("non-academic"="non.academic")

data_xin = read_excel("xinweiw_baseline_data.xlsx")
data_xin %>% head()
data_xin %>% names()
data_xin$Date = data_xin$Date %>% as.Date(.,format="%m/%d/%Y")
data_xin = data_xin %>% rename("course.hours"="course hours")

data_zhang = read_excel("data.csv") %>% tibble::tibble() 
data_zhang %>% head()
data_zhang %>% names()
data_zhang = data_zhang %>% rename("course.hours"="course hours")
data_zhang = data_zhang %>% select( data_xin %>% names())

data_merge = rbind(data_bulun, data_xin, data_zhang)

#save(data_merge,data_bulun,data_xin,data_zhang, file = "GroupData.RData")

