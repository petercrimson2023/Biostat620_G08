library(here)
setwd(here())

library(tidyverse)



#-----Data Read-------

data_bulun_temp = read.csv("./data/BulunTe_IphoneMonitor.csv") %>% tibble::tibble()


#-----Data preprocessing -------
names(data_bulun_temp) = data_bulun_temp %>% names() %>% tools::toTitleCase()
data_bulun_temp %>% head()
data_bulun_temp$Date = data_bulun_temp$Date %>% as.Date(.,format="%Y/%m/%d")


# Rename the columns
data_bulun_temp = data_bulun_temp %>% rename("Non-academic"="Non.academic")

# Mutating data
data_bulun_temp = data_bulun_temp %>% 
  mutate(Social.Time.Ratio = Social.ST.min/Total.ST.min, 
         Duration.per.use = Total.ST.min/Pickups,
         Stay.late = as.numeric(Pickup.1st < "03:00"),
         Weekday = as.numeric(weekdays(Date)  %in% c( "Monday", 
                                                      "Tuesday", 
                                                      "Wednesday", 
                                                      "Thursday", 
                                                      "Friday")),
         Semester = as.numeric(Date > as.Date("2024-01-09",format="%Y-%m-%d")),
         Semester.weekday = Semester * Weekday
  )

data_bulun_temp %>% head()

#--------Data Extract---------


source("EDA_func.R")

data_bulun_matrix = data_select(data_bulun_temp,"Bulun Te")

X = data_bulun_matrix$x
Y = data_bulun_matrix$y


#---------Summary Statistics----------

XtX = (t(X)%*%X)
XtY = (t(X)%*%Y)
Y_mean = mean(Y)
count = length(Y)
YtY = (t(Y)%*%Y) %>% as.numeric()

data_bulun_list = list(XtX=XtX,
                       XtY=XtY,
                       Y_mean=Y_mean,
                       count=count,
                       YtY=YtY)

save(data_bulun_list,file="./FederatedLR/BulunTe_summary.RData")






