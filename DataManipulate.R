if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}
library(here)
setwd(here())
library(dplyr)
load("GroupData.RData")

# Adding proportion and duration-per-use columns
# Adding weekdays and vacation indicator

data_bulun = data_bulun %>% 
  mutate(proportion = Social.ST.min/Total.ST.min, 
         duration_per_use = Total.ST.min/Pickups)
data_bulun$weekday = (weekdays(data_bulun$Date)  %in% c( "Monday", 
                                                   "Tuesday", 
                                                   "Wednesday", 
                                                   "Thursday", 
                                                   "Friday")) %>% as.numeric()
data_bulun$semester = (data_bulun$Date > as.Date("2024-01-09",format="%Y-%m-%d")) %>% as.numeric()
data_bulun$semester_weekday = data_bulun$semester * data_bulun$weekday


data_xin = data_xin %>% 
  mutate(proportion = Social.ST.min/Total.ST.min, 
         duration_per_use = Total.ST.min/Pickups)
data_xin$weekday = (weekdays(data_xin$Date)  %in% c( "Monday", 
                                                   "Tuesday", 
                                                   "Wednesday", 
                                                   "Thursday", 
                                                   "Friday")) %>% as.numeric()
data_xin$semester = (data_xin$Date > as.Date("2024-01-09",format="%Y-%m-%d")) %>% as.numeric()
data_xin$semester_weekday = data_xin$semester * data_xin$weekday


data_zhang = data_zhang %>% 
  mutate(proportion = Social.ST.min/Total.ST.min, 
         duration_per_use = Total.ST.min/Pickups)
data_zhang$weekday = (weekdays(data_zhang$Date)  %in% c( "Monday", 
                                                   "Tuesday", 
                                                   "Wednesday", 
                                                   "Thursday", 
                                                   "Friday")) %>% as.numeric()
data_zhang$semester = (data_zhang$Date > as.Date("2024-01-09",format="%Y-%m-%d")) %>% as.numeric()
data_zhang$semester_weekday = data_zhang$semester * data_zhang$weekday

#Resaving the merged data

data_merge = rbind(data_bulun, data_xin, data_zhang)

save(data_merge,data_bulun,data_xin,data_zhang, file = "GroupData.RData")

