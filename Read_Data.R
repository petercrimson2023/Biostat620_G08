
# Set working directory to the project root
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}
library(here)
setwd(here())

# set default language and time zone
Sys.setenv(LANGUAGE = "en")
Sys.setlocale("LC_TIME", "en_US.UTF-8")


# Load the libraries
library(tidyverse)
library(readxl)


data_xin = read_excel("xinweiw_baseline_data.xlsx")
data_xin %>% head()
data_xin$Date = data_xin$Date %>% as.Date(.,format="%m/%d/%Y")
data_xin = data_xin %>% mutate(
  Duration.per.use = Total.ST.min/Pickups
)
data_xin$Weekday = (weekdays(data_xin$Date)  %in% c( "Monday", 
                                  "Tuesday", 
                                  "Wednesday", 
                                  "Thursday", 
                                  "Friday")) %>% as.numeric()
data_xin$Semester = (data_xin$Date > as.Date("2024-01-09",format="%Y-%m-%d")) %>% as.numeric()
data_xin$Semester.weekday = data_xin$Semester * data_xin$Weekday

data_xin$Pickup.1st = data_xin$Pickup.1st %>% 
  strptime(., format = "%Y-%m-%d %H:%M:%S", tz = "UTC") %>% 
  format(., format = "%H:%M") %>%
  replace_na(., "00:00")

column_names = data_xin %>% names()

#----------------------------Bulun Te`s Data--------------------------`

data_bulun = read.csv("BulunTe_IphoneMonitor.csv") %>% tibble::tibble()
names(data_bulun) = data_bulun %>% names() %>% tools::toTitleCase()
data_bulun %>% head()
data_bulun$Date = data_bulun$Date %>% as.Date(.,format="%Y/%m/%d")

# Data needed to be filled
data_bulun$Temperature_C = rep(0, nrow(data_bulun))
data_bulun$Temperature_F = rep(0, nrow(data_bulun))
data_bulun$Snow = rep(0, nrow(data_bulun))

# Rename the columns
data_bulun = data_bulun %>% rename("Non-academic"="Non.academic")

# Mutating data
data_bulun = data_bulun %>% 
  mutate(Social.Time.Ratio = Social.ST.min/Total.ST.min, 
         Duration.per.use = Total.ST.min/Pickups,
         Stay.late = as.numeric(Pickup.1st < "3:00"),
         Weekday = as.numeric(weekdays(Date)  %in% c( "Monday", 
                                                     "Tuesday", 
                                                     "Wednesday", 
                                                     "Thursday", 
                                                     "Friday")),
         Semester = as.numeric(Date > as.Date("2024-01-09",format="%Y-%m-%d")),
         Semester.weekday = Semester * Weekday
  )


# Check the difference between the column names
setdiff(column_names,names(data_bulun))
setdiff(names(data_bulun),column_names)


#-----------------------------Zhang`s Data--------------------------`

#data_xin = data_xin %>% rename("course.hours"="course hours")

data_zhang = read_excel("data.csv") %>% tibble::tibble() 
data_zhang %>% head()
data_zhang %>% names()
names(data_zhang) = data_zhang %>% names() %>% tools::toTitleCase()
data_zhang$Date = data_zhang$Date %>% as.Date(.,format="%Y-%m-%d")

data_zhang = data_zhang %>% rename("Course.hours"="Course Hours",
                                   "Non-academic"="Non-Academic")

data_zhang$Pickup.1st = data_zhang$Pickup.1st %>% 
  strptime(., format = "%Y-%m-%d %H:%M:%S", tz = "UTC") %>% 
  format(., format = "%H:%M") %>%
  replace_na(., "00:00")

data_zhang = data_zhang %>% 
  mutate(Social.Time.Ratio = Social.ST.min/Total.ST.min, 
         Duration.per.use = Total.ST.min/Pickups,
         Stay.late = as.numeric(Pickup.1st < "3:00"),
         Weekday = as.numeric(weekdays(Date)  %in% c( "Monday", 
                                                     "Tuesday", 
                                                     "Wednesday", 
                                                     "Thursday", 
                                                     "Friday")),
         Semester = as.numeric(Date > as.Date("2024-01-09",format="%Y-%m-%d")),
         Semester.weekday = Semester * Weekday,
         Temperature_C = rep(0, nrow(data_zhang)),
         Temperature_F = rep(0, nrow(data_zhang)),
         Snow = rep(0, nrow(data_zhang))
  ) %>% select(column_names) 

# Check the difference between the column names
setdiff(column_names,names(data_zhang))
setdiff(names(data_zhang),column_names)

#---------------Save as RData for next load------------------

data_merge = rbind(data_xin, data_bulun, data_zhang)

save(data_xin,data_bulun,
     data_zhang,data_merge,
     file = "GroupData.RData")



