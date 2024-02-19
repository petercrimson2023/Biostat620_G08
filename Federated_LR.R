if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}
library(here)
setwd(here())

Sys.setenv(LANGUAGE = "en")
Sys.setlocale("LC_TIME", "en_US.UTF-8")
load("GroupData.RData")

data_select = function(data,user_name)
{
  data_select = data %>% select(Social.ST.min,
                                Stay.late,
                                Duration.per.use,
                                Temperature_F,
                                Snow,
                                Weekday,
                                Semester,
                                Steps) %>% mutate(
                                                  Steps = (Steps-mean(Steps))/100,
                                                  Step_Week = Steps*Weekday,
                                                  Temperature_F = (Temperature_F-mean(Temperature_F)),
                                                  Temperature_F_Snow = Temperature_F*Snow,
                                                  Intercept = rep(1,nrow(data)))
  
  
  y = data_select["Social.ST.min"] %>% as.matrix()
  x= data_select %>% select(-Social.ST.min) %>% as.matrix()
  return(list(x=x,y=y))
}


xin_select = data_select(data_xin,"Xinwei Wang")
bulun_select = data_select(data_bulun,"Bulun Te")
zhang_select = data_select(data_zhang,"Xinyu Zhang")

distributed_statistics = list(XtX = t(xin_select$x) %*% xin_select$x+
                                     t(bulun_select$x) %*% bulun_select$x+
                                     t(zhang_select$x) %*% zhang_select$x,
                             XtY = t(xin_select$x) %*% xin_select$y+
                                      t(bulun_select$x) %*% bulun_select$y+
                                      t(zhang_select$x) %*% zhang_select$y,
                             YtY =  t(xin_select$y) %*% xin_select$y+
                                      t(bulun_select$y) %*% bulun_select$y+
                                      t(zhang_select$y) %*% zhang_select$y)

beta_dist = (distributed_statistics$XtX %>% solve()) %*% distributed_statistics$XtY

beta_dist
