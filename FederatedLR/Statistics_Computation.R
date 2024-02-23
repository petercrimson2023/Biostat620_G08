# Statistics Calculation

library(here)
setwd(here())
library(tidyverse)


load("./FederatedLR/RtR_bulun.RData")
load("./FederatedLR/RtR_xin.RData")
load("./FederatedLR/RtR_zhang.RData")
load("./FederatedLR/BulunTe_summary.RData")
load("./FederatedLR/XinweiWang_summary.RData")
load("./FederatedLR/XinyuZhang_summary.RData")
load("./FederatedLR/beta.RData")


n = data_bulun_list$count+data_xinwei_list$count+data_zhang_list$count
p = length(beta)

RtR_bulun
RtR_xin
RtR_zhang

MSE= (RtR_bulun+RtR_xin+RtR_zhang)/(n-p-1)

Y2_SUM = data_bulun_list$YtY+data_xinwei_list$YtY+data_zhang_list$YtY
Y_mean = (data_bulun_list$Y_mean*data_bulun_list$count+
            data_xinwei_list$Y_mean*data_xinwei_list$count+
            data_zhang_list$Y_mean*data_zhang_list$count)/n
Y_mean_2 = Y_mean^2 * n

Multiple_R2 = 1 - MSE*(n-p-1)/(Y2_SUM-Y_mean_2)
Adjusted_R2 = 1 - (1-Multiple_R2)*(n-1)/(n-p-1)

t_stat = 






