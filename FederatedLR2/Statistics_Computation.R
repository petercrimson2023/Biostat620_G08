# Statistics Calculation

library(here)
setwd(here())
library(tidyverse)


Sys.setenv(LANGUAGE = "en")
Sys.setlocale("LC_TIME", "en_US.UTF-8")


load("./FederatedLR2/RtR_bulun.RData")
load("./FederatedLR2/RtR_xin.RData")
load("./FederatedLR2/RtR_zhang.RData")
load("./FederatedLR2/BulunTe_summary.RData")
load("./FederatedLR2/XinweiWang_summary.RData")
load("./FederatedLR2/XinyuZhang_summary.RData")
load("./FederatedLR2/beta.RData")


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

XtX = data_bulun_list$XtX+data_xinwei_list$XtX+data_zhang_list$XtX
beta_var = MSE*diag(solve(XtX))
beta_std = sqrt(beta_var)

t_stat = beta/beta_std
p_value = 2*pt(-abs(t_stat),n-p-1)

Coeff = data.frame(Estimate = beta%>% as.vector(), 
                   Std.Error = beta_std, 
                   t_value = t_stat %>% as.vector(), 
                   p_value = p_value %>% as.vector()) %>% round(.,5)

Coeff

# rownames(Coeff) = rownames(beta)


