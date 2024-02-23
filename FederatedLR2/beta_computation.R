# Beta calculation
library(here)
setwd(here())


load("./FederatedLR2/BulunTe_summary.RData")
load("./FederatedLR2/XinweiWang_summary.RData")
load("./FederatedLR2/XinyuZhang_summary.RData")

XtX_all = data_bulun_list$XtX + data_xinwei_list$XtX + data_zhang_list$XtX

XtY_all = data_bulun_list$XtY + data_xinwei_list$XtY + data_zhang_list$XtY

beta = solve(XtX_all) %*% XtY_all

beta

save(beta,file="./FederatedLR2/beta.RData")
