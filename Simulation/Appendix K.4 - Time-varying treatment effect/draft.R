
setwd("~/N100T30")

library("foreach")
library("doParallel")
library("parallel")
source("init.R")
source("group.R")
if(!require(popbio)){
  install.packages("popbio")
  library(popbio)
}

## set number of Monte Carlo replicates
M <- 1000


## set number of threads to use for parallel processing and the random seed
## (nb: these two values ensure that the results are replicable)
cores <- 4
seed <- 234

cl <- makeCluster(getOption("cl.cores", cores))
clusterEvalQ(cl, source("init.R"))
registerDoParallel(cl)


M = 1000

# create an unique test set 1000 replicates
# load("~/Documents/MRT_competition/Sim0_susan/data_all_p/base_draft.RData")


n = 250
tmax = 30
b = 0.2
group = group_all[[as.character(n)]]
group_ls = group
control <- rsnmm.control(beta0 = c(-0.2, 0.02, 0, b, 0))

all_data <- foreach(m = 1:M) %dopar% {
  d <- rsnmm.R(n, tmax,group_ls, control = control)
  d$pn <- d$pd <- d$prob
  
  
  statec = aggregate(state~time,data = d,FUN = mean)$state
  d$statec = rep(statec,times=n)
  d$statet = 0
  
  save(d, file = "d_data.RData")
  d
}

# before: the causal effect contains -0.2 + N(0,1) noise * 0.2 + 0.2 * Ber[-1,1] prob time-varying & depend on previous action
# after: the causal effect contains -0.2 + 0.01 *(j-1) + 0.2 * uniform range time-varying, mean depends on time & previous action


save(all_data,file = "data_250_30.RData")

stopCluster(cl)





