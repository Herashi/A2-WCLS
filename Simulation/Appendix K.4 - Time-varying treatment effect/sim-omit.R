# setwd("~/N100T30")
setwd("~/time-varying-sim")

library("foreach")
library("doParallel")
library("parallel")
source("init.R")
source("group.R")
if(!require(popbio)){
  install.packages("popbio")
  library(popbio)
}
load("data_250_30.RData")


## set number of Monte Carlo replicates
M <- 1000


## set number of threads to use for parallel processing and the random seed
## (nb: these two values ensure that the results are replicable)
cores <- 4
seed <- 234

cl <- makeCluster(getOption("cl.cores", cores))
clusterEvalQ(cl, source("init.R"))
registerDoParallel(cl)


sim.omit <- function() {
  out <- NULL
  ## low, medium and high degrees of moderation by state
  for (b in 0.2) {
    for (n in 250) {
      group = group_all[[as.character(n)]]
      for (tmax in 30) {
        clusterSetRNGStream(cl, seed)
        out <-sim_wc(n, tmax, M, all_data = all_data,
                     ## regress response on state and proximal treatment,
                     ## ignoring the underlying interaction between the two
                     y.formula = list(w = y ~ state + I(a - pn) + I((a - pn) * time),
                                      u = y ~ state + I(a - pn) + I((a - pn) * time) + I((a - pn) * (state - statet)) ),
                     contrast_vec = list(w = list(c(0,0,1,0), c(0,0,0,1)),
                                         u = list(c(0,0,1,0,0),c(0,0,0,1,0))),
                     y.moderator = list(w = "None", 
                                        u = "Centered State"),
                     y.names = c(w = "Weighted and centered"),
                     ## term labels for proximal treatment
                     y.label = list(w = "I(a - pn)"),
                     ## specify weights and working correlation structure
                     y.args = list(w = list(wn = "pn", wd = "prob")),
                     ## specify weight numerator model
                     a.formula = list(pn = a ~ 1),
                     a.names = c(pn = "intercept-only"),
                     ## use default generative model, but with the specified
                     ## level of moderation by the time-varying state
                     # \beta_10 + \beta_11 E(S_t)
                     # true_effect = -0.2,
                     group_ls = group,
                     beta0 = c(-0.2, 0.02, 0, b, 0))
      }
    }
  }
  out
}




omit <- sim.omit()

save(omit,file = "test_250_30_t.RData")

stopCluster(cl)