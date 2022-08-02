setwd("~/Documents/MRT_competition/Sim0_susan/lin")

library("foreach")
library("doParallel")
library("parallel")
source("init.R")
source("group.R")
load("data_all_0.2.RData")

## set number of Monte Carlo replicates
M <- 1000


## set number of threads to use for parallel processing and the random seed
## (nb: these two values ensure that the results are replicable)
cores <- 4
seed <- 0

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
                             y.formula = list(w = y ~ state + I(a - pn),
                                              u = y ~ state + I(a - pn) + I((a - pn) * (state - statet))),
                             contrast_vec = list(w = c(0,0,1),
                                                 u = c(0,0,1,0)),
                             y.moderator = list(w = "None", 
                                                u = "Centered State"),
                             y.names = c(w = "Weighted and centered"),
                             ## term labels for proximal treatment
                             y.label = list(w = "I(a - pn)"),
                             ## specify weights and working correlation structure
                             y.args = list(w = list(wn = "pn", wd = "prob")),
                             ## specify weight numerator model
                             a.formula = list(pn = a ~ 1),
                             a.names = c(pn = "Intercept-only"),
                             ## use default generative model, but with the specified
                             ## level of moderation by the time-varying state
                             # \beta_10 + \beta_11 E(S_t)
                             true_effect = -0.2,
                             group_ls = group,
                             beta0 = c(-0.2, 0, 0, b, 0))
      }
    }
  }
  out
}




omit <- sim.omit()

out = omit[["out"]]
out_u = out[seq(2,2*M,by = 2),]
out_w = out[seq(1,2*M-1,by = 2),]
df = out_w$sec - out_u$sec
summary(df)
hist(df)
mean(df/out_w$sec)


save(omit,file = "test_III.RData")

stopCluster(cl)
