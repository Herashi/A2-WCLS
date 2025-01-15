setwd("~/Documents")
# loading packages
source("init.R")


## set number of Monte Carlo replicates
M <- 1000


## set number of threads to use for parallel processing and the random seed
## (nb: these two values ensure that the results are replicable)
cores <- 4
seed <- 234

cl <- makeCluster(getOption("cl.cores", cores))
clusterEvalQ(cl, source("init.R"))
registerDoParallel(cl)


## the main simulation functions
sim.omit <- function() {
  out <- NULL
  ## low, medium and high degrees of moderation by state
  for (b in c(0.2, 0.5, 0.8)) {
    for (n in 250) {
      for (tmax in 30) {
        clusterSetRNGStream(cl, seed)
        out <-sim_wc(n, tmax, M, all_data = all_data,
                             ## regress response on state and proximal treatment,
                             ## ignoring the underlying interaction between the two
                             y.formula = list(w = y ~ state + I(a - pn),
                                              u = y ~ state + I(a - pn) + I((a - pn) * (state - statet)) ),
                             # to extract the coefficients of interest from the fitted model above
                             contrast_vec = list(w = c(0,0,1),
                                                 u = c(0,0,1,0)),
                             # the non-adjusted method versus the A2-WCLS auxiliary variable adjusted approach
                             y.moderator = list(w = "None", 
                                                u = "A2-WCLS"),
                             y.names = c(w = "Causal Excursion Effect"),
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
                             true_effect = -0.2,
                             # the cluster structure
                             group_ls = NULL,
                             # data generating parameter specification
                             # varying b produces different level of moderation effect (small, medium, strong)
                             beta0 = c(-0.2, 0, 0, b, 0))
      }
    }
  }
  out
}


out <- sim.omit()
save(out,file = "test.RData")

stopCluster(cl)
