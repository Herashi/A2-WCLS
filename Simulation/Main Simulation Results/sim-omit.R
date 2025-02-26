setwd("~/Documents")
sink("outputfile.txt")
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
start_time <- Sys.time()  # Record start time

## the main simulation functions
sim.omit <- function() {
  out <- NULL
  ## low, medium and high degrees of moderation by state
  for (b in 0) {
    for (n in 250) {
      for (tmax in 30) {
        clusterSetRNGStream(cl, seed)
        out <-sim_wc(n, tmax, M, 
                     ## regress response on state and proximal treatment,
                     ## ignoring the underlying interaction between the two
                     y.formula = list(w = y ~ I(lag1a - lag1pn),
                                      u = y ~ I(a - prob) + I((a - prob) * state.center) + I(lag1a - lag1pn) + I((lag1a - lag1pn) * (lag1state - state_mod))),
                     # to extract the coefficients of interest from the fitted model above
                     contrast_vec = list(w = c(0,1),
                                         u = c(0,0,0,1,0)),
                     # the non-adjusted method versus the A2-WCLS auxiliary variable adjusted approach
                     y.moderator = list(w = "None", 
                                        u = "lag1state"),
                     y.names = c(w = "Causal Excursion Effect"),
                     ## term labels for proximal treatment
                     y.label = list(w = "I(lag1a - lag1pn)"),
                     ## specify weights and working correlation structure
                     y.args = list(w = list(wn = "pn", wd = "prob")),
                     ## specify weight numerator model
                     a.formula = list(pn = lag1a ~ 1),
                     a.names = c(pn = "intercept-only"),
                     ## use default generative model, but with the specified
                     lag = 1,
                     ## level of moderation by the time-varying state
                     # \beta_10 + \beta_11 E(S_t)
                     true_effect = -0.1,
                     # data generating parameter specification
                     # varying b produces different level of moderation effect (small, medium, strong)
                     beta1 = c(-0.1,0,0,b))
      }
    }
  }
  out
}
## the main simulation functions


out <- sim.omit()
save(out,file = "test_mega0.RData")

stopCluster(cl)

end_time <- Sys.time()  # Record end time

# Print the elapsed time
print(end_time - start_time)
sink()