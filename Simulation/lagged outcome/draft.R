setwd("~/Documents/GitHub/A2-WCLS/Simulation/lagged outcome")
# loading packages
source("init.R")


## set number of Monte Carlo replicates
M <- 1000
tmax = 30
n = 250
y.formula = list(w = y ~ I(lag1a - pn),
                 u = y ~ I(state - state_int) + I(lag1a - pn) + I((lag1a - pn) * (state - state_mod)))

contrast_vec = list(w = c(0,1),u = c(0,0,1,0))
y.moderator = list(w = "None", u = "state")
y.names = c(w = "Causal Excursion Effect")
y.label = list(w = "I(lag1a - pn)")
y.args = list(w = list(wn = "pn", wd = "prob"))
a.formula = list(pn = a ~ 1)
a.names = c(pn = "intercept-only")
true_effect = -0.1
group_ls = NULL
lag = 1
control <- rsnmm.control()
runin.fita <- control$lag
runin.fity <- control$lag + lag
y.coef <- mapply(which.terms, x = y.formula, label = y.label,
                 stripnames = TRUE, SIMPLIFY = FALSE)

if (!is.null(a.formula)) {
  y.prob <- lapply(y.args, function(x) do.call("c", x[c("wn", "wd")]))
  y.prob <- lapply(y.prob, function(x) x[x %in% names(a.formula)])
}else{
  y.prob <- lapply(y.formula, function(x) list())
} 


formula = y.formula[["u"]]
args = y.args
prob = y.prob
coef = y.coef
label = y.label 
c_vec= contrast_vec[["u"]]
moderator = y.moderator[["u"]]
response = "y"
addvar = NULL

d <- rsnmm.R(n, tmax, control = control)
d$pn <- d$pd <- d$prob
state_int = d$a * sum(w*(l$x[,3])^2*l$x[,2])/sum(w*(l$x[,3])^2)
state_mod = sum(w*(l$x[,3])^2*l$x[,2])/sum(w*(l$x[,3])^2)


### set it to be? 
b = 0.5


beta0 = c(-0.2, 0, 0, b, 0)
beta1 = c(-0.1,0,0,0)
eta = c(0, 0, 0.8, -0.8, 0)
mu = rep(0, 3)
theta0 = c(0, 0.8)
theta1 = c(0, 0)
coef.avail = c(100, rep(0, 3))
coef.state = rep(0, 5)







