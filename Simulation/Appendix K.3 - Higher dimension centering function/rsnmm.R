library(purrr)
library("zoo")
library(foreach)
source("xzoo.R")
## load functions needed for variance estimation
source("xgeepack_s.R")
library(Matrix)
library(MASS)
library(geepack)



#define expit(a)
expit = function(a){
  return(exp(a) / (1 + exp(a)))
}

group_str = function(group){
  group[["group size"]] = unname(table(group[["group_id"]]))
  group[["#groups"]] = length(unique(group[["group_id"]]))
  X = diag(rep(1,group[["#groups"]]))
  X = X[rep(seq_len(nrow(X)),group[["group size"]]),]
  colnames(X) = paste(rep("Group", group[["#groups"]]),seq(1,group[["#groups"]],1), sep = "_")
  group[["indicator matrix"]] = X
  
  
  err = c()
  for (i in 1:group[["#groups"]]){
    e = rnorm(1,mean = 0,sd = sqrt(group[["baseline sigma2"]][i]))
    err = c(err,e)
  }
  group[["err"]] = err
  group[["group err"]] = rep(group[["err"]],group[["group size"]])
  
  bg = c()
  for (i in 1:group[["#groups"]]){
    e = rnorm(1,mean = 0,sd = sqrt(group[["bg sigma2"]][i]))
    bg = c(bg,e)
  }
  group[["bg"]] = bg
  group[["random bg"]] = rep(group[["bg"]],group[["group size"]])
  
  
  return(group)
} 



rsnmm = function(n, T,
                 ty, tmod, tavail, tstate,
                 beta, eta, mu, theta,
                 coefavail, coefstate, coeferr,
                 avail, base, state, a, prob,
                 y, err, q, statec, ac, availc, 
                 group_err, bg){
  # a list indicating grouping structure
  
  for (i in 0:(n-1)) {
    for (j in 2:T) {
      # probability of availabilty 
      r = expit(coefavail[1]
                + coefavail[2] * tavail[j]
                + coefavail[3] * a[i*T + j-1]
                + coefavail[4] * y[i*T + j-1])
      # availability - uncentered and centered 
      avail[i*T + j] = as.numeric(rbernoulli(1,r))
      availc[i*T + j] = avail[i*T + j] - r
      
      # linearly change with time
      # q = 0.1 + 0.026 *(j-1)
      # time varying state, postively depending on the previous state
      # 
      # q[i*T + j] = q[i*T + j]+ coefstate[2] * q[i*T + j - 1]
      # 
      # #GP
      # state[i*T + j] =  coefstate[1] *(j-1) +  q[i*T + j]
      # statec[i*T + j] = state[i*T + j] - coefstate[1] *(j-1)

      
      # probability that binary state is +1
      q_state = expit(coefstate[1]*(j-1)
                + coefstate[2] * tstate[j]
                + coefstate[3] * base[i*T + j-1 ]
                + coefstate[4] * state[i*T + j-1]
                + coefstate[5] * a[i*T + j-1])

      # binary state on {-1, 1} - uncentered and centered
      state[i*T + j] = ifelse(as.numeric(rbernoulli(1,q_state)) < 1 ,-1 ,1)
      statec[i*T + j] = state[i*T + j] - (q_state - (1 - q_state))
      
      # treatment probability 
      prob[i*T + j] = avail[i*T + j] * expit(eta[1]
                                             + eta[2] * base[i*T + j]
                                             + eta[3] * state[i*T + j]
                                             + eta[4] * a[i*T + j - 1]
                                             + eta[5] * y[i*T + j - 1])
      
      # treatment indicator - uncentered and centered 
      a[i*T + j] = as.numeric(rbernoulli(1, prob[i*T + j]))
      ac[i*T + j] = a[i*T + j] - prob[i*T + j]
      # conditional mean response 
      ym = mu[1]+ 
        mu[2] * ty[j]+  # pre-evaluated time function 
        mu[3] * base[i*T + j]+
        ac[i*T + j] * (beta[1]+ 
                         bg[i+1] + 
                         beta[2] * q[i*T + j] + # pre-evaluated time function
                         beta[3] * base[i*T + j]+
                         beta[4] * statec[i*T + j]+
                         beta[5] * a[i*T + j - 1])+
        ac[i*T + j - 1] * (beta[6]+
                             beta[7] * tmod[j - 1]+
                             beta[8] * base[i*T + j - 1]+
                             beta[9] * state[i*T + j - 1])+
        theta[1] * availc[i*T + j]+
        theta[2] * state[i*T + j]+
        theta[3] * availc[i*T + j - 1]+
        theta[4] * statec[i*T + j - 1]
      # error 
      err[i*T + j] = err[i*T + j]+ coeferr * err[i*T + j - 1] 
      # response 
      y[i*T + j] = ym + err[i*T + j]+ group_err[i+1]
    }
  }
  
  d = data.frame(ty = ty, tmod = tmod, tavail = tavail, tstate = tstate,
                 base = base, state = state, q = q, a = a, y = y, err = err,
                 avail = avail, p = prob, a.center = ac,
                 state.center = statec, avail.center = availc)
  return(d)
}



rsnmm.control <- function(origin = 1, sd = 1,
                          coralpha = sqrt(0.5),
                          corstr = c("ar1", "exchangeable"),
                          beta0 = c(-0.2, 0.2, 0, 0.2, 0), beta1 = rep(0, 4),
                          eta = c(0, 0, 0.8, -0.8, 0), mu = rep(0, 3),
                          theta0 = c(0, 0.8), theta1 = c(0, 0),
                          coef.avail = c(100, rep(0, 3)), coef.state = c(0.05,0,0,0,0.1) ,
                          tfun = NULL, lag = 3 + any(beta1 != 0)) {
  corstr <- match.arg(corstr)
  if (is.null(tfun))
    tfun <- rep(list(function(tcur, tmax) rep(0, length(tcur))), 4)
  list(origin = 1, lag = lag,
       ## error SD, correlation
       sd = sd, coralpha = coralpha, corstr = corstr,
       ## proximal effect coefficients
       beta0 = setNames(beta0, c("one", "tmod", "base", "state", "lag1a")),
       ## delayed effect coefficients
       beta1 = setNames(beta1, c("one", "lag1tmod", "base", "lag1state")),
       ## treatment probability model coefficients
       eta = setNames(eta, c("one", "base", "state", "lag1a", "lag1y")),
       ## exogenous or time-invariant main effects
       mu = setNames(mu, c("one", "ty", "base")),
       ## time-varying main effects, centered and proximal
       theta0 = setNames(theta0, c("avail", "state")),
       ## time-varying main effects, centered and delayed
       theta1 = setNames(theta1, c("lag1avail", "lag1state")),
       ## availability model coefficients
       coef.avail = setNames(coef.avail, c("one", "tavail", "lag1a", "lag1y")),
       ## binary state model coefficients
       coef.state = setNames(coef.state,
                             c("one", "tstate", "base", "lag1state", "lag1a")),
       ## functions of time in the main effect, proximal effect,
       ## availability model, and binary state model
       tfun = setNames(tfun, c("ty", "tmod", "tavail", "tstate")))
}



rsnmm.R <- function(n, tmax, group_ls, control, ...) {
  control <- if (missing(control)) rsnmm.control(...)
  else do.call("rsnmm.control", control)
  tmax <- tmax + (tmax %% 2) + 1
  time <- rep(0:(tmax - 1), n)
  tfun <- do.call("data.frame", lapply(control$tfun, function(f) f(time, tmax)))
  
  coef.err <- 0
  control$cormatrix <- matrix(control$coralpha, tmax, tmax)
  diag(control$cormatrix) <- 1
  if (control$corstr == "exchangeable") {
    err <- sqrt(control$coralpha) * rep(rnorm(n, sd = control$sd), each = tmax)
    err <- err + rnorm(n * tmax, sd = sqrt(with(control, sd^2 * (1 - coralpha))))
  }else {
    ## provisional error
    err <- ifelse(time == 0, rnorm(n, sd = control$sd),
                  rnorm(n * (tmax - 1),
                        sd = sqrt(with(control, sd^2 * (1 - coralpha^2)))))
    err[time == 0] <- rnorm(n, sd = control$sd)
    coef.err <- control$coralpha
    control$cormatrix <- matrix(with(control,
                                     coralpha^(abs(row(cormatrix) -
                                                     col(cormatrix)))), tmax, tmax)
  }
  
  
  #### provisional state probability GP
  q_sd = 1
  corq = 0
  q <- ifelse(time == 0, rnorm(n, sd = 1),
              rnorm(n * (tmax - 1),
                    sd = sqrt(q_sd^2 * (1 - corq^2))))
  q[time == 0] <- rnorm(n, sd = q_sd)
  #######
  group = group_str(group_ls)
  group_err = group[["group err"]]
  bg = group[["random bg"]]
  
  d <- rsnmm(
    n = as.integer(n) ,
    T = as.integer(tmax),
    ty = as.double(tfun$ty),
    tmod = as.double(tfun$tmod),
    tavail = as.double(tfun$tavail),
    tstate = as.double(tfun$tstate),
    beta = with(control, as.double(c(beta0, beta1))),
    eta = as.double(control$eta),
    mu = as.double(control$mu),
    theta = with(control, as.double(c(theta0, theta1))),
    coefavail = as.double(control$coef.avail),
    coefstate = as.double(control$coef.state),
    coeferr = as.double(coef.err),
    avail = as.integer(rep(0, n * tmax)),
    base = as.double(rep(rnorm(n), each = tmax)),
    state = as.integer(rep(0, n * tmax)),
    a = as.integer(rep(0, n * tmax)),
    prob = as.double(rep(0, n * tmax)),
    y = as.double(rep(0, n * tmax)),
    err = as.double(err),
    q = as.double(q),
    statec = as.double(rep(0, n*tmax)),
    ac = as.double(rep(0, n*tmax)),
    availc = as.double(rep(0, n*tmax)),
    group_err =as.double(group_err),
    bg = as.double(bg))
  
  d <- data.frame(id = rep(1:n, each = tmax), time = time,
                  ty = d$ty, tmod = d$tmod, tavail = d$tavail, tstate = d$tstate,
                  base = d$base, state = d$state, q = d$q, a = d$a, y = d$y, err = d$err,
                  group_err = rep(group_err, each = tmax), bg = rep(bg, each = tmax),
                  avail = d$avail, prob = d$p, a.center = d$a.center, state.center = d$state.center, 
                  avail.center = d$avail.center, one = 1)
  
  ## nb: for a given row, y is the proximal response
  d$lag1y <- with(d, delay(id, time, y))
  d$lag2y <- with(d, delay(id, time, y, 2))
  d$lag1err <- with(d, delay(id, time, err))
  d$lag1avail <- with(d, delay(id, time, avail))
  d$lag1avail.center <- with(d, delay(id, time, avail.center))
  d$lag2avail <- with(d, delay(id, time, avail, 2))
  d$lag2avail.center <- with(d, delay(id, time, avail.center, 2))
  d$lag1a <- with(d, delay(id, time, a))
  d$lag2a <- with(d, delay(id, time, a, 2))
  d$lag1prob <- with(d, delay(id, time, prob))
  d$lag2prob <- with(d, delay(id, time, prob, 2))
  d$lag1a.center <- with(d, delay(id, time, a.center))
  d$lag2a.center <- with(d, delay(id, time, a.center, 2))
  d$lag1tmod <- with(d, delay(id, time, tmod))
  d$lag2tmod <- with(d, delay(id, time, tmod, 2))
  d$lag1state <- with(d, delay(id, time, state))
  d$lag1state.center <- with(d, delay(id, time, state.center))
  rownames(d) <- NULL
  attributes(d) <- c(attributes(d), control)
  
  return(d)
}

sim_wc <- function(n = 100, tmax = 30, M = 1000,all_data,
                   ## response regression models
                   y.formula = list(w = y ~ state + I(a - pn),
                                    u = y ~ I(state-statec) * I(a - pn)),
                   contrast_vec = list(w = c(0,0,1),
                                       u = c(0,0,1,0)),
                   y.moderator = list(w = "None", 
                                      u = "Centered State"),
                   ## names for each regression model
                   y.names = c(w = "Weighted and centered"),
                   ## labels for regression terms of the treatment effect
                   y.label = list(w = "I(a - pn)"),
                   ## names of the treatment probability models or variables used
                   ## for the weight numerator ('wn') or denominator ('wd') and
                   ## arguments for the estimation routine
                   y.args = list(w = list(wn = "pn", wd = "pd")),
                   ## treatment probability models named in 'y.args'
                   a.formula = list(pn = a ~ lag1a,
                                    pd = a ~ lag1a + state),
                   ## names for each treatment probability model
                   a.names = c(pn = "Last treatment",
                               pd = "Last treatment and current state"),
                   ## proximal (0) or delayed (1) treatment effect?
                   lag = 0,
                   # \beta_10 + \beta_11 E(S_t)
                   true_effect = -0.2 ,
                   ## print generative and analysis model details
                   verbose = TRUE,
                   ## group structure
                   group_ls, 
                   ## control parameters for 'rsnmm.R'
                   control, ...) {
  control <- if (missing(control)) rsnmm.control(...)
  else control <- do.call("rsnmm.control", control)
  ## times to use in the model fit
  runin.fita <- control$lag
  runin.fity <- control$lag + lag
  ## retrieve causal control parameter values
  ## nb: if the regression models 'y.formula' average over an underlying
  ##     moderator these will not represent the true causal effect unless this
  ##     moderator has conditional mean zero
  y.coef <- mapply(which.terms, x = y.formula, label = y.label,
                   stripnames = TRUE, SIMPLIFY = FALSE)
  # truth <- control[[paste0("beta", lag)]]
  # truth <- truth[Reduce("intersect", lapply(y.coef, names))]
  # y.coef <- lapply(y.coef, function(x) x[names(truth)])
  
  
  ## corresponding treatment probability models
  ## nb: we avoid delayed evaluation in 'y.args' (e.g. passing a 'weights'
  ##     argument directly) to avoid scoping issues in 'foreach'
  if (!is.null(a.formula)) {
    y.prob <- lapply(y.args, function(x) do.call("c", x[c("wn", "wd")]))
    y.prob <- lapply(y.prob, function(x) x[x %in% names(a.formula)])
  }else{
    y.prob <- lapply(y.formula, function(x) list())
  } 
  
  
  ## print generative and analysis model properties
  if (verbose) {
    cat("\nGenerative model attributes\n\n")
    print(control)
    cat("Analysis models\n\n")
    mapply(function(f, nm) write.table(cbind("  ", nm, ": y ~ ",
                                             as.character(f)[3]), sep = "",
                                       row.names = FALSE, col.names = FALSE,
                                       quote = FALSE, eol = "\n\n"),
           f = y.formula, nm = y.names)
    cat("Treatment probability models\n\n")
    mapply(function(f, nm) write.table(cbind("  ", nm, ": a ~ ",
                                             as.character(f)[3]), sep = "",
                                       row.names = FALSE, col.names = FALSE,
                                       quote = FALSE, eol = "\n\n"),
           f = a.formula, nm = a.names)
  }
  ## general model fitter
  ## nb: d is the data frame for the replicate
  fitter <- function(formula, args, prob, coef, label, response = "y",
                     addvar = NULL,c_vec, moderator) {
    if (response == "y") {
      runin <- runin.fity
    }else{
      args$family <- binomial()
      runin <- runin.fita
    } 
    r <- which(d$time >= runin)
    l <- list(x = model.matrix(formula, data = d[r, ]), y = d[r, response])
    v_base = diag(1,nrow = length(r)/n)
    # no availability has 0 weight
    w <- rep(1, nrow(d))
    w <- w * d$avail
    # lag != 0
    if (lag){
      w <- delay(d$id, d$time, w, lag)
    } 
    w <- w[r]
    
    if (!is.null(args$corstr)) {
      fun <- "geese.glm"
      l$id <- d$id[r]
    }else if (!is.null(args$family)){
      fun <- "glm.fit"
    } else{
      fun <- "lm.wfit"
    } 
    
    if (sum(d$statet) ==0 & moderator != "None"){
      d[r,"statet"] = sum(w*(l$x[,3])^2*l$x[,2])/sum(w*(l$x[,3])^2)
      l <- list(x = model.matrix(formula, data = d[r, ]), y = d[r, response])
      d$statet = 0
    }
    l$w = w
    # fit <- do.call(fun, c(l, args))
    fit <- do.call(fun, l)
    
    if (!inherits(fit, "geeglm")){
      fit <- glm2gee(fit, d$id[r])
      fit$geese$X <- l$x
      fit$y <- l$y
      fit$terms <- terms(formula)
      fit$v_base = v_base
    }
    
    
    if (!is.null(addvar)) {
      newvar <- paste0(c("", "lag1"), addvar)
      d[, newvar] <- NA
      d[r, newvar[1]] <- fit$fitted.values
      d[, newvar[2]] <- delay(d$id, d$time, d[, newvar[1]])
    }else {
      ## usual variance sandwich estimator
      fit$vcov <- vcov.geeglm(l,x = fit,moderator= moderator)
      est <- estimate(fit, rbind("Average group treatment effect" = c_vec))[,1:4]
      ## correction for any estimates in weights
      
      if (length(prob)){
        d[r, args[["w"]][["wn"]]] = fita[["fitted.values"]]
        # update the model matrix
        w <- ifelse(d[r, "a"] == 1, d[r, args[["w"]][["wn"]]]/ d[r, args[["w"]][["wd"]]],
                    (1 - d[r, args[["w"]][["wn"]]]) / (1 - d[r, args[["w"]][["wd"]]]))
        
        state_mmse = 0
        state_tmse = 0
        for(i in 1:(tmax+1-runin.fity)) { 
          nam <- paste("mu", i, sep = "_")
          assign(nam,0)
        }
        
        if (sum(d$statet) ==0 & moderator != "None"){
          l <- list(x = model.matrix(formula, data = d[r, ]), y = d[r, response])
          T = tmax+1-runin.fity
          #### change the V base here
          
          # III: only constant model
          # v_base = matrix(rep(c(1,0),c(T,(T-1)*T)),nrow = T,ncol = T)
          
          
          ### Lin's
          # v_base = diag(1, nrow = T, ncol = T)
          
          #### IV
          # f = rep(1,T)
          # # p_t = rep(unique(d[r, args[["w"]][["wn"]]]),length.out = T)
          # p_t = rowMeans(matrix(w*(l$x[,3])^2,ncol = n))
          # weights = p_t * (1-p_t)
          # projection = f%*%solve(t(f) %*% diag(weights) %*% f, t(f) %*% diag(weights))
          # orthogonal_projection = diag(1, T) - projection
          # PCA = eigen(orthogonal_projection)
          # 
          # v_base = PCA$vectors
          
          # V
          
          # linear function of time
          v_base = matrix(0,nrow = T, ncol=T)
          v_base[,1] =1
          v_base[,2] = 1:T
          
          
          centering_model<- do.call(lm.wfit, list(x = v_base[rep(1:nrow(v_base),times = n),],
                                                  y = d[r, "state"],
                                                  w = w*(l$x[,3])^2))
          
          
          if (!inherits(centering_model, "geeglm")){
            centering_model <- glm2gee(centering_model, d$id[r])
          }
          
          ######
          d[r,"statet"] = centering_model[["fitted.values"]]
      
          
          state_mmse = mean((d[r,"statec"]-d[r,"statet"])^2)
          state_tmse = mean((d[r,"state"]- d[r,"state.center"]-d[r,"statet"])^2)
          for(i in 1:T) { 
            nam <- paste("mu", i, sep = "_")
            assign(nam,centering_model[["fitted.values"]][i] )
          }
          l <- list(x = model.matrix(formula, data = d[r, ]), y = d[r, response])
        }
        
        ### adjsut A_{t-1}
        # w = w * (1- d[r,"lag1a"])
        ####
        
        l$w = w
        
      } 
      # refit the model
      
      fit <- do.call(fun, l)
      if (!inherits(fit, "geeglm")){
        fit <- glm2gee(fit, d$id[r])
        fit$geese$X <- l$x
        fit$y <- l$y
        fit$terms <- terms(formula)
        fit$v_base = v_base
      }
      another_vcov = vcov.geeglm(l,x=fit,moderator= "None")
      fit$vcov <- vcov.geeglm(l,x=fit,moderator= moderator)
      
      dif_vcov = sqrt(fit$vcov[3,3])-sqrt(another_vcov[3,3])
      estc <- estimate(fit, rbind("Average treatment effect" = c_vec))[,1:4]
      fit <- data.frame(moderator = moderator,
                        est = est["Estimate"], se = est["SE"],
                        lcl = est["95% LCL"], ucl = est["95% UCL"],estc = estc["Estimate"],
                        sec = estc["SE"], lclc = estc["95% LCL"],
                        uclc = estc["95% UCL"], state_mmse = state_mmse,
                        state_tmse = state_tmse, dif_vcov = dif_vcov, fita = unique(fita[["fitted.values"]]),
                        # paste0("mu_", 1:28,"=","mu_",1:28 ,collapse = ",",sep = "")
                        mu_1=mu_1,mu_2=mu_2,mu_3=mu_3,mu_4=mu_4,mu_5=mu_5,mu_6=mu_6,mu_7=mu_7,mu_8=mu_8,mu_9=mu_9,mu_10=mu_10,
                        mu_11=mu_11,mu_12=mu_12,mu_13=mu_13,mu_14=mu_14,mu_15=mu_15,mu_16=mu_16,mu_17=mu_17,mu_18=mu_18,mu_19=mu_19,
                        mu_20=mu_20,mu_21=mu_21,mu_22=mu_22,mu_23=mu_23,mu_24=mu_24,mu_25=mu_25,mu_26=mu_26,mu_27=mu_27,mu_28=mu_28,
                        row.names = NULL)
    }
    fit
  }
  fita <- list()
  
  out = NULL
  
  out <- foreach(m = 1:M, .combine = "rbind") %dopar% {
    d = all_data[[m]]
    # d <- rsnmm.R(n, tmax,group_ls, control = control)
    # d$pn <- d$pd <- d$prob
    # 
    # statec = aggregate(state~time,data = d,FUN = mean)$state
    # d$statec = rep(statec,times=n)
    # d$statet = mean(d$state)
    
    ## ... fit treatment probability models
    if (!is.null(a.formula)){
      fita <- fitter(formula = a.formula[["pn"]], addvar = names(a.formula),
                     args = list(), prob = list(),coef = list(), label = list(),
                     response = "a",moderator = "None")
    }
    ## ... fit response models
    
    fity_w <- fitter(formula = y.formula[["w"]], args = y.args, prob = y.prob,
                     coef = y.coef, label = y.label, c_vec= contrast_vec[["w"]],
                     moderator = y.moderator[["w"]])
    
    fity_u <- fitter(formula = y.formula[["u"]], args = y.args, prob = y.prob,
                     coef = y.coef, label = y.label,c_vec= contrast_vec[["u"]],
                     moderator = y.moderator[["u"]])
    
    fity = rbind(fity_w,fity_u)
    
    fity <- data.frame(iter = m, true = true_effect,
                       method = c("Weighted and centered"),
                       fity, row.names = NULL)
    
    fity
    
  }
  
  
  out <- data.frame(n, tmax, out)
  
  
  out_w = out[seq(1,2*M-1,by = 2),]
  ## 95% CI coverage probability using uncorrected SEs
  out_w$cp <- with(out_w, lcl <= true_effect & true_effect <= ucl)
  ## coverage probability using SEs corrected for estimates in weights
  out_w$cpc <- with(out_w, lclc <= true_effect & true_effect <= uclc)
  ## root MSE
  out_w$rmse <- with(out_w, (estc - true_effect)^2)
  
  
  ## mean and SD estimate, number of replicates
  out_w <- cbind(aggregate(cbind(est,estc, se, sec, cp, cpc, rmse,lclc, uclc) ~
                             method + moderator +  n + tmax,
                           data = out_w, FUN = mean),
                 sd = aggregate(estc ~ method + moderator + n + tmax,
                                data = out_w, FUN= sd)$estc,
                 iter = aggregate(iter ~ method + moderator  + n + tmax,
                                  data = out_w,
                                  FUN = function(x) length(unique(x)))$iter)
  out_w$rmse <- sqrt(out_w$rmse)
  
  
  
  out_u = out[seq(2,2*M,by = 2),]
  
  ## 95% CI coverage probability using uncorrected SEs
  out_u$cp <- with(out_u, lcl <= true_effect & true_effect <= ucl)
  ## coverage probability using SEs corrected for estimates in weights
  out_u$cpc <- with(out_u, lclc <= true_effect & true_effect <= uclc)
  ## root MSE
  out_u$rmse <- with(out_u, (estc - true_effect)^2)
  
  
  ## mean and SD estimate, number of replicates
  out_u <- cbind(aggregate(cbind(est,estc, se, sec, cp, cpc, rmse,lclc, uclc,state_mmse,state_tmse, dif_vcov,fita) ~
                             method + moderator +  n + tmax,
                           data = out_u, FUN = mean),
                 sd = aggregate(estc ~ method + moderator + n + tmax,
                                data = out_u, FUN= sd)$estc,
                 iter = aggregate(iter ~ method + moderator  + n + tmax,
                                  data = out_u,
                                  FUN = function(x) length(unique(x)))$iter)
  out_u$rmse <- sqrt(out_u$rmse)
  
  
  return(list(out= out, out_w = out_w, out_u = out_u))
  
}
