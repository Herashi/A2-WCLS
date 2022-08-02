
runin <- runin.fity

out <- foreach(m = 1:M, .combine = "rbind") %dopar% {
  
  #d = all_data[[m]]
  
  d <- rsnmm.R(n, tmax,group_ls, control = control)
  d$pn <- d$pd <- d$prob


  statec = aggregate(state~time,data = d,FUN = mean)$state
  d$statec = rep(statec,times=n)
  d$statet = 0
  
  r <- which(d$time >= runin)

  ## ... fit treatment probability models
  fita <- fitter(formula = a.formula[["pn"]], addvar = names(a.formula),
                   args = list(), prob = list(),coef = list(), label = list(),
                   response = "a",moderator = "None")
  
  ## ... fit response models
  
  fity_w <- fitter(formula = y.formula[["w"]], args = y.args, prob = y.prob,
                   coef = y.coef, label = y.label, c_vec= contrast_vec[["w"]],
                   moderator = y.moderator[["w"]])
  
  se_w = estimate(fity_w, rbind("Average treatment effect" = c(0,0,1)))[,4]
  
  res_w = sum(fity_w$residuals^2)
  
  fity_u <- fitter(formula = y.formula[["u"]], args = y.args, prob = y.prob,
                   coef = y.coef, label = y.label,c_vec= contrast_vec[["u"]],
                   moderator = y.moderator[["u"]])
  res_u = sum(fity_u$residuals^2)
  
  df = as.data.frame(model.matrix(fity_u))
  w = fity_u[["prior.weights"]]
  beta_1 = fity_u[["coefficients"]][["I((a - pn) * (state - statet))"]]
  df$`I(state - statet)` = df$`I((a - pn) * (state - statet))`/df$`I(a - pn)`
  
  res_u_w = sum((fity_u$residuals+ beta_1*df$`I((a - pn) * (state - statet))`)^2)
  
  se_u = estimate(fity_u, rbind("Average treatment effect" = c(0,0,1,0)))[,4]
  # wcls fity_w
  # c wcls fity_u
  # b= 0.8
  
  x = fity_u
  res = fity_w$residuals
    
  ### bread and meat for wcls
  w_cov <- working.covariance(fity_w, invert = TRUE)
  b_w <- bread.geeglm(fity_w, wcovinv = w_cov)[3,3]

  meat_w = meat.geeglm(fity_w, wcovinv = w_cov)[3,3]

  #### bread and meat for cwcls
  
  b_u <- bread.geeglm(fity_u, wcovinv = w_cov)[3,3]
  
  l <- list(x = model.matrix(fity_u), y = d[r, "y"], w=w)
  meat_u = new_meat(l,fity_u)[3,3]
  meat_u_w = meat.geeglm(fity_u, wcovinv = w_cov)[3,3]
  epsilon_w_u = beta_1* df$`I((a - pn) * (state - statet))` + fity_u$residuals
  meat_w_u = sum((epsilon_w_u* w*df$`I(a - pn)`)^2)
  # Empirical
  U_1 = split(res*w*df$`I(a - pn)`, x$id)
  U_1 <- lapply(U_1, sum)
  
  U_2 = split(w*df$`I(a - pn)`^2*df$`I(state - statet)`, x$id)
  U_2 <- lapply(U_2, sum)
  
  
  U1U2 <- mean(unlist(U_1)*unlist(U_2))
  U2_2 = mean(unlist(U_2)*unlist(U_2))
  
  # sufficient condition
  # 2*U1U2/U2_2
  
  # Expectation
  p_tilde = fita[["fitted.values"]]
  term_1 = split(w* p_tilde*(1-p_tilde)*(1-3*p_tilde+3*p_tilde^2)*b*d[r,"state.center"]*df$`I(state - statet)`, x$id)
  term_1 = lapply(term_1, sum)
  
  term_2 = split(p_tilde*(1-p_tilde)*b*d[r,"state.center"], x$id)
  # term_2 = do.call("rbind", term_2)
  term_3 = split(p_tilde*(1-p_tilde)*df$`I(state - statet)`, x$id)
  
  minus_term1 = mapply(function(S, V) sum(S*V),
                       S = term_2,
                       V = term_3,
                       SIMPLIFY = FALSE)
  
  minus_term2 = mapply(function(S, V) sum(S*V),
                       S = term_3,
                       V = term_3,
                       SIMPLIFY = FALSE)
  
  term_2 = lapply(term_2, sum)
  term_3 = lapply(term_3, sum)
  
  term_4 = split(w* p_tilde*(1-p_tilde)*(1-3*p_tilde+3*p_tilde^2)*df$`I(state - statet)`*df$`I(state - statet)`, x$id)
  term_4 = lapply(term_4, sum)

  
  E_U1U2 = mean(unlist(term_1))+ mean(unlist(term_2)*unlist(term_3)-unlist(minus_term1))
  E_U2_2 = mean(unlist(term_4)) +mean(unlist(term_3)*unlist(term_3)-unlist(minus_term2))
  
  # m = 1
  fity <- data.frame(iter = m,beta_1 = beta_1,
                     se_u = se_u,
                     se_w = se_w,
                     res_u = res_u,
                     res_w = res_w,
                     res_u_w = res_u_w,
                     b_w = b_w,
                     b_u = b_u,
                     meat_w = meat_w,
                     meat_w_u = meat_w_u,
                     meat_u = meat_u,
                     meat_u_w = meat_u_w,
                     extra = se_u^2 - se_w^2,
                     E_U1U2 = E_U1U2,
                     U1U2= U1U2,
                     E_U2_2=E_U2_2,
                     U2_2=U2_2, 
                     extra_em = -2* beta_1*U1U2 + beta_1^2*U2_2,
                     extra_exp = -2* beta_1*E_U1U2 + beta_1^2*E_U2_2,
                     row.names = NULL)
  fity
}

save(out,file = "out_plus.RData") 

######## check which part might be wrong

load("out_plus.RData")
out_plus = out

load("out_minus.RData") 
out_minus = out

#### 1
p1 <- hist(out_plus$E_U1U2)
p2 <- hist(out_minus$E_U1U2)

plot( p1, col=rgb(0,0,1,1/4))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), add=T)  # second


#### 2   
p1 <- hist(out_plus$U1U2)
p2 <- hist(out_minus$U1U2)

plot( p1, col=rgb(0,0,1,1/4))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), add=T)  # second


#### 3

p1 <- hist(out_plus$E_U2_2)
p2 <- hist(out_minus$E_U2_2)

plot( p1, col=rgb(0,0,1,1/4))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), add=T)  # second


#### 4
p1 <- hist(out_plus$U2_2)
p2 <- hist(out_minus$U2_2)

plot( p1, col=rgb(0,0,1,1/4))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), add=T)  # second


### plus extra term

p1 <- hist(out_plus$extra_em)
p2 <- hist(out_plus$extra_exp)

plot( p1, col=rgb(0,0,1,1/4))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), add=T)  # second


#### minus extra term

p1 <- hist(out_minus$extra_em)
p2 <- hist(out_minus$extra_exp)

plot( p1, col=rgb(0,0,1,1/4))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), add=T)  # second

###############
p1 <- hist(out_plus$extra_em)
p2 <- hist(out_minus$extra_em)

plot( p1, col=rgb(0,0,1,1/4))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), add=T)  # second


#######
p1 <- hist(out_plus$extra_exp)
p2 <- hist(out_minus$extra_exp)

plot( p1, col=rgb(0,0,1,1/4),xlim = c(-1.3,-0.7))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), add=T)  # second


############



### compare two methods

p1 <- hist(out_plus$meat_w)
p2 <- hist(out_plus$meat_u)

plot( p1, col=rgb(0,0,1,1/4),xlim = c(1000,2600) )  # first histogram
plot( p2, col=rgb(1,0,0,1/4), add=T)  # second

##### compare two methods

p1 <- hist(out_minus$meat_w)
p2 <- hist(out_minus$meat_u)

plot( p1, col=rgb(0,0,1,1/4),xlim = c(1500,4000))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), add=T)  # second

### compare two methods

p1 <- hist(out_plus$meat_w)
p2 <- hist(out_plus$meat_u_w)

plot( p1, col=rgb(0,0,1,1/4))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), add=T)  # second


##### compare two methods

p1 <- hist(out_minus$meat_w)
p2 <- hist(out_minus$meat_u_w)

plot( p1, col=rgb(0,0,1,1/4),xlim = c(1200,4000))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), add=T)  # second

### se w
p1 <- hist(out_plus$se_w)
p2 <- hist(out_minus$se_w)

plot( p1, col=rgb(0,0,1,1/4), xlim = c(0.020,0.034))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), add=T)  # second


### meat w
p1 <- hist(out_plus$meat_w)
p2 <- hist(out_minus$meat_w)

plot( p1, col=rgb(0,0,1,1/4), xlim = c(1100,4000))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), add=T)  # second


### meat u
p1 <- hist(out_plus$meat_u)
p2 <- hist(out_minus$meat_u)

plot( p1, col=rgb(0,0,1,1/4), xlim = c(1200,2800))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), add=T)  # second

### meat u_w
p1 <- hist(out_plus$meat_u_w)
p2 <- hist(out_minus$meat_u_w)

plot( p1, col=rgb(0,0,1,1/4))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), add=T)  # second



######################################################## 

out_minus$extra_real = (out_minus$meat_u_w - out_minus$meat_w)/250
out_plus$extra_real = (out_plus$meat_u_w - out_plus$meat_w)/250




### the extra variance term
p1 <- hist(out_plus$extra_real)
p2 <- hist(out_minus$extra_real)

plot( p1, col=rgb(0,0,1,1/4),xlim = c(-6,1))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), add=T)  # second

### plus extra term

p1 <- hist(out_plus$extra_real)
p2 <- hist(out_plus$extra_em)

plot( p1, col=rgb(0,0,1,1/4),xlim = c(-2.5,1.5))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), add=T)  # second


#### minus extra term

p1 <- hist(out_minus$extra_real)
p2 <- hist(out_minus$extra_em)

plot( p1, col=rgb(0,0,1,1/4),xlim = c(-6,0))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), add=T)  # second


#############################
# numeric error for two different epsilon_wcls
############################

out_minus$epsilon_diff = ( out_minus$meat_w_u - out_minus$meat_w)/250
out_plus$epsilon_diff = ( out_plus$meat_w_u - out_plus$meat_w)/250

p1 <- hist(out_plus$epsilon_diff)
p2 <- hist(out_minus$epsilon_diff)

plot( p1, col=rgb(0,0,1,1/4),xlim = c(-6,3))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), add=T)  # second


###########

p1 <- hist(out_minus$meat_w/250)
p2 <- hist(out_minus$meat_w_u/250)

plot( p1, col=rgb(0,0,1,1/4),xlim = c(7.5,15))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), add=T)  # second

############
p1 <- hist(out_plus$meat_w/250)
p2 <- hist(out_plus$meat_w_u/250)

plot( p1, col=rgb(0,0,1,1/4))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), add=T)  # second




##################
# load output data
##################
load("test_minus_1.RData")
omit_minus = omit

load("test_plus_1.RData")
omit_plus = omit

M = 1000

out_minus = omit_minus[["out"]]
out_minus_u = out_minus[seq(2,2*M,by = 2),]
out_minus_w = out_minus[seq(1,2*M-1,by = 2),]

out_plus = omit_plus[["out"]]
out_plus_u = out_plus[seq(2,2*M,by = 2),]
out_plus_w = out_plus[seq(1,2*M-1,by = 2),]

#### coef of the state

p1 <- hist(out_minus_w$state)
p2 <- hist(out_plus_w$state)

plot( p1, col=rgb(0,0,1,1/4))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), add=T)  # second

#### estc

p1 <- hist(out_minus_w$estc)
p2 <- hist(out_plus_w$estc)

plot( p1, col=rgb(0,0,1,1/4))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), add=T)  # second

#### coef of the state

p1 <- hist(out_minus_u$state)
p2 <- hist(out_plus_u$state)

plot( p1, col=rgb(0,0,1,1/4))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), add=T)  # second

#### estc

p1 <- hist(out_minus_u$estc)
p2 <- hist(out_plus_u$estc)

plot( p1, col=rgb(0,0,1,1/4))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), add=T)  # second

#### b

p1 <- hist(out_minus_u$interaction)
p2 <- hist(out_plus_u$interaction)

plot( p1, col=rgb(0,0,1,1/4))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), add=T)  # second


#### SE c-wcls

p1 <- hist(out_minus_u$sec)
p2 <- hist(out_plus_u$sec)

plot( p1, col=rgb(0,0,1,1/4))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), add=T)  # second

#### SE wcls

p1 <- hist(out_minus_w$sec)
p2 <- hist(out_plus_w$sec)

plot( p1, col=rgb(0,0,1,1/4),xlim = c(0.022,0.034))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), add=T)  # second



######## check stationary distribution
expit = function(a){
  return(exp(a) / (1 + exp(a)))
}
## plus

p_0 = (1-expit(0.8))/(0.5+1-expit(0.8))
p_1 = 0.5/(0.5+1-expit(0.8))

var_plus = p_0*p_1

## minus

p_0 = (1-expit(-0.8))/(0.5+1-expit(-0.8))
p_1 = 0.5/(0.5+1-expit(-0.8))

var_minus = p_0*p_1

### the transition matrix of action has to be non positive-definite matrix. 

p_plus = matrix(c(0.5,0.31,0.5,0.69), nrow = 2)
eigen(p_plus)$values
p_minus = matrix(c(0.5,0.69,0.5,0.31), nrow = 2)
eigen(p_minus)$values

















