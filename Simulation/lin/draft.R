M = 1000
n = 250
tmax = 30
b = 0.8
# group = group_all[[as.character(n)]]
# group_ls = group
# control <- rsnmm.control(beta0 = c(-0.2, 0, 0, b, 0))
# 
# all_data <- foreach(m = 1:M) %dopar% {
#   d <- rsnmm.R(n, tmax,group_ls, control = control)
#   d$pn <- d$pd <- d$prob
#   
#   
#   statec = aggregate(state~time,data = d,FUN = mean)$state
#   d$statec = rep(statec,times=n)
#   d$statet = 0
#   
#   d
# }
# 
# save(all_data,file = "data_all_0.2.RData")

#### some tries
# Lin's
##### Only set the last time point's treatment as random
# All history all fixed. 
# load the packages and expit function

load("~/Documents/MRT_competition/Sim0_susan/data_all_p/result_0.8/Binary/data_all_0.8.RData")
setwd("~/Documents/MRT_competition/Sim0_susan/lin")

set.seed(234)

ref_data = all_data[[1]]
data_tmax_random = list()
data_tmax_random[[1]] = ref_data

for (i in 1:M){
  df = ref_data
  df[which(df$time==tmax),"err"] = rnorm(n, sd = sqrt(0.5))
  for (j in 1:n){
    # treatment probability 0.8 * df[j*(tmax+1),"state"]-
    prob = expit( 0.8 * df[j*(tmax+1)-1,"a"])
    # prob = 0.5
    # treatment indicator - uncentered and centered 
    df[j*(tmax+1),"a"] = as.numeric(rbernoulli(1, prob))
    df[j*(tmax+1),"a.center"] = df[j*(tmax+1),"a"] - prob
    
    ym = df[j*(tmax+1),"a.center"] * (-0.2 + b * df[j*(tmax+1),"state.center"])+
          0.8 * df[j*(tmax+1),"state"]
    # error 
    df[j*(tmax+1),"err"] = df[j*(tmax+1),"err"]+ sqrt(0.5) * df[j*(tmax+1)-1,"err"] 
    # response 
    df[j*(tmax+1),"y"] = ym + df[j*(tmax+1),"err"]
     
  }
  data_tmax_random[[i]] = df[which(df$time==tmax),]
}


all_data = data_tmax_random
save(all_data,file = "data_all_0.2.RData")


########## perform WCLS in only one time point per person


load("data_all_0.2.RData")
M = 1000

output_wcls = data.frame(matrix(NA, nrow = M, ncol = 4))
output_cwcls = data.frame(matrix(NA, nrow = M, ncol = 4))
colnames(output_wcls) = colnames(output_cwcls) = c("Estimate","Std. Error" ,"t value","Pr(>|t|)" )

form_1 = as.formula("y ~ state + I(a - pn)")
form_2 = as.formula("y ~ state + I(a - pn) + I((a - pn) * (state - statet))")

for (i in 1:M){
  data = all_data[[i]]
  fita = glm(a~1, data = data,family = binomial())
  data$pn = unique(fita[["fitted.values"]])
  
  w <- ifelse(data[, "a"] == 1, data[, "pn"]/ data[, "pd"],
              (1 - data[, "pn"]) / (1 - data[,"pd"]))

  fit_1 <- geeglm(form_1, data = data, weights = w, id = id)
  output_wcls[i,] = summary(fit_1)$coefficients["I(a - pn)",]
  
  
  # the empirical calculation
  l <- list(x = model.matrix(form_1, data = data), y = data[, "y"])
  data[,"statet"] = sum(w*(l$x[,3])^2*l$x[,2])/sum(w*(l$x[,3])^2)
  
  # data[,"statet"] = mean(data[,"state"])
  
  fit_2 <- geeglm(form_2, data = data, weights = w, id = id)
  output_cwcls[i,] = summary(fit_2)$coefficients["I(a - pn)",]
}
colMeans(output_wcls)
colMeans(output_cwcls)
RE = (output_wcls$`Std. Error`)^2/((output_cwcls$`Std. Error`)^2)
hist(RE)
mean(RE)
sum(RE>=1)

output_wcls$lcl = output_wcls$Estimate- 1.96*output_wcls$`Std. Error`
output_wcls$ucl = output_wcls$Estimate+ 1.96*output_wcls$`Std. Error`
mean(with(output_wcls, lcl <= -0.2 & -0.2 <= ucl))

output_cwcls$lcl = output_cwcls$Estimate- 1.96*output_cwcls$`Std. Error`
output_cwcls$ucl = output_cwcls$Estimate+ 1.96*output_cwcls$`Std. Error`
mean(with(output_cwcls, lcl <= -0.2 & -0.2 <= ucl))

var(output_wcls$Estimate)/var(output_cwcls$Estimate)




