M = 1000

# create an unique test set 1000 replicates
# load("~/Documents/MRT_competition/Sim0_susan/data_all/base_draft.RData")


n = 250
tmax = 30
b = 0.5
group = group_all[[as.character(n)]]
group_ls = group
control <- rsnmm.control(beta0 = c(-0.2, 0, 0, b, 0))

all_data <- foreach(m = 1:M) %dopar% {
  d <- rsnmm.R(n, tmax,group_ls, control = control)
  d$pn <- d$pd <- d$prob
  
  
  statec = aggregate(state~time,data = d,FUN = mean)$state
  d$statec = rep(statec,times=n)
  d$statet = 0
  
  d
}

save(all_data,file = "data_all_0.5.RData")






