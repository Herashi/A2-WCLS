M = 1000

# create an unique test set 1000 replicates
#load("~/Documents/MRT_competition/Sim0_susan/data_all/base_draft.RData")

n = 250
tmax = 30
b = 0.8
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

save(all_data,file = "data_all_0.8.RData")


load("~/Documents/MRT_competition/Sim0_susan/data_all/test_V.RData")
out = omit[["out"]]
out_u = out[seq(2,2*M,by = 2),]
out_w = out[seq(1,2*M-1,by = 2),]
df_w_1 = out_w[,c("estc","sec")]
df_1 = out_u[,c("estc","sec")]


load("~/Documents/MRT_competition/Sim0_susan/data_all/test.RData")
out = omit[["out"]]
out_u = out[seq(2,2*M,by = 2),]
out_w = out[seq(1,2*M-1,by = 2),]
df_w_2 = out_w[,c("estc","sec")]
df_2 = out_u[,c("estc","sec")]




