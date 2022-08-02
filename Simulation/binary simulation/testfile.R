load("result_all.RData")

nsim = 1000
result_1 = result_all[seq(1,2*nsim-1,by=2)]
result_2 = result_all[seq(2,2*nsim,by=2)]

beta_se_1 <- matrix(sapply(result_1, function(l) l$beta_se))
beta_se_2 <- matrix(sapply(result_2, function(l) l$beta_se[1]))

df = (beta_se_1)^2/(beta_se_2)^2
mean(df)
hist(df)
sum(df>=1)


beta_sd_1 <- result_df_collected_1$sd
beta_sd_2 <- result_df_collected_2$sd

(beta_sd_1)^2/(beta_sd_2)^2
