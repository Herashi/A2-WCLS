load("~/Documents/MRT_competition/Sim0_susan/data_all_p/result_0.5/data_all_0.5.RData")
n = length(all_data)
library(dplyr)
prob_T = matrix(0,nrow = 31, ncol = n)

for (i in 1:n){
  d = all_data[[i]]
  summ = d %>% group_by(time) %>% summarise(pr = mean(prob))%>% select(pr)
  prob_T[,i] = as.matrix(summ)
}

prob_A = rowMeans(prob_T)
save(prob_A,file = "prob_A_0.5.RData")
