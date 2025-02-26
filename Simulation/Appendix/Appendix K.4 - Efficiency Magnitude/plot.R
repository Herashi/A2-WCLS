setwd("~/Documents/Github/A2-WCLS/Simulation/Appendix/Appendix K.5 - Efficiency Magnitude")
library(ggplot2)
M = 1000

df_n <- data.frame(var1=rep(0,1000),
                 var2=rep(0,1000),
                 var3=rep(0,1000))


load("test_100_50.RData")

out = out[["out"]]
out_u = out[seq(2,2*M,by = 2),]
out_w = out[seq(1,2*M-1,by = 2),]

df_n$var1 = out_w$sec^2 / (out_u$sec^2)

load("test_250_50.RData")

out = out[["out"]]
out_u = out[seq(2,2*M,by = 2),]
out_w = out[seq(1,2*M-1,by = 2),]

df_n$var2 = out_w$sec^2 / (out_u$sec^2)



load("test_500_50.RData")

out = out[["out"]]
out_u = out[seq(2,2*M,by = 2),]
out_w = out[seq(1,2*M-1,by = 2),]

df_n$var3 = out_w$sec^2 / (out_u$sec^2)

library(reshape)
colnames(df_n) = c("N = 100","N = 250", "N = 500")

#convert from wide format to long format
data <- melt(df_n)
colnames(data) = c("Sample size","RE per replicate")



#create overlaying density plots
p_n = ggplot(data, aes(x=`RE per replicate`, fill=`Sample size`)) +
  geom_density(alpha=.25)+
  ggtitle("T = 50")


df_t <- data.frame(var1=rep(0,1000),
                   var2=rep(0,1000),
                   var3=rep(0,1000))

load("test_250_30.RData")

out = out[["out"]]
out_u = out[seq(2,2*M,by = 2),]
out_w = out[seq(1,2*M-1,by = 2),]

df_t$var1 = out_w$sec^2 / (out_u$sec^2)

load("test_250_50.RData")

out = out[["out"]]
out_u = out[seq(2,2*M,by = 2),]
out_w = out[seq(1,2*M-1,by = 2),]

df_t$var2 = out_w$sec^2 / (out_u$sec^2)



load("test_250_100.RData")

out = out[["out"]]
out_u = out[seq(2,2*M,by = 2),]
out_w = out[seq(1,2*M-1,by = 2),]

df_t$var3 = out_w$sec^2 / (out_u$sec^2)

colnames(df_t) = c("T = 30","T = 50", "T = 100")

#convert from wide format to long format
data <- melt(df_t)
colnames(data) = c("Total time points","RE per replicate")

#create overlaying density plots
p_t = ggplot(data, aes(x=`RE per replicate`, fill=`Total time points`)) +
  geom_density(alpha=.25)+
  ggtitle("N = 250")

ggpubr::ggarrange(p_n,p_t)
