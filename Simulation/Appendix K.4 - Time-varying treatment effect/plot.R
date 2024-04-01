var_wcls = matrix(omit[["out_w"]][["int_time_cov"]], nrow = 2, ncol = 2)
diag(var_wcls) = omit[["out_w"]][["varc"]]

var_awcls = matrix(omit[["out_u"]][["int_time_cov"]], nrow = 2, ncol = 2)
diag(var_awcls) = omit[["out_u"]][["varc"]]


coeff_wcls = omit[["out_w"]][["estc"]]
coeff_awcls = omit[["out_w"]][["estc"]]


data = as.data.frame(matrix(NA, nrow = 60, ncol = 5))
colnames(data) = c("x","y","lower","upper","Method")
data$x = rep(1:30,2)
data$Method = rep(c("A2-WCLS","WCLS"),each = 30)

data$y[1:30] = coeff_awcls[1]+ coeff_awcls[2] *data$x[1:30] 
data$y[31:60] = coeff_wcls[1]+ coeff_wcls[2]*data$x[31:60]


## a2
calculate_se = function(x) sqrt(t(c(1,x))%*%var_awcls%*%c(1,x))

se_cor = sapply(data$x[1:30],FUN=calculate_se)

t_quantile <- qt(0.975, 250-5)

data$lower[1:30] = data$y[1:30] - t_quantile*se_cor
data$upper[1:30] = data$y[1:30] + t_quantile*se_cor

se_awcls = se_cor

## wcls

calculate_se = function(x) sqrt(t(c(1,x))%*%var_wcls%*%c(1,x))

se_cor = sapply(data$x[31:60],FUN=calculate_se)

t_quantile <- qt(0.975, 250-4)

data$lower[31:60] = data$y[31:60] - t_quantile*se_cor
data$upper[31:60] = data$y[31:60] + t_quantile*se_cor

se_wcls = se_cor

summary((se_wcls)^2/(se_awcls)^2)


library(ggplot2)
p<-ggplot(data=data, aes(x=x, y=y, colour=Method)) + 
  geom_line()+
  geom_ribbon(aes(ymin=lower, ymax=upper), linetype=2, alpha=0.1)+
  xlab("Time in study")+
  ylab("Time-varying treatment Effect")+
  theme_bw()+
  scale_color_brewer(palette="Paired",direction = -1)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  geom_hline(yintercept =0, linetype=2)+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))
p


