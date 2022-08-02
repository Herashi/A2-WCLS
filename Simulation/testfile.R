M = 1000

out = omit[["out"]]
out_u = out[seq(2,2*M,by = 2),]
out_w = out[seq(1,2*M-1,by = 2),]

omit[["out_u"]][["estc"]]
omit[["out_u"]][["sec"]]
omit[["out_u"]][["cpc"]]
omit[["out_u"]][["dif_vcov"]]
summary(omit[["out"]][["dif_vcov"]][seq(2,2*M,by = 2)])
hist(omit[["out"]][["dif_vcov"]][seq(2,2*M,by = 2)])


omit[["out_u"]][["state_tmse"]]

df = out_w$sec - out_u$sec
summary(df)
hist(df)
mean(df/out_w$sec)


df = out_w$sec^2 / (out_u$sec^2)
sum(df>=1)
summary(df)
hist(df)

# make sure they align with each other
(omit[["out_w"]][["sd"]]/omit[["out_u"]][["sd"]])^2
(omit[["out_w"]][["sec"]]/omit[["out_u"]][["sec"]])^2
