
out[["out_w"]][["estc"]]
out[["out_w"]][["sec"]]
out[["out_w"]][["cpc"]]

out[["out_u"]][["estc"]]
out[["out_u"]][["sec"]]
out[["out_u"]][["cpc"]]

out[["out_w"]][["sd"]]/out[["out_u"]][["sd"]]

out = out[["out"]]
out_u = out[seq(2,2*M,by = 2),]
out_w = out[seq(1,2*M-1,by = 2),]


df = out_w$sec^2 / (out_u$sec^2)
mean(df)
sum(df>1)
