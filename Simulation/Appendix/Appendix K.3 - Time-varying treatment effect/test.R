M = 1000

out = omit[["out"]]
out_u = out[out$moderator != "None",]
out_w = out[out$moderator == "None",]

omit[["out_w"]][["estc"]]
omit[["out_w"]][["sec"]]
omit[["out_w"]][["cpc"]]


omit[["out_u"]][["estc"]]
omit[["out_u"]][["sec"]]
omit[["out_u"]][["cpc"]]




# df = out_w$sec[out_w$coef=="intercept"] - out_u$sec[out_u$coef=="intercept"]
# summary(df)

df = out_w$sec[out_w$coef=="intercept"]^2 / (out_u$sec[out_u$coef=="intercept"]^2)
sum(df>=1)
summary(df)


# df = out_w$sec[out_w$coef!="intercept"] - out_u$sec[out_u$coef!="intercept"]
# summary(df)

df = out_w$sec[out_w$coef!="intercept"]^2 / (out_u$sec[out_u$coef!="intercept"]^2)
sum(df>=1)
summary(df)


# make sure they align with each other
omit[["out_w"]][["sd"]]^2/omit[["out_u"]][["sd"]]^2
omit[["out_w"]][["sec"]]^2/omit[["out_u"]][["sec"]]^2
