library("geepack")
library("zoo")

source("xgeepack_s.R")
source("xzoo.R")

#system("R CMD SHLIB rsnmm.c")
#dyn.load(if (Sys.info()["sysname"] == "Windows") "rsnmm.dll" else "rsnmm.so")
source("rsnmm.R")

