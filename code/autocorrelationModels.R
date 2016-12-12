library(sna)
library(numDeriv)
library(tidyverse)
theme_set(theme_bw())
source("code/functions/calcAlters.R")
source("../../R/util.R")
n = readRDS("data/net.RDS")
# Filter to just the girls for whom we have network data
nr = which(!(n %v% "net.data"))
delete.vertices(n, nr)

paModel =
  lnam(
    y = n %v% "TotalPAChange",
    x = cbind(age = n %v% "T1Age",
              friends = degree(n, cmode = "outdegree")),
    W1 = t(rowNorm(n[,]))
  ) 
sink("results/activity_model.txt")
summary(paModel) 
sink()

snackModel = 
  lnam(
    y = n %v% "netSnackChange",
    x = cbind(age = n %v% "T1Age",
              friends = degree(n, cmode = "outdegree")),
    W1 = t(rowNorm(n[,]))
  ) 
sink("results/snacking_model.txt")
summary(snackModel) 
sink()