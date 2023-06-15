rm(list=ls())
source('./code/functions/run_RF.R')

# final analysis uses allvar = T and centerscale = F

run_RF(nreps=10, nfolds=10, centerscale=F, write_output=T)

#run_RF(nreps=5, nfolds=10, centerscale=T, write_output=T)

