rm(list=ls())
source('./code/functions/run_RF.R')


run_RF(nreps=10, nfolds=10, centerscale=F, write_output=T)

run_RF(nreps=10, nfolds=10, centerscale=T, write_output=T)

