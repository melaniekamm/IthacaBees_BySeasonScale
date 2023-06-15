# how many variables are included in each decision tree?

load("./data/RF_results/LandYearSite_AllVariables_spring_abundance.RDA")

source('./code/functions/getTreeranger.R')
ranger_trees <- getTreeranger(landyearsite_rf$finalModel, ntree=1000)


for (i in 1:1000) {
  # how many variables were included in this tree?
  nvars <- length(which(!is.na(ranger_trees[[i]]$`splitpoint`)))
  
  if (i == 1) {
    nvars_alltrees <- nvars
  } else {
    nvars_alltrees <- c(nvars_alltrees, nvars)
  }
}
summary(nvars_alltrees)

vip(landyearsite_rf$finalModel)


library(zeallot)
season <- 'summer'
response <- 'abundance'
centerscale <- F

nfolds <- 10
nreps <- 3
mtrybest <- 14

source('./code/functions/load_data.R')


# make manual version of cross validation to compare train vs. test RMSE
beedata %<-% 
  load_data(season=season, response=response, centerscale=centerscale)

#specify name of response variable to use
if (response == 'abundance') {
  responsevar <- 'AbundDayTrap'
} else if (response == 'richness') {
  responsevar <- 'richness'
}

#############################################################  
# run random forest analysis


# #remove columns with NA values
# nacells <- which(is.na(beedata))
# 
# if (length(nacells) > 0 ) {
#   remove_columns <- which(colSums(is.na(beedata)) > 0)
#   beedata <- beedata[-remove_columns]
# }

landyearsite_response <- dplyr::select(beedata, all_of(responsevar))

nacells2 <- which(is.na(landyearsite_response))

if (length(nacells2) > 0 ) {
  warning('NA values in response variable. These sites were removed.')
  beedata <- beedata[-nacells2,]
}

nfolds <- 10

#Randomly shuffle the data
beedata<-beedata[sample(nrow(beedata)),]
#Create 10 equally size folds
folds <- cut(seq(1,nrow(beedata)),breaks=nfolds,labels=FALSE)

#Perform 10 fold cross validation
for(i in 1:nfolds){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- beedata[testIndexes, ]
  trainData <- beedata[-testIndexes, ]
  
  #Use the test and train data partitions however you desire...
  
  #split dataframes into response and predictor 
  #avoids using formula interface that turns factors into dummy variables
  train_response <- dplyr::select(trainData, all_of(responsevar))
  train_pred <- dplyr::select(trainData, -all_of(responsevar))
  
  test_response <- dplyr::select(testData, all_of(responsevar))
  test_pred <- dplyr::select(testData, -all_of(responsevar))
  
  rf_onefold <- ranger::ranger(y=train_response[,1], x=train_pred, num.trees=2000, mtry=14,
                 importance='permutation', oob.error=T)
  
  train_predict <- predict(rf_onefold, data=trainData)
  test_predict <- predict(rf_onefold, data=testData) 
  
  train_rmse <- sqrt(mean((train_response[,1]-train_predict$predictions)^2))
  test_rmse <- sqrt(mean((test_response[,1]-test_predict$predictions)^2))

 if (i == 1) {
   train_error <- train_rmse
   test_error <- test_rmse
 } else {
   train_error <- c(train_error, train_rmse)
   test_error <- c(test_error, test_rmse)
 }
}

summary((test_error-train_error)/train_error)

mean_response <- dplyr::select(beedata, all_of(responsevar)) %>%
  pull() %>%
  mean()

summary(train_error/mean_response)



