run_RF <- function(nreps, nfolds=10, centerscale, write_output=T) {
  library(zeallot)
  
  # create directory for results if it doesn't exist
  if (!dir.exists('./data/RF_results/')) {
    dir.create('./data/RF_results/')
  }
  
  for (season in c('spring', 'summer')) {
    for (response in c('abundance', 'richness')) {

      
      source('./code/functions/load_data.R')
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
      
      #split dataframes into response and predictor 
      #avoids using formula interface that turns factors into dummy variables
      landyearsite_response <- dplyr::select(beedata, all_of(responsevar))
      landyearsite_pred <- dplyr::select(beedata, -all_of(responsevar))
      
      #remove columns with NA values
      nacells <- which(is.na(landyearsite_pred))
      
      
      if (length(nacells) > 0 ) {
        remove_columns <- which(colSums(is.na(landyearsite_pred)) > 0)
        landyearsite_pred <- landyearsite_pred[-remove_columns]
        landyearsite_response <- landyearsite_response[-remove_columns]
      }
      nacells2 <- which(is.na(landyearsite_response))
      
      if (length(nacells2) > 0 ) {
        warning('NA values in response variable. These sites were removed.')
        landyearsite_pred <- landyearsite_pred[-nacells2,]
        landyearsite_response <- data.frame(landyearsite_response[-nacells2,])
      }
      
      # set up random forest
      control <- caret::trainControl(method="repeatedcv", number=nfolds, repeats=nreps, search="grid")
      
      # specify mtry range
      tunegrid <- expand.grid(.mtry=seq(from=4, to=18, by=2), .splitrule= 'maxstat', .min.node.size=5)

      #run several random forest models to choose mtry
      landyearsite_mtry <- caret::train(y=landyearsite_response[,1], x=landyearsite_pred, method="ranger", metric='RMSE',
                                        trControl=control, num.trees=500, importance='permutation', replace=T,
                                        tuneGrid=tunegrid)
      
      mtrybest <- landyearsite_mtry$finalModel$mtry
      
      # manual search to tune number of trees
      tunegrid <- expand.grid(.mtry=mtrybest, .splitrule= 'maxstat', .min.node.size=5)
      modellist <- list()
      for (ntree in c(1000, 2000, 3000, 4000, 5000)) {
        fit <- caret::train(y=landyearsite_response[,1], x=landyearsite_pred,  method="ranger", metric='RMSE',
                            tuneGrid=tunegrid, trControl=control, num.trees=ntree, importance='permutation', 
                            replace=T)
        key <- toString(ntree)
        modellist[[key]] <- fit
      }
      # compare results
      results <- resamples(modellist)
      
      # extract info on best ntrees to use
      test <- summary(results)
      test <- data.frame(test$statistics$RMSE)
      test$Model <- rownames(test)
      ntrees <- test$Model[test$Mean == min(test$Mean)]
      ntreesbest <- as.numeric(ntrees)
      
      # run with best mtry and number of trees
      landyearsite_rf <- caret::train(y=landyearsite_response[,1], x=landyearsite_pred,  method="ranger", metric='RMSE',
                                      tuneGrid=tunegrid, trControl=control, num.trees=ntreesbest, importance='permutation', 
                                      replace=T)
      
      # save random forest models (will use for plotting later)
      if (centerscale == F) {
        save(landyearsite_rf, file=paste0('./data/RF_results/LandYearSite_AllVariables_', season, '_', response, '.RDA'))
      } else if (centerscale == T) {
        save(landyearsite_rf, file=paste0('./data/RF_results/LandYearSite_AllVariables_CenterScale_', season, '_', response, '.RDA'))
        
      }
      

      landyearsite_df <- dplyr::mutate(landyearsite_rf$results, VariableSet = 'LandYearSite_ALL',
                                       Season=season, Variable=response,
                                       mtry=mtrybest, ntrees=ntreesbest)
      
      if (write_output == T) {
        
        write <- varImp(landyearsite_rf)$importance
        names(write) <- 'importance'
        write$variable <- rownames(write)
        
        write$season <- season
        write$response_var <- response
        write$ntrees <- ntreesbest
        write$mtry <- mtrybest
        write$VariableSet <- 'LandYearSite_ALL'
      }
      
      a <- varImp(landyearsite_rf)
      # identify top variables (1/3rd of total n)
      b <- a$importance
      b$variable <- row.names(b)
      
      # nrows to keep
      nrows <- 30
      
      b <- dplyr::arrange(b, desc(Overall)) %>%
        dplyr::slice(1:nrows)
      
      
      if (season == 'spring' & response == 'abundance') {
        all <- landyearsite_df
        all_VI <- write

      } else {
        all <- rbind(all, landyearsite_df) 
        all_VI <- rbind(all_VI, write)

      }
    }
  }
  
  if (centerscale == T) {
    write.csv(all, './data/RF_results/LandYearSite_Variance_CenterScale.csv')
    write.csv(all_VI, './data/RF_results/LandYearSite_VariableImportance_AllVariables_CenterScale.csv')

  } else {
    write.csv(all, './data/RF_results/LandYearSite_Variance.csv')
    write.csv(all_VI, './data/RF_results/LandYearSite_VariableImportance_AllVariables.csv')
  }
}
