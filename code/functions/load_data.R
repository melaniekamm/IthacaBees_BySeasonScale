load_data <- function(season, response, centerscale) {
  
  library(dplyr)
  
  if (season == 'both') {
    season_set <- c('spring', 'summer')
    season_load <- 'summer'
  } else {
    season_set <- season_load <- season
  }
  
  beedata <- read.csv('./data/analysis_datasets/bee_abundance_richness_all_predictors.csv') %>%
    dplyr::filter(Season %in% season_set) %>%
    dplyr::mutate(Year = as.factor(Year)) %>%
    dplyr::select(-Site, -Season, -date_set, -Abundance, -AbundTrap, -Iverson_name, -S.obs, -habitat) # take out some variables not needed for richness nor abund models
  
  if (response == 'abundance') {
    beedata <- dplyr::select(beedata, -richness)
  } else if (response == 'richness') {
    beedata <- dplyr::select(beedata, -AbundDayTrap)
  }
  
  #specify name of response variable to use
  if (response == 'abundance') {
    responsevar <- 'AbundDayTrap'
  } else if (response == 'richness') {
    responsevar <- 'richness'
  }
  
  #centerscale response variables, if specified
  if (centerscale == T & responsevar == 'AbundDayTrap') {
    beedata$AbundDayTrap <- as.numeric(scale( beedata$AbundDayTrap))
  } else if (centerscale == T & responsevar == 'richness') {
    beedata$richness <- as.numeric(scale( beedata$richness))
  }
  #############################################################
  #split dataframes into response and predictor 
  #avoids using formula interface that turns factors into dummy variables
  land_response <- dplyr::select(beedata, all_of(responsevar))
  land_pred <- dplyr::select(beedata, -all_of(responsevar))
  
  #remove columns with NA values
  nacells <- which(is.na(land_pred))
  
  
  if (length(nacells) > 0 ) {
    remove_columns <- which(colSums(is.na(land_pred)) > 0)
    land_pred <- land_pred[-remove_columns]
    land_response <- land_response[-remove_columns]
  }
  nacells2 <- which(is.na(land_response))
  
  if (length(nacells2) > 0 ) {
    warn('NA values in response variable. These sites were removed.')
      land_pred <- land_pred[-nacells2]
      land_response <- land_response[-nacells2]
  }
  
  
  ############################################################# 
  
  #if season == spring, take out floral resources for summer and fall
  if (season == 'spring') {
    
    landyearsite <- dplyr::select(beedata, -contains('summer'), -contains('fall'),
                              -total_FA.IP.site, -max_FA.IP.site, -total_FA.IP.land, -max_FA.IP.land)
  } else {
    landyearsite <- beedata
  }
  
  return(landyearsite)
}