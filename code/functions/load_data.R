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
  
  # take out whole season total floral area (isn't really meaningful for EITHER spring or summer bees)
  beedata <- beedata %>%
    dplyr::select( -starts_with('total_FA'))
  
  #if season == spring, take out floral resources for summer and fall
  if (season == 'spring') {
    
    landyearsite <- dplyr::select(beedata, -contains('summer'), -contains('fall'), 
                                 -contains('max_FA.IP'))
  } else {
    landyearsite <- dplyr::select(beedata, -contains('spring'))
  }
  
  return(landyearsite)
}