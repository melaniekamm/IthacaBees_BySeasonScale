rm(list=ls())
library(dplyr)
# calculate total, min, max, CV, and season total metrics of floral area by site and landscape

# which urban tree method (only applied to landscape floral resources)
#urb_tree <- 'MesicComposition'
urb_tree <- 'MeanArea'

#specify if using distance weighted or regular landscape composition data
dist_weight <- T

# run with data_type = 'site AND data_type = 'landscape' 
# summarize floral resource curves for all plants and insect pollinated plants

for (data_type in c('site', 'landscape')) {
  
  for (insect_poll in  c(F, T)) {
  
    
    if (insect_poll == T) {
      plants <- 'insectpollinated'
    } else {
      plants <- 'allplants'
    }
    
    if (data_type == 'site') {
      if (insect_poll == T) {
        floral_area <- read.csv('./data/Iverson_plant/insect_pollinated/floral_area_by_site_by_day.csv')
      } else {
        floral_area <- read.csv('./data/Iverson_plant/allplants/floral_area_by_site_by_day.csv')
      }
      
    } else if (data_type == 'landscape') {
      
      if (dist_weight == T) {
        floral_area <- read.csv(paste0('./data/Iverson_plant/LFA/landscape_floral_area_distweighted_', 
                                       urb_tree, '_', plants,'2.csv')) 
      } else {
        floral_area <- read.csv(paste0('./data/Iverson_plant/LFA/landscape_floral_area_unweighted_',
                                       urb_tree, '_', plants,'2.csv')) 
      }
    }
    
    
    floral_area <- dplyr::mutate(floral_area, Site = gsub(Site, pattern=" ", replacement="_")) %>%
              dplyr::mutate(Site= gsub(Site, pattern='Black_Diamond', 
                                       replacement='Merwin'))
    
    floral_long <- tidyr::pivot_longer(floral_area, cols=starts_with("d"), names_to = 'DOY', 
                                       values_to="floral_area")
    
    options(scipen=999)
    
    #define seasons (from Iverson and Grab methodology)
    withinseasondays <- paste0("d", 92:310) #relevant for calculating coefficient of variation
    growingdays <- paste0("d", 137:258) #for calculating seasonal minimum
    springdays <- paste0("d", 92:163)
    summerdays <- paste0("d", 164:238)
    falldays <- paste0("d", 239:310)
    
    
    #calculate floral area metrics for whole season (slighly less than whole season for minimum and CV)
    whole_season <- dplyr::group_by(floral_long, Site) %>%
             dplyr::mutate(total_FA = sum(floral_area), max_FA=max(floral_area)) %>%
             dplyr::filter(DOY %in% withinseasondays) %>%
             dplyr::mutate(CV_FA = sd(floral_area)/mean(floral_area)) %>%
             dplyr::filter(DOY %in% growingdays) %>%
             dplyr::mutate(min_FA = min(floral_area)) %>%
             dplyr::summarise(total_FA = unique(total_FA), max_FA = unique(max_FA), CV_FA=unique(CV_FA), min_FA=unique(min_FA))
    
    spring <- dplyr::group_by(floral_long, Site) %>%
              dplyr::filter(DOY %in% springdays) %>%
              dplyr::summarise(spring_total_FA = sum(floral_area))
    
    
    summer <- dplyr::group_by(floral_long, Site) %>%
              dplyr::filter(DOY %in% summerdays) %>%
              dplyr::summarise(summer_total_FA = sum(floral_area))
      
    fall <- dplyr::group_by(floral_long, Site) %>%
              dplyr::filter(DOY %in% falldays) %>%
              dplyr::summarise(fall_total_FA = sum(floral_area))
    
    fa <- dplyr::full_join(whole_season, spring) %>%
              dplyr::full_join(summer) %>%
              dplyr::full_join(fall)
      
    if (data_type == 'landscape') {
      if (dist_weight == T) {
        write.csv(fa, paste0('./data/Iverson_plant/LFA/', data_type,'_floral_area_distweighted_', urb_tree,
                             '_', plants,'_summary2.csv'), row.names = F)
      } else if (dist_weight == F) {
        write.csv(fa, paste0('./data/Iverson_plant/LFA/', data_type,'_floral_area_unweighted_', urb_tree, 
                             '_', plants,'_summary.csv'), row.names = F)
      }
    } else {
      write.csv(fa, paste0('./data/Iverson_plant/', data_type,'_floral_area_', plants, '_summary.csv'), row.names = F)
    }
  }
}