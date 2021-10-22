##### Overall goal:
##### combine landscape composition and habitat floral area curves to generate 'landscape floral resource' curves
##### landscape floral resources = sum(landscape composition * floral area per day) for all land cover classes 

##We needed to make some adjustments to floral area in urban areas
##see email from Aaron (1/31/20)

###FYI: landscape floral area estimates do NOT include all habitats
#specifically, treerow, old field, forest MesicUpSuc, floodplain forest, and forest edge are not included
#this happens because these habitats are not designated in the high resolution land cover map


#choose which method to use for urban tree (see Aaron's emails)
urb_tree <- 'MeanArea' #urban tree method 1
#urb_tree <- 'MesicComposition' #urban tree method 2

#specify if using distance weighted or regular landscape composition data
dist_weight <- T

# calculate floral resource curves for all plants and insect pollinated plants

for (insect_poll in  c(F, T)) {
  
  #create folder for output
  if (!dir.exists('./data/Iverson_plant/LFA')) {
    dir.create('./data/Iverson_plant/LFA')
  }
  
  #read in landscape composition data (either distance weighted or not)
  if (dist_weight == T) {
    comp <- read.csv('./data/landscape_composition/HighRes2018_KammererSites_land_composition_distweighted.csv')
    #rename percent landscape column so it's the same as unweighted data
    comp <- dplyr::rename(comp, Pct_Land = PctLand_DistWeighted)
    
    } else {
    comp <- read.csv('./data/landscape_composition/HighRes2018_KammererSites_land_composition_unweighted.csv')
    comp <- dplyr::rename(comp, landcover_class = VALUE) %>%
            dplyr::select(-Cell_Num) %>%
            dplyr::select(Landscape, landcover_class, Pct_Land)
    }
  
  #calculate amount of suburban greenspace in 30m zone (any values less than 100 represent pct impervious surface)
  #create new land cover class (1) that is suburban greenspace in 30m zone 
  #land cover 1 = sum of 100-impervious cover classes for each landscape
  
  subgreen30 <- dplyr::group_by(comp, Landscape) %>%
            dplyr::filter(landcover_class < 100) %>%
            dplyr::summarise(landcover_class= 1,
                              Pct_Land = sum( ((100-landcover_class)/100*(Pct_Land/100)))*100)
  
  #calculate amount of urban greenspace in 1m zone 
  #create new land cover class (2) that is suburban greenspace in 1m zone
  #land cover 2 = 203 + 204 (lawn and urban trees)
  subgreen1 <- dplyr::group_by(comp, Landscape) %>%
            dplyr::filter(landcover_class %in% c(203, 204)) %>%
            dplyr::summarise(landcover_class= 2, Pct_Land = sum(Pct_Land))
    
  #join new suburban green space classes (1 and 2) to landscape composition dataset
  comp <- dplyr::filter(comp, landcover_class >= 100) %>%
            dplyr::full_join(subgreen30) %>%
            dplyr::full_join(subgreen1)
  
  rm(subgreen30); rm(subgreen1)
  
  #change land cover codes (from land use raster) into habitat class names that match floral area data
  codes <- read.csv('./data/landscape_composition/LandcoverCodes_finalMap_modified.csv') %>%
                dplyr::mutate(floral_area_habitat = as.character(Matching.habitat.with.floral.resource.data))
  
  #update habitat names to match those used in floral area data
  codes$floral_area_habitat[codes$floral_area_habitat == 'Alfalfa'] <- 'Alf'
  codes$floral_area_habitat[codes$floral_area_habitat == 'Conifer_mixed'] <- 'Conifer'
  codes$floral_area_habitat[codes$floral_area_habitat == 'Perennial'] <- 'OrchardFloor'
  codes$floral_area_habitat[codes$floral_area_habitat == 'Rowcrop'] <- 'RowcropCover'
  codes$floral_area_habitat[codes$floral_area_habitat == 'Rowcrop_wintercover'] <- 'RowcropWinterFallow'
  codes$floral_area_habitat[codes$floral_area_habitat == 'Dry_oak'] <- 'DryOak'
  codes$floral_area_habitat[codes$floral_area_habitat == 'Flood'] <- 'ForestFloodplain'
  codes$floral_area_habitat[codes$floral_area_habitat == 'Grass_hay'] <- 'GrassHay'
  codes$floral_area_habitat[codes$floral_area_habitat == 'Mesic_upland'] <- 'MesicUpRem'
  codes$floral_area_habitat[codes$floral_area_habitat == 'Vegetable'] <- 'Veg'
  codes$floral_area_habitat[codes$floral_area_habitat == 'Urban_tree'] <- 'UrbanTree'
  codes$floral_area_habitat[codes$floral_area_habitat == 'Wet_emergent'] <- 'WetlandEmer'
  codes$floral_area_habitat[codes$floral_area_habitat == 'Wet_shrub'] <- 'WetlandShrub'
  
  #add suburban greenspace to land cover classes table
  sub_green <- codes[1:2,]
  sub_green$Landcover <- c('SuburbanGreen30', 'SuburbanGreen1')
  sub_green$Group_ <- 'Developed'
  sub_green$Code <- c(1:2)
  sub_green$Matching.habitat.with.floral.resource.data <- NA
  sub_green$floral_area_habitat <- c('SuburbanGreen30', 'SuburbanGreen1')
  
  codes <- rbind(codes, sub_green)
  
  #join habitat name to landscape composition data
  comp2 <- dplyr::select(codes, Landcover, Group_, Code, floral_area_habitat) %>%
            dplyr::rename(Group = Group_) %>%
            dplyr::right_join(comp, by=c('Code' = 'landcover_class')) %>%
            dplyr::rename(Site = Landscape) %>%
            dplyr::mutate(PropLand = Pct_Land/100) %>%
            dplyr::select(-Pct_Land) %>%
            dplyr::filter(!Code == 203) #remove lawn land cover class bc it is contained in SuburbanGreen
  
  #if using the average number of urban trees (method 1), remove the land cover class specific to urban tree area
  if (urb_tree == 'MeanArea') {
    comp2 <- dplyr::filter(comp2, !Code == 204)
  }
  
  #read in dataset of floral area per habitat per day
  if (insect_poll == T) {
    habitat_floral <- read.csv('./data/Iverson_plant/insect_pollinated/floral_area_by_habitat_by_day.csv')
    
  } else {
    habitat_floral <- read.csv('./data/Iverson_plant/allplants/floral_area_by_habitat_by_day.csv')
  }
  
  #create new floral area curve for suburban low vegetation 
  #suburabn low veg = lawn, ornamental flower gardens, shrubland, and veg gardens
  #proportional area in each from Aaron's email
  
  #make floral area data long so all days floral area can be manipulated at the same time
  floral_long <- tidyr::pivot_longer(habitat_floral, -habitat, names_to = 'DOY', values_to='FloralArea')
  
  suburban_veg <- dplyr::filter(floral_long, habitat %in% c('Lawn', 'GardenFlrs', 'Shrubland', 'Veg')) %>%
            dplyr::mutate(FloralArea= dplyr::if_else(habitat == 'Lawn', FloralArea*0.7012338,
                                       if_else(habitat == 'GardenFlrs', FloralArea*0.1474899,
                                       if_else(habitat == 'Shrubland', FloralArea*0.118186,
                                       if_else(habitat == 'Veg', FloralArea*0.0038659, FloralArea))))) %>%
            dplyr::group_by(DOY) %>%
            dplyr::summarise(habitat='UrbanLowVeg', FloralArea= sum(FloralArea)) %>%
            tidyr::pivot_wider(names_from=DOY, values_from=FloralArea)
  
  #make version of suburban veg for 1m zone             
  lowveg1<- dplyr::mutate(suburban_veg, sub_habitat = as.character(habitat)) %>%
            dplyr::mutate(habitat= 'SuburbanGreen1') %>%
            dplyr::select(habitat, sub_habitat, dplyr::everything())
  
  #make version of suburban veg for 30m zone             
  lowveg30 <- dplyr::mutate(suburban_veg, sub_habitat = as.character(habitat)) %>%
            dplyr::mutate(habitat= 'SuburbanGreen30') %>%
            dplyr::select(habitat, sub_habitat, dplyr::everything())
  
  #make version of suburban veg for 1m zone, choosing sub_habitat based on urban tree method selected             
  if (urb_tree == 'MeanArea') {
    tree1 <- dplyr::filter(habitat_floral, habitat == "UrbanTree") %>%
              dplyr::mutate(sub_habitat = as.character(habitat)) %>%
              dplyr::mutate(habitat= 'SuburbanGreen1') %>%
              dplyr::select(habitat, sub_habitat, dplyr::everything())
    
  } else if (urb_tree == 'MesicComposition') {
    tree1 <- dplyr::filter(habitat_floral, habitat == "MesicUpRem") %>%
              dplyr::mutate(sub_habitat = "MesicUpRem") %>%
              dplyr::mutate(habitat= 'UrbanTree') %>%
              dplyr::select(habitat, sub_habitat, dplyr::everything())
  }
  
  #make version of suburban veg for 30m zone, choosing sub_habitat based on urban tree method selected             
  tree30 <- dplyr::filter(habitat_floral, habitat == "UrbanTree") %>%
            dplyr::mutate(sub_habitat = as.character(habitat)) %>%
            dplyr::mutate(habitat= 'SuburbanGreen30') %>%
            dplyr::select(habitat, sub_habitat, dplyr::everything())
  
  #add all low veg and urban tree floral area curves to other floral area curves
  #habitat column matches SuburbanGreen1 and SuburbanGreen30 land cover classes with urban floral area curves
  habitat_floral <- dplyr::filter(habitat_floral, !habitat %in% c('Lawn', 'GardenFlrs', 'UrbanTree')) %>%
            dplyr::mutate(sub_habitat = as.character(habitat)) %>%
            dplyr::select(habitat, sub_habitat, dplyr::everything()) %>%
            dplyr::full_join(lowveg1) %>%
            dplyr::full_join(lowveg30) %>%
            dplyr::full_join(tree1) %>%
            dplyr::full_join(tree30)
  
  rm(lowveg1); rm(lowveg30); rm(tree1); rm(tree30)
    
  #make floral area data long
  floral_long <- tidyr::pivot_longer(habitat_floral, cols=-(habitat:sub_habitat), names_to = 'DOY', 
                                     values_to='FloralArea')
  
  #merge floral area and landscape composition data
  #calculate weighted floral area (floral area in x habitat * area of x habitat)
  #summarize landscape floral resources for each site
  landscape_floral_area <- dplyr::full_join(floral_long, comp2, by=c('habitat' = 'floral_area_habitat')) %>%
            dplyr::mutate(FloralAreaWeighted= FloralArea*PropLand) %>%
            dplyr::filter(!is.na(FloralAreaWeighted)) %>%
            dplyr::group_by(Site, DOY) %>%
            dplyr::summarise(LandscapeFloralArea = sum(FloralAreaWeighted)) %>%
            dplyr::mutate(DOY = as.integer(stringr::str_replace(DOY, stringr::fixed("d"), ""))) %>% # temporarily change DOY so it sorts properly
            dplyr::arrange(Site, DOY) %>%
            dplyr::mutate(DOY = paste0('d', DOY)) %>%
    tidyr::pivot_wider(names_from=DOY, values_from = LandscapeFloralArea)
  
  #write landscape floral area curves
  if (insect_poll == T) {
    plants <- 'insectpollinated'
  } else {
    plants <- 'allplants'
  }
  
  if (dist_weight == T) {
    write.csv(landscape_floral_area, paste0('./data/Iverson_plant/LFA/landscape_floral_area_distweighted_',
              urb_tree, '_', plants, '.csv'),
              row.names = F)
  
    } else {
    write.csv(landscape_floral_area, paste0('./data/Iverson_plant/LFA/landscape_floral_area_unweighted_',
              urb_tree, '_', plants,'.csv'),
              row.names = F)
  }

}