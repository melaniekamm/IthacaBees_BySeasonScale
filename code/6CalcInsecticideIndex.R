#rm(list=ls())
library(dplyr)

#translating high resolution land cover map classes into Crop Data Layer 
#to calculate Douglas et al insecticide index
#according to visual overlay, 'perennial' class is almost all grapes
#'rowcrop' is mix of oats, barley, and other small grains. Assigned to oat

dist_weight <- T

ITL <- read.csv('./data/Insecticide/NY_InsecticideIndex_2014.csv')
key <- read.csv('./data/landscape_composition/LandcoverCodes_finalMap_modified.csv')

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

#filter out land use classes less than 100 (in suburban/urban areas that have no insect toxic load in Douglas index)
comp <- dplyr::filter(comp, landcover_class >=100)

#add insecticide toxicity to land cover data

insect_land <- dplyr::select(ITL, value, ld50_ct_ha_bil, ld50_or_ha_bil) %>%
               dplyr::right_join(dplyr::select(key, Equiv_CDL_class, Landcover, Code), 
                                 by = c('value' = 'Equiv_CDL_class')) %>%
               dplyr::mutate(ld50_mean_ha_bil = (ld50_ct_ha_bil + ld50_or_ha_bil)/2) %>%
               dplyr::select(Code, ld50_mean_ha_bil) %>%
               dplyr::right_join(comp, by=c('Code' = 'landcover_class')) %>%
               tidyr::replace_na(list(ld50_mean_ha_bil = 0)) %>%
               dplyr::mutate(ITL_mean_landscape = ld50_mean_ha_bil * (Pct_Land/100)) %>%
               dplyr::group_by(Landscape) %>%
               dplyr::summarise(ITL_mean_landscape = sum(ITL_mean_landscape))

write.csv(insect_land, './data/insecticide/insecticide_toxic_load_landscape.csv', row.names = F)



    