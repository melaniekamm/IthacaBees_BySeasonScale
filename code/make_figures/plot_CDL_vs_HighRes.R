rm(list=ls())

library(raster); library(dplyr)
hr <- raster::raster('./spatial_data/HighRes2018_KammererSites1100m/Field_1 Power.tif')
cdl <- raster::raster('./spatial_data/CDL2018_1100m/Field_1_Power.tif')

#plot(cdl)
raster::ratify(cdl)

cdl_classes <- read.csv('./spatial_data/landuse_rasters/NASS_classes.csv') %>%
          dplyr::select(-CROP_TYPE, -HERB_STATUS, -POLL_DEP, -POLLDEP_SOURCE) 
        
r <- ratify(cdl)
rat <- levels(r)[[1]]
rat <- dplyr::left_join(rat, cdl_classes, by= c('ID' = 'VALUE'))
rat$CLASS_NAME <- rat$CLASS_NAME[drop=T]


class_order <- c('Alfalfa', 'Corn', 
           'Clover/Wildflowers', 'Oats', 'Rye', 'Winter Wheat', 'Barren', 'Fallow/Idle Cropland',
           'Grass/Pasture', 'Other Hay/Non Alfalfa', 'Shrubland', 'Herbaceous Wetlands', 
           'Woody Wetlands', 'Open Water', 'Conifer Forest', 'Mixed Forest', 'Deciduous Forest',
           'Developed/Open Space')

rat$CLASS_NAME <- factor(rat$CLASS_NAME, levels=class_order)
            
levels(r) <- rat

library(ggspatial); library(ggplot2)

cdl_data <- ggspatial::df_spatial(r) %>%
            dplyr::left_join(cdl_classes, by= c('band1' = 'VALUE')) %>%
            dplyr::mutate(band1 = as.factor(band1)) %>%
            dplyr::filter(!is.na(band1)) %>%
            dplyr::mutate(CLASS_NAME = factor(CLASS_NAME, levels=class_order))

rat <- dplyr::arrange(rat, CLASS_NAME)

ggplot() + geom_raster(data=cdl_data, aes(x=x, y=y, fill=CLASS_NAME)) + 
  scale_fill_manual(values = rgb(rat$RED, rat$GREEN, rat$BLUE, alpha=1)) + 
  theme_void() +
  labs(fill= "CDL Land Cover")

ggsave('./figures/CompareLandCover/CDL_AppleGrisamore.svg', device='svg')

#high resolution land cover map
highres_classes <- read.csv('D:/Documents/IthacaBees/spatial_data/landuse_rasters/HighRes_LandCoverClasses.csv') %>%
          dplyr::mutate(Code = as.integer(Code))

r2 <- ratify(hr)
rat2 <- levels(r2)[[1]]
rat2 <- dplyr::left_join(rat2, highres_classes, by= c('ID' = 'Code'))
rat2$NameMap <- rat2$NameMap[drop=T]

hr_names <- c('Alfalfa', 'Corn', 'Soybeans', 'Rowcrop', 
              'Rowcrop, wintercover', 'Grass/hay', 'Pasture', 'Shrubland', 'Old field', 
              'Wetland, emergent', 'Wetland, shrub', 'Swamp', 'Water','Conifer/Mixed forest', 
              'Mesic Upland forest', 'Dry Oak forest',
              'Lawn', 'Roadside ditch', 'No resource')

rat2$NameMap <- factor(rat2$NameMap, levels=hr_names)
levels(r2) <- rat2

hr_data <- ggspatial::df_spatial(r2) %>%
          dplyr::left_join(highres_classes, by= c('band1' = 'Code')) %>%
          dplyr::mutate(band1 = as.factor(band1)) %>%
          dplyr::filter(!is.na(band1)) %>%
          dplyr::mutate(NameMap = factor(NameMap, levels= hr_names))

rat2 <- dplyr::arrange(rat2, NameMap)

ggplot() + geom_raster(data=hr_data, aes(x=x, y=y, fill=NameMap)) + 
  scale_fill_manual(values = rgb(rat2$RED, rat2$GREEN, rat2$BLUE, alpha=1)) + 
  theme_void() +
  labs(fill= "High-Res Land Cover")

ggsave('./figures/CompareLandCover/HighRes_AppleGrisamore.svg', device='svg')


#plot examples of temporal availability of resources from Aaron's data vs. Koh et al forage index
site <- read.csv('./data/Iverson_plant/allplants/floral_area_by_site_by_day.csv') %>%
  dplyr::mutate(Site = gsub(gsub(Site, pattern=" ", replacement = "_", fixed=T),
                            pattern='Black_Diamond', replacement = "Merwin", fixed=T))


all <-  tidyr::pivot_longer(site, cols= c(-Site, -habitat), names_to = 'DOY', 
                            values_to = 'FloralArea') %>%
  dplyr::mutate(DOY = as.integer(substr(DOY, start = 2, stop=10))) %>%
  dplyr::filter(DOY > 60)  #%>%
  #dplyr::mutate(habitat = factor(habitat, levels= c('Edge', "MesicUpRem", "ForestFlood",  "OldField",
                                                    #"Ditch", "OrchardFloor", "Veg")))


meanhab <-  tidyr::pivot_longer(site, cols= c(-Site, -habitat), names_to = 'DOY', 
                                values_to = 'FloralArea') %>%
  dplyr::mutate(DOY = as.integer(substr(DOY, start = 2, stop=10))) %>%
  dplyr::filter(DOY > 90 & DOY < 300 & habitat %in% c('Conifer',  "MesicUpRem", "MesicUpSuc")) %>%
  dplyr::group_by(habitat, DOY) %>%
  dplyr::summarise(FloralArea = mean(FloralArea)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(habitat = factor(habitat, levels= c('Conifer',  "MesicUpRem", "MesicUpSuc")))

library(ggplot2); library(gghighlight)
habitatplot <- ggplot(meanhab, aes(x=DOY, y=FloralArea, color=habitat, fill=habitat)) + geom_line() + 
  geom_polygon() + theme_classic() + 
  labs(y='Mean floral area (all plants), site (m2/ha)', x= 'Day of year') +
  theme_classic(base_size = 16) +
  scale_fill_manual(values= c("#651669", "#5da13b", "#a559b3", "#0b4512", "#61a3dc", 
                              "#26496d", "#1fa198")) +
  scale_color_manual(values= c("#651669", "#5da13b", "#a559b3", "#0b4512", "#61a3dc", 
                               "#26496d", "#1fa198")) +
  gghighlight(use_direct_label = T)

habitatplot + facet_wrap(~habitat, nrow=4) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )
ggsave(filename='FloralAreaForests_AllPlants_HabitatMean.svg', device='svg', 
       path='./figures/CompareLandCover', height=8, width=5)
