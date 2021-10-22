rm(list=ls())

library(dplyr)

# #read in AbundTrap and land quality classes from landscape random forest
landmetrics <- read.csv('./data/analysis_datasets/All_landscape_predictors.csv')

#site variables

#floral area
allplantsFA <- read.csv('./data/Iverson_plant/site_floral_area_allplants_summary.csv')
insectpollFA <- read.csv('./data/Iverson_plant/site_floral_area_insectpollinated_summary.csv')

FA <- full_join(allplantsFA, insectpollFA, suffix=c(".all", ".IP"), by='Site')

#local plant richness
allplants_rich <- read.csv('./data/Iverson_plant/allplants/richness_by_site.csv') %>%
  dplyr::mutate(Site = gsub(Site, pattern='Black_Diamond', replacement = "Merwin", fixed=T))

insectpoll_rich <- read.csv('./data/Iverson_plant/insect_pollinated/richness_by_site.csv') %>%
  dplyr::mutate(Site = gsub(Site, pattern='Black_Diamond', replacement = "Merwin", fixed=T))

#total vegetation cover by site
vegcover <- read.csv('./data/Iverson_plant/plant_cover_by_site.csv') 

#read in plant community composition summary (NMDS axes)
axes <- read.csv('./data/Iverson_plant/allplants/NMDS_Ordination_Axis_Loadings.csv') %>%
  dplyr::select(-X) %>%
  dplyr::rename(Site=SampleID, NMDS_mmt=NMDS1, NMDS_water=NMDS2)

site_metrics <- dplyr::full_join(allplants_rich, insectpoll_rich, by=c ("Site", "habitat"), 
                                 suffix=c('.all', '.IP')) %>%
  dplyr::full_join(vegcover) %>%
  dplyr::full_join(axes) %>%
  dplyr::full_join(FA)

rm(allplants_rich, allplantsFA, insectpoll_rich, insectpollFA, FA, vegcover, axes)

#read in and summarize soil variables
soil <- read.csv("./data/soil/FertilityTexture_withSampleCodes.csv", strip.white=T) 
summary(soil)

#read in soil moisture data and join to other soil variables
#remove categorical variables (soil texture class)
soil_withM <- read.csv("./data/soil/SoilMoistureRaw.csv") %>%
  dplyr::rename(Field_ID= Sample_ID) %>%
  dplyr::select(Field_ID, site, habitat, sub_sample, 
                Grav_WaterContent_g.g) %>%
  dplyr::full_join(soil) %>%
  dplyr::select(Field_ID, Lab_ID, site, habitat, 
                sub_sample, dplyr::everything()) %>%
  dplyr::select(-Soil_Textural_Class)

#add bulk density info (by site)
bd <- read.csv('./data/soil/bulk_density.csv')
names(bd) <- c('site', 'subsample', 'bulk_density')

bd_bysite <- dplyr::group_by(bd, site) %>%
  dplyr::summarise(bulk_density=mean(bulk_density))

soil_withM <- dplyr::full_join(soil_withM, bd_bysite)

#center and standardize soil variables
soil.c <- cbind(soil_withM[,1:5], data.frame(scale(
  soil_withM[, c(6:(length(soil_withM)))], center=T, scale=T)))


#take out K, Mg, and Ca as percent of CEC variables, as they're correlated with ppm
#multiple correlated variables can weight analysis more than single variable gradients
soil.c <- dplyr::select(soil_withM,  -dplyr::contains('Pct_CEC'), -pH_Acidity_index)
soil_by_site <- dplyr::select(soil_withM, -Field_ID, -Lab_ID, -pH_Acidity_index, -dplyr::contains('Pct_CEC'))

write.csv(soil_by_site, './data/soil/AllSoilVariables_by_site_unaggregated.csv', row.names=F)


soil_site_means <-group_by(soil_by_site, site, habitat) %>%
  summarize(across(where(is.numeric), mean, .names="{.col}_mean")) %>%
  ungroup()

rm(bd, bd_bysite, soil, soil_by_site, soil_withM)
write.csv(soil_site_means, './data/soil/AllSoilVariables_by_site.csv', row.names = F)


#merge soil variables together with other site-level predictors

site_metrics <- dplyr::select(landmetrics, Site, landscape_names) %>%
  dplyr::left_join(site_metrics, by=c("landscape_names" = "Site"), suffix=c('.land', '.site')) 


site_data <- dplyr::select(soil_site_means, -habitat) %>%
  dplyr::mutate(site = gsub(site, pattern='Salmon Creek', replacement = 'SalmonCreek', fixed=T)) %>%
  dplyr::left_join(site_metrics, by=c('site' = 'Site')) %>%
  dplyr::rename(Site=site) 

rm(site_metrics, soil_site_means)

# add .land suffix to floral area metrics (to differentiate from landscape-level measures)
site_data <- site_data %>% dplyr::rename_with(.fn= ~paste0(.x, '.site'), .cols=dplyr::contains('FA'))


write.csv(site_data, './data/analysis_datasets/All_site_predictors.csv', row.names = F)