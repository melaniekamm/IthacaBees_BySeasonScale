rm(list=ls())

library(dplyr)

# compile all landscape variables into one csv file

if (!dir.exists('./data/analysis_datasets')) {
  dir.create('./data/analysis_datasets')
}

#topo
topo <- read.csv('./data/Iverson_sites_topography.csv') %>%
  dplyr::select(-X, -habitat, -aspect360, -slope_deg, -optional, 
                -dplyr::starts_with("coords"), -Lat, -Long) %>%
  dplyr::rename(Site = site)

#distance to water
water <- read.csv('./data/distance_to_water.csv') %>%
  dplyr::rename(Site = site) %>%
  dplyr::mutate(Site = gsub(gsub(Site, pattern=" ", replacement = "_", fixed=T),
                            pattern='Black_Diamond', replacement = "Merwin", fixed=T)) %>%
  dplyr::rename(distance_to_water = distance) %>%
  dplyr::select(Site, distance_to_water)

#floral area
allplantsFA <- read.csv('./data/Iverson_plant/LFA/landscape_floral_area_distweighted_MeanArea_allplants_summary.csv')
insectpollFA <- read.csv('./data/Iverson_plant/LFA/landscape_floral_area_distweighted_MeanArea_insectpollinated_summary.csv')

FA <- full_join(allplantsFA, insectpollFA, suffix=c(".all", ".IP"), by='Site')

#insecticide
insecticide <- read.csv('./data/Insecticide/insecticide_toxic_load_landscape.csv')

#landscape composition
comp <- read.csv('./data/landscape_composition/landscape_composition_distweighted_wide.csv') %>%
  dplyr::select(-PctLand_Ditch)

#landscape configuration
config <- read.csv('./data/landscape_composition/landscape_configuration_metrics_highres.csv')


compconfig <- dplyr::full_join(comp, insecticide) %>%
  dplyr::full_join(config) %>%
  dplyr::mutate(Landscape = gsub(gsub(Landscape, pattern=" ", replacement = "_", fixed=T),
                                 pattern='Black_Diamond', replacement = "Merwin", fixed=T))


landmetrics <- dplyr::right_join(topo, water) %>%
  dplyr::full_join(FA) %>%
  dplyr::full_join(compconfig, by=c('Site' = 'Landscape'))

#add habitat type
addhab <- read.csv('./data/Iverson_plant/allplants/richness_by_site.csv') %>%
  dplyr::mutate(Site = gsub(gsub(Site, pattern=" ", replacement = "_", fixed=T),
                            pattern='Black_Diamond', replacement = "Merwin", fixed=T))

landmetrics <- dplyr::select(addhab, Site, habitat) %>%
  dplyr::right_join(landmetrics)


#fixed site names manually, do not overwrite
#names <- data.frame(sort(unique(beeabund$Site)), sort(unique(landmetrics$Site)))
#write.csv(names, './data/SiteNamesKey.csv')

# names <- read.csv('./data/SiteNamesKey.csv')
# beeabund <- read.csv('./data/bee_abundance_per_seasonyearsite.csv')
# 
# 
# rm(list= ls()[!(ls() %in% c('landmetrics','beeabund', 'names'))])
# 
# beeabund <- dplyr::full_join(beeabund, names, by=c("Site" = 'bee_names')) %>%
#   dplyr::full_join(landmetrics, by=c('landscape_names' = 'Site'))

#save full version for reference
write.csv(landmetrics, './data/analysis_datasets/All_landscape_predictors.csv', row.names = F)

# write.csv(beeabund, './data/analysis_datasets/bee_abundance_landscape_predictors.csv', row.names = F)