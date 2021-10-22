library(dplyr)
rm(list=ls())

#percent vegetation cover (approximate inverse of percent bare ground)
#calculate at site and habitat level


plant_rich <- read.csv('./data/Iverson_plant/allplants/richness_by_site.csv')%>%
  dplyr::mutate(Site= gsub(Site, pattern='Black_Diamond', 
                           replacement='Merwin'))

cover_long <- read.csv('./data/Iverson_plant/plant_cover_by_species.csv')

#for percent vegetation calculations, use subplots only because other A-E denote presence/absence only
cover_long <- dplyr::filter(cover_long, !subplot %in% c('Plot A', 'Plot B',
                                                          'Plot C', 'D', 'E', 'No_canopy_trees'))

#fix site names and calculate mean vegetation cover per site over all subplots
cover_long <- dplyr::mutate(cover_long, Site= gsub(Site, pattern='Black_Diamond', 
                           replacement='Merwin')) %>%
  dplyr::mutate(site_plot = paste0(Site, "_", subplot)) %>%
  dplyr::group_by(Site, subplot) %>%
  dplyr::summarise(pct_cover = sum(pct_cover)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Site) %>%
  dplyr::summarise(mean_pct_cover = mean(pct_cover))

#write results (includes all sites, not just those sampled in bee work)
write.csv(cover_long, './data/Iverson_plant/plant_cover_by_site.csv', row.names = F)
