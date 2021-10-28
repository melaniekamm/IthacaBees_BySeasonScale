library(dplyr)

#read landscape quality dataset and filter to only one year and season. Remove year and season columns
land <- read.csv('./data/analysis_datasets/bee_richness_coveragemeanALL_landscape_predictors.csv') %>%
  dplyr::filter(Year == 2018 & Season == 'spring') %>%
  dplyr::select(-Year, -Season, -S.obs, -richness, -habitat)


lc <- dplyr::select(land, Site, landscape_names, PctLand_Agriculture:PctLand_Natural, distance_to_water, ITL_mean_landscape) %>%
  dplyr::rename(Insecticide_Toxic_Load = ITL_mean_landscape, Distance_to_water = distance_to_water) %>%
  tidyr::pivot_longer(cols= c(PctLand_Agriculture:PctLand_Natural, 'Distance_to_water', 
      'Insecticide_Toxic_Load'), names_to= 'cover_type') %>%
  dplyr::mutate(cover_type = as.factor(gsub(cover_type, pattern='PctLand_', replacement="Percent ")))

lc$cover_type <- factor(lc$cover_type,  levels=c('Percent Agriculture','Percent Developed', 'Percent Ditch', 
                                                       'Percent Forest', 'Percent Natural', 'Percent Successional', 
                                                       'Percent Water', 'Percent Wetland', 'Distance_to_water', 
                                                        'Insecticide_Toxic_Load'),
                        
                                        labels=c('Percent Agriculture','Percent Developed', 'Percent Ditch', 
                                                 'Percent Forest', 'Percent Natural', 'Percent Successional', 
                                                 'Percent Open Water', 'Percent Wetland', 'Distance to water (m)', 
                                                 'Insecticide Toxic Load'))

library(ggplot2); library(gghighlight)
plot <- ggplot(lc, aes(x=value)) +
  geom_histogram(aes(y=..density..), position="identity", bins=33,  fill="#208eb7") +
  geom_density(alpha=.2) +  
  labs(y='density', x= 'Landscape composition value') +
  theme_classic(base_size = 16) +
  #scale_fill_manual(values= c("#40655e", "#84ba55", "#702cb4", "#6ab3e1", "#423071", "#ca94fd", "#a1085c", "#0cc0aa")) + 
  theme(legend.position = "none")

plot + facet_wrap(vars(cover_type), scales='free', ncol=4)

ggsave(filename='Histogram_LandscapeComposition.svg', device='svg', path='./figures/supplementary', width=11, height=8.35)

#Topography


topo <- dplyr::select(land, Site, landscape_names, elevation, slope_pct, aspectEW, aspectNS) %>%
  tidyr::pivot_longer(cols= c('elevation', 'slope_pct', 'aspectEW', 'aspectNS'), names_to= 'topo_var')

topo$topo_var <- factor(topo$topo_var,  levels=c('elevation', 'slope_pct', 'aspectEW', 'aspectNS'),
                        labels=c('Elevation (m)', 'Slope (%)', 'Aspect (E-W)', 'Aspect (N-S)'))

plot_topo <- ggplot(topo, aes(x=value)) +
  geom_histogram(aes(y=..density..), position="identity", bins=33,  fill="#8a2a7b") +
  geom_density(alpha=.2) +  
  labs(y='density', x= 'Topography value') +
  theme_classic(base_size = 16) +
  theme(legend.position = "none")

plot_topo + facet_wrap(vars(topo_var), scales='free', ncol=4)

ggsave(filename='Histogram_Topography.svg', device='svg', path='./figures/supplementary', width=11, height=3.2)



##### Landscape configuration histograms

#read landscape quality dataset and filter to only one year and season. Remove year and season columns
lconfig <- dplyr::select(land, Site, landscape_names, para_mn:sidi) %>%
  tidyr::pivot_longer(cols= para_mn:sidi, names_to= 'config_metric')



library(ggplot2); library(gghighlight)
plot2 <- ggplot(lconfig, aes(x=value)) +
  geom_histogram(aes(y=..density..), position="identity", bins=33, fill="#9693f7") +
  geom_density(alpha=.2) +  
  labs(y='density', x= 'Landscape configuration value') +
  theme_classic(base_size = 16) +
  theme(legend.position = "none")

plot2 + facet_wrap(vars(config_metric), scales='free', ncol=4)

ggsave(filename='Histogram_LandscapeConfiguration.svg', device='svg', path='./figures/supplementary', width=11, height=5.5)
