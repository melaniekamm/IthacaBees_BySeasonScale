rm(list=ls())

library(dplyr)
#read in dataset to use to add habitat information to site variables
habitats <- read.csv('./data/soil/AllSoilVariables_by_site.csv') %>%
  dplyr::select(site, habitat) %>%
  dplyr::rename(Habitat=habitat, Site=site)

#read site quality data and add habitat information
site_stats <- read.csv('./data/analysis_datasets/All_site_predictors.csv') %>%
  dplyr::left_join(habitats) %>%
  dplyr::mutate(Habitat = if_else(is.na(Habitat), 'Flood', Habitat)) %>%
  dplyr::select(Site, Habitat, everything())


richness <- site_stats %>% 
  dplyr::select(Site, Habitat, starts_with('richness')) %>%
  tidyr::pivot_longer(names_to='plantcomm', values_to='richness', cols=starts_with('richness')) %>%
  dplyr::mutate(plantcomm = as.factor(plantcomm))

library(ggplot2)
#measured plant richness
b <- ggplot(richness, aes(x=Habitat, y=richness)) + 
  geom_boxplot(aes( fill=plantcomm)) + labs(y= 'Plant species richness') + theme_classic(base_size=12) +
  theme(legend.position = 'none', axis.text.x=element_text(angle=30, hjust=1)) +
  scale_fill_manual(values= c("#8de4d3", "#2a6866")) 

plant.labs <- c('All plants', 'Insect-pollinated plants')
names(plant.labs) <- c("richness.all", "richness.IP")

b + facet_grid(~plantcomm, labeller= labeller(plantcomm = plant.labs))

ggsave(filename='ObservedPlantRichness_byHabitat.svg', device='svg', path='./explore_plant_data_figures', width=13.5, height=10, unit='cm')


