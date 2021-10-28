rm(list=ls())

library(dplyr)
#read in dataset to use to add habitat information to site variables
habitats <- read.csv('./data/soil/AllSoilVariables_by_site.csv') %>%
  dplyr::select(site, habitat) %>%
  dplyr::rename(Habitat=habitat, Site=site)

#read site quality data and add habitat information
site <- read.csv('./data/analysis_datasets/All_site_predictors.csv') %>%
  dplyr::left_join(habitats) %>%
  dplyr::mutate(Habitat = if_else(is.na(Habitat), 'Flood', Habitat)) %>%
  dplyr::select(Site, Habitat, everything()) %>%
  dplyr::select(-habitat)

#read landscape quality dataset and filter to only one year and season. Remove year and season columns
land <- read.csv('./data/analysis_datasets/bee_richness_coveragemeanALL_landscape_predictors.csv') %>%
  dplyr::filter(Year == 2018 & Season == 'spring') %>%
  dplyr::select(-Year, -Season, -S.obs, -richness, -habitat)


#join together data on site and landscape quality
all_vars <- dplyr::full_join(site, land, suffix=c(".site", ".land"), by=c('Site', 'landscape_names')) %>%
  dplyr::select(Site, Habitat, landscape_names, everything())



corr <- cor(dplyr::select(all_vars, -Site, -Habitat, -landscape_names))

cross_scale <- as.data.frame(corr) %>%
  dplyr::slice(1:35) %>%
  dplyr::select(elevation:shdi)

row.names(cross_scale)[1:35]

woah <- ggcorrplot::ggcorrplot(corr, type = "lower", lab = F)
woah

ggplot2::ggsave('./figures/supplementary/Correlation_SiteLandVariables.svg', width=12.85, height=10.5)


woah2 <- ggcorrplot::ggcorrplot(cross_scale, type = "lower", lab = F)
woah2

ggplot2::ggsave('./figures/supplementary/Correlation_SiteLandVariables.svg', width=12.85, height=10.5)