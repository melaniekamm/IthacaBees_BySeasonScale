rm(list=ls())

library(ggplot2); library(gghighlight)

names <- read.csv('./data/misc/SiteNamesKey.csv')

habitat_names_ordered <- c('Mesic upland remnant forest', 'Floodplain forest', 'Forest edge', 'Old field', 'Roadside ditch', 'Mixed vegetable farm', 'Apple orchard')


#all plants floral area
land <- read.csv('./data/Iverson_plant/LFA/landscape_floral_area_distweighted_MeanArea_allplants.csv') %>%
  dplyr::mutate(Site = gsub(gsub(Site, pattern=" ", replacement = "_", fixed=T),
                            pattern='Black_Diamond', replacement = "Merwin", fixed=T))%>%
        dplyr::left_join(names, by=c('Site' = 'landscape_names'))

site <- read.csv('./data/Iverson_plant/allplants/floral_area_by_site_by_day.csv') %>%
    dplyr::mutate(Site = gsub(gsub(Site, pattern=" ", replacement = "_", fixed=T),
                                         pattern='Black_Diamond', replacement = "Merwin", fixed=T))

siteIP <- read.csv('./data/Iverson_plant/insect_pollinated/floral_area_by_site_by_day.csv')  %>%
    dplyr::mutate(Site = gsub(gsub(Site, pattern=" ", replacement = "_", fixed=T),
                            pattern='Black_Diamond', replacement = "Merwin", fixed=T))


all <-  tidyr::pivot_longer(site, cols= c(-Site, -habitat), names_to = 'DOY', 
                            values_to = 'FloralArea') %>%
        dplyr::mutate(DOY = as.integer(substr(DOY, start = 2, stop=10))) %>%
        dplyr::filter(DOY > 60  & Site %in% land$Site)  %>%
        dplyr::mutate(habitat = factor(habitat, levels= c('Edge', "MesicUpRem", "ForestFlood",  "OldField",
                                                    "Ditch", "OrchardFloor", "Veg")))


meanhab <-  tidyr::pivot_longer(site, cols= c(-Site, -habitat), names_to = 'DOY', 
                            values_to = 'FloralArea') %>%
        dplyr::mutate(DOY = as.integer(substr(DOY, start = 2, stop=10))) %>%
        dplyr::filter(DOY > 60 & Site %in% land$Site) %>%
        dplyr::group_by(habitat, DOY) %>%
        dplyr::summarise(FloralArea = mean(FloralArea)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(habitat = factor(habitat, levels=
           c('MesicUpRem', 'ForestFlood', 'Edge', 'OldField', 'Ditch', 'Veg', 'OrchardFloor')))

levels(meanhab$habitat) <- habitat_names_ordered

# allplot <- ggplot(all, aes(x=DOY, y=FloralArea, color=habitat)) + geom_line() + 
#   theme_minimal() + labs(y='Site floral area, all plants (m2/ha)', 
#                                            x= 'Day of year') +
#   theme_classic(base_size = 16) +
#   scale_color_manual(values= c("#651669", "#5da13b", "#a559b3", "#0b4512", "#61a3dc", 
#                                "#26496d", "#1fa198")) +
#   gghighlight()
# 
# allplot + facet_wrap(~habitat, nrow=4) +
#   theme(
#     strip.background = element_blank(),
#     strip.text.x = element_blank()
#   )
# 
# ggsave(filename='FloralAreaSite_AllPLants_byHabitat.svg', device='svg', path='./figures', height=8, width=8)


habitatplot <- ggplot(meanhab, aes(x=DOY, y=FloralArea, color=habitat, fill=habitat)) + geom_line() + 
  geom_polygon() + theme_classic() + 
  labs(y='Mean floral area (all plants), site (m2/ha)', x= 'Day of year') +
  theme_classic(base_size = 16) +
  scale_fill_manual(values= c("#651669", "#5da13b", "#a559b3", "#0b4512", "#61a3dc", 
                              "#26496d", "#1fa198"), labels=habitat_names_ordered) +
  scale_color_manual(values= c("#651669", "#5da13b", "#a559b3", "#0b4512", "#61a3dc", 
                              "#26496d", "#1fa198"), labels=habitat_names_ordered) +
  gghighlight(use_direct_label = T)

habitatplot + facet_wrap(~habitat, nrow=4) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )
ggsave(filename='FloralAreaSite_AllPlants_HabitatMean.svg', 
       device='svg', path='./figures', height=8, width=8)

######################################################################################

##### insect pollinated floral area
all2 <-  tidyr::pivot_longer(siteIP, cols= c(-Site, -habitat), names_to = 'DOY', 
                            values_to = 'FloralArea') %>%
  dplyr::mutate(DOY = as.integer(substr(DOY, start = 2, stop=10))) %>%
  dplyr::filter(DOY > 60  & Site %in% land$Site)  %>%
  dplyr::mutate(habitat = factor(habitat, levels= c('Edge', "MesicUpRem", "ForestFlood",  "OldField",
                                                    "Ditch", "OrchardFloor", "Veg")))


meanhab2 <-  tidyr::pivot_longer(siteIP, cols= c(-Site, -habitat), names_to = 'DOY', 
                                values_to = 'FloralArea') %>%
  dplyr::mutate(DOY = as.integer(substr(DOY, start = 2, stop=10))) %>%
  dplyr::filter(DOY > 60 & Site %in% land$Site) %>%
  dplyr::group_by(habitat, DOY) %>%
  dplyr::summarise(FloralArea = mean(FloralArea)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(habitat = factor(habitat, levels=
    c('MesicUpRem', 'ForestFlood', 'Edge', 'OldField', 'Ditch', 'Veg', 'OrchardFloor')))

levels(meanhab2$habitat) <- habitat_names_ordered

# allplot <- ggplot(all2, aes(x=DOY, y=FloralArea, color=habitat)) + geom_line() + 
#   theme_minimal() + labs(y='Site floral area, insect pollinated (m2/ha)', 
#                          x= 'Day of year') +
#   theme_classic(base_size = 16) +
#   scale_color_manual(values= c("#651669", "#5da13b", "#a559b3", "#0b4512", "#61a3dc", 
#                                "#26496d", "#1fa198")) +
#   gghighlight()
# 
# allplot + facet_wrap(~habitat, nrow=4) +
#   theme(
#     strip.background = element_blank(),
#     strip.text.x = element_blank()
#   )
# 
# ggsave(filename='FloralAreaSite_InsectPollinated_byHabitat.svg', device='svg', path='./figures', height=8, width=8)


habitatplot_ip <- ggplot(meanhab2, aes(x=DOY, y=FloralArea, color=habitat, fill=habitat)) + geom_line() + 
  geom_polygon() + theme_classic() + 
  labs(y='Mean floral area (insect pollinated), site (m2/ha)', x= 'Day of year') +
  theme_classic(base_size = 16) +
  scale_fill_manual(values= c("#651669", "#5da13b", "#a559b3", "#0b4512", "#61a3dc", 
                              "#26496d", "#1fa198")) +
  scale_color_manual(values= c("#651669", "#5da13b", "#a559b3", "#0b4512", "#61a3dc", 
                               "#26496d", "#1fa198")) +
  gghighlight(use_direct_label = T)

habitatplot_ip + facet_wrap(~habitat, nrow=4) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )
ggsave('./figures/supplementary/FloralAreaSite_InsectPollinated_HabitatMean.svg', 
       height=8, width=8)
