rm(list=ls())

names <- read.csv('./data/SiteNamesKey.csv')

#all plants floral area
land <- read.csv('./data/Iverson_plant/LFA/landscape_floral_area_distweighted_MeanArea_allplants.csv') %>%
  dplyr::mutate(Site = gsub(gsub(Site, pattern=" ", replacement = "_", fixed=T),
                            pattern='Black_Diamond', replacement = "Merwin", fixed=T))%>%
        dplyr::left_join(names, by=c('Site' = 'landscape_names')) %>%
        dplyr::mutate(Site = bee_names) %>%
        dplyr::select(-bee_names)


# land <- read.csv('./data/Iverson_plant/allplants/floral_area_by_site_by_day.csv') %>%
#   dplyr::mutate(Site = gsub(gsub(Site, pattern=" ", replacement = "_", fixed=T),
#                             pattern='Black_Diamond', replacement = "Merwin", fixed=T))%>%
#   dplyr::left_join(names, by=c('Site' = 'landscape_names')) %>%
#   dplyr::mutate(Site = bee_names) %>%
#   dplyr::select(-bee_names, -habitat)

site <- read.csv('./data/Iverson_plant/allplants/floral_area_by_site_by_day.csv')
siteIP <- read.csv('./data/Iverson_plant/insect_pollinated/floral_area_by_site_by_day.csv')

# meanIP <- dplyr::select(siteIP, -habitat, -Site) %>%
#           summarize_all(.funs = mean)

#high landscape quality = field power, field parsons
high <- dplyr::filter(land, Site %in% c('Field Power', 'Field Parsons')) %>%
        tidyr::pivot_longer(cols= -Site, names_to = 'DOY', values_to = 'FloralArea') %>%
        dplyr::mutate(DOY = as.integer(substr(DOY, start = 2, stop=10)),
                      landgroup= 'high-quality landscape')
                      

#med quality = flood genung, forest jones
med <- dplyr::filter(land, Site %in% c('Flood Genung', 'Forest Jones')) %>%
  tidyr::pivot_longer(cols= -Site, names_to = 'DOY', values_to = 'FloralArea') %>%
  dplyr::mutate(DOY = as.integer(substr(DOY, start = 2, stop=10)),
                landgroup= 'medium-quality landscape')


#low quality = apple merwin, ditch cornell veg

low <- dplyr::filter(land, Site %in% c('Apple Merwin', 'Ditch CornellVeg')) %>%
  tidyr::pivot_longer(cols= -Site, names_to = 'DOY', values_to = 'FloralArea') %>%
  dplyr::mutate(DOY = as.integer(substr(DOY, start = 2, stop=10)),
                landgroup= 'low-quality landscape')

all <- dplyr::full_join(high, med) %>%
          dplyr::full_join(low) %>%
          dplyr::mutate(landgroup = factor(landgroup, levels=c('low-quality landscape', 
                                                               'medium-quality landscape', 
                                                               'high-quality landscape')),
                        Site = factor(Site, levels=c('Apple Merwin', 'Ditch CornellVeg',
                                                     'Flood Genung', 'Forest Jones',
                                                     'Field Power', 'Field Parsons')))


lowplot <- ggplot(all, aes(x=DOY, y=FloralArea, color=Site)) + geom_line(size=1.3) + 
  ylim(c(0,3050)) + theme_minimal() + labs(y='Landcape floral area, all plants (m2/ha)', 
                                           x= 'Day of year') +
  theme(strip.text.x = element_text(size = 10, face = "bold.italic")) + 
  scale_color_manual(values=c("#56ebd3", "#323d96", "#afe642", "#8c46d0", "#6c9f30", "#074d65"))

lowplot + facet_wrap(~landgroup)

##### insect pollinated floral area

land <- read.csv('./data/Iverson_plant/LFA/landscape_floral_area_distweighted_MeanArea_insectpollinated.csv') %>%
  dplyr::mutate(Site = gsub(gsub(Site, pattern=" ", replacement = "_", fixed=T),
                            pattern='Black_Diamond', replacement = "Merwin", fixed=T))%>%
  dplyr::left_join(names, by=c('Site' = 'landscape_names')) %>%
  dplyr::mutate(Site = bee_names) %>%
  dplyr::select(-bee_names)


# land <- read.csv('./data/Iverson_plant/allplants/floral_area_by_site_by_day.csv') %>%
#   dplyr::mutate(Site = gsub(gsub(Site, pattern=" ", replacement = "_", fixed=T),
#                             pattern='Black_Diamond', replacement = "Merwin", fixed=T))%>%
#   dplyr::left_join(names, by=c('Site' = 'landscape_names')) %>%
#   dplyr::mutate(Site = bee_names) %>%
#   dplyr::select(-bee_names, -habitat)

site <- read.csv('./data/Iverson_plant/allplants/floral_area_by_site_by_day.csv')
siteIP <- read.csv('./data/Iverson_plant/insect_pollinated/floral_area_by_site_by_day.csv')

meanIP <- dplyr::select(siteIP, -habitat, -Site) %>%
  summarize_all(.funs = mean)

#high landscape quality = field power, field parsons
high <- dplyr::filter(land, Site %in% c('Field Power', 'Field Parsons')) %>%
  tidyr::pivot_longer(cols= -Site, names_to = 'DOY', values_to = 'FloralArea') %>%
  dplyr::mutate(DOY = as.integer(substr(DOY, start = 2, stop=10)),
                landgroup= 'high-quality landscape')


#med quality = flood genung, forest jones
med <- dplyr::filter(land, Site %in% c('Flood Genung', 'Forest Jones')) %>%
  tidyr::pivot_longer(cols= -Site, names_to = 'DOY', values_to = 'FloralArea') %>%
  dplyr::mutate(DOY = as.integer(substr(DOY, start = 2, stop=10)),
                landgroup= 'medium-quality landscape')


#low quality = apple merwin, ditch cornell veg

low <- dplyr::filter(land, Site %in% c('Apple Merwin', 'Ditch CornellVeg')) %>%
  tidyr::pivot_longer(cols= -Site, names_to = 'DOY', values_to = 'FloralArea') %>%
  dplyr::mutate(DOY = as.integer(substr(DOY, start = 2, stop=10)),
                landgroup= 'low-quality landscape')

all <- dplyr::full_join(high, med) %>%
  dplyr::full_join(low) %>%
  dplyr::mutate(landgroup = factor(landgroup, levels=c('low-quality landscape', 
                                                       'medium-quality landscape', 
                                                       'high-quality landscape')),
                Site = factor(Site, levels=c('Apple Merwin', 'Ditch CornellVeg',
                                             'Flood Genung', 'Forest Jones',
                                             'Field Power', 'Field Parsons')))


lowplot <- ggplot(all, aes(x=DOY, y=FloralArea, color=Site)) + geom_line(size=1.3) + 
  ylim(c(0,3050)) + theme_minimal() + labs(y='Landcape floral area, insect pollinated (m2/ha)', 
                                           x= 'Day of year') +
  theme(strip.text.x = element_text(size = 10, face = "bold.italic")) + 
  scale_color_manual(values=c("#56ebd3", "#323d96", "#afe642", "#8c46d0", "#6c9f30", "#074d65"))

lowplot + facet_wrap(~landgroup)


