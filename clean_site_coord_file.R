

sites <- sf::st_read('Z:/PhD2015_2020/DissertationProjects/IthacaBees/logistics/DesignSampling/FINAL_Sites_Selected.shp') %>%
  dplyr::select(site, habitat, Lat, Long, LatLong) %>%
  dplyr::mutate(Iverson_site_name = gsub(gsub(site, pattern=" ", replacement = "_", fixed=T),
                                         pattern='Black_Diamond', replacement = "Merwin", fixed=T)) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(-site)

aaron_sites <- sf::st_read('Z:/PhD2015_2020/DissertationProjects/IthacaBees/logistics/DesignSampling/NYBeeSitesFinal/allsites_plantsurvey.shp')
head(sites)
names(sites)

names_key <- read.csv('Z:/PhD2015_2020/DissertationProjects/IthacaBees/data/SiteNamesKey.csv')
head(names_key)

sites <- dplyr::left_join(sites, names_key, by=c('Iverson_site_name' = 'landscape_names')) %>%
  dplyr::rename(Site = bee_names) %>%
  dplyr::select(Site, dplyr::everything())

write.csv(sites, './data/final_site_coords.csv', row.names = F)
