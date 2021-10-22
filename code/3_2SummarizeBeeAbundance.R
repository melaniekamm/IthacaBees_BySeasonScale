rm(list=ls())

library(dplyr)

#read in bee data
raw <- read.csv("./data/bee_specimens/IthacaBees_Final.csv") %>%
  dplyr::mutate(Site = gsub(Site, pattern = 'Blueheron', replacement = 'BlueHeron', fixed=T))

raw$Year <- as.factor(raw$Year)
summary(raw)

table(raw$Year, raw$Season)
which(table(raw$SpecimenID) > 1)

#correct specimen IDs 297, 369 and 516
raw[raw$SpecimenID %in% c(297, 369, 516),]

#read in missing bowls data
missing <- read.csv('./data/missing_bowls/emptybowls_bothyears_cleaned.csv')

names(missing) <- c('Site', 'date_set', 'missing', 'remaining', 'color_missing', 
                    'notes', 'Season', 'Year')

#add number of sampling days (14 in spring and 7 in summer)
missing <- dplyr::mutate(missing, Site= bpa::trim_ws(Site),
                         days = if_else(Season == 'spring', 14, 7),
                         Year = as.factor(Year)) %>%
  dplyr::select(-notes) %>%
  dplyr::rename(trapsmissing= missing, trapsremaining=remaining)


#summarize bee community into abundance per site (adjusted by sampling effort)
beeabund <- dplyr::group_by(raw, Site, Year, Habitat, Season) %>%
  dplyr::summarise(Abundance = n())  %>%
  tidyr::replace_na(list("Abundance" = 0)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(SiteName= Site, Site = paste(Habitat, Site, sep=" ")) %>%
  dplyr::full_join(missing) %>%
  dplyr::mutate(AbundDayTrap = Abundance/trapsremaining/days, AbundTrap = Abundance/trapsremaining) %>%
  tidyr::replace_na(list("Abundance" = 0, "AbundDayTrap" = 0, 'AbundTrap'  = 0)) %>%
  dplyr::select(Site, Year, Season, date_set, Abundance, AbundTrap, AbundDayTrap)
write.csv(beeabund, './data/analysis_datasets/bee_abundance_per_seasonyearsite.csv', row.names = F)


#summarize bee community into abundance per site (adjusted by sampling effort)
beeabund_byspecies <- dplyr::group_by(raw, Site, Year, Habitat, Season, name) %>%
  dplyr::summarise(Abundance = n())  %>%
  tidyr::replace_na(list("Abundance" = 0)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(SiteName= Site, Site = paste(Habitat, Site, sep=" ")) %>%
  dplyr::full_join(missing) %>%
  dplyr::mutate(AbundDayTrap = Abundance/trapsremaining/days, AbundTrap = Abundance/trapsremaining) %>%
  tidyr::replace_na(list("Abundance" = 0, "AbundDayTrap" = 0, 'AbundTrap'  = 0)) %>%
  dplyr::select(Site, Year, Season, date_set, name, Abundance, AbundTrap, AbundDayTrap)

write.csv(beeabund_byspecies, './data/analysis_datasets/bee_abundance_per_speciesseasonyearsite.csv', row.names = F)


#### Add landscape and site metrics to bee abundance and richness data
beerich <- read.csv('./data/analysis_datasets/bee_richness.csv') %>%
  dplyr::mutate(Year = as.factor(Year))

names <- read.csv('./data/misc/SiteNamesKey.csv')
landmetrics <- read.csv('./data/analysis_datasets/All_landscape_predictors.csv')
sitemetrics <- read.csv('./data/analysis_datasets/All_site_predictors.csv')


rm(list= ls()[!(ls() %in% c('landmetrics','beeabund', 'names', 'sitemetrics', 'beerich'))])

beeabundrich <- dplyr::full_join(beeabund, names, by=c("Site" = 'bee_names')) %>%
  dplyr::left_join(beerich, by=c('landscape_names', 'Site', 'Year', 'Season')) %>%
  dplyr::full_join(landmetrics, by=c('landscape_names', 'Site')) %>%
  dplyr::full_join(sitemetrics, by=c('landscape_names', 'Site', 'habitat')) %>%
  dplyr::rename(Iverson_name = landscape_names)


#save full version for reference
write.csv(beeabundrich, './data/analysis_datasets/bee_abundance_richness_all_predictors.csv', row.names = F)
