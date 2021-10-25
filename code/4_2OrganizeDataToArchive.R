# prep data for archiving (adding some meta-data as columns in the dataset, removing site names etc)

library(dplyr)

# read dataset used for random forest
alldata <- read.csv('./data/analysis_datasets/bee_abundance_richness_all_predictors.csv')

# add information on sampling effort
alldata <- dplyr::mutate(alldata, NDaysTrapsSet=if_else(Season == 'spring', 14, 7),
  Year = as.character(Year))

effort <- read.csv('./data/missing_bowls/emptybowls_bothyears_cleaned.csv') %>%
  dplyr::rename(MissingEmptyBowls = tidyr::starts_with('X._Missing'), NTrapsSuccessful=X.Remaining) %>%
  dplyr::mutate(Year = as.character(Year), Site = stringr::str_trim(Site))

alldata <- dplyr::full_join(alldata, dplyr::select(effort, Site, Year, Season, NTrapsSuccessful), alldata, 
                             by=c('Site', 'Year', 'Season'))


# translate site names to less identifiable strings (first two sections of Iverson et al names)
site_pieces <- data.frame(stringr::str_split_fixed(alldata$Iverson_name, pattern="_", n=3))

alldata <- dplyr::mutate(alldata, SiteName = paste0(site_pieces[,1], "_", site_pieces[,2]))

# reorganize columns to more logical order

alldata_final <- dplyr::select(alldata, SiteName, Year, Season, date_set, NTrapsSuccessful, NDaysTrapsSet, 
                               Abundance, AbundTrap, AbundDayTrap, richness:summer_total_FA.IP.site) %>%
  dplyr::rename(richness.plants.all = richness.all, richness.plants.IP = richness.IP) %>%
  dplyr::mutate(S.obs = if_else(is.na(S.obs), 0, as.numeric(S.obs)), #convert S.obs to 0 if abundance is 0
  richness = if_else(is.na(richness), "", as.character(richness)))

names(alldata_final)

write.csv(alldata_final, "./data/analysis_datasets/bee_abundance_richness_all_predictors_toarchive.csv", row.names = F)
