# prep data for archiving (adding some meta-data as columns in the dataset, removing site names etc)

library(dplyr)

# read dataset used for random forest
alldata <- read.csv('./data/analysis_datasets/bee_abundance_richness_all_predictors.csv')

# add information on sampling effort
alldata <- dplyr::mutate(alldata, NTrapsSuccessful = Abundance/AbundTrap, NDaysTrapsSet=AbundTrap/AbundDayTrap)

# translate site names to less identifiable strings (first two sections of Iverson et al names)
site_pieces <- data.frame(stringr::str_split_fixed(alldata$Iverson_name, pattern="_", n=3))

alldata <- dplyr::mutate(alldata, SiteName = paste0(site_pieces[,1], "_", site_pieces[,2]))

# reorganize columns to more logical order

alldata_final <- dplyr::select(alldata, SiteName, Year, Season, date_set, NTrapsSuccessful, NDaysTrapsSet, 
                               Abundance, AbundTrap, AbundDayTrap, richness:summer_total_FA.IP.site) %>%
  dplyr::rename(richness.plants.all = richness.all, richness.plants.IP = richness.IP)

names(alldata_final)

write.csv(alldata_final, "./data/analysis_datasets/bee_abundance_richness_all_predictors_toarchive.csv", row.names = F)
