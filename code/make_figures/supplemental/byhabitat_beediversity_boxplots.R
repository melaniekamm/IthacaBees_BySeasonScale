rm(list=ls())

library(dplyr)

#read in bee data
raw <- read.csv("./data/bee_specimens/IthacaBees_Final.csv") %>%
  dplyr::mutate(Site = gsub(Site, pattern = 'Blueheron', replacement = 'BlueHeron', fixed=T)) %>%
  dplyr::filter(name != 'Apis mellifera') %>%
  dplyr::mutate(SiteName= Site, Site = paste(Habitat, Site, sep=" "))
  

raw$Year <- as.factor(raw$Year)

#correct specimen IDs 297, 369 and 516
raw[raw$SpecimenID %in% c(297, 369, 516),]

#read in missing bowls data
missing <- read.csv('./data/missing_bowls/emptybowls_bothyears_cleaned.csv')

names(missing) <- c('Site', 'date_set', 'date_collected', 'missing', 'remaining', 'color_missing', 
                         'notes', 'Season', 'Year')

#add number of sampling days (14 in spring and 7 in summer)
missing <- dplyr::mutate(missing, Site= bpa::trim_ws(Site),
                         days = if_else(Season == 'spring', 14, 7),
                         Year = as.factor(Year)) %>%
            dplyr::select(-notes)

rare_rich <- read.csv('../IthacaBees/data/analysis_datasets/bee_richness_coveragemeanALL_landscape_predictors.csv') %>%
          dplyr::mutate(Year = as.factor(Year)) %>%
          dplyr::select(-habitat)

temp <- dplyr::select(raw, Site, Habitat, Year, Season) %>%
            dplyr::mutate(ID = paste0(Site, Year, Season))%>%
            dplyr::filter(!duplicated(ID))

rare_rich <- dplyr::right_join(dplyr::select(temp, -ID), rare_rich) %>%
  dplyr::mutate(Habitat = if_else(Habitat == 'Edge', 'Forest edge',
                                  if_else(Habitat == 'Field', 'Old field',
                                          if_else(Habitat == 'Veg', 'Mixed vegetable', 
                                                  as.character(Habitat))))) %>%
  dplyr::mutate(Season = if_else(Season == 'spring', 'early', 'late'))

rm(temp)

sitestats <- dplyr::group_by(raw, Site, Year, Habitat, Season) %>%
            dplyr::summarise(Abundance = n(), Richness=length(unique(name)))  %>%
            dplyr::ungroup() %>%
            dplyr::full_join(missing) %>%
            dplyr::mutate(AbundTrapDay = Abundance/remaining/days) %>%
            dplyr::mutate(Habitat = if_else(Habitat == 'Edge', 'Forest edge',
                                           if_else(Habitat == 'Field', 'Old field',
                                                   if_else(Habitat == 'Veg', 'Mixed vegetable', 
                                                           as.character(Habitat))))) %>%
  dplyr::mutate(Season = if_else(Season == 'spring', 'early', 'late'))



library(ggplot2)
sitestats <- dplyr::filter(sitestats, !is.na(Abundance))

#bee abund/trap/day
a <- ggplot(sitestats, aes(x=Habitat, y=AbundTrapDay, fill=Season)) + 
  geom_boxplot() +   labs(y=expression(paste('Bee abundance ', "day"^-1, " trap"^-1))) +
  theme_classic(base_size=12) +
  theme(legend.position = 'none', axis.text.x=element_text(angle=30, hjust=1)) +
  #scale_fill_manual(values= c("#4f8c9d", "#a6e590")) 
  scale_fill_manual(values= c("#0072B2", "#E69F00")) 

a <- a + facet_wrap(~Season)

#ggsave(filename='Abund_byHabitat.svg', device='svg', path='./explore_bee_data_figures', width=13.5, height=10, unit='cm')

#measured bee richness
b <- ggplot(rare_rich, aes(x=Habitat, y=S.obs, fill=Season)) + 
  geom_boxplot() + labs(y= 'Measured bee species richness') + theme_classic(base_size=12) +
  theme(legend.position = 'none', axis.text.x=element_text(angle=30, hjust=1)) +
  #scale_fill_manual(values= c("#4f8c9d", "#a6e590")) 
  scale_fill_manual(values= c("#0072B2", "#E69F00")) 

b <- b + facet_wrap(~Season)

#ggsave(filename='ObservedRichness_byHabitat.svg', device='svg', path='./explore_bee_data_figures', width=13.5, height=10, unit='cm')

#rarified bee richness
c <- ggplot(rare_rich, aes(x=Habitat, y=richness, fill=Season)) + 
  geom_boxplot() + labs(y= 'Rarified bee species richness') + theme_classic(base_size=12) +
  theme(legend.position = 'none', axis.text.x=element_text(angle=30, hjust=1)) +
  #scale_fill_manual(values= c("#4f8c9d", "#a6e590")) 
  scale_fill_manual(values= c("#0072B2", "#E69F00")) 


c <- c + facet_wrap(~Season) 

#ggsave(filename='RarifiedRichness_byHabitat_CoverMeanAll.svg', device='svg', path='./explore_bee_data_figures', width=13.5, height=10, unit='cm')

a;b;c

# combine bar plots into one figure
plot_grid(a, c, ncol=1) + 
  draw_label("A)", color = "black", size = 18, x=0.03,y=0.97)  + 
  draw_label("B)", color = "black", size = 18, x=0.03, y=0.47)

ggsave(filename='Boxplots_BeeAbundRich_byHabitat.svg', device='svg', path='./figures/supplementary/', width=13.5, height=20, unit='cm')

