#stacked bar plot of bee abundance in different seasons and years
rm(list=ls())

library(dplyr)
#read in bee data
raw <- read.csv("./data/bee_specimens/IthacaBees_Final.csv") %>%
          dplyr::mutate(Site = gsub(Site, pattern = 'Blueheron', replacement = 'BlueHeron', fixed=T)) %>%
          dplyr::filter(name != 'Apis mellifera')

raw$Year <- as.factor(raw$Year)
summary(raw)

#nspecimens
length(raw$SpecimenID[raw$name != 'Apis mellifera'])

#nspecies
#Lasioglossum sp1 might be L. ephialtum, which is not in dataset, so count as distinct species
notonespecies <- c('Andrena dunningi/barbara', 'Apis mellifera', 'Hylaeus modestus group1', 
                   'Hylaeus modestus group2', 'Lasioglossum (Dialictus) sp', 'Lasioglossum species')

length(unique(raw$name[!raw$name %in% notonespecies]))
length(unique(raw$genus[!raw$name %in% notonespecies]))

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

#####plot community composition by season

#summarize bee community into abundance per site (adjusted by sampling effort)
beeabund <- dplyr::group_by(raw, Site, Year, Habitat, Season, genus) %>%
          dplyr::summarise(Abundance = n())  %>%
          tidyr::replace_na(list("Abundance" = 0)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(SiteName= Site, Site = paste(Habitat, Site, sep=" ")) %>%
          dplyr::left_join(missing) %>%
          dplyr::mutate(AbundDayTrap = Abundance/trapsremaining/days, AbundTrap = Abundance/trapsremaining) %>%
          tidyr::replace_na(list("Abundance" = 0, "AbundDayTrap" = 0, 'AbundTrap'  = 0)) %>%
          dplyr::select(Site, Year, Season, genus, Abundance, AbundTrap, AbundDayTrap) 

tocombine <- dplyr::group_by(beeabund, Season, genus) %>%
          dplyr::summarise(Abundance = sum(Abundance), AbundDayTrap = sum(AbundDayTrap)) %>%
          dplyr::ungroup() %>%
          dplyr::group_by(Season) %>%
          dplyr::mutate(RelAbund = (Abundance/sum(Abundance))*100, 
                        RelAbundDayTrap = (AbundDayTrap/sum(AbundDayTrap))*100) %>%
          dplyr::filter(RelAbundDayTrap < 3) %>%
          dplyr::summarise_if(is.numeric, sum) %>%
          dplyr::mutate(genus = 'Other')

relabund <- dplyr::group_by(beeabund, Season, genus) %>%
          dplyr::summarise(Abundance = sum(Abundance), AbundDayTrap = sum(AbundDayTrap)) %>%
          dplyr::ungroup() %>%
          dplyr::group_by(Season) %>%
          dplyr::mutate(RelAbund = (Abundance/sum(Abundance))*100, 
                        RelAbundDayTrap = (AbundDayTrap/sum(AbundDayTrap))*100) %>%
          dplyr::filter(RelAbundDayTrap >= 3) %>%
          dplyr::full_join(tocombine) 

#create all Genus/Date combinations to populate with zeros (necessary for plot)
all <- expand.grid(genus=unique(relabund$genus), Season=unique(relabund$Season))

#merge with actual data
toplot <- dplyr::full_join(relabund, all) %>%
          tidyr::replace_na(list(AbundDayTrap = 0, Abundance = 0,
                                 RelAbund=0, RelAbundDayTrap=0)) %>%
          dplyr::arrange(Season) %>%
          mutate(SeasonOrd = dplyr::if_else(Season == 'spring', 1, 2)) %>%
          mutate(genus = factor(genus, levels= c('Andrena', 'Osmia', 'Nomada','Ceratina','Other',
                                                 'Lasioglossum', 'Agapostemon','Augochlora', 'Halictus', 'Peponapis', 'Melissodes')))


ggplot(toplot, aes(x=SeasonOrd, y=AbundDayTrap, fill=genus)) + geom_area() + labs(x="", y=expression(paste('Total bee abundance ', "day"^-1, " trap"^-1))) +
  scale_fill_manual(values= c("#b4ddd4", "#154e56", "#7feb90", "#8a2a7b", "#b2b2f9", "#1945c5", 
                               "#2eece6", "#673d17", "#0ca82e", "#e30293", "#208eb7"), name='Genus') + 
  scale_x_continuous(breaks=c(1,2), labels=c('Spring', 'Summer'), expand=c(0.04,0)) +
  scale_y_continuous(expand=c(0.02,0)) +
  theme_classic(base_size=13) +
  theme(axis.text.x=element_text(size=14, face='bold')) 

ggsave(filename='Abund_bySeason.svg', device='svg', path= './explore_bee_data_figures/', width=10.5, height=15, 
       units='cm', dpi='retina')

#####plot community composition by year

#summarize bee community into abundance per site (adjusted by sampling effort)
beeabund <- dplyr::group_by(raw, Site, Year, Habitat, Season, genus) %>%
  dplyr::summarise(Abundance = n())  %>%
  tidyr::replace_na(list("Abundance" = 0)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(SiteName= Site, Site = paste(Habitat, Site, sep=" ")) %>%
  dplyr::left_join(missing) %>%
  dplyr::mutate(AbundDayTrap = Abundance/trapsremaining/days, AbundTrap = Abundance/trapsremaining) %>%
  tidyr::replace_na(list("Abundance" = 0, "AbundDayTrap" = 0, 'AbundTrap'  = 0)) %>%
  dplyr::select(Site, Year, Season, genus, Abundance, AbundTrap, AbundDayTrap) 

tocombine <- dplyr::group_by(beeabund, Year, genus) %>%
  dplyr::summarise(Abundance = sum(Abundance), AbundDayTrap = sum(AbundDayTrap)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Year) %>%
  dplyr::mutate(RelAbund = (Abundance/sum(Abundance))*100, 
                RelAbundDayTrap = (AbundDayTrap/sum(AbundDayTrap))*100) %>%
  dplyr::filter(RelAbundDayTrap < 3) %>%
  dplyr::summarise_if(is.numeric, sum) %>%
  dplyr::mutate(genus = 'Other')

relabund <- dplyr::group_by(beeabund, Year, genus) %>%
  dplyr::summarise(Abundance = sum(Abundance), AbundDayTrap = sum(AbundDayTrap)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Year) %>%
  dplyr::mutate(RelAbund = (Abundance/sum(Abundance))*100, 
                RelAbundDayTrap = (AbundDayTrap/sum(AbundDayTrap))*100) %>%
  dplyr::filter(RelAbundDayTrap >= 3) %>%
  dplyr::full_join(tocombine) 

#create all Genus/Date combinations to populate with zeros (necessary for plot)
all <- expand.grid(genus=unique(relabund$genus), Year=unique(relabund$Year))

#merge with actual data
toplot <- dplyr::full_join(relabund, all) %>%
  tidyr::replace_na(list(AbundDayTrap = 0, Abundance = 0,
                         RelAbund=0, RelAbundDayTrap=0)) %>%
  dplyr::arrange(Year) %>%
  mutate(YearOrd = dplyr::if_else(Year == '2018', 1, 2)) %>%
  mutate(genus = factor(genus, levels= c('Andrena', 'Osmia', 'Nomada','Ceratina','Other',
                                         'Lasioglossum', 'Agapostemon','Augochlora', 'Halictus', 'Peponapis', 'Melissodes')))


ggplot(toplot, aes(x=YearOrd, y=AbundDayTrap, fill=genus)) + geom_area() + 
  labs(x="", y=expression(paste('Total bee abundance ', "day"^-1, " trap"^-1))) +
  scale_fill_manual(values= c("#b4ddd4", "#154e56", "#7feb90", "#8a2a7b", "#b2b2f9", "#1945c5", 
                              "#2eece6", "#673d17", "#0ca82e", "#e30293", "#208eb7"), name='Genus') + 
  scale_x_continuous(breaks=c(1,2), labels=c('2018', '2019'), expand=c(0.04,0)) +
  scale_y_continuous(expand=c(0.02,0)) +
  theme_classic(base_size=13) +
  theme(axis.text.x=element_text(size=14, face='bold')) 

ggsave(filename='Abund_byYear.svg', device='svg', path= './explore_bee_data_figures/', width=10.5, height=15,
       units='cm', dpi='retina')


