rm(list=ls())
library(dplyr)

# final analysis uses allvar = T and centerscale = F
centerscale <- F

if (centerscale == T) {
all_VI <- read.csv('./data/RF_results/LandYearSite_VariableImportance_AllVariables_CenterScale.csv') %>%
          dplyr::select(-X)
} else {
all_VI <- read.csv('./data/RF_results/LandYearSite_VariableImportance_AllVariables.csv') %>%
          dplyr::select(-X)
}

if ('response_var' %in% names(all_VI)) {
  all_VI <- dplyr::mutate(all_VI, response = gsub(response_var, pattern='AbundDayTrap', replacement="abundance")) %>%
            dplyr::select(-response_var)
}

bestvar <- dplyr::group_by(all_VI, season, response) %>%
          dplyr::mutate(Quan95 = quantile(importance, probs=(0.90)),
                        rank = dense_rank(desc(importance))) %>%
          dplyr::ungroup() %>%
          dplyr::group_by(variable) %>%
          #dplyr::mutate(IsImportant = any(importance > Quan95)) %>%
          dplyr::mutate(IsImportant = any(rank <= 6)) %>%
          dplyr::ungroup() %>%
          dplyr::filter(IsImportant == T) %>%
          dplyr::mutate(Scale = dplyr::if_else(grepl(variable, pattern='.land')|
                                                 grepl(variable, pattern="PctLand")|
                                                 grepl(variable, pattern='distance')|
                                                 variable == 'ed'|
                                                 grepl(variable, pattern='aspect')|
                                                 grepl(variable, pattern='elevation'), 'Landscape', 'Site')) %>%
          dplyr::mutate(Scale = if_else(variable == 'Year', 'Year', Scale)) %>%
          dplyr::mutate(Longname = gsub(gsub(gsub(gsub(gsub(gsub(gsub(gsub(gsub(gsub(gsub(
                                   gsub(gsub(gsub(gsub(gsub(gsub(gsub(gsub(gsub(variable, pattern='_FA', 
                                                       replacement=' floral area', fixed=T),
                        pattern=".IP", replacement=' (insect-pollinated)', fixed=T),
                        pattern=".all", replacement = " (all plants)", fixed=T),
                        pattern= 'PctLand_', replacement = "Percent ", fixed=T),
                        pattern= "_", replacement=" ", fixed=T),
                        pattern='Grav', replacement='Soil gravimetric', fixed=T),
                        pattern='g.g mean', replacement="", fixed=T),
                        pattern="Pct mean", replacement="", fixed=T),
                        pattern="P ppm mean", replacement="Soil phosphorus", fixed=T),
                        pattern="K ppm mean", replacement='Soil potassium', fixed=T),
                        pattern=".land", replacement=", landscape", fixed=T),
                        pattern=".site", replacement=', local', fixed=T),
                        pattern="mean", replacement="", fixed=T),
                        pattern="WaterContent", replacement='water content', fixed=T),
                        pattern="OM", replacement="Soil organic matter", fixed=T),
                        pattern="pct cover", replacement='Percent plant cover', fixed=T),
                        pattern="Total N", replacement='Soil total nitrogen', fixed=T),
                        pattern="bulk density", replacement='Soil bulk density', fixed=T),
                        pattern='richness', replacement='plant richness', fixed=T),
                        pattern="NMDS mmt", replacement='Plant composition, mmt intensity', fixed=T)) %>%
        dplyr::mutate(Longname = if_else(Longname == 'ed', 'Edge density', as.character(Longname))) %>%
        dplyr::mutate(Longname = Hmisc::capitalize(trimws(Longname))) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(season = if_else(season == 'spring', 'early', 
                                       if_else(season == 'summer', 'late', "")))


# make variable to group year, landscape, and local variables together
o2 <- c(unique(bestvar$Longname[bestvar$Scale == 'Year']), unique(bestvar$Longname[bestvar$Scale == 'Landscape']),
  unique(bestvar$Longname[bestvar$Scale == 'Site']))

bestvar$variable[bestvar$season == 'early' & bestvar$response == 'abundance' & bestvar$rank <= 5]

bestvar$Longname <- factor(bestvar$Longname, levels=rev(o2)) # define factor levels to order variables on plot

library(ggplot2)

#ggplot version of coefficient matrix
t <- ggplot(bestvar, aes(response, Longname, z= importance)) + geom_tile(aes(fill = importance)) +
  scale_fill_gradient2(low="#DEEBF7", high="#3182BD", limits=c(0, 100)) +
  theme_classic(base_size=17) +
  ylab('') +
  xlab('') +
  theme(axis.text.x = element_text(angle=30, hjust=1, color='black'))

t  + ggplot2::facet_wrap(~season)

if (centerscale == T) {
  ggsave(path='./figures/', device='svg', filename='VariableImportance_AllVariables_CenterScale.svg', height=8, width=8.75)
} else {
  ggsave(path='./figures/', device='svg', filename='VariableImportance_AllVariables.svg', height=8, width=8.75)
  
}

