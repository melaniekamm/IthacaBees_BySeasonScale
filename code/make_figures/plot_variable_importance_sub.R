#rm(list=ls())
library(dplyr)

centerscale <- T

if (centerscale == T) {
  all_VI <- read.csv('./data/RF_results/LandYearSite_VariableImportance_SubsetVariables_CenterScale.csv') %>%
    dplyr::select(-X)
} else {
  all_VI <- read.csv('./data/RF_results/LandYearSite_VariableImportance_SubsetVariables.csv') %>%
    dplyr::select(-X)
}

if ('response_var' %in% names(all_VI)) {
  all_VI <- dplyr::mutate(all_VI, response = gsub(response_var, pattern='AbundDayTrap', replacement="abundance")) %>%
    dplyr::select(-response_var)
}

if (centerscale == T) {
  p <- 0.8
} else if (centerscale == F) {
  p <- 0.8
}

bestvar <- dplyr::group_by(all_VI, season, response) %>%
          dplyr::mutate(Quan95 = quantile(importance, probs=(p))) %>%
          dplyr::ungroup() %>%
          dplyr::group_by(variable) %>%
          dplyr::mutate(IsImportant = any(importance > Quan95)) %>%
          dplyr::ungroup() %>%
          dplyr::filter(IsImportant == T) %>%
          dplyr::mutate(Scale = dplyr::if_else(grepl(variable, pattern='.land')|
                                                 grepl(variable, pattern="PctLand")|
                                                 grepl(variable, pattern='distance')|
                                                 grepl(variable, pattern='elevation'), 'Landscape', 'Site')) %>%
          dplyr::mutate(Scale = if_else(variable == 'Year', 'Year', Scale)) %>%
          dplyr::mutate(Longname = gsub(gsub(gsub(gsub(gsub(gsub(gsub(gsub(gsub(gsub(gsub(gsub(
                                   gsub(gsub(gsub(gsub(gsub(gsub(gsub(gsub(gsub(variable, pattern='_FA', 
                                                       replacement=' floral area', fixed=T),
                        pattern=".IP", replacement=' (IP plants)', fixed=T),
                        pattern=".all", replacement = " (all plants)", fixed=T),
                        pattern= 'PctLand_', replacement = "Percent ", fixed=T),
                        pattern= "_", replacement=" ", fixed=T),
                        pattern='Grav', replacement='Soil gravimetric', fixed=T),
                        pattern='g.g mean', replacement="", fixed=T),
                        pattern="Pct mean", replacement="", fixed=T),
                        pattern="P ppm mean", replacement="Soil phosphorus", fixed=T),
                        pattern="K ppm mean", replacement='Soil potassium', fixed=T),
                        pattern=".land", replacement=", landscape", fixed=T),
                        pattern=".site", replacement=', site', fixed=T),
                        pattern="mean", replacement="", fixed=T),
                        pattern="WaterContent", replacement='water content', fixed=T),
                        pattern="OM", replacement="Soil organic matter", fixed=T),
                        pattern="pct cover", replacement='Percent plant cover', fixed=T),
                        pattern="Total N", replacement='Soil total nitrogen', fixed=T),
                        pattern="bulk density", replacement='Soil bulk density', fixed=T),
                        pattern='richness', replacement='plant richness', fixed=T),
                        pattern='enn cv', replacement='Landscape aggregation (CV patch distance)', fixed=T),
                        pattern="NMDS mmt", replacement='Plant composition, mmt intensity', fixed=T)) %>%
    dplyr::mutate(Longname = if_else(Longname == 'ed', 'Edge density', Longname)) %>%
        dplyr::mutate(Longname = Hmisc::capitalize(trimws(Longname))) %>%
        dplyr::ungroup()

if (centerscale == T) {
  o2 <- c("Year", "Elevation", "AspectNS", "Distance to water", 
          "Percent Agriculture", "Percent Developed", "Percent Water", 
          "Percent Wetland", "Percent Natural",
          "Landscape aggregation (CV patch distance)",
          "Soil gravimetric water content", "Soil phosphorus", "Soil potassium","Soil total nitrogen",
          "Soil organic matter", "Soil bulk density", 
          "Plant composition, mmt intensity",  "Plant richness (IP plants)",
          "Percent plant cover", "Fall total floral area (all plants), site")
} else if (centerscale == F) {
  o2 <- c("Year", "Elevation",  "AspectNS", "Distance to water", 
          "CV floral area (all plants), landscape",
          "Percent Agriculture", "Percent Developed", "Percent Water", 
          "Percent Wetland", "Percent Natural", 
          "Soil gravimetric water content", "Soil phosphorus", "Soil potassium", "Soil total nitrogen",
          "Soil organic matter", "Soil bulk density", 
          "Plant composition, mmt intensity",  "Plant richness (IP plants)",
          "Percent plant cover", "Fall total floral area (all plants), site",
          "Spring total floral area (all plants), site")
  
}

length(unique(bestvar$Longname))
unique(bestvar$Longname)

bestvar$Longname <- factor(bestvar$Longname, levels=rev(o2))

library(ggplot2); library(gghighlight)

#ggplot version of coefficient matrix
t <- ggplot(bestvar, aes(response, Longname, z= importance)) + geom_tile(aes(fill = importance)) +
  scale_fill_gradient2(low="#DEEBF7", high="#3182BD", limits=c(0, 100)) +
  theme_classic(base_size=17) +
  ylab('') +
  xlab('') +
  #ggtitle(paste0('CenterScale = ', centerscale,", All = FALSE")) +
  theme(axis.text.x = element_text(angle=30, hjust=1, color='black', size=14),
        axis.text.y = element_text(size=14))
  

t  + ggplot2::facet_wrap(~season)

if (centerscale == T) {
  ggsave(path='./figures/', device='svg', filename='VariableImportance_SubsetVariables_CenterScale.svg', height=7.9, width=9)
} else {
  ggsave(path='./figures/', device='svg', filename='VariableImportance_SubsetVariables.svg', height=7.9, width=9)
  
}
