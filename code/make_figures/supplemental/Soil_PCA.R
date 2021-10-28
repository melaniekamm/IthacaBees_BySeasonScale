rm(list=ls())
library(dplyr)
library(vegan)

library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

soil_by_site <- read.csv('./data/soil/AllSoilVariables_by_site_unaggregated.csv')
soil_site_means <- read.csv('./data/soil/AllSoilVariables_by_site.csv')



#center and standardize soil variables
soil.c <- dplyr::mutate(soil_by_site, across(where(is.double), scale))


soil.pca <- prcomp(dplyr::select(soil_by_site, -site, -habitat, -sub_sample), center=T, scale = T) 
summary(soil.pca)

soil.pca$rotation
ggbiplot(soil.pca, groups=soil.c$habitat, labels = soil.c$Site, obs.scale=T)

ordiview <- ggbiplot(soil.pca, groups=soil.c$habitat, ellipse=T, obs.scale=T, varname.size=4, var.axes = F) + 
  theme_classic(base_size=16) +
  ylim(c(-5, 5)) +
  labs(color= 'habitat type') +
  scale_color_manual(values=c("#35618f", "#87d6bc", "#0b5313", "#9fd841", "#6f2b6e", "#54b2fc", "#5336c5"))
ordiview

# ggplot2::ggsave(plot=ordiview, device='svg', path='./figures/PCA_soils', filename='ordiplot_soilPCA_ellipses.svg',
#                 width=8.2, height=5.73)

ordiview2 <- ggbiplot(soil.pca, ellipse=F, obs.scale=T, varname.size=4, var.axes = T, groups=soil.c$habitat) + 
  theme_classic(base_size=16) +
  ylim(c(-5, 5)) +
  labs(color= 'habitat type') +
  scale_color_manual(values=c('#989898', '#989898', '#989898', '#989898', '#989898', '#989898', '#989898')) #+ 
  #theme(legend.position = "none")
ordiview2

# ggplot2::ggsave(plot=ordiview2, device='svg', path='./figures/PCA_soils', filename='ordiplot_soilPCA_variables.svg',
#                 width=8.2, height=5.73)
