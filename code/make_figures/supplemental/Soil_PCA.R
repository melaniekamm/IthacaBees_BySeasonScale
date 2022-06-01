rm(list=ls())
library(vegan)

library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

soil_by_site <- read.csv('./data/soil/AllSoilVariables_by_site_unaggregated.csv')
soil_site_means <- read.csv('./data/soil/AllSoilVariables_by_site.csv')

habitat_names_ordered <- c('Mesic upland remnant forest', 'Floodplain forest', 'Forest edge', 'Old field', 'Roadside ditch', 'Mixed vegetable farm', 'Apple orchard')

#center and standardize soil variables
soil.c <- dplyr::mutate(soil_by_site, across(where(is.double), scale)) %>%
  dplyr::mutate(habitat = factor(habitat, levels=c('Forest', 'Flood', 'Edge', 'Field', 'Ditch', 'Veg', 'Apple')))


soil.pca <- prcomp(dplyr::select(soil_by_site, -site, -habitat, -sub_sample), center=T, scale = T) 
summary(soil.pca)

soil.pca$rotation
ggbiplot(soil.pca, groups=soil.c$habitat, labels = soil.c$Site, obs.scale=T)

ordiview <- ggbiplot(soil.pca, groups=soil.c$habitat, ellipse=T, obs.scale=T, varname.size=4, var.axes = F) + 
  theme_classic(base_size=14) +
  ylim(c(-5, 5)) +
  scale_color_manual(values=c("#35618f", "#87d6bc", "#0b5313", "#9fd841", "#6f2b6e", "#54b2fc", "#5336c5"),
                     labels=stringr::str_wrap(habitat_names_ordered, width=12),
                     name='habitat') +
  guides(color=guide_legend(byrow=T)) +
  theme(legend.spacing.y = unit(0.2, "cm"),
        legend.position = c(0.88, 0.6))
ordiview

# ggplot2::ggsave(plot=ordiview,'./figures/supplementary/PCA_soils/ordiplot_soilPCA_ellipses.svg',
#                 width=7, height=5.75)


ordiview2 <- ggbiplot(soil.pca, ellipse=F, obs.scale=T, varname.size=4, var.axes = T, groups=soil.c$habitat) + 
  theme_classic(base_size=14) +
  ylim(c(-5, 5)) +
  labs(color= 'habitat type') +
  scale_color_manual(values=c('#989898', '#989898', '#989898', '#989898', '#989898', '#989898', '#989898')) + 
  theme(legend.position = "none")
ordiview2


library(cowplot); library(gridExtra)

finalPlot <- ggdraw() +
  draw_plot(ordiview2, 0, 0, 0.5, 1) +
  draw_plot(ordiview, 0.5,0, 0.5, 1) + 
  draw_label("A)", color = "black", size = 18, x=0.08, y=0.9)  + 
  draw_label("B)", color = "black", size = 18, x=0.58, y=0.9)

finalPlot

ggplot2::ggsave(plot=finalPlot,'./figures/supplementary/PCA_soils/ordiplot_soilPCA_TwoPanels.svg',
                width=12, height=5.5)
