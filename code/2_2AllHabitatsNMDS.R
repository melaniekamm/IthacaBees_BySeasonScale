library(dplyr)
rm(list=ls())

if (!dir.exists('./figures')) {
  dir.create('./figures')
  dir.create('./figures/supplementary')
  dir.create('./figures/supplementary/NMDS_plants')
  
}

plant_rich <- read.csv('./data/Iverson_plant/allplants/richness_by_site.csv')%>%
          dplyr::mutate(Site= gsub(Site, pattern='Black_Diamond', 
                           replacement='Merwin'))

cover_long <- read.csv('./data/Iverson_plant/plant_cover_by_species.csv') 
cover_long <- dplyr::filter(cover_long, !subplot %in% c('Plot A', 'Plot B',
                                                          'Plot C', 'D', 'E', 'No_canopy_trees'))

cover_long <- dplyr::mutate(cover_long, Site = gsub(Site, pattern='Black_Diamond', 
                                 replacement='Merwin', fixed=T)) %>%
          dplyr::mutate(site_plot = paste0(Site, "_", subplot),
                        Genus_species = paste0(Genus, ".", species)) %>%
          dplyr::group_by(Genus_species, Genus, species, family, common, Site) %>%
          dplyr::summarise(mean_pct_cover = sum(pct_cover)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(family = as.character(family))

#add one missing family
cover_long$family[cover_long$Genus == 'Cuscuta'] <- 'Convolvulaceae'


#NMDS ordination on plant community data
colors <- read.csv('./data/misc/habitat_colors.csv') %>%
  dplyr::mutate(color2 = rev(color))


#try a different package for ordination and plotting
#devtools::install_github("MadsAlbertsen/ampvis2")

library(ampvis2); library(ggplot2) 
#get species data in correct format
speciessite <- dplyr::mutate(cover_long, rounded_cover = mean_pct_cover*10,
                             Genus_species = as.factor(Genus_species),
                             Site = as.factor(Site)) %>%
  dplyr::select(-common, -mean_pct_cover) %>%
  tidyr::spread(key=Site, value=rounded_cover) %>%
  replace(is.na(.), 0) %>%
  dplyr::rename(Family=family, Species= species, OTU=Genus_species) %>%
  dplyr::mutate(Kingdom = 'Plantae', Phylum = "A", Class= "B", Order="C") %>%
  dplyr::select(Kingdom, Phylum, Class, Order, Family, Genus, Species, dplyr::everything())
a <- speciessite[,1:7]
b <- speciessite[,8:length(speciessite)]

speciessite <- cbind(b,a)

md <- dplyr::rename(plant_rich, SampleID=Site)
ampdata <- amp_load(otutable = speciessite, metadata = md)

t <- amp_ordinate(data=ampdata, type='NMDS', distmeasure='bray', transform='none', filter_species = 0,
                  species_plot = F, species_label_taxonomy = 'OTU',
                  sample_colorframe = 'habitat',
                  sample_colorframe_label="habitat",
                  sample_color_by='habitat', #sample_point_size=4,
                  species_nlabels = 0, print_caption = T,
                  detailed_output = T)

t$plot + scale_color_manual(values= as.character(colors$color)) + 
  scale_fill_manual(values= as.character(colors$color)) + 
  guides(color=guide_legend(ncol=1)) +
  theme_classic()

# ggsave(device='svg', filename='PlantComm_NMDS_AllHabitats.svg', path='./figures/supplementary/NMDS_plants',
#        width=7, height=6.25)


tlab <- amp_ordinate(data=ampdata, type='NMDS', distmeasure='bray', transform='none', 
                  filter_species = 0,
                  species_plot = T, species_label_taxonomy = 'OTU',
                  sample_colorframe = 'habitat',
                  #sample_colorframe_label="habitat",
                  sample_color_by='habitat', #sample_point_size=4,
                  species_nlabels = 25, 
                  print_caption = F,
                  detailed_output = T)

tlab$plot + scale_color_manual(values= as.character(colors$color)) + 
  scale_fill_manual(values= as.character(colors$color)) + 
  guides(color=guide_legend(ncol=1)) +
  theme_classic()

# ggsave(device='svg', filename='PlantComm_NMDS_AllHabitats_SpeciesNames.svg', path='./figures/supplementary'/NMDS_plants,
#        width=7, height=6.25)
# 
# axes <- dplyr::select(t$plot$data, SampleID, NMDS1, NMDS2)
# write.csv(axes, './data/Iverson_plant/allplants/NMDS_Ordination_Axis_Loadings.csv')


