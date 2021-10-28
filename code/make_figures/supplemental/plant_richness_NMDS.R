library(dplyr)
rm(list=ls())

# fa <- read.csv('./data/Iverson_plant/floral_area_summary_by_site.csv') %>%
#       dplyr::select(-X)
# land <- read.csv('./data/landscape_composition/LandComp500_wide.csv') %>%
#         dplyr::select(Landscape, developed, arable, natural)

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


cover_wide <- dplyr::select(cover_long, -family, -common, -Genus, -species) %>% #, -subplot, -family) %>%
          tidyr::spread(key=Genus_species, value=mean_pct_cover) %>%
          replace(is.na(.), 0)

topo <- read.csv("./data/Iverson_sites_topography.csv") %>%
          dplyr::select(-X, -optional, -aspect360, -slope_deg, -coords.x1, -coords.x2) %>%
          dplyr::mutate(Site= as.character(site)) %>%
          dplyr::select(-site)


#join topo with plant richness because they use same site names
plant_rich <- left_join(plant_rich, topo)

bd <- read.csv('./data/soil/bulk_density.csv')
names(bd) <- c('Site', 'subsample', 'bulk_density')

bd_bysite <- dplyr::group_by(bd, Site) %>%
             dplyr::summarise(bulk_density=mean(bulk_density))


soil_moisture <- read.csv('./data/soil/SoilMoistureRaw.csv') %>%
                dplyr::rename(Site=site) %>%
                dplyr::left_join(bd_bysite) %>%
                dplyr::mutate(Volum_WaterContent =  Grav_WaterContent_g.g * bulk_density) %>%
                dplyr::filter(Volum_WaterContent < 1.13) #exclude one VERY high soil moisture reading from edge McGowan

moisture_bysite <- group_by(soil_moisture, Site) %>%
                   dplyr::summarise(Vol_WaterContent = mean(Volum_WaterContent),
                                    BulkDensity= mean(bulk_density))

texture_fertility <- read.csv('./data/soil/FertilityTexture_withSampleCodes.csv') %>%
                  dplyr::rename(Site=site) %>%
  
                  dplyr::group_by(Site) %>%
                  dplyr::mutate(Total_N_Pct = dplyr::if_else(Total_N_Pct > 0.7, 
                                                                  1000, Total_N_Pct),
                      OM_Pct = dplyr::if_else(Total_N_Pct > 0.7, 1000, OM_Pct))

texture_fertility$OM_Pct[texture_fertility$OM_Pct > 999] <- NA
texture_fertility$Total_N_Pct[texture_fertility$Total_N_Pct > 999] <- NA

fertility_bysite <- dplyr::select(texture_fertility, -Lab_ID, -Field_ID, 
                                  -sub_sample, -Soil_Textural_Class, -habitat) %>%
                    dplyr::summarise_all(mean, na.rm=T)
        
allsoil_bysite <- left_join(moisture_bysite, fertility_bysite) %>%
        dplyr::mutate(Site=if_else(Site == 'Flood Salmon Creek', "Flood SalmonCreek", as.character(Site)))            

sitekey <- read.csv('./data/misc/SiteNamesKey.csv')


explan <- dplyr::select(allsoil_bysite, -dplyr::contains("_CEC")) %>%
  dplyr::full_join(sitekey, by=c('Site' = 'bee_names')) %>%
  dplyr::left_join(dplyr::select(plant_rich, richness, Site),
                    by=c("landscape_names" = "Site")) %>%
  dplyr::left_join(topo, by=c("landscape_names" = "Site"))
  
#NMDS ordination on plant community data
colors <- read.csv('./data/habitat_colors.csv') %>%
  dplyr::mutate(color2 = rev(color))
explan <- dplyr::left_join(explan, colors) %>%
  dplyr::mutate(color = as.character(color),
                color2 = as.character(color2),
                category_color=as.character(category_color),
                Kammerer_colors = as.character(Kammerer_colors))


#filter plant abundance data to sites with soil info
cover_wide <- dplyr::filter(cover_wide, Site %in% explan$landscape_names)

rownames(cover_wide) <- cover_wide$Site
cover_wide <- dplyr::ungroup(cover_wide) %>%
  dplyr::select( -Site)#, -site_plot)



#might be too many habitat types, sites to tell much...
ord <- vegan::metaMDS(cover_wide, distance='bray', trymax=250, autotransform = F, halfchange=T)
ord

#plot NMDS results
#plot all habitat types
vegan::ordiplot(ord, display="sites", type="text")

vegan::ordiplot(ord, display="sites", type="n", xlim=c(-1,1.5))
points(ord,display='sites', col=explan$Kammerer_colors, cex=1.5, pch=18)
legend("bottomleft", legend=unique(explan$habitat), col=unique(explan$Kammerer_colors), pch=18, pt.cex=1.5)

fit <- vegan::envfit(ord, env=dplyr::select(explan,-Site, -habitat, -Long, -Lat, -category, -color, -category_color, 
                                            -Kammerer_colors, -color2, -pH_Acidity_index, -landscape_names, -habitat,
                                            -richness))
plot(fit, p.max=0.1)

vegan::ordiplot(ord, display="species", type="text", xlim=c(-1,1.5))
points(ord,display='sites', col=explan$Kammerer_colors, cex=1.5, pch=18)
legend("bottomleft", legend=unique(explan$habitat), col=unique(explan$Kammerer_colors), pch=18, pt.cex=1.5)


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

ggsave(device='svg', filename='PlantComm_NMDS_AllHabitats.svg', path='./figures',
       width=7, height=6.25)


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

ggsave(device='svg', filename='PlantComm_NMDS_AllHabitats_SpeciesNames.svg', path='./figures',
       width=7, height=6.25)

axes <- dplyr::select(t$plot$data, SampleID, NMDS1, NMDS2)
write.csv(axes, './data/Iverson_plant/allplants/NMDS_Ordination_Axis_Loadings.csv')






#which plants lead to differences between habitats
allsites_wide <- dplyr::mutate(cover_long, 
          Genus_species = as.factor(Genus_species),
          Site = as.factor(Site)) %>%
  dplyr::select(-common, -Genus, -species, -family) %>%
  tidyr::spread(key=Genus_species, value=mean_pct_cover) %>%
  replace(is.na(.), 0)

rownames(allsites_wide) <- allsites_wide$Site

allsites_wide <- dplyr::select(allsites_wide, -Site)
                               

sim <- with(dune.env, simper(dune, Management))
summary(sim)

#similarity percentages
sim <- with(md, simper(allsites_wide, habitat))
summary(sim, ordered=T)

sim$average


#do ordination over again with just sites I sampled for bees
#get species data in correct format
speciessite <- dplyr::mutate(cover_long, rounded_cover = mean_pct_cover*10,
                             Genus_species = as.factor(Genus_species),
                             Site = as.factor(Site)) %>%
  dplyr::filter(Site %in% explan$landscape_names) %>%
  dplyr::select(-common, -mean_pct_cover) %>%
  tidyr::spread(key=Site, value=rounded_cover) %>%
  replace(is.na(.), 0) %>%
  dplyr::rename(Family=family, Species= species, OTU=Genus_species) %>%
  dplyr::mutate(Kingdom = 'Plantae', Phylum = "A", Class= "B", Order="C") %>%
  dplyr::select(Kingdom, Phylum, Class, Order, Family, Genus, Species, dplyr::everything())
  
a <- speciessite[,1:7]
b <- speciessite[,8:length(speciessite)]

speciessite <- cbind(b,a)

md <- dplyr::rename(explan, SampleID=landscape_names) %>%
          dplyr::mutate(habitat= factor(habitat, 
          levels=c('MesicUpRem', 'Flood', 'Edge', 'Ditch', 'Field', 'Apple', 'Veg'))) %>%
          dplyr::select(-Site) %>%
          dplyr::select(SampleID, everything())


ampdata <- amp_load(otutable = speciessite, metadata = md)

t <- amp_ordinate(data=ampdata, type='NMDS', distmeasure='bray', transform='none', 
                  filter_species = 0,
                  species_plot = F, species_label_taxonomy = 'OTU',
                  sample_colorframe = 'habitat',
                  sample_colorframe_label="habitat",
                  sample_color_by='habitat', #sample_point_size=4,
                  species_nlabels = 0, 
                  envfit_numeric=names(explan)[-c(1,19:21,26:32)],
                  envfit_signif_level = 0.08,
                  envfit_numeric_arrows_scale = 2,
                  print_caption = F)
                  #detailed_output = T)

t + scale_color_manual(values= c("#104507", "#ad32b7", "#5b8313", "#540765", "#4d7ba1", "#1f2c64", "#2d70f0")) + 
  scale_fill_manual(values= c("#104507", "#ad32b7", "#5b8313", "#540765", "#4d7ba1", "#1f2c64", "#2d70f0")) +
  theme(legend.position = "none") 


ggsave(device='svg', filename='PlantComm_NMDS_BeeHabitats_soilvectors.svg', path='./figures',
       width=8.5, height=7)

#without soil vectors
t <- amp_ordinate(data=ampdata, type='NMDS', distmeasure='bray', transform='none', 
                  filter_species = 0,
                  species_plot = F, species_label_taxonomy = 'OTU',
                  sample_colorframe = 'habitat',
                  sample_colorframe_label="habitat",
                  sample_color_by='habitat', #sample_point_size=4,
                  species_nlabels = 0, 
                  print_caption = F)
#detailed_output = T)

t + scale_color_manual(values= c("#104507", "#ad32b7", "#5b8313", "#540765", "#4d7ba1", "#1f2c64", "#2d70f0")) + 
  scale_fill_manual(values= c("#104507", "#ad32b7", "#5b8313", "#540765", "#4d7ba1", "#1f2c64", "#2d70f0")) +
  theme(legend.position = "none") 


ggsave(device='svg', filename='PlantComm_NMDS_BeeHabitats.svg', path='./figures',
       width=8.5, height=7)

#labeled species names
#without soil vectors
b <- amp_ordinate(data=ampdata, type='NMDS', distmeasure='bray', transform='none', 
                  filter_species = 0,
                  species_plot = T, species_label_taxonomy = 'OTU',
                  sample_colorframe = 'habitat',
                  sample_colorframe_label="habitat",
                  sample_color_by='habitat', #sample_point_size=4,
                  species_nlabels = 50, 
                  print_caption = F)
#detailed_output = T)

b + scale_color_manual(values= c("#104507", "#ad32b7", "#5b8313", "#540765", "#4d7ba1", "#1f2c64", "#2d70f0")) + 
  scale_fill_manual(values= c("#104507", "#ad32b7", "#5b8313", "#540765", "#4d7ba1", "#1f2c64", "#2d70f0")) +
  theme(legend.position = "none") 


ggsave(device='svg', filename='PlantComm_NMDS_BeeHabitats_SpNames.svg', path='./figures',
       width=12, height=10)
