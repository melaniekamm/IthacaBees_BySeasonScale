#most of this code was written by Aaron Iverson

###set a few parameters for which plant communities to use

#include trees in floral area values?
include_trees <- T


for (insect_poll in  c(F, T)) {
  
  #make output directories to store summarized versions of floral area, plant richness, etc.
  if (!dir.exists('./data/Iverson_plant')) {
    
    dir.create('./data/Iverson_plant')
    dir.create('./data/Iverson_plant/allplants')
    dir.create('./data/Iverson_plant/insect_pollinated')
    
  }
  
  
  #install.packages('dplyr')
  library(readxl)
  library(ggplot2)
  library(magrittr)#part of dplyr package
  library(RColorBrewer)
  
  #READING-IN DATA 
  #1) Floral density:
  fl.density<-read.csv("./data/floral_area_raw/Flowers_24.csv", na.strings = "#DIV/0!", skip=1)  #This DIV/O addition changes div/o into NA values
  fl.density$Genus<-as.factor(trimws(fl.density$Genus))
  fl.density$species<-as.factor(trimws(fl.density$species))
  
  #2) Floral area:
  fl.area<-read.csv("./data/floral_area_raw/FloralArea_11.csv")
  fl.area$Genus<-as.factor(trimws(fl.area$Genus))
  fl.area$species<-as.factor(trimws(fl.area$species))
  
  #Bloom dates:
  bloom.date<-read.csv("./data/floral_area_raw/bloom_period_shifted_18Oct2018.csv", skip=1)
  bloom.date$Genus<-as.factor(trimws(bloom.date$Genus))
  bloom.date$species<-as.factor(trimws(bloom.date$species))
  
  #CREATING JULIAN DAY RANGE
  Jdate <- 1:365
  
  #READ EXCEL FILE OF SITES
  SitesExcel <- read_excel("./data/floral_area_raw/Plots_Final_2016_26.xlsx")
  #need to delete first and last worksheets
  SiteSheetList <- excel_sheets("./data/floral_area_raw/Plots_Final_2016_26.xlsx")
  
  #CREATING BLANK DATA FRAME FOR SUMMED CURVES AT EACH SITE
  #each row is the site (column is called 'site' and in it is tab name)
  site.summaries <- data.frame(site=SiteSheetList)
  site.summaries$habitat<-substr(site.summaries$site, start=1, stop=regexpr("_| ", site.summaries$site)-1) #this takes the tab and names the sites based on the tab by stopping at the space or the _ in the tab text. regexpr indexes where the space is, we don't want to include the space, so we subtract 1. 
  #site.summaries$habitat<-substr(site.summaries$site, start=1, stop=2)
  
  table(site.summaries$habitat)#to see the N of each habitat category
  
  site.summaries[paste("d", 1:365, sep="")]<-NA #default is to add space, so sep is to get rid of space. 
  
  site.richness <- site.summaries
  site.summaries$richness<-NA#to make column which we will fill with richness data
  #i<-SiteSheetList[115] #to get 3rd site in sitesheetlist
  #
  for (i in SiteSheetList){
    #read sheet
    thissite<-read_excel("./data/floral_area_raw/Plots_Final_2016_26.xlsx", sheet=i, na=c("", 'tree', 'sap', 'Tree', 'Sap', '(sap)', '(tree)', 's', 't'), n_max=661)#some subplots have tree instead of number (actually 0 value), n_max means only read first 661 rows
    thissite<-thissite[,-grep(pattern="in.flower|flowers|X|Bloom_guide|unidentified|notes", names(thissite))]
    thissite$Genus<-as.factor(trimws(thissite$Genus))
    thissite$species<-as.factor(trimws(thissite$species))
    
    #subplots 1-10 only
    #dplyr::select(thissite, Genus, species, author, family, common, 
    #dplyr::contains('sublplot'), contains('subplot')) %>%
    
    long <- tidyr::gather(thissite, key='subplot', value='pct_cover', -Genus, -species, -author, 
                          -family, -common) %>%
      dplyr::filter(!is.na(pct_cover)) %>%
      dplyr::mutate(site = i)
  
    if (i == SiteSheetList[1]) {
      long_all <- long
    } else {
      long_all <- rbind(long_all, long)
    }
  #create column "avg_cov" (based on 0.5x0.5m subplots 1-10 and bigger subplots A-E)
    thissite$avg_cov<-rowSums(thissite[c("subplot_1","subplot_2","sublplot_3","sublplot_4","sublplot_5","sublplot_6","sublplot_7","sublplot_8","sublplot_9","sublplot_10")], na.rm=TRUE)/10/100*250*40 + rowSums(cbind(thissite$`Plot A`*0.0083*10000, thissite$`Plot B`*0.0059*10000, thissite$`Plot C`*0.0009*10000,thissite$D*0.0022*10000,thissite$E*0.0007*10000), na.rm=TRUE)  
  
    sub_pctcover <- colSums(dplyr::select(thissite,subplot_1, subplot_2,sublplot_3,sublplot_4,
                                          sublplot_5,sublplot_6,sublplot_7, sublplot_8,
                                          sublplot_9, sublplot_10), na.rm=T)
    sub_cover <- data.frame(sub_pctcover)
    names(sub_cover) <- 'pct_cover'
    sub_cover$plot <- paste0('subplot_', c(1:10))
    sub_cover$site <- i
    
    if (i == SiteSheetList[1]) {
      pct_cover <- sub_cover
    } else {
      pct_cover <- rbind(pct_cover, sub_cover)
    }
    
    #left join with floral density and floral area and bloom date
    thissite2<-dplyr::left_join(thissite,fl.density[c("Genus", "species", "Insect_pollinated", "Avg_Flrs_1m", "Avg_Flrs_Tree")], by=c("Genus"="Genus", "species"="species"))
    
    thissite3<-dplyr::left_join(thissite2,fl.area[c("Genus", "species", "area.per.flower.mm2")], by=c("Genus"="Genus", "species"="species")) 
    
    thissite4<-dplyr::left_join(thissite3,bloom.date[c("Genus", "species", "Start_final", "End_final")], by=c("Genus"="Genus", "species"="species")) 
    
    sum(thissite4$avg_cov>0)#count the number of trues
    
    #Calculate Curves (Resource_curves_4.R line 11 - 17 )
    
    #Add this if want just insect-pollinated
    if (insect_poll == T) {
    thissite5<-thissite4[thissite4$Insect_pollinated==1,]  
    
    } else if (insect_poll == F){
    #otherwise use this:
    thissite5<-thissite4  
    }
    
    if (include_trees == F) {
      
    thissite5$peakarea<-thissite5$Avg_Flrs_1m*thissite5$area.per.flower.mm2*thissite5$avg_cov #this is to use if no trees are included
    
    } else if (include_trees == T){ 
    #use the following if including trees
    thissite5$peakarea.herb<-thissite5$Avg_Flrs_1m*thissite5$area.per.flower.mm2*thissite5$avg_cov#This results in mm2 floral area per ha 
    thissite5$peakarea.tree<-thissite5$No_canopy_trees*40*thissite5$area.per.flower.mm2*thissite5$Avg_Flrs_Tree#multiply by 40 to get mm2 floral area per ha
    thissite5$peakarea<-rowSums(cbind(thissite5$peakarea.herb, thissite5$peakarea.tree), na.rm=TRUE)
    }
    
    thissite5$span<-thissite5$End_final-thissite5$Start_final
    thissite5$peakjday<-thissite5$Start_final+thissite5$span/2
    thissite5$periodsd<-thissite5$span/6
    thissite5$mult <- thissite5$peakarea/dnorm(thissite5$peakjday, mean=thissite5$peakjday, sd=thissite5$periodsd)
    
    thissite5[paste("d", 1:365, sep="")]<-NA #makes empty dataframe, the paste command puts a d (day) in front of 1 to 365 with no space (sep="")
    
    thissite5 <- data.frame(thissite5)
    
    # save a copy of floral area curves by species
    site.byspecies <- dplyr::select(thissite5, Genus:common, d1:d365) %>%
      dplyr::mutate(Site = i) %>%
      dplyr::select(Site, everything())
    
    # bind together by species curves for all sites
    if (i == SiteSheetList[1]) {
      site.byspecies.all <- site.byspecies
    } else {
      site.byspecies.all <- rbind(site.byspecies.all, site.byspecies)
    }
    
  #Sum curves (Resource_curves_4.R line 45), this takes each plot and sums the amount of resources per day per species
    for (j in 1:nrow(thissite5)){ #a for loop within a for loop
      thissite5[j,paste("d", 1:365, sep="")]<-dnorm(Jdate, mean=thissite5$peakjday[j], sd=thissite5$periodsd[j])*thissite5$mult[j]
    }
    
    site.summaries[which(site.summaries$site==i),paste("d", 1:365, sep="")]<-colSums(thissite5[,paste("d", 1:365, sep="")], na.rm=TRUE)/1000000#divide by 1 million to go from mm2 to m2 per ha
  
    site.richness[which(site.richness$site==i),paste("d", 1:365, sep="")]<- colSums(thissite5[,paste("d", 1:365, sep="")] > 1000, na.rm=TRUE)
    
    #site.summaries now is each row is a different site and each column is a day
    #changed this to use 'site5' so the species richness is the same plant community as the floral area 
    site.summaries$richness[which(site.summaries$site==i)]<-sum(thissite5$avg_cov>0, na.rm=T)#calculates species richness per site
  }
  
  #change some habitat names to be a bit more descriptive
  site.summaries <- dplyr::rename(site.summaries, Site=site) %>%
            dplyr::mutate(habitat = gsub(gsub(gsub(gsub(gsub(gsub(gsub(gsub(gsub(gsub(gsub(habitat, 
              pattern='Apple', replacement= 'OrchardFloor', fixed=T),
              pattern="Cover", replacement='RowcropCover'),
              pattern="WinterFallow", replacement='RowcropWinterFallow', fixed=T),
              pattern='Emer', replacement='WetlandEmer', fixed=T),
              pattern='Flood', replacement= 'ForestFlood', fixed=T),
              pattern='Field', replacement='OldField', fixed=T),
              pattern='Shrub', replacement='WetlandShrub', fixed=T),
              pattern='WetlandShrubland', replacement='Shrubland', fixed=T),
              pattern='Developed', replacement='Lawn', fixed=T),
              pattern='_f', replacement='F', fixed=T),
              pattern='_t', replacement="T", fixed=T))
  
  long_all <- dplyr::rename(long_all, Site=site) %>%
              dplyr::mutate(Site = gsub(Site, pattern=" ", replacement="_", fixed=T)) %>%
              dplyr::mutate(Site = gsub(gsub(gsub(gsub(
              Site, pattern="Conn._Hill", replacement="Conn", fixed=T),
                    pattern="Conn_Hill", replacement="Conn", fixed=T),
                    pattern="Conifer_For34_Danby", replacement="Conifer_For34a_Danby", fixed=T),
                    pattern="MesicUpRem_For34_Arnot", replacement="MesicUpRem_For34b_Arnot", fixed=T))
    
  
  write.csv(long_all, './data/Iverson_plant/plant_cover_by_species.csv', row.names = F)
  
  site.richness <- dplyr::select(site.summaries, Site, habitat, richness) %>%
            dplyr::mutate(Site = gsub(Site, pattern=" ", replacement="_", fixed=T)) %>%
            dplyr::mutate(Site = gsub(gsub(gsub(gsub(Site, pattern="Conn._Hill", replacement="Conn", fixed=T),
                               pattern="Conn_Hill", replacement="Conn", fixed=T),
                          pattern="Conifer_For34_Danby", replacement="Conifer_For34a_Danby", fixed=T),
                     pattern="MesicUpRem_For34_Arnot", replacement="MesicUpRem_For34b_Arnot", fixed=T))
  
  
  site.summaries <- dplyr::select(site.summaries, -richness) %>%
            dplyr::mutate(Site = gsub(Site, pattern=" ", replacement="_", fixed=T)) %>%
            dplyr::mutate(Site = gsub(gsub(gsub(gsub(Site, pattern="Conn._Hill", replacement="Conn", fixed=T),
                                        pattern="Conn_Hill", replacement="Conn", fixed=T),
                                   pattern="Conifer_For34_Danby", replacement="Conifer_For34a_Danby", fixed=T),
                              pattern="MesicUpRem_For34_Arnot", replacement="MesicUpRem_For34b_Arnot", fixed=T))
  
  site.byspecies.all <- dplyr::mutate(site.byspecies.all, Site = gsub(Site, pattern=" ", replacement="_", fixed=T)) %>%
    dplyr::mutate(Site = gsub(gsub(gsub(gsub(Site, pattern="Conn._Hill", replacement="Conn", fixed=T),
                                        pattern="Conn_Hill", replacement="Conn", fixed=T),
                                   pattern="Conifer_For34_Danby", replacement="Conifer_For34a_Danby", fixed=T),
                              pattern="MesicUpRem_For34_Arnot", replacement="MesicUpRem_For34b_Arnot", fixed=T))
  if (insect_poll == F) {
    write.csv(site.byspecies.all, './data/Iverson_plant/site_floral_area_allplants_byday_byspecies.csv', row.names = F)
  } else if (insect_poll == T) {
    write.csv(site.byspecies.all, './data/Iverson_plant/site_floral_area_insectpollinated_byday_byspecies.csv', row.names = F)
  }
    
  if (insect_poll == F) {
    if (!dir.exists('./data/Iverson_plant/allplants')) {
      dir.create('./data/Iverson_plant/allplants')
    }
    
    write.csv(site.summaries, './data/Iverson_plant/allplants/floral_area_by_site_by_day.csv', row.names = F)
    write.csv(site.richness, './data/Iverson_plant/allplants/richness_by_site.csv', row.names = F)
  } else if (insect_poll == T){
    
    if (!dir.exists('./data/Iverson_plant/insect_pollinated')) {
      dir.create('./data/Iverson_plant/insect_pollinated')
    }
    write.csv(site.summaries, './data/Iverson_plant/insect_pollinated/floral_area_by_site_by_day.csv', row.names = F)
    write.csv(site.richness, './data/Iverson_plant/insect_pollinated/richness_by_site.csv', row.names = F)
    
  }
  #calculate habitat mean floral resources
  habitat_mean<- dplyr::group_by(site.summaries, habitat) %>%
                 dplyr::select(-Site) %>%
                 dplyr::summarize_all(mean, na.rm=T)
  
  
  urban <- read.csv('./data/floral_area_raw/urban_floral_area_by_day.csv') %>%
            dplyr::select(-X,) %>%
            dplyr::rename(Site = site)
  
  habitat_mean <- dplyr::full_join(habitat_mean, urban) %>%
            dplyr::select(-Site) %>%
            dplyr::arrange(habitat)
  
  if (insect_poll == F) {
    write.csv(habitat_mean, './data/Iverson_plant/allplants/floral_area_by_habitat_by_day.csv', row.names = F)
  } else if (insect_poll == T){
    write.csv(habitat_mean, './data/Iverson_plant/insect_pollinated/floral_area_by_habitat_by_day.csv', row.names = F)
  }

  }
}