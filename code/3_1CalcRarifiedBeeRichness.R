#calculate rarefied bee richness using iNEXT package
library(dplyr)
rm(list=ls())

remove_single <- prune_poorsampled <- F
cover_method <- 'mean'

if (cover_method == 'mean') {
  coverage <- 'mean'
} else {
  coverage <- 0.3
}

#read in bee data
raw <- read.csv("./data/bee_specimens/IthacaBees_Final.csv") %>%
  dplyr::mutate(Site = gsub(Site, pattern = 'Blueheron', replacement = 'BlueHeron', fixed=T)) %>%
  dplyr::filter(name != 'Apis mellifera') %>%
  dplyr::mutate(SiteName= Site, Site = paste(Habitat, Site, sep=" "))

raw$Year <- as.factor(raw$Year)
summary(raw)

raw$Abundance <- 1

#create 'wide' dataset with species as rows, input to iNEXT package
wide <- reshape2::dcast(raw, name~Site + Year + Season, fun.aggregate=sum, 
                        value.var='Abundance') %>%
  dplyr::select(-name)

if (remove_single == T) {
  #store site/years that only have one species
  onespecies <- apply(wide, FUN=function(x) length(x[x > 0]) == 1, MARGIN=2)
  
  single <- length(which(onespecies))
  #take out site-years that only have one species (causes errors in rarefaction below)
  wide <- wide[,!onespecies]
} else { single <- 0 }

##### Part 3: Calculate species richness based on 'sample coverage' #####
model <- iNEXT::iNEXT(wide, q=0, datatype="abundance", size=NULL, se=F)

max_richness <- model$AsyEst

#mean sample coverage of real data
med_coverage <- median(model$DataInfo$SC); mean_coverage <- mean(model$DataInfo$SC)
min_coverage <- min(model$DataInfo$SC)

if (cover_method == 'mean') {
  cover <- mean_coverage
} else if (cover_method == 'median') {
  cover <- med_coverage
} else if (cover_method == 'minimum') {
  cover <- min_coverage
} else {
  cover <- coverage #default is coverage value used in Bartomeus paper
}


#for unknown reasons, estimateD function works with matrix as input, but not data.frame
richresults <- apply(wide, FUN=iNEXT::estimateD,datatype="abundance", base="coverage", 
                     conf=NULL, level=cover, MARGIN=2)

rich <- data.frame(matrix(unlist(richresults), nrow=length(names(wide)), byrow=T), 
                   stringsAsFactors = F)
names(rich) <- c("m", "method", "SC", "q = 0", "q = 1", "q = 2")
rich$site <- names(richresults)

widematrix <- as.matrix(wide)
rich2 <- iNEXT::estimateD(widematrix, datatype="abundance", base="coverage", conf=NULL, level=cover)

#store integer of duplicated rows that are missing from matrix version
dupls <- which(duplicated(rich[-length(rich)]))

if (length(dupls) > 0) {
  rich2$site <- rich$site[-dupls]
} else {rich2$site <- rich$site}

#for some reason one site with duplicated coverage, richness results was deleted when using matrix input data
#use merge to add it back in
final_richness <- merge(rich2, rich[,-2], all.y=T, by=c('site',
                                                        names(rich[-c(2, length(rich))])))

#add observed species richness to final_richness dataframe
final_richness <- merge(final_richness, model$DataInfo[,c(1:4)], by='site')

#change column names to be easier to reference
final_richness <- dplyr::rename(final_richness, ID=site, richness='q = 0', 
                                shannon='q = 1', simpson='q = 2', SC_adj=SC.x, SC_org=SC.y)


finalfinal <- final_richness
#ID variable produced by rarefaction is several variables, split into components
ids <- data.frame( do.call( rbind, strsplit(finalfinal$ID, '_' ) ) )
names(ids) <- c('Site', 'Year', 'Season')
finalfinal <- cbind(finalfinal, ids)

#extrapolation bias is relevant for richness, not diversity calculations
poorsampled <- finalfinal[finalfinal$m > 2*finalfinal$n,] #sites that have low ratio of observations: species (aka 8 observations, 6 species) cannot be reliably be adjusted to coverage levels much higher than the original sample

if (prune_poorsampled == T) {
  mostconservative <- finalfinal[!finalfinal$ID %in% poorsampled$ID,]
} else {
  mostconservative <- finalfinal
}

#### Add alternative landscape names to bee richness data

#read in key to link names in bee database to landscape variables
names <- read.csv('./data/misc/SiteNamesKey.csv')

beerich <- dplyr::select(mostconservative, Site, Year, Season, richness, S.obs) %>%
  dplyr::left_join(names, by=c("Site" = 'bee_names'))

#save full version for reference
write.csv(beerich, './data/analysis_datasets/bee_richness.csv', row.names = F)