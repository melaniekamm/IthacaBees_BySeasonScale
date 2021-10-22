library(devtools); library(rstudioapi)
install_github("land-4-bees/beecoSp")

#restart R session to make sure most recent version of 'beecoSp' is loaded. 
#Not totally sure why this is necessary...
#rstudioapi::restartSession()

#load relevant libraries
library(sf); library(beecoSp); library(dplyr); library(raster); library(doParallel)


#for help info: https://github.com/land-4-bees/beecoSp/blob/master/man/apply_distweight.Rd

#requires cdlTools, plyr, doSnow, doParallel, doMPI packages. Please install if you don't have them.
#decay function is linear

#### 1) Set necessary inputs, file paths, etc.

  #Necessary for code to work. Please change these!!
  project_name <- 'HighRes2018_KammererSites' #choose name to use for naming folders (hopefully less confusing!)

  forage_range <- 500 #desired foraging range in meters (total size of landscape will be foraging range*2) (eg. at 800 this would stop hard at 1600)
  shapefile_path <- "./data/spatial_data/Kammerer_sites_shapefile/FINAL_Sites_Selected.shp" #path to shapefile of sampling points
  yr <- 2018 #year of sampling

  #if desired, adjust these file paths too (folders will be created automatically, just specify name)
  #regional_landcover_path <-paste0('./CDL_rasters/NY_CDL', yr, '_NA.tif') #path where CDL raster will be saved IF YOU WANT CDL
  regional_landcover_path <-"./data/spatial_data/landuse_rasters/FR2018_imp.tif"
  radius <- (forage_range*2)+100
  clipdir <- paste0("./data/spatial_data/",project_name, radius, 'm/') #path to folder where clipped landscape rasters will be stored.


#### 3) create individual landscape rasters for each site

  #generate landscape buffers around each sampling location
  landbuffers <- beecoSp::bufferproject(rasterpath=regional_landcover_path, featurepath=shapefile_path, 
                               bufferdist=radius)

  plot(landbuffers)#to see buffers in geographical space (no background)

  #clip regional land cover to landscape buffers, write .tif files to specified directory.
  land_ids <- beecoSp::execute_landclip(polygons=landbuffers, rasterpath=regional_landcover_path,
                                     idvar='site', outdir=clipdir, overrast=T)
  #specify variable name of sites in 'idvar' and add na_value=255 if using CDL

#### 4) calculate landscape composition (regular and distance weighted)

  #calculate distance weighted landscape compostion for each raster in 'clipdir'
  dwtcomp <- beecoSp::apply_distweight(landdir=T, landfiles=clipdir, forage_range=forage_range)

  dwtcomp <- dplyr::select(dwtcomp, Landscape, landcover_class, normalized_distweight) %>%
             dplyr::mutate(PctLand_DistWeighted = (normalized_distweight*100)) %>%
             dplyr::select(-normalized_distweight)

  #move some spatial files that ended up in the wrong place
  tomove <-  list.files('./site_clips_2xforage', full.names = T)
  filesstrings::file.move(files=tomove, destinations = './data/spatial_data/HighRes2018_2xforage')
  unlink('./site_clips_2xforage', recursive=T)
  
  #calculate landscape composition for each raster
  #must use landscapes saved from 'apply_distweight' 
  #or landscape radius will be different between distance weighted and regular composition measures
  #JUST USE THIS IF YOU WANTED NOT-DISTANCE-WEIGHTED LANDCOVER
  lcomp <- beecoSp::landcomp(landdir=T, landfiles="./data/spatial_data/HighRes2018_2xforage/", writeoutput=F,
                    background=T, bgvalues=NA)

  #look at results rounded and without scientific notation
  options(scipen=999)

  
  if (!dir.exists('./data/landscape_composition/')) {
    dir.create('./data/landscape_composition/')
  }

  write.csv(dwtcomp, paste0('./data/landscape_composition/', project_name, 
                            '_land_composition_distweighted.csv'), row.names = F)
  write.csv(lcomp, paste0('./data/landscape_composition/', project_name, '_land_composition_unweighted.csv'), row.names = F)

  #calculate and write wide version of landscape composition
  #read landscape composition data
  codes <- read.csv('./data/landscape_composition/LandcoverCodes_finalMap_modified.csv')
  
  DW_wide <- dplyr::left_join(dwtcomp, dplyr::select(codes, Landcover, Group_, Code),
                              by= c('landcover_class' = 'Code')) %>%
            dplyr::mutate(Group = if_else(landcover_class < 100, 'Developed', as.character(Group_))) %>%
            dplyr::select(-Group_) %>%
            dplyr::mutate(Group = if_else(Group == 'Succesional', "Successional", Group)) %>%
            dplyr::group_by(Landscape, Group) %>%
            dplyr::summarise(Pct_Land = sum(PctLand_DistWeighted, na.rm=T)) %>%
            tidyr::pivot_wider(names_from=Group, values_from=Pct_Land, names_prefix= "PctLand_", 
                               values_fill = list(Pct_Land=0)) %>%
            dplyr::select(-PctLand_NA)
  write.csv(DW_wide, './data/landscape_composition/landscape_composition_distweighted_wide.csv', row.names = F)
                               