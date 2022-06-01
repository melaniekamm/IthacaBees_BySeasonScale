
library(dplyr); library(raster)

lu_classes <- read.csv('../../../ResearchCollaborations/SpatialForageOrganicBees/data/spatial_data/example_map_layers/HighRes_LandCoverClasses.csv') %>%
  dplyr::mutate(Landcover=as.factor(Landcover), NameMap=as.factor(NameMap), Group=as.factor(Group)) %>%
  dplyr::select(-contains('X')) %>%
  dplyr::select(Code, everything()) %>%
  dplyr::rename(ID=Code)

landuse <- raster::raster('./data/spatial_data/landuse_rasters/FR2018_imp.tif') %>%
  raster::aggregate(fact=30, fun=modal, na.rm=T)

allsites <- sf::st_read('./data/spatial_data/Kammerer_sites_shapefile/FINAL_sites_Selected.shp')  %>%
  sf::st_transform(crs=raster::crs(landuse)) %>%
  sf::as_Spatial()

terra::ext(landuse)
terra::ext(allsites)

fingerlakes_lu <- raster::crop(landuse, allsites)

plot(fingerlakes_lu)
levels(fingerlakes_lu) <- lu_classes
fingerlakes_lu


mycattheme <- rasterTheme(rgb(lu_classes$RED, lu_classes$GREEN, lu_classes$BLUE))

plot(allsites)

lu_map <- rasterVis::levelplot(blueheron_lu, par.settings=mycattheme,
                               margin=list(draw=F), scales=list(draw=FALSE)) +
  layer(sp.points(allsites))
