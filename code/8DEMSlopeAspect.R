#USGS 3DEP (1/3 arc sec) digital elevation model

library(raster)

# #USGS 3DEP DEM tiles 
# a <- raster::raster('F:/SpatialData/Elevation/IthacaRegion/USGS_NED_13_n43w077_IMG.img')
# b <- raster::raster('F:/SpatialData/Elevation/IthacaRegion/USGS_NED_13_n43w078_IMG.img')
# c <- raster::raster('F:/SpatialData/Elevation/IthacaRegion/USGS_NED_13_n44w077_IMG.img')
# d <- raster::raster('F:/SpatialData/Elevation/IthacaRegion/USGS_NED_13_n44w078_IMG.img')
# 
# #merge together DEM tiles that cover greater Ithaca region
# x <- raster::mosaic(a,b, fun=mean)
# y <- raster::mosaic(c,d, fun=mean)
# 
# dem <- raster::mosaic(x,y, fun=mean)
# raster::plot(dem)
# 
# #write merged DEM raster
# raster::writeRaster(dem, 'F:/SpatialData/Elevation/Ithaca_DEM_3DEP.tif')

#calculate slope and aspect from elevation raster
dem <- raster::raster('./data/spatial_data/Ithaca_DEM_3DEP.tif')

slope <- raster::terrain(dem, opt='slope', unit='tangent', neighbors=8)
slope_deg <- raster::terrain(dem, opt='slope', unit='degrees', neighbors=8)

aspect <- raster::terrain(dem, opt='aspect', unit='degrees', neighbors=8)


#read shapefiles of all plant sampling locations and reproject
sites <- sf::st_read("./data/spatial_data/Kammerer_sites_shapefile/allsites_plantsurvey.shp") %>%
         dplyr::mutate(site = gsub(gsub(gsub(gsub(site, pattern="Conn._Hill", replacement="Conn"),
                               pattern="Conn_Hill", replacement="Conn"),
                          pattern="Conifer_For34_Danby", replacement="Conifer_For34a_Danby"),
                     pattern="MesicUpRem_For34_Arnot", replacement="MesicUpRem_For34b_Arnot")) %>%
        sf::st_transform(crs= crs(slope)) %>%
        dplyr::select(site, habitat) %>%
        sf::as_Spatial()

sites$elevation <- raster::extract(dem, sites)
sites$slope_pct <- raster::extract(slope, sites)*100
sites$slope_deg <- raster::extract(slope_deg, sites)
sites$aspect360 <- raster::extract(aspect, sites)

# convert aspect into east/west (-1 = due west) and north/south (-1 = due south) vectors
sites$aspectEW <- sin(REdaS::deg2rad(sites$aspect360))
sites$aspectNS <- cos(REdaS::deg2rad(sites$aspect360))

#save Lat/Long as variables
sites$Long <- coordinates(sites)[,1]
sites$Lat <- coordinates(sites)[,2]

write.csv(sites, "./data/Iverson_sites_topography.csv")
