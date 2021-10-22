
library(sf); library(dplyr)
#downloaded National Hydrography Dataset for NY state from 
#https://viewer.nationalmap.gov/basic/?basemap=b1&category=nhd&title=NHD%20View#startUp
#https://www.usgs.gov/core-science-systems/ngp/national-hydrography/national-hydrography-dataset?qt-science_support_page_related_con=0#qt-science_support_page_related_con


#flowline-flowline3 are line representation of streams and rivers from largest (flowline) to smallest (flowline3)
#Waterbody is polygon layer of ponds and lakes
#Area is polygon layer of larger rivers (large enough to have perceptible width, all I insepcted were flowline, not 2 or 3)

#large rivers = flowline | area
#large and med rivers = flowline + flowline2 | area
#all streams/rivers = flowline + flowline2 + flowline3 | area

#all water = all streams/rivers + waterbody

#excluded point layer. Not immediately clear what points represent, and none were the closest water feature to sampling locations

files <- list.files('./data/spatial_data/NHD_H_New_York_State_Shape/Shape', pattern='.shp', full.names = T)
files <- files[!grepl(files, pattern=".xml", fixed=T)]

#read gps points of sampling sites
sites <- sf::st_read('./data/spatial_data/Kammerer_sites_shapefile/FINAL_Sites_Selected.shp') %>%
  sf::st_transform("+init=epsg:4269") %>%
  sf::as_Spatial()

#save bounding box (+ buffer) of sampling sites to clip NY state layers
bbox <- sf::st_bbox(sites)
bbox[1] <- bbox[1]- 0.4
bbox[3] <- bbox[3] + 0.
bbox[2] <- bbox[2] - 0.2
bbox[4] <- bbox[4] + 0.2


flowline3 <- sf::st_read(files[grepl(files, pattern='Flowline3')]) %>%
            sf::st_zm(drop=T, what="ZM")%>%
            sf::st_transform("+init=epsg:4269") %>%
            sf::st_crop(bbox) %>%
            sf::as_Spatial()

flowline2 <- sf::st_read(files[grepl(files, pattern='Flowline2')]) %>%
            sf::st_zm(drop=T, what="ZM")%>%
            sf::st_transform("+init=epsg:4269") %>%
            sf::st_crop(bbox) %>%
            sf::as_Spatial()

flowline <- sf::st_read(files[grepl(files, pattern='Flowline.shp')]) %>%
            sf::st_zm(drop=T, what="ZM")%>%
            sf::st_transform("+init=epsg:4269") %>%
            sf::st_crop(bbox) %>%
            sf::as_Spatial()

all_waterlines <- rbind(flowline, flowline2) %>%
            rbind(flowline3)

#area of larger rivers
area <- sf::st_read(files[grepl(files, pattern='Area.shp')]) %>%
        sf::st_zm(drop=T, what="ZM") %>%
        sf::st_transform("+init=epsg:4269") %>%
        sf::st_crop(bbox) %>%
        sf::as_Spatial()

waterbody <- sf::st_read(files[grepl(files, pattern='Waterbody')])%>%
        sf::st_zm(drop=T, what="ZM") %>%
        sf::st_transform("+init=epsg:4269") %>%
        sf::st_crop(bbox) %>%
        sf::as_Spatial()

dist_to_streams <- geosphere::dist2Line(p=sites, line=all_waterlines)
dist_to_stream_area <- geosphere::dist2Line(p=sites, line=area)
dist_to_ponds<- geosphere::dist2Line(p=sites, line=waterbody)

dist_to_ponds <- data.frame(dist_to_ponds) %>%
        dplyr::mutate(site = sites$site, water_type= 'pond/lake') %>%
        dplyr::select(site, distance, water_type, lon, lat)
dist_to_streams <- data.frame(dist_to_streams)%>%
        dplyr::mutate(site = sites$site, water_type= 'stream') %>%
        dplyr::select(site, distance, water_type, lon, lat)
dist_to_stream_area <- data.frame(dist_to_stream_area) %>%
        dplyr::mutate(site = sites$site, water_type= 'stream_polygon') %>%
        dplyr::select(site, distance, water_type, lon, lat)

distance <- dplyr::full_join(dist_to_ponds, dist_to_streams) %>%
        dplyr::full_join(dist_to_stream_area) %>%
        dplyr::group_by(site) %>%
        dplyr::filter(distance == min(distance))

write.csv(distance, './data/distance_to_water.csv', row.names = F)
