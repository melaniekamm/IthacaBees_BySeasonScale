library(landscapemetrics); library(dplyr)
# metrics from Kennedy et al (2013) metrics
# plus edge density and landscape diversity metrics (of particular interest to me)

# calculate config metrics with more standard input data (30m resolution CDL)
# here, the goal is to make sure the results (e.g small patch side) are not due to high-resolution land cover data
use_CDL <- F

#store CV function that excludes missing values
CV <- function(x){
  (sd(x, na.rm=T)/mean(x, na.rm=T))*100
}

#first check landscape raster file
if (use_CDL == F) {
  lands <- list.files('./data/spatial_data/HighRes2018_2xforage', full.names = T)
} else if (use_CDL == T) {
  lands <- list.files('./data/spatial_data/CDL2018_1100m', full.names = T)
}


for (i in c(1:length(lands))) {

  example_land <- raster::raster(lands[i])
  
  check <- check_landscape(example_land, verbose=T)
  
  if (!check$units == 'm' & check$class == 'integer') {
    stop('Check raster file for landscape. Seems like there is a problem')
  }
  
  landname <- gsub(basename(lands[i]), pattern=".tif", replacement = "", fixed=T)
  
  enn_cv <- lsm_c_enn_cv(example_land) %>%
            summarize(level='landscape', metric= unique(metric), value= CV(value))
  
  
  landmetrics <- lsm_l_para_mn(example_land) %>%
            dplyr::full_join(enn_cv) %>%
            dplyr::full_join(lsm_l_ed(example_land)) %>%
            dplyr::full_join(lsm_l_iji(example_land)) %>%
            dplyr::full_join(lsm_l_shdi(example_land)) %>%
            dplyr::full_join(lsm_l_sidi(example_land)) %>%
            dplyr::full_join(lsm_l_area_mn(example_land)) %>%
            dplyr::mutate(Landscape=landname) %>%
            dplyr::select(Landscape, level, metric, value)
  
  
  
  if (i == 1) {
    allmetrics <- landmetrics
  } else {
    allmetrics <- rbind(allmetrics, landmetrics)
  }

}

metrics_wide <- dplyr::select(allmetrics, -level) %>%
                tidyr::pivot_wider(names_from=metric, values_from=value)

if (use_CDL == F) {
  write.csv(metrics_wide, './data/landscape_composition/landscape_configuration_metrics_highres.csv', row.names = F)
} else {
  write.csv(metrics_wide, './data/landscape_composition/landscape_configuration_metrics_CDL30m.csv', row.names = F)
}