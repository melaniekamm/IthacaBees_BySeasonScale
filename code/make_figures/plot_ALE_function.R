plot_ALE <- function(season, response, centerscale, allvars, ymin, ymax, yearmin=-0.8, yearmax=0.8) {
  
library(iml); library(ggplot2); library(dplyr); library(zeallot)

# specify name of response variable to use
if (response == 'abundance') {
  responsevar <- 'AbundDayTrap'
} else if (response == 'richness') {
  responsevar <- 'richness'
}

# load data
source('./code/functions/load_data.R')
landyearsite %<-%
  load_data(season=season, response=response, centerscale=centerscale)


# split dataframes into response and predictor
# avoids using formula interface that turns factors into dummy variables

landyearsite_response <- dplyr::select(landyearsite, all_of(responsevar))
landyearsite_pred <- dplyr::select(landyearsite, -all_of(responsevar))

#remove columns with NA values
nacells <- which(is.na(landyearsite_pred))


if (length(nacells) > 0 ) {
  remove_columns <- which(colSums(is.na(landyearsite_pred)) > 0)
  landyearsite <- landyearsite[-remove_columns]
  landyearsite_pred <- landyearsite_pred[-remove_columns]
  landyearsite_response <- landyearsite_response[-remove_columns]
}
nacells2 <- which(is.na(landyearsite_response))

if (length(nacells2) > 0 ) {
  warning('NA values in response variable. These sites were removed.')
  landyearsite <- landyearsite[-nacells2,]
  landyearsite_pred <- landyearsite_pred[-nacells2,]
  landyearsite_response <- data.frame(landyearsite_response[-nacells2,])
}

# load saved random forest models
if (season == 'spring') {
  
  if (centerscale == T) {
    if (allvars == T) {
      if (response == 'abundance') {
        load('./data/RF_results/LandYearSite_AllVariables_CenterScale_spring_abundance.RDA')
      } else {
        load('./data/RF_results/LandYearSite_AllVariables_CenterScale_spring_richness.RDA')
      }
    } else if (allvars == F) {
      if (response == 'abundance') {
        load('./data/RF_results/LandYearSite_SubsetVariables_CenterScale_spring_abundance.RDA')
      } else {
        load('./data/RF_results/LandYearSite_SubsetVariables_CenterScale_spring_richness.RDA')
      }
    }
  } else if (centerscale == F) {
    if (allvars == T) {
      if (response == 'abundance') {
        load('./data/RF_results/LandYearSite_AllVariables_spring_abundance.RDA')
      } else {
        load('./data/RF_results/LandYearSite_AllVariables_spring_richness.RDA')
      }
  } else if (allvars == F) {
    if (response == 'abundance') {
      load('./data/RF_results/LandYearSite_SubsetVariables_spring_abundance.RDA')
    } else {
      load('./data/RF_results/LandYearSite_SubsetVariables_spring_richness.RDA')
      }
    }
  }
  
} else if (season == 'summer') {

  if (centerscale == T) {
    if (allvars == T) {
      if (response == 'abundance') {
        load('./data/RF_results/LandYearSite_AllVariables_CenterScale_summer_abundance.RDA')
      } else {
        load('./data/RF_results/LandYearSite_AllVariables_CenterScale_summer_richness.RDA')
      }
    } else if (allvars == F) {
      if (response == 'abundance') {
      load('./data/RF_results/LandYearSite_SubsetVariables_CenterScale_summer_abundance.RDA')
      } else {
        load('./data/RF_results/LandYearSite_SubsetVariables_CenterScale_summer_richness.RDA')
      }
    }
  } else if (centerscale == F) {
    if (allvars == T) {
      if (response == 'abundance') {
      load('./data/RF_results/LandYearSite_AllVariables_summer_abundance.RDA')
      } else {
      load('./data/RF_results/LandYearSite_AllVariables_summer_richness.RDA')
      }
    } else if (allvars == F) {
      load('./data/RF_results/LandYearSite_SubsetVariables_CenterScale_summer_abundance.RDA')
      load('./data/RF_results/LandYearSite_SubsetVariables_CenterScale_summer_richness.RDA')
    }
  }
}

  if (allvars == T) {
    fit <- landyearsite_rf
  } else if (allvars == F) {
    fit <- landyearsite_sub
  }

  # make predictor object to use with IML package below
  predictor <- iml::Predictor$new(model=fit, y=landyearsite_response[,1], data=landyearsite)
  
  if (season == 'spring' & response == 'abundance') {
    var <- c("PctLand_Wetland",'PctLand_Water', "PctLand_Natural", "CV_FA.all.land", 
             "NMDS_mmt",  "P_ppm_mean", "K_ppm_mean", "Year")
    varname <- c("Percent Wetland", "Percent Water", "Percent Natural",
                 "CV floral area (all sp.), landscape", "Plant composition, mmt intensity", 
                 "Soil phosphorus (ppm)", "Soil potassium (ppm)", "Year")
    if (centerscale == T) {
      ylab <- expression(Normalized~Abund~Day^-1~Trap^-1)
    } else {
      ylab <- expression(Delta~Abund~Day^-1~Trap^-1)
    }
    colortouse <- "#0072B2"
    
  } else if (season == 'spring' & response == 'richness') {
    var <- c("elevation", "PctLand_Developed", "richness.IP","distance_to_water", "Year", "PctLand_Water")
    varname <- c("Elevation","Percent Developed",  "Plant richness (IP sp.)", 
                 "Distance to water", "Year", "Percent Water")
    if (centerscale == T) {
      ylab <- "Normalized richness"
    } else {
      ylab <- expression(Delta~Species~richness)
    }
    colortouse <- "#0072B2"
    
  } else if (season == 'summer' & response == 'abundance') {
    var <- c("NMDS_mmt","Grav_WaterContent_g.g_mean", "fall_total_FA.all.site", "mean_pct_cover", "Year")
    varname <- c("Plant composition, mmt intensity", "Soil gravimetric water content", 
    "Fall floral area (all sp.), site", "Percent plant cover", "Year")
    if (centerscale == T) {
      ylab <- expression(Normalized~Abund~Day^-1~Trap^-1)
    } else {
      ylab <- expression(Delta~Abund~Day^-1~Trap^-1)
    }
    colortouse <- "#E69F00"
    
  } else if (season == 'summer' & response == 'richness') {
    var <- c("PctLand_Agriculture", "OM_Pct_mean", "Total_N_Pct_mean", "bulk_density_mean", 
             "Grav_WaterContent_g.g_mean", "Year", "aspectNS", "PctLand_Natural", "OM_Pct_mean",
             "NMDS_mmt", "fall_total_FA.all.site")
    varname <- c("Percent Agriculture", "Soil organic matter", "Soil total nitrogen (%)", 
                 "Soil bulk density", "Soil gravimetric water content", "Year", "AspectNS", 
                 "Percent Natural", "Soil organic matter", 
                 "Plant composition, mmt intensity", "Fall floral area (all sp.), site")
    if (centerscale == T) {
      ylab <- "Normalized richness"
    } else {
      ylab <- expression(Delta~Species~richness)
    }
    colortouse <- "#E69F00"
  }
  
    
  plots <- list()
  
  for (i in c(1:length(var))) {
      
    df <- FeatureEffect$new(predictor, feature = var[i], method='ale')
    
    plotdf <- df$results; names(plotdf) <- c( '.type','.ale', 'variable')
    plotdf$ID <- paste0(season, " ", response) 
    
    if (var[i] == 'Year') {
      plots[[i]] <- ggplot(data=plotdf, aes(x=variable, y=.ale)) + theme_classic(base_size=13) +
        geom_point(aes(color=ID), size=4) +
        labs(y=ylab,
             x= varname[i]) +
        scale_colour_manual(values=colortouse) +
        theme(legend.position="none") +
        geom_rug(data=plotdf, aes(x=variable), length= unit(0.03, 'npc'), inherit.aes = F) +       
        scale_y_continuous(limits= c(yearmin, yearmax), labels= scales::number_format(accuracy=0.01))
    } else {
      plots[[i]] <- ggplot(data=plotdf, aes(x=variable, y=.ale)) +  theme_classic(base_size=13) +
        geom_line(aes(color=ID), size=1.5) +
        labs(y=ylab,
             x= varname[i]) +
        scale_colour_manual(values=colortouse) +
        theme(legend.position="none") +
        geom_rug(data=plotdf, aes(x=variable), length= unit(0.03, 'npc'), inherit.aes = F) +       
        scale_y_continuous(limits = c(ymin, ymax), labels= scales::number_format(accuracy=0.01)) +
        scale_x_continuous(limits = quantile(plotdf$variable, probs=c(0.1, 0.9)))
    }
  }
  
  return(plots)
}
