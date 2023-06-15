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
  
  season_newname <- 'early'
  
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

  season_newname <- 'late'
  
  
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
    var <- c("PctLand_Wetland",'PctLand_Water', "PctLand_Natural", "P_ppm_mean", "CV_FA.all.land", 
             "K_ppm_mean", "NMDS_mmt","Year")
    varname <- c("Percent Wetland", "Percent Water", "Percent Natural", "Soil phosphorus (ppm)",
                 "CV floral area (all sp.), landscape","Soil potassium (ppm)",
                 "Plant composition, mmt intensity", "Year")
    if (centerscale == T) {
      ylab <- expression(Normalized~Abund~Day^-1~Trap^-1)
    } else {
      ylab <- expression(Delta~Abund~Day^-1~Trap^-1)
    }
    colortouse <- "#0072B2"
    
  } else if (season == 'spring' & response == 'richness') {
    var <- c("distance_to_water", "ed", "elevation", "PctLand_Water", "PctLand_Developed", "CV_FA.all.land","richness.IP", "Year")
    varname <- c("Distance to water", "Edge density", "Elevation",
                 "Percent Water", "Percent Developed", "CV floral area (all sp.), local", "Plant richness (insect-pollinated)", "Year")
    if (centerscale == T) {
      ylab <- "Normalized richness"
    } else {
      ylab <- expression(Delta~Species~richness)
    }
    colortouse <- "#0072B2"
    
  } else if (season == 'summer' & response == 'abundance') {
    var <- c("NMDS_mmt", "fall_total_FA.all.site", "mean_pct_cover","Grav_WaterContent_g.g_mean", "Year")
    varname <- c("Plant composition, mmt intensity", "Fall floral area (all sp.), local",
                  "Percent plant cover", "Soil gravimetric water content", "Year")
    if (centerscale == T) {
      ylab <- expression(Normalized~Abund~Day^-1~Trap^-1)
    } else {
      ylab <- expression(Delta~Abund~Day^-1~Trap^-1)
    }
    colortouse <- "#E69F00"
    
  } else if (season == 'summer' & response == 'richness') {
    var <- c("PctLand_Agriculture", "bulk_density_mean", "OM_Pct_mean", "Total_N_Pct_mean",
              'aspectNS', 'Grav_WaterContent_g.g_mean', 'summer_total_FA.IP.site', 'PctLand_Natural', "Year")
    varname <- c("Percent Agriculture", expression(Soil~bulk~density~(g~per~cm^3)), "Soil organic matter (%)", "Soil total nitrogen (%)", 
                  "Aspect North-South", "Soil gravimetric water content", "Summer total floral area (insect-pollinated), local", 
                  "Percent Natural", "Year")
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
    plotdf$ID <- paste0(season_newname, " ", response) 
    
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
        geom_line(aes(color=ID), linewidth=1.5) +
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
