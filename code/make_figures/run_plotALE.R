
library(gridExtra); library(cowplot)
# final analysis uses allvar = T and centerscale = F
allvars <- T
centerscale <- F
source('./code/make_figures/plot_ALE_function.R') # update plotting figure to trim x axis to 10-90 quantiles

if (centerscale == F) {
  springabund <- plot_ALE(season='spring', response='abundance', allvars=allvars, centerscale=centerscale, 
                          ymin=-0.05, ymax=0.05, yearmin=-0.05, yearmax=0.05)
} else if (centerscale == T) {
  springabund <- plot_ALE(season='spring', response='abundance', allvars=allvars, centerscale=centerscale, 
                          ymin=-0.2, ymax=0.2)
}

springrich <- plot_ALE(season='spring', response='richness', allvars=allvars, centerscale=centerscale, 
                       ymin=-1.4, ymax=1.4,  yearmin=-1.4, yearmax=1.4)

summerabund <- plot_ALE(season='summer', response='abundance', allvars=allvars, centerscale=centerscale, 
                        ymin=-0.05, ymax=0.05, yearmin=-0.05, yearmax=0.05)

summerrich <- plot_ALE(season='summer', response='richness', allvars=allvars, centerscale=centerscale, 
                       ymin=-1.4, ymax=1.4, yearmin=-1.4, yearmax=1.4)

springabund; springrich; summerabund; summerrich

SA <- springabund; SR <- springrich; UA <- summerabund; UR <- summerrich
SA[[1]] <- directlabels::direct.label(SA[[1]], list(directlabels::dl.trans(y=y+0.15, x=x), 'top.bumpup'))
SA[[2]] <- directlabels::direct.label(SA[[2]], list(directlabels::dl.trans(y=y+0.15, x=x-0.8), 'top.bumpup'))
SA[[3]] <- directlabels::direct.label(SA[[3]], list(directlabels::dl.trans(y=y+0.15, x=x-1.6), 'top.bumpup'))
SA[[4]] <- directlabels::direct.label(SA[[4]], list(directlabels::dl.trans(y=y+0.15, x=x-1.2), 'top.bumpup'))
SA[[5]] <- directlabels::direct.label(SA[[5]], list(directlabels::dl.trans(y=y+0.15, x=x-0.8), 'top.bumpup'))
SA[[6]] <- directlabels::direct.label(SA[[6]], list(directlabels::dl.trans(y=y+0.15, x=x+1.2), 'top.bumpup'))
SA[[7]] <- directlabels::direct.label(SA[[7]], list(directlabels::dl.trans(y=y+0.15, x=x-0.8), 'top.bumpup'))
SA[[8]] <- directlabels::direct.label(SA[[8]], list(directlabels::dl.trans(y=y+0.35), 'top.bumpup'))

#SA[[7]] <- directlabels::direct.label(SA[[7]], list(directlabels::dl.trans(y=y-0.75), 'top.bumpup'))


SR[[1]] <- directlabels::direct.label(SR[[1]], list(directlabels::dl.trans(y=y+0.15, x=x+1.3), 'top.bumpup'))
SR[[2]] <- directlabels::direct.label(SR[[2]], list(directlabels::dl.trans(y=y+0.15, x=x-1.2), 'top.bumpup'))
SR[[3]] <- directlabels::direct.label(SR[[3]], list(directlabels::dl.trans(y=y+0.15, x=x+1), 'top.bumpup'))
SR[[4]] <- directlabels::direct.label(SR[[4]], list(directlabels::dl.trans(y=y+0.15, x=x+1.3), 'top.bumpup'))
SR[[5]] <- directlabels::direct.label(SR[[5]], list(directlabels::dl.trans(y=y-0.75), 'top.bumpup'))
SR[[6]] <- directlabels::direct.label(SR[[6]], list(directlabels::dl.trans(y=y+0.15, x=x+1.3), 'top.bumpup'))


UA[[1]] <- directlabels::direct.label(UA[[1]], list(directlabels::dl.trans(y=y+0.25, x=x-1.5), 'top.bumpup'))
UA[[2]] <- directlabels::direct.label(UA[[2]], list(directlabels::dl.trans(y=y+0.25, x=x+1.5), 'top.bumpup'))
UA[[3]] <- directlabels::direct.label(UA[[3]], list(directlabels::dl.trans(y=y+0.25, x=x-0.3), 'top.bumpup'))
UA[[4]] <- directlabels::direct.label(UA[[4]], list(directlabels::dl.trans(y=y+0.25, x=x-1), 'top.bumpup'))
UA[[5]] <- directlabels::direct.label(UA[[5]], list(directlabels::dl.trans(y=y-0.75, x=x+0.5), 'top.bumpup'))


UR[[1]] <- directlabels::direct.label(UR[[1]], list(directlabels::dl.trans(y=y+0.15), 'top.bumpup'))
UR[[2]] <- directlabels::direct.label(UR[[2]], list(directlabels::dl.trans(y=y+0.15), 'top.bumpup'))
UR[[3]] <- directlabels::direct.label(UR[[3]], list(directlabels::dl.trans(y=y+0.15), 'top.bumpup'))
UR[[4]] <- directlabels::direct.label(UR[[4]], list(directlabels::dl.trans(y=y+0.15, x=x+0.5), 'top.bumpup'))
UR[[5]] <- directlabels::direct.label(UR[[5]], list(directlabels::dl.trans(y=y+0.15, x=x+0.8), 'top.bumpup'))
UR[[6]] <- directlabels::direct.label(UR[[6]], list(directlabels::dl.trans(y=y+0.35, x=x+0.5), 'top.bumpup'))

#UR[[4]] <- directlabels::direct.label(UR[[4]], list(directlabels::dl.trans(y=y-0.75), 'top.bumpup'))

##### Spring ALE plots 
plot_grid(SA[[1]], SA[[2]], SA[[3]], SA[[4]],
           SR[[1]], SR[[2]], SR[[3]], SR[[4]], ncol=2)

if (centerscale == T) {
  ggplot2::ggsave(filename= 'ALE_plot_Spring_CenterScale.svg', device='svg', path='./figures/', units='in',
                  width=7, height=9, dpi="retina")
} else {
  ggplot2::ggsave(filename= 'ALE_plot_Spring.svg', device='svg', path='./figures/', units='in',
                  width=7, height=9, dpi="retina")
}

# extra spring ALE plots 
plot_grid(SA[[5]], SA[[6]], SA[[7]], SR[[6]], ncol=2)

if (centerscale == T) {
  ggplot2::ggsave(filename= 'ALE_plot_SpringExtra_CenterScale.svg', device='svg', path='./figures/', units='in',
                  width=7, height=4.5, dpi="retina")
} else {
  ggplot2::ggsave(filename= 'ALE_plot_SpringExtra.svg', device='svg', path='./figures/', units='in',
                  width=7, height=4.5, dpi="retina")
}

##### Summer ALE plots
plot_grid(UA[[1]], UA[[2]], UA[[3]], UA[[4]],
          UR[[1]], UR[[3]], UR[[4]], UR[[5]], ncol=2)

if (centerscale == T) {
  ggplot2::ggsave(filename= 'ALE_plot_Summer_CenterScale.svg', device='svg', path='./figures/', units='in',
                  width=12, height=5, dpi="retina")
} else {
  ggplot2::ggsave(filename= 'ALE_plot_Summer.svg', device='svg', path='./figures/', units='in',
                  width=7, height=9, dpi="retina")
}

##### Year ALE plot
plot_grid(SA[[8]], UA[[5]], SR[[5]], UR[[6]], ncol=2)

if (centerscale == T) {
  ggplot2::ggsave(filename= 'ALE_plot_Year_CenterScale.svg', device='svg', path='./figures/', units='in',
                  width=8, height=6, dpi="retina")
} else {
  ggplot2::ggsave(filename= 'ALE_plot_Year.svg', device='svg', path='./figures/', units='in',
                  width=7, height=4.5, dpi="retina")
}


