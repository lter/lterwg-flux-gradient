# Data must already be unzipped
# Make sure you source the functions concatAndFilterH5
source('./scripts/def.concat_from_h5.R')
source('./scripts/wrap.concatAndFilterH5.R')

pathUnzipped <- 'C:/Users/csturtevant/Dropbox/Proposals/FluxGradient/unzippedFiles'
sitecode <- 'JORN'

# H5 paths to extract with the data variables (note - the tables must all have the same columns)
dirHdf5Data <- c(paste0('/',sitecode,'/dp01/data/ch4Conc/000_040_09m/rtioMoleDryCh4'),
             paste0('/',sitecode,'/dp01/data/ch4Conc/000_030_09m/rtioMoleDryCh4'),
             paste0('/',sitecode,'/dp01/data/ch4Conc/000_020_09m/rtioMoleDryCh4'),
             paste0('/',sitecode,'/dp01/data/ch4Conc/000_010_09m/rtioMoleDryCh4'),
             paste0('/',sitecode,'/dp01/data/isoCo2/000_040_09m/rtioMoleDryCo2'),
             paste0('/',sitecode,'/dp01/data/isoCo2/000_030_09m/rtioMoleDryCo2'),
             paste0('/',sitecode,'/dp01/data/isoCo2/000_020_09m/rtioMoleDryCo2'),
             paste0('/',sitecode,'/dp01/data/isoCo2/000_010_09m/rtioMoleDryCo2'),
             paste0('/',sitecode,'/dp01/data/isoCo2/000_040_09m/rtioMoleDryH2o'),
             paste0('/',sitecode,'/dp01/data/isoCo2/000_030_09m/rtioMoleDryH2o'),
             paste0('/',sitecode,'/dp01/data/isoCo2/000_020_09m/rtioMoleDryH2o'),
             paste0('/',sitecode,'/dp01/data/isoCo2/000_010_09m/rtioMoleDryH2o')
)# array of paths

# H5 paths to extract with the final flags for the above variables (note - the tables must all have the same columns, and should correspond to the data listed above)
dirHdf5Qf <- c(paste0('/',sitecode,'/dp01/qfqm/ch4Conc/000_040_09m/rtioMoleDryCh4'),
               paste0('/',sitecode,'/dp01/qfqm/ch4Conc/000_030_09m/rtioMoleDryCh4'),
               paste0('/',sitecode,'/dp01/qfqm/ch4Conc/000_020_09m/rtioMoleDryCh4'),
               paste0('/',sitecode,'/dp01/qfqm/ch4Conc/000_010_09m/rtioMoleDryCh4'),
             paste0('/',sitecode,'/dp01/qfqm/isoCo2/000_040_09m/rtioMoleDryCo2'),
             paste0('/',sitecode,'/dp01/qfqm/isoCo2/000_030_09m/rtioMoleDryCo2'),
             paste0('/',sitecode,'/dp01/qfqm/isoCo2/000_020_09m/rtioMoleDryCo2'),
             paste0('/',sitecode,'/dp01/qfqm/isoCo2/000_010_09m/rtioMoleDryCo2'),
             paste0('/',sitecode,'/dp01/qfqm/isoCo2/000_040_09m/rtioMoleDryH2o'),
             paste0('/',sitecode,'/dp01/qfqm/isoCo2/000_030_09m/rtioMoleDryH2o'),
             paste0('/',sitecode,'/dp01/qfqm/isoCo2/000_020_09m/rtioMoleDryH2o'),
             paste0('/',sitecode,'/dp01/qfqm/isoCo2/000_010_09m/rtioMoleDryH2o')
)# array of paths

# Variable of interest
var <- 'rtioMoleDryCh4'
Levels <- c(1,2,3,4)

# Do the stuff
library(plotly)

dataFilt <- wrap.concatAndFilterH5(pathUnzipped,dirHdf5Data,dirHdf5Qf)

# Get diel average concentrations
dataFilt$decday <- lubridate::yday(dataFilt$timeBgn)+dataFilt$timeBgn$hour/24 + dataFilt$timeBgn$min/24/60
breaks <- seq(from=0,to=1,by=1/24/2)
dataFilt$binDiel <- .bincode(x=dataFilt$decday-floor(dataFilt$decday),breaks=breaks,right=TRUE,include.lowest=TRUE)

binDiel <- 1:48

for(lvl in Levels){
  dielAvgCh4Lvl <- unlist(lapply(binDiel,FUN=function(bin){
  mean(dataFilt$mean[dataFilt$binDiel==bin & dataFilt$var==var & dataFilt$level == lvl],na.rm=TRUE)
  }
  ))
  if(lvl == Levels[1]){
    dielAvgCh4 <- data.frame(binDiel=binDiel,level=lvl,mean=dielAvgCh4Lvl)
  } else {
    dielAvgCh4 <- rbind(dielAvgCh4,data.frame(binDiel=binDiel,level=lvl,mean=dielAvgCh4Lvl))
  }
}


# Plot the diel average pattern 
plot <- plotly::plot_ly(data=dielAvgCh4, x=~binDiel, y=~mean, split = ~level, type='scatter', mode='lines') %>%
  plotly::layout(margin = list(b = 50, t = 50, r=50),
                 title = 'Diel Average',
                 xaxis = list(title = base::paste0(c(rep("\n&nbsp;", 3),
                                                     rep("&nbsp;", 20),
                                                     paste0("Bin"),
                                                     rep("&nbsp;", 20)),
                                                   collapse = ""),
                              nticks=6,
                              range = c(1,48),
                              zeroline=FALSE
                 ),
                 yaxis = list(title = var),
                 showlegend=TRUE)

print(plot)

# Plot the timeseries
dataPlot <- dataFilt[dataFilt$var==var,]
plot <- plotly::plot_ly(data=dataPlot, x=~decday, y=~mean, split = ~level, type='scatter', mode='lines') %>%
  plotly::layout(margin = list(b = 50, t = 50, r=50),
                 title = '9-min CH4',
                 xaxis = list(title = base::paste0(c(rep("\n&nbsp;", 3),
                                                     rep("&nbsp;", 20),
                                                     paste0("DOY"),
                                                     rep("&nbsp;", 20)),
                                                   collapse = ""),
                              nticks=6,
                              range = c(min(dataFilt$decday,na.rm=TRUE),max(dataFilt$decday,na.rm=TRUE)),
                              zeroline=FALSE
                 ),
                 yaxis = list(title = var),
                 showlegend=TRUE)

print(plot)


