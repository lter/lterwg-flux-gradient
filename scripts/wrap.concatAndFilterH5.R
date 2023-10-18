# pathUnzipped <- 'C:/Users/csturtevant/Dropbox/Proposals/FluxGradient/unzippedFiles'

# dirHdf5Data <- c(paste0('/',sitecode,'/dp01/data/ch4Conc/000_040_09m/rtioMoleDryCh4'),
#                  paste0('/',sitecode,'/dp01/data/ch4Conc/000_030_09m/rtioMoleDryCh4'),
#                  paste0('/',sitecode,'/dp01/data/ch4Conc/000_020_09m/rtioMoleDryCh4'),
#                  paste0('/',sitecode,'/dp01/data/ch4Conc/000_010_09m/rtioMoleDryCh4'),
#                  paste0('/',sitecode,'/dp01/data/isoCo2/000_040_09m/rtioMoleDryCo2'),
#                  paste0('/',sitecode,'/dp01/data/isoCo2/000_030_09m/rtioMoleDryCo2'),
#                  paste0('/',sitecode,'/dp01/data/isoCo2/000_020_09m/rtioMoleDryCo2'),
#                  paste0('/',sitecode,'/dp01/data/isoCo2/000_010_09m/rtioMoleDryCo2'),
#                  paste0('/',sitecode,'/dp01/data/isoCo2/000_040_09m/rtioMoleDryH2o'),
#                  paste0('/',sitecode,'/dp01/data/isoCo2/000_030_09m/rtioMoleDryH2o'),
#                  paste0('/',sitecode,'/dp01/data/isoCo2/000_020_09m/rtioMoleDryH2o'),
#                  paste0('/',sitecode,'/dp01/data/isoCo2/000_010_09m/rtioMoleDryH2o')
# )# array of paths

# # H5 paths to extract with the final flags for the above variables (note - the tables must all have the same columns, and should correspond to the data listed above)
# dirHdf5Qf <- c(paste0('/',sitecode,'/dp01/qfqm/ch4Conc/000_040_09m/rtioMoleDryCh4'),
#                paste0('/',sitecode,'/dp01/qfqm/ch4Conc/000_030_09m/rtioMoleDryCh4'),
#                paste0('/',sitecode,'/dp01/qfqm/ch4Conc/000_020_09m/rtioMoleDryCh4'),
#                paste0('/',sitecode,'/dp01/qfqm/ch4Conc/000_010_09m/rtioMoleDryCh4'),
#                paste0('/',sitecode,'/dp01/qfqm/isoCo2/000_040_09m/rtioMoleDryCo2'),
#                paste0('/',sitecode,'/dp01/qfqm/isoCo2/000_030_09m/rtioMoleDryCo2'),
#                paste0('/',sitecode,'/dp01/qfqm/isoCo2/000_020_09m/rtioMoleDryCo2'),
#                paste0('/',sitecode,'/dp01/qfqm/isoCo2/000_010_09m/rtioMoleDryCo2'),
#                paste0('/',sitecode,'/dp01/qfqm/isoCo2/000_040_09m/rtioMoleDryH2o'),
#                paste0('/',sitecode,'/dp01/qfqm/isoCo2/000_030_09m/rtioMoleDryH2o'),
#                paste0('/',sitecode,'/dp01/qfqm/isoCo2/000_020_09m/rtioMoleDryH2o'),
#                paste0('/',sitecode,'/dp01/qfqm/isoCo2/000_010_09m/rtioMoleDryH2o')
# )# array of paths

wrap.concatAndFilterH5 <- function(pathUnzipped,dirHdf5Data,dirHdf5Qf){

  # Grab the data
  data <- def.concat_from_h5(pathUnzipped,dirHdf5Data)
  
  # Grab the quality flags
  qf <- def.concat_from_h5(pathUnzipped,dirHdf5Qf)
  
  # If nrow data and qf are the same, assume timestamps align
  if(nrow(data) == nrow(qf)){
    dataFilt <- data
    dataFilt[qf$qfFinl == 1,'min'] <- NA
    dataFilt[qf$qfFinl == 1,'max'] <- NA
    dataFilt[qf$qfFinl == 1,'mean'] <- NA
    dataFilt[qf$qfFinl == 1,'vari'] <- NA
  } else {
    stop('Cannot produce filtered data. Data and flags data frames do not have the same number of rows')
  }
  
  # Convert ver to tower level
  dataFilt$level <- as.numeric(dataFilt$ver)/10
  
  return(dataFilt)

}

