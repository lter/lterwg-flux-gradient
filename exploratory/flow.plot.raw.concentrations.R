rm(list=ls())

# This script requires download of the IP0 data from the NEON GCS bucket:
# https://console.cloud.google.com/storage/browser/neon-sae-files/ods/dataproducts/IP0;tab=objects

#fileDnld <- 'F:/FluxGradientProject/IP0/KONZ/NEON.D06.KONZ.IP0.00200.001.ecse.2021-08-01.l0p.h5.gz'
fileDnld <- 'F:/FluxGradientProject/IP0/TOOL/NEON.D18.TOOL.IP0.00200.001.ecse.2021-08-25.l0p.h5.gz'
var <- 'tempWbox'
lvl <- c('000_040','000_030','000_020','000_010')
dirHdf5pre <- '/TOOL/dp0p/data/crdCo2'

# Is the file zipped?
if(grepl(pattern='.gz',fileDnld)){
  system(paste0('gzip -d ',fileDnld))
  fileDnld <- base::substr(fileDnld,1,nchar(fileDnld)-3)
}

# Grab the file structure
listObj <- base::try(rhdf5::h5ls(fileDnld, datasetinfo = FALSE),silent=TRUE)

if (base::class(listObj) == "try-error"){
  base::stop('Cannot open file. Aborting...')
}

# Combine path and name
listObjName <- base::paste(listObj$group, listObj$name, sep = "/") # combined path and name

# Read in the data
numDir <- base::length(lvl)
data <- vector(mode='list',length=numDir)
base::names(data) <- lvl
for(idxLvl in lvl){
  dirHdf <- paste0(dirHdf5pre,'/',idxLvl,'/',var)
  dataIdx <- base::try(rhdf5::h5read(file=fileDnld,name=dirHdf),silent=TRUE)
  
  # Error-check
  if(base::class(dataIdx) == 'try-error'){
    # Close the file
    rhdf5::h5closeAll()
    
    # Remove downloaded file if selected
    # if(Rm){
    #   base::unlink(fileDnld)
    # }
    
    # Did we want the whole file?
    if(dirHdf == '/'){
      base::stop('File is unreadable. Aborting...')
    } else {
      base::stop(base::paste0('Could not retrieve ',dirHdf,' from file ',fileDnld,'. Aborting...'))
    }
  } 
  
  dataIdx <- data.frame(idx=1:length(dataIdx),conc=dataIdx,lvl=idxLvl)
  data[[idxLvl]] <- dataIdx
  
}

dataAll <- do.call('rbind',data)

# Close the file
rhdf5::h5closeAll()

library(plotly)
plot <- plotly::plot_ly(data=dataAll, x=~idx, y=~conc, split=~lvl,type='scatter', mode='markers') %>%
  plotly::layout(margin = list(b = 50, t = 50, r=50),
                 title = 'Concentration trace',
                 xaxis = list(title = base::paste0(c(rep("\n&nbsp;", 3),
                                                     rep("&nbsp;", 20),
                                                     paste0("Seconds"),
                                                     rep("&nbsp;", 20)),
                                                   collapse = ""),
                              #range = c(1,48),
                              zeroline=TRUE
                 ),
                 yaxis = list(title = paste0(var,' Concentration')),
                 showlegend=TRUE)

print(plot)