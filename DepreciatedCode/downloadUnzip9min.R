library("neonUtilities")

savepath <- 'C:/Users/csturtevant/Dropbox/Proposals/FluxGradient'

sitecode <- 'JORN'
startdate <- '2022-03-01'
enddate <- '2022-04-01'
Pack <- 'basic'
neonUtilities::zipsByProduct(dpID="DP4.00200.001", sitecode,startdate, enddate,package=Pack, check.size=F,savepath=savepath)


#run through all the files and unzip
pathDnld <- fs::path(savepath,'filesToStack00200')
files <- list.files(path=pathDnld)
for(file in files){
  # Is the file zipped?
  if(grepl(pattern='.zip',file)){
    utils::unzip(fs::path(pathDnld,file),exdir=fs::path(savepath,'unzippedFiles'))
  }
}
# Run through all the unzipped files and concatenate
pathUnzipped <- fs::path(savepath,'unzippedFiles')
print(paste0('Unzipped data in ',pathUnzipped))
