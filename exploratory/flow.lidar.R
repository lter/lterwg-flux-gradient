#CHM https://www.neonscience.org/resources/learning-hub/tutorials/create-chm-rasters-r

# canopy rugosity: https://rdrr.io/github/akamoske/canopyLazR/man/toc.rugosity.html


# canopy gaps: https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13211
library(terra)
library(viridis)
#install.packages("remotes")
#remotes::install_github("akamoske/canopyLazR")
library('canopyLazR')

abby <- rast('/Volumes/MaloneLab/Research/FluxGradient/NEON_indices-veg-spectrometer-mosaic/Output/ABBY/2017_CHM_2017.tif' )

plot(abby)
global(abby, fun ='mean', na.rm=T)
global(abby, fun ='sd', na.rm=T) 



abby.rugosity <-toc.rugosity(chm.raster = abby, xy.res = 1, z.res=1)
library(dplyr)
abby.rugosity %>% plot


