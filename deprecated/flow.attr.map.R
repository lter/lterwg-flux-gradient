# View site attributes and create a site visualization:
library(tidyverse)
library(sf)
library(AOI)

setwd('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/Data')

dir <- c('HARV/data/HARV/HARV_attr.Rdata',
         'GUAN/data/GUAN/GUAN_attr.Rdata',
         'JORN/data/JORN/JORN_attr.Rdata',
         'KONZ/data/KONZ/KONZ_attr.Rdata')

site.att <- data.frame()

for( i in 1:length(dir)){
  print(i)
  load(dir[i])
  site.att <- site.att %>% rbind(attr.df )
  rm(attr.df)
}

site.att %>% names
site.att.sf <- st_as_sf(x = site.att,                         
               coords = c("LonTow", "LatTow"),
               crs = 4326)

site.att.sf$geometry %>% plot

aoi.usa <- aoi_get(country = c('PR'),
                   state = 'conus')

ggplot() + geom_sf(data = aoi.usa, fill='white', color="navy", lwd=1.5) + geom_sf(data = site.att.sf, size=2, color = 'goldenrod') + theme_bw()
