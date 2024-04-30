# Spatial Homogeneity:

rm(list=ls())

# Load shapefile created in Site.Spatial.Homo:
load('/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/data/NEONLTERsiteBuffers.Rdata')

library( sf)
library(terra)
library(AOI)
library(ggplot2)
library(tidyverse)
library(tidyterra)

# Data Prep : ####
# Need to download a different date for GUAN. the data doesnt 
# Load EVI tiffs for NEON sites:
BONA.evi <- rast("/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/NEON_indices-veg-spectrometer-mosaic/SiteEVI/BONA_evi_2021.tif")
CPER.evi <- rast("/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/NEON_indices-veg-spectrometer-mosaic/SiteEVI/CPER_evi_2021.tif")
GUAN.evi <- rast("/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/NEON_indices-veg-spectrometer-mosaic/SiteEVI/GUAN_evi_2018.tif")
HARV.evi <- rast("/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/NEON_indices-veg-spectrometer-mosaic/SiteEVI/HARV_evi_2019.tif")
JORN.evi <- rast("/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/NEON_indices-veg-spectrometer-mosaic/SiteEVI/JORN_evi_2021.tif")
KONZ.evi <- rast("/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/NEON_indices-veg-spectrometer-mosaic/SiteEVI/KONZ_evi_2020.tif")
NIWO.evi <- rast("/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/NEON_indices-veg-spectrometer-mosaic/SiteEVI/NIWO_evi_2019.tif")
TOOL.evi <- rast("/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/NEON_indices-veg-spectrometer-mosaic/SiteEVI/TOOL_evi_2020.tif")

# filter the EVI :

filterOutofBounds <- function(raster, min, max){
  raster[raster > max]<- NA
  raster[raster < min]<- NA
  return(raster)
}
BONA.evi <- filterOutofBounds(BONA.evi, max=1, min=0)
CPER.evi <- filterOutofBounds(CPER.evi, max=1, min=0)
GUAN.evi <- filterOutofBounds(GUAN.evi, max=1, min=0)
HARV.evi <- filterOutofBounds(HARV.evi, max=1, min=0)
JORN.evi <- filterOutofBounds(JORN.evi, max=1, min=0)
KONZ.evi <- filterOutofBounds(KONZ.evi, max=1, min=0)
NIWO.evi  <- filterOutofBounds(NIWO.evi , max=1, min=0)
TOOL.evi  <- filterOutofBounds(TOOL.evi , max=1, min=0)

# Subset Site Shapefiles:
BONA.shp <- Site.Buffers %>% filter(Site.Buffers$Site.Id.NEON == "BONA" )
CPER.shp <- Site.Buffers %>% filter(Site.Buffers$Site.Id.NEON == "CPER" )
GUAN.shp <- Site.Buffers %>% filter(Site.Buffers$Site.Id.NEON == "GUAN" )
HARV.shp <- Site.Buffers %>% filter(Site.Buffers$Site.Id.NEON == "HARV" )
JORN.shp <- Site.Buffers %>% filter(Site.Buffers$Site.Id.NEON == "JORN" )
KONZ.shp <- Site.Buffers %>% filter(Site.Buffers$Site.Id.NEON == "KONZ" )
NIWO.shp <- Site.Buffers %>% filter(Site.Buffers$Site.Id.NEON == "NIWO" )
TOOL.shp <- Site.Buffers %>% filter(Site.Buffers$Site.Id.NEON == "TOOL" )

plot(BONA.shp$geometry)
methods(class = "sf")



#crop and mask
BONA.evi.proj.cm <-BONA.evi %>% crop(BONA.shp, mask=TRUE)
CPER.evi.proj.cm <-CPER.evi %>% crop(CPER.shp, mask=TRUE)
GUAN.evi.proj.cm <-GUAN.evi %>% crop(GUAN.shp, mask=TRUE)
HARV.evi.proj.cm <-HARV.evi %>% crop(HARV.shp, mask=TRUE)
JORN.evi.proj.cm <-JORN.evi %>% crop(JORN.shp, mask=TRUE)
KONZ.evi.proj.cm <-KONZ.evi %>% crop(KONZ.shp, mask=TRUE)
NIWO.evi.proj.cm <-NIWO.evi %>% crop(NIWO.shp, mask=TRUE)
TOOL.evi.proj.cm <-TOOL.evi %>% crop(TOOL.shp, mask=TRUE)

# Spatial Figure:

plot.BONA. <- ggplot( ) + geom_spatraster(data= BONA.evi.proj.cm) + geom_sf(data= BONA.shp, fill="transparent", linewidth=1) +   scale_fill_hypso_tint_c( limits = c(0,1),  direction = -1) +   
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

plot.CPER. <- ggplot( ) + geom_spatraster(data= CPER.evi.proj.cm) + geom_sf(data= CPER.shp, fill="transparent", linewidth=1) +   scale_fill_hypso_tint_c( limits = c(0,1),  direction = -1)+   
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

plot.GUAN. <- ggplot( ) + geom_spatraster(data= GUAN.evi.proj.cm) + geom_sf(data= GUAN.shp, fill="transparent", linewidth=1) +   scale_fill_hypso_tint_c( limits = c(0,1),  direction = -1) +   
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

plot.HARV. <- ggplot( ) + geom_spatraster(data= HARV.evi.proj.cm) + geom_sf(data= HARV.shp, fill="transparent", linewidth=1) +   scale_fill_hypso_tint_c( limits = c(0,1),  direction = -1) +   
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

plot.JORN. <- ggplot( ) + geom_spatraster(data= JORN.evi.proj.cm) + geom_sf(data= JORN.shp, fill="transparent", linewidth=1) +   scale_fill_hypso_tint_c( limits = c(0,1),  direction = -1) +   
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

plot.KONZ. <- ggplot( ) + geom_spatraster(data= KONZ.evi.proj.cm) + geom_sf(data= KONZ.shp, fill="transparent", linewidth=1) +   scale_fill_hypso_tint_c( limits = c(0,1),  direction = -1) +   
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

plot.NIWO. <- ggplot( ) + geom_spatraster(data= NIWO.evi.proj.cm) + geom_sf(data= NIWO.shp, fill="transparent", linewidth=1) +   scale_fill_hypso_tint_c( limits = c(0,1),  direction = -1) +   
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

plot.TOOL. <- ggplot( ) + geom_spatraster(data= TOOL.evi.proj.cm) + geom_sf(data= TOOL.shp, fill="transparent", linewidth=1) +   scale_fill_hypso_tint_c( limits = c(0,1),  direction = -1) +   
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())


library(ggpubr)

ggarrange(plot.HARV., plot.GUAN., plot.BONA., plot.CPER.,
          plot.TOOL., plot.KONZ.,plot.NIWO., plot.JORN., 
          labels= c('A', 'B', 'C', 'D',
                    'E', 'F','G', 'H'),
          ncol=4, nrow=2,
          common.legend = TRUE)


# Calculate zonal statistics for the buffers:

zone.var <- function( shp, rast){
 
   shp$VAR <- NA

  for ( i in 1:length( shp$VAR)){
    print(i)
    evi <- terra::extract( rast, shp[i,])[,2] %>% as.data.frame()
    evi.f <- evi %>% filter(. <= 1)
    shp$VAR[i] <- sd( evi.f$., na.rm=T) %>% round(digits = 3)
    rm( evi, evi.f)
  }
  
  return(shp)
}

BONA.shp.var <- zone.var(BONA.shp, BONA.evi.proj.cm)
CPER.shp.var <- zone.var(CPER.shp, CPER.evi.proj.cm)
GUAN.shp.var <- zone.var(GUAN.shp, GUAN.evi.proj.cm)
HARV.shp.var <- zone.var(HARV.shp, HARV.evi.proj.cm)
JORN.shp.var <- zone.var(JORN.shp, JORN.evi.proj.cm)
KONZ.shp.var <- zone.var(KONZ.shp, KONZ.evi.proj.cm)
NIWO.shp.var <- zone.var(NIWO.shp, NIWO.evi.proj.cm)
TOOL.shp.var <- zone.var(TOOL.shp, TOOL.evi.proj.cm)

save(BONA.shp.var, CPER.shp.var, GUAN.shp.var,
     HARV.shp.var, JORN.shp.var, KONZ.shp.var, 
     NIWO.shp.var, TOOL.shp.var, 
     BONA.evi.proj.cm,
     CPER.evi.proj.cm,
     GUAN.evi.proj.cm, 
     HARV.evi.proj.cm,
     JORN.evi.proj.cm,
     KONZ.evi.proj.cm,
     NIWO.evi.proj.cm, 
     TOOL.evi.proj.cm,
     file='/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/data/NEONLTERsite_varBuffers.Rdata')


# Homo exploration: ####

load('/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/data/NEONLTERsite_varBuffers.Rdata')

# fit lines to the var versus distance to plot for all sites together:

plot.dist.BONA <- ggplot( ) + 
  geom_smooth( data= BONA.shp.var, aes( x=dist_m, y = VAR), color="black", method=lm) +
  geom_point( data= BONA.shp.var, aes( x=dist_m, y = VAR), color="black")+
  xlab("Buffer (meters) ") + ylab("Standard Deviation") + ylim(0, 0.3)


plot.dist.CPER <-ggplot( ) +
  geom_smooth( data= CPER.shp.var, aes( x=dist_m, y = VAR), color="black", method=lm) +
  geom_point( data= CPER.shp.var, aes( x=dist_m, y = VAR), color="black")+
  xlab("Buffer (meters) ") + ylab("Standard Deviation")+ ylim(0, 0.3)
  
  
plot.dist.GUAN <-ggplot( ) + 
  geom_smooth( data= GUAN.shp.var, aes( x=dist_m, y = VAR), color="black", method=lm) +
  geom_point( data= GUAN.shp.var, aes( x=dist_m, y = VAR), color="black")+
  xlab("Buffer (meters) ") +ylab("Standard Deviation")+ ylim(0, 0.3)
  

plot.dist.HARV <-ggplot( ) + 
  geom_smooth( data= HARV.shp.var, aes( x=dist_m, y = VAR), color="black", method=lm) +
  geom_point( data= HARV.shp.var, aes( x=dist_m, y = VAR), color="black") + ylim(0,0.3)+
  xlab("Buffer (meters) ") + ylab("Standard Deviation")+ ylim(0, 0.3)

plot.dist.KONZ <-ggplot( ) +
  geom_smooth( data= KONZ.shp.var, aes( x=dist_m, y = VAR), color="black", method=lm) +
  geom_point( data= KONZ.shp.var, aes( x=dist_m, y = VAR), color="black")+ ylim(0,0.3)+
  xlab("Buffer (meters) ") + ylab("Standard Deviation")+ ylim(0, 0.3)

plot.dist.TOOL <-plot.dist.BONA <-ggplot( ) +
  geom_smooth( data= TOOL.shp.var, aes( x=dist_m, y = VAR), color="black", method=lm) +
  geom_point( data= TOOL.shp.var, aes( x=dist_m, y = VAR), color="black")+ ylim(0,0.3)+
  xlab("Buffer (meters) ") + ylab("Standard Deviation")+ ylim(0, 0.3)


plot.dist.JORN <-ggplot( ) +
  geom_smooth( data= JORN.shp.var, aes( x=dist_m, y = VAR), color="black", method=lm) +
  geom_point( data= JORN.shp.var, aes( x=dist_m, y = VAR), color="black")+ ylim(0,0.3)+
  xlab("Buffer (meters) ") + ylab("Standard Deviation")+ ylim(0, 0.3)

plot.dist.NIWO <-ggplot( ) +
  geom_smooth( data= NIWO.shp.var, aes( x=dist_m, y = VAR), color="black", method=lm) +
  geom_point( data= NIWO.shp.var, aes( x=dist_m, y = VAR), color="black")+ ylim(0,0.3) +
  xlab("Buffer (meters) ") + ylab("Standard Deviation")+ ylim(0, 0.3)

# Create the plot and layout

ggarrange(plot.dist.HARV, plot.dist.GUAN, plot.dist.BONA, plot.dist.CPER,
          plot.dist.TOOL, plot.dist.KONZ,plot.dist.NIWO, plot.dist.JORN, 
          labels= c('A', 'B', 'C', 'D',
                    'E', 'F','G', 'H'),
          ncol=4, nrow=2,
          common.legend = TRUE)

# Create  Summary Table of these  results


# the slope is the measure of homogeniety.
table.delta.var <- data.frame( site = c('HARV', 'GUAN', 'BONA', 'CPER', 
                                          'TOOL', 'KONZ', 'NIWO', 'JORN' ))
site <- 'HARV'
Delta.var.evi <- function( site, shp){
  lm.site <- summary(lm( data= shp,VAR ~ dist_m))
  lm.site$coefficients[2]
  return( lm.site$coefficients[2])
}

table.delta.var$Delta.var <-NA

table.delta.var$Delta.var[ table.delta.var$site == 'HARV'] <- Delta.var.evi(i, HARV.shp.var) %>%  round(6)
table.delta.var$Delta.var[ table.delta.var$site == 'GUAN'] <- Delta.var.evi(i, GUAN.shp.var) %>%  round(6)
table.delta.var$Delta.var[ table.delta.var$site == 'BONA'] <- Delta.var.evi(i, BONA.shp.var) %>%  round(6)
table.delta.var$Delta.var[ table.delta.var$site == 'CPER'] <- Delta.var.evi(i, CPER.shp.var) %>%  round(6)
table.delta.var$Delta.var[ table.delta.var$site == 'TOOL'] <- Delta.var.evi(i, TOOL.shp.var) %>%  round(6)
table.delta.var$Delta.var[ table.delta.var$site == 'KONZ'] <- Delta.var.evi(i, KONZ.shp.var) %>%  round(6)
table.delta.var$Delta.var[ table.delta.var$site == 'NIWO'] <- Delta.var.evi(i, NIWO.shp.var) %>%  round(6)
table.delta.var$Delta.var[ table.delta.var$site == 'JORN'] <- Delta.var.evi(i, JORN.shp.var) %>%  round(6)





# Create a panel of the EVI and buffer for SITES:
ggplot( ) + geom_spatraster(data = BONA.evi.proj.cm) + geom_sf( data = BONA.shp, color="black", fill=NA) +
  scale_fill_whitebox_c(limits = c(0,1), direction = -1)
ggplot( ) + geom_spatraster(data = CPER.evi.proj.cm) + geom_sf( data = CPER.shp, color="black", fill=NA)+
  scale_fill_whitebox_c(limits = c(0,1), direction = -1)
ggplot( ) + geom_spatraster(data = GUAN.evi.proj.cm) + geom_sf( data = GUAN.shp, color="black", fill=NA)+
  scale_fill_whitebox_c(limits = c(0,1), direction = -1)
ggplot( ) + geom_spatraster(data = HARV.evi.proj.cm) + geom_sf( data = HARV.shp, color="black", fill=NA)+
  scale_fill_whitebox_c(limits = c(0,1), direction = -1)
ggplot( ) + geom_spatraster(data = KONZ.evi.proj.cm) + geom_sf( data = KONZ.shp, color="black", fill=NA)+
  scale_fill_whitebox_c(limits = c(0,1), direction = -1)
ggplot( ) + geom_spatraster(data = TOOL.evi.proj.cm) + geom_sf( data = TOOL.shp, color="black", fill=NA)+
  scale_fill_whitebox_c(limits = c(0,1), direction = -1)
ggplot( ) + geom_spatraster(data = JORN.evi.proj.cm) + geom_sf( data = JORN.shp, color="black", fill=NA)+
  scale_fill_whitebox_c(limits = c(0,1), direction = -1)
ggplot( ) + geom_spatraster(data = NIWO.evi.proj.cm) + geom_sf( data = NIWO.shp, color="black", fill=NA)+
  scale_fill_whitebox_c(limits = c(0,1), direction = -1)


# Mean SD

mean(BONA.shp.var$VAR)
mean(KONZ.shp.var$VAR)
mean(NIWO.shp.var$VAR)

mean(GUAN.shp.var$VAR)
mean(HARV.shp.var$VAR)

mean(CPER.shp.var$VAR)
mean(TOOL.shp.var$VAR)
mean(JORN.shp.var$VAR)





