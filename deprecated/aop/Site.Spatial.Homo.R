# Spatial Homogeneity:

rm(list=ls())

# Load shapefile created in flow.neon.site.simplefeatures:
load('/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/data/NEONLTERsiteBuffers.Rdata')

load('/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/FG_Site_Wdges.RDATA')

library( sf)
library(terra)
library(AOI)
library(ggplot2)
library(tidyverse)
library(tidyterra)
library(ggpubr)

# Data Prep : ####
# Load EVI tiffs for NEON sites made in flow.NEONAOP.EVI.Download.R:
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
BONA.shp <- BONA.wedges
CPER.shp <- CPER.wedges
GUAN.shp <- GUAN.wedges
HARV.shp <- HARV.wedges
JORN.shp <- JORN.wedges
KONZ.shp <- KONZ.wedges
NIWO.shp <- NIWO.wedges
TOOL.shp <- TOOL.wedges

#crop and mask
BONA.evi.proj.cm <-BONA.evi %>% crop(BONA.shp, mask=TRUE)
CPER.evi.proj.cm <-CPER.evi %>% crop(CPER.shp, mask=TRUE)
GUAN.evi.proj.cm <-GUAN.evi %>% crop(GUAN.shp, mask=TRUE)
HARV.evi.proj.cm <-HARV.evi %>% crop(HARV.shp, mask=TRUE)
JORN.evi.proj.cm <-JORN.evi %>% crop(JORN.shp, mask=TRUE)
KONZ.evi.proj.cm <-KONZ.evi %>% crop(KONZ.shp, mask=TRUE)
NIWO.evi.proj.cm <-NIWO.evi %>% crop(NIWO.shp, mask=TRUE)
TOOL.evi.proj.cm <-TOOL.evi %>% crop(TOOL.shp, mask=TRUE)

# Zonal Statistics: #####
# Calculates the %variance (sd(EVI/mean(EVI)))
zone.var <- function( shp, rast){
  evi.mean <- terra::global(rast,"mean", na.rm=T)[,1]
 
   shp$VAR <- NA

  for ( i in 1:length( shp$VAR)){
    print(i)
    evi <- terra::extract( rast, shp[i,])[,2] %>% as.data.frame()
    evi.f <- evi %>% filter(. <= 1)
    shp$VAR[i] <- (sd( evi.f$., na.rm=T) %>% round(digits = 3)/evi.mean)*100
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
     file='/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/data/NEONLTERsite_varBuffersWedges.Rdata')

# Homo exploration: ####

load('/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/data/NEONLTERsite_varBuffers.Rdata')

# Create  Summary Table of these  results
# the slope is the measure of homogeniety.
table.delta.var <- data.frame( site = c('HARV', 'GUAN', 'BONA', 'CPER', 
                                          'TOOL', 'KONZ', 'NIWO', 'JORN' ))

Delta.var.evi.r <- function(shp){
  lm.site <- summary(lm( data= shp,VAR ~ dist_m))
  lm.site$coefficients[2]
  return( lm.site$coefficients[2])
}
Delta.var.evi.w <- function( shp){
  lm.site <- summary(lm( data= shp,VAR ~ wedge))
  lm.site$coefficients[2]
  return( lm.site$coefficients[2])
}

table.delta.var$Delta.var.r <-NA

table.delta.var$Delta.var.r[ table.delta.var$site == 'HARV'] <- Delta.var.evi.r(HARV.shp.var) %>%  round(6)
table.delta.var$Delta.var.r[ table.delta.var$site == 'GUAN'] <- Delta.var.evi.r(GUAN.shp.var) %>%  round(6)
table.delta.var$Delta.var.r[ table.delta.var$site == 'BONA'] <- Delta.var.evi.r(BONA.shp.var) %>%  round(6)
table.delta.var$Delta.var.r[ table.delta.var$site == 'CPER'] <- Delta.var.evi.r(CPER.shp.var) %>%  round(6)
table.delta.var$Delta.var.r[ table.delta.var$site == 'TOOL'] <- Delta.var.evi.r(TOOL.shp.var) %>%  round(6)
table.delta.var$Delta.var.r[ table.delta.var$site == 'KONZ'] <- Delta.var.evi.r(KONZ.shp.var) %>%  round(6)
table.delta.var$Delta.var.r[ table.delta.var$site == 'NIWO'] <- Delta.var.evi.r(NIWO.shp.var) %>%  round(6)
table.delta.var$Delta.var.r[ table.delta.var$site == 'JORN'] <- Delta.var.evi.r(JORN.shp.var) %>%  round(6)

table.delta.var$Delta.var.w <-NA

table.delta.var$Delta.var.w[ table.delta.var$site == 'HARV'] <- Delta.var.evi.w(HARV.shp.var) %>%  round(6)
table.delta.var$Delta.var.w[ table.delta.var$site == 'GUAN'] <- Delta.var.evi.w(GUAN.shp.var) %>%  round(6)
table.delta.var$Delta.var.w[ table.delta.var$site == 'BONA'] <- Delta.var.evi.w(BONA.shp.var) %>%  round(6)
table.delta.var$Delta.var.w[ table.delta.var$site == 'CPER'] <- Delta.var.evi.w(CPER.shp.var) %>%  round(6)
table.delta.var$Delta.var.w[ table.delta.var$site == 'TOOL'] <- Delta.var.evi.w(TOOL.shp.var) %>%  round(6)
table.delta.var$Delta.var.w[ table.delta.var$site == 'KONZ'] <- Delta.var.evi.w(KONZ.shp.var) %>%  round(6)
table.delta.var$Delta.var.w[ table.delta.var$site == 'NIWO'] <- Delta.var.evi.w(NIWO.shp.var) %>%  round(6)
table.delta.var$Delta.var.w[ table.delta.var$site == 'JORN'] <- Delta.var.evi.w(JORN.shp.var) %>%  round(6)

# Add the Mean of the SD across all conditions (Radi and Wedge)
table.delta.var$MEAN.SD[table.delta.var$site == "HARV" ] <- mean(HARV.shp.var$VAR)
table.delta.var$MEAN.SD[table.delta.var$site == "GUAN" ] <- mean(GUAN.shp.var$VAR)
table.delta.var$MEAN.SD[table.delta.var$site == "BONA" ] <- mean(BONA.shp.var$VAR)
table.delta.var$MEAN.SD[table.delta.var$site == "CPER" ] <- mean(CPER.shp.var$VAR)
table.delta.var$MEAN.SD[table.delta.var$site == "TOOL" ] <- mean(TOOL.shp.var$VAR)
table.delta.var$MEAN.SD[table.delta.var$site == "KONZ" ] <- mean(KONZ.shp.var$VAR)
table.delta.var$MEAN.SD[table.delta.var$site == "NIWO" ] <- mean(NIWO.shp.var$VAR)
table.delta.var$MEAN.SD[table.delta.var$site == "JORN" ] <- mean(JORN.shp.var$VAR)

table.delta.var$SD.SD[table.delta.var$site == "HARV" ] <- sd(HARV.shp.var$VAR)
table.delta.var$SD.SD[table.delta.var$site == "GUAN" ] <- sd(GUAN.shp.var$VAR)
table.delta.var$SD.SD[table.delta.var$site == "BONA" ] <- sd(BONA.shp.var$VAR)
table.delta.var$SD.SD[table.delta.var$site == "CPER" ] <- sd(CPER.shp.var$VAR)
table.delta.var$SD.SD[table.delta.var$site == "TOOL" ] <- sd(TOOL.shp.var$VAR)
table.delta.var$SD.SD[table.delta.var$site == "KONZ" ] <- sd(KONZ.shp.var$VAR)
table.delta.var$SD.SD[table.delta.var$site == "NIWO" ] <- sd(NIWO.shp.var$VAR)
table.delta.var$SD.SD[table.delta.var$site == "JORN" ] <- sd(JORN.shp.var$VAR)


p.Delta.SD <-ggplot(data=table.delta.var, aes(x=site, y=SD.SD)) + geom_point(size=5) + ylim(0,10)

p.Delta.radius <- ggplot(data=table.delta.var, aes(x=site, y=Delta.var.r)) + geom_point(size=5) 

p.Delta.wedge <-ggplot(data=table.delta.var, aes(x=site, y=Delta.var.w)) + geom_point(size=5)

ggarrange(p.Delta.SD , p.Delta.radius, p.Delta.wedge, nrow=3, ncol=1)

write.csv( table.delta.var,  '/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/table.delta.var.csv')

# EVI Wedge Figure: #####
# Spatial Figure:

plot.BONA. <- ggplot( ) + geom_spatraster(data= BONA.evi.proj.cm) + geom_sf(data= BONA.shp, fill="transparent", linewidth=1) +   scale_fill_hypso_tint_c( limits = c(0,1),  direction = -1) +   
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + ggtitle("BONA")

plot.CPER. <- ggplot( ) + geom_spatraster(data= CPER.evi.proj.cm) + geom_sf(data= CPER.shp, fill="transparent", linewidth=1) +   scale_fill_hypso_tint_c( limits = c(0,1),  direction = -1)+   
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + ggtitle("CPER")

plot.GUAN. <- ggplot( ) + geom_spatraster(data= GUAN.evi.proj.cm) + geom_sf(data= GUAN.shp, fill="transparent", linewidth=1) +   scale_fill_hypso_tint_c( limits = c(0,1),  direction = -1) +   
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + ggtitle("GUAN")

plot.HARV. <- ggplot( ) + geom_spatraster(data= HARV.evi.proj.cm) + geom_sf(data= HARV.shp, fill="transparent", linewidth=1) +   scale_fill_hypso_tint_c( limits = c(0,1),  direction = -1) +   
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + ggtitle("HARV")

plot.JORN. <- ggplot( ) + geom_spatraster(data= JORN.evi.proj.cm) + geom_sf(data= JORN.shp, fill="transparent", linewidth=1) +   scale_fill_hypso_tint_c( limits = c(0,1),  direction = -1) +   
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + ggtitle("JORN")

plot.KONZ. <- ggplot( ) + geom_spatraster(data= KONZ.evi.proj.cm) + geom_sf(data= KONZ.shp, fill="transparent", linewidth=1) +   scale_fill_hypso_tint_c( limits = c(0,1),  direction = -1) +   
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + ggtitle("KONZ")

plot.NIWO. <- ggplot( ) + geom_spatraster(data= NIWO.evi.proj.cm) + geom_sf(data= NIWO.shp, fill="transparent", linewidth=1) +   scale_fill_hypso_tint_c( limits = c(0,1),  direction = -1) +   
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + ggtitle("NIWO")

plot.TOOL. <- ggplot( ) + geom_spatraster(data= TOOL.evi.proj.cm) + geom_sf(data= TOOL.shp, fill="transparent", linewidth=1) +   scale_fill_hypso_tint_c( limits = c(0,1),  direction = -1) +   
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + ggtitle("TOOL")



ggarrange(plot.HARV., plot.GUAN., plot.BONA., plot.CPER.,
          plot.TOOL., plot.KONZ.,plot.NIWO., plot.JORN., 
          labels= c('A', 'B', 'C', 'D',
                    'E', 'F','G', 'H'),
          ncol=4, nrow=2,
          common.legend = TRUE)


# EVI LM Figures: #####
# fit lines to the var versus distance to plot for all sites together:

plot.dist.BONA.r <- ggplot( ) + 
  geom_smooth( data= BONA.shp.var, aes( x=dist_m, y = VAR), color="black", method=lm) +
  geom_point( data= BONA.shp.var, aes( x=dist_m, y = VAR), color="black")+
  xlab("Delta Radii") + ylab( "Variance %") + ylim(0, 100) + ggtitle("BONA")

plot.dist.CPER.r <-ggplot( ) +
  geom_smooth( data= CPER.shp.var, aes( x=dist_m, y = VAR), color="black", method=lm) + geom_point( data= CPER.shp.var, aes( x=dist_m, y = VAR), color="black")+ xlab("Delta Radii") + ylab( "Variance %")+ ylim(0, 100)+ ggtitle("CPER")

plot.dist.GUAN.r <-ggplot( ) + 
  geom_smooth( data= GUAN.shp.var, aes( x=dist_m, y = VAR), color="black", method=lm) +
  geom_point( data= GUAN.shp.var, aes( x=dist_m, y = VAR), color="black")+
  xlab("Delta Radii") +ylab( "Variance %")+ ylim(0, 100)+ ggtitle("GUAN")

plot.dist.HARV.r <-ggplot( ) + 
  geom_smooth( data= HARV.shp.var, aes( x=dist_m, y = VAR), color="black", method=lm) +
  geom_point( data= HARV.shp.var, aes( x=dist_m, y = VAR), color="black") + ylim(0, 100)+
  xlab("Delta Radii") + ylab( "Variance %")+ ylim(0, 100)+ ggtitle("HARV")

plot.dist.KONZ.r <-ggplot( ) +
  geom_smooth( data= KONZ.shp.var, aes( x=dist_m, y = VAR), color="black", method=lm) +
  geom_point( data= KONZ.shp.var, aes( x=dist_m, y = VAR), color="black")+ ylim(0, 100)+
  xlab("Delta Radii") + ylab( "Variance %")+ ylim(0, 100)+ ggtitle("KONZ")

plot.dist.TOOL.r <-plot.dist.BONA <-ggplot( ) +
  geom_smooth( data= TOOL.shp.var, aes( x=dist_m, y = VAR), color="black", method=lm) +
  geom_point( data= TOOL.shp.var, aes( x=dist_m, y = VAR), color="black")+ ylim(0, 100)+
  xlab("Delta Radii") + ylab( "Variance %")+ ylim(0, 100)+ ggtitle("TOOL")

plot.dist.JORN.r <-ggplot( ) +
  geom_smooth( data= JORN.shp.var, aes( x=dist_m, y = VAR), color="black", method=lm) +
  geom_point( data= JORN.shp.var, aes( x=dist_m, y = VAR), color="black")+ ylim(0, 100)+
  xlab("Delta Radii") + ylab( "Variance %")+ ylim(0, 100)+ ggtitle("JORN")

plot.dist.NIWO.r <-ggplot( ) +
  geom_smooth( data= NIWO.shp.var, aes( x=dist_m, y = VAR), color="black", method=lm) +
  geom_point( data= NIWO.shp.var, aes( x=dist_m, y = VAR), color="black")+ ylim(0, 100) +
  xlab("Delta Radii") + ylab( "Variance %")+ ylim(0, 100)+ ggtitle("NIWO")

# Create the plot and layout

ggarrange(plot.dist.HARV.r, plot.dist.GUAN.r, plot.dist.BONA.r, plot.dist.CPER.r,
          plot.dist.TOOL.r, plot.dist.KONZ.r,plot.dist.NIWO.r, plot.dist.JORN.r, 
          labels= c('A', 'B', 'C', 'D',
                    'E', 'F','G', 'H'),
          ncol=4, nrow=2,
          common.legend = TRUE)

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

plot.dist.BONA.w <- ggplot( ) + 
  geom_smooth( data= BONA.shp.var, aes( x=wedge, y = VAR), color="black", method=lm) +
  geom_point( data= BONA.shp.var, aes( x=wedge, y = VAR), color="black")+
  xlab("Wedge") + ylab( "Variance %") + ylim(0, 0.3)+ ggtitle("BONA")

plot.dist.CPER.w <-ggplot( ) +
  geom_smooth( data= CPER.shp.var, aes( x=wedge, y = VAR), color="black", method=lm) +
  geom_point( data= CPER.shp.var, aes( x=wedge, y = VAR), color="black")+
  xlab("Wedge") + ylab( "Variance %")+ ylim(0, 0.3)+ ggtitle("CPER")

plot.dist.GUAN.w <-ggplot( ) + 
  geom_smooth( data= GUAN.shp.var, aes( x=wedge, y = VAR), color="black", method=lm) +
  geom_point( data= GUAN.shp.var, aes( x=wedge, y = VAR), color="black")+
  xlab("Wedge") +ylab( "Variance %")+ ylim(0, 0.3)+ ggtitle("GUAN")

plot.dist.HARV.w <-ggplot( ) + 
  geom_smooth( data= HARV.shp.var, aes( x=wedge, y = VAR), color="black", method=lm) +
  geom_point( data= HARV.shp.var, aes( x=wedge, y = VAR), color="black") + ylim(0, 100)+
  xlab("Wedge") + ylab( "Variance %")+ ylim(0, 0.3)+ ggtitle("HARV")

plot.dist.KONZ.w <-ggplot( ) +
  geom_smooth( data= KONZ.shp.var, aes( x=wedge, y = VAR), color="black", method=lm) +
  geom_point( data= KONZ.shp.var, aes( x=wedge, y = VAR), color="black")+ ylim(0, 100)+
  xlab("Wedge") + ylab( "Variance %")+ ylim(0, 0.3)+ ggtitle("KONZ")

plot.dist.TOOL.w <-plot.dist.BONA <-ggplot( ) +
  geom_smooth( data= TOOL.shp.var, aes( x=wedge, y = VAR), color="black", method=lm) +
  geom_point( data= TOOL.shp.var, aes( x=wedge, y = VAR), color="black")+ ylim(0, 100)+
  xlab("Wedge") + ylab( "Variance %")+ ylim(0, 0.3)+ ggtitle("TOOL")

plot.dist.JORN.w <-ggplot( ) +
  geom_smooth( data= JORN.shp.var, aes( x=wedge, y = VAR), color="black", method=lm) +
  geom_point( data= JORN.shp.var, aes( x=wedge, y = VAR), color="black")+ ylim(0, 100)+
  xlab("Wedge") + ylab( "Variance %")+ ylim(0, 0.3)+ ggtitle("JORN")

plot.dist.NIWO.w <-ggplot( ) +
  geom_smooth( data= NIWO.shp.var, aes( x=wedge, y = VAR), color="black", method=lm) +
  geom_point( data= NIWO.shp.var, aes( x=wedge, y = VAR), color="black")+ ylim(0, 100) +
  xlab("Wedge") + ylab( "Variance %")+ ylim(0, 0.3)+ ggtitle("NIWO")


ggarrange(plot.dist.HARV.w, plot.dist.GUAN.w, plot.dist.BONA.w, plot.dist.CPER.w,
          plot.dist.TOOL.w, plot.dist.KONZ.w,plot.dist.NIWO.w, plot.dist.JORN.w, 
          labels= c('A', 'B', 'C', 'D',
                    'E', 'F','G', 'H'),
          ncol=4, nrow=2,
          common.legend = TRUE)
