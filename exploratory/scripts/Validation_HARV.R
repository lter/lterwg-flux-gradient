# Build site flux files from the .Rdata objects:

rm(list=ls())
library(dplyr)
library(ggplot2)

load( "/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/Data/HARV_AE_AH_2023-12-05.RDATA" )

HARV.AE <- min9.FG.AE.list$CO2

load( "/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/Data/HARV_WP_AH_2023-12-05.RDATA" )

HARV.WP <- min9.FG.WP.list$CO2

load( "/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/Data/HARV_MBRflux.RDATA" )

HARV.MBR <- min9.FG.WP.list$CO2

rm( MBRflux_align, min9.FG.AE.list, min9.FG.WP.list)


ggplot(data=HARV.AE) + geom_point(aes(x=timeEnd_A , y=FG),cex=0.3) + ylim(-100, 100) + geom_point(aes(x=timeEnd_A, y=FC_interp, ), col="red", cex=0.25)

ggplot(data=HARV.WP) + geom_point(aes(x=timeEnd_A , y=FG),cex=0.3) + ylim(-100, 100) + geom_point(aes(x=timeEnd_A, y=FC_interp, ), col="red", cex=0.25)

ggplot(data=HARV.MBR) + geom_point(aes(x=timeEnd_A , y=FG),cex=0.3) + ylim(-100, 100) + geom_point(aes(x=timeEnd_A, y=FC_interp, ), col="red", cex=0.25)



