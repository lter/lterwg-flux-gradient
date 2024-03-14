
rm(list=ls())
library(dplyr)

load( "/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/Data/KONZ_AE_AH_2023-12-05.RDATA" )

KONZ.AE <- min9.FG.AE.list$CO2$gas

load( "/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/Data/KONZ_WP_AH_2023-12-05.RDATA" )

KONZ.WP <- min9.FG.WP.list$CO2$gas

load( "/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/Data/KONZ_MBRflux.RDATA" )

KONZ.MBR <- MBRflux_align

rm( MBRflux_align, min9.FG.AE.list, min9.FG.WP.list)

# Time series plots:
ggplot(data=KONZ.AE) + geom_point(aes(x=timeEnd_A , y=FG),cex=0.3) + ylim(-100, 100) + geom_point(aes(x=timeEnd_A, y=FC_interp, ), col="red", cex=0.25)

ggplot(data=KONZ.WP) + geom_point(aes(x=timeEnd_A , y=FG),cex=0.3) + ylim(-100, 100) + geom_point(aes(x=timeEnd_A, y=FC_interp, ), col="red", cex=0.25)

ggplot(data=KONZ.MBR) + geom_point(aes(x=timeEnd_A_CO2 , y=FCO2_MBR_H2Otrace),cex=0.3) + ylim(-100, 100) + geom_point(aes(x=timeEnd_A_CO2, y=FC_interp_CO2, ), col="red", cex=0.25)


# Filtering:
KONZ.AE$FG[KONZ.AE$FG > 100 | KONZ.AE$FG < -100] <-NA
KONZ.WP$FG[KONZ.WP$FG > 100 | KONZ.WP$FG < -100] <-NA
# Linear comparisons and r2 values:

ggplot(data=KONZ.AE) + geom_point(aes(x=FG, y=FC_interp), col="red", cex=0.25) + ylim(-100, 100)
summary(lm( data=KONZ.AE, FG ~ FC_interp))

ggplot(data=KONZ.WP) + geom_point(aes(x=FG, y=FC_interp), col="red", cex=0.25) + ylim(-100, 100)

summary(lm( data=KONZ.WP, FG ~ FC_interp))

# Diurnal pattern

KONZ.WP$Hour <- format(KONZ.WP$timeEnd_A, format = "%H")
KONZ.WP$Month <- format(KONZ.WP$timeEnd_A, format = "%m")


ggplot( data= KONZ.WP, aes(x= Hour, y= FG)) +
  geom_point() + geom_smooth(method="auto", se=TRUE, fullrange=TRUE, level=0.95)

ggplot( data= KONZ.WP, aes(x= Hour, y= FG)) +
  geom_boxplot() +  facet_grid(. ~ Month)

ggplot( data= KONZ.WP[which( KONZ.WP$Month == '05'),], aes(x= Hour, y= FG)) + geom_violin() 

