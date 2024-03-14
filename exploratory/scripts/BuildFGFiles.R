# Build site flux files from the .Rdata objects:

rm(list=ls())
library(dplyr)

ggplot(data=KNOZ.AE) + geom_point(aes(x=timeEnd_A , y=FG),cex=0.3) + ylim(-100, 100) + geom_point(aes(x=timeEnd_A, y=FC_interp, ), col="red", cex=0.25)

ggplot(data=KNOZ.WP) + geom_point(aes(x=timeEnd_A , y=FG),cex=0.3) + ylim(-100, 100) + geom_point(aes(x=timeEnd_A, y=FC_interp, ), col="red", cex=0.25)


ggplot(data=KNOZ.MBR) + geom_point(aes(x=timeEnd_A_CO2 , y=FCO2_MBR_H2Otrace),cex=0.3) + ylim(-100, 100) + geom_point(aes(x=timeEnd_A_CO2, y=FC_interp_CO2, ), col="red", cex=0.25)

rm( MBRflux_align, min9.FG.AE.list, min9.FG.WP.list)


