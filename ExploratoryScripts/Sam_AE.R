install.packages('BiocManager', dependencies = TRUE)
BiocManager::install('rhdf5')
install.packages('dplyr')
install.packages("oce")
install.packages("tidyr")
install.packages("lubridate")
install.packages("naniar")

library(rhdf5)
library(dplyr)
library(oce)
library(tidyr)
library(lubridate)
library(naniar)

#Calling in Data
setwd("/Users/jurado/Harvard_Forest/filesToStack00200/NEON.D01.HARV.DP4.00200.001.2022-06.basic.20230127T120753Z.RELEASE-2023")
h5ls("NEON.D01.HARV.DP4.00200.001.nsae.2022-06.basic.20221216T041952Z.h5")

P <- h5read("NEON.D01.HARV.DP4.00200.001.nsae.2022-06.basic.20221216T041952Z.h5", "/HARV/dp01/data/presBaro/000_025_30m/presAtm")
temp <- h5read("NEON.D01.HARV.DP4.00200.001.nsae.2022-06.basic.20221216T041952Z.h5", "/HARV/dp01/data/tempAirLvl/000_010_30m/temp")


F_h2o <- h5read("NEON.D01.HARV.DP4.00200.001.nsae.2022-06.basic.20221216T041952Z.h5", "/HARV/dp04/data/fluxH2o/nsae")


CH41 <- h5read("NEON.D01.HARV.DP4.00200.001.nsae.2022-06.basic.20221216T041952Z.h5", "/HARV/dp01/data/ch4Conc/000_010_30m/rtioMoleDryCh4")
CH42 <- h5read("NEON.D01.HARV.DP4.00200.001.nsae.2022-06.basic.20221216T041952Z.h5", "/HARV/dp01/data/ch4Conc/000_020_30m/rtioMoleDryCh4")
CH43 <- h5read("NEON.D01.HARV.DP4.00200.001.nsae.2022-06.basic.20221216T041952Z.h5", "/HARV/dp01/data/ch4Conc/000_030_30m/rtioMoleDryCh4")
CH44 <- h5read("NEON.D01.HARV.DP4.00200.001.nsae.2022-06.basic.20221216T041952Z.h5", "/HARV/dp01/data/ch4Conc/000_040_30m/rtioMoleDryCh4")
CH45 <- h5read("NEON.D01.HARV.DP4.00200.001.nsae.2022-06.basic.20221216T041952Z.h5", "/HARV/dp01/data/ch4Conc/000_050_30m/rtioMoleDryCh4")
CH46 <- h5read("NEON.D01.HARV.DP4.00200.001.nsae.2022-06.basic.20221216T041952Z.h5", "/HARV/dp01/data/ch4Conc/000_060_30m/rtioMoleDryCh4")

Ufric <- h5read("NEON.D01.HARV.DP4.00200.001.nsae.2022-06.basic.20221216T041952Z.h5", "/HARV/dp04/data/fluxMome/turb")

r <- h5read("NEON.D01.HARV.DP4.00200.001.nsae.2022-06.basic.20221216T041952Z.h5", "/HARV/dp01/data/h2oTurb/000_060_30m/rtioMoleDryH2o")
H <- h5read("NEON.D01.HARV.DP4.00200.001.nsae.2022-06.basic.20221216T041952Z.h5", "/HARV/dp04/data/fluxTemp/nsae")
LE <- h5read("NEON.D01.HARV.DP4.00200.001.nsae.2022-06.basic.20221216T041952Z.h5", "/HARV/dp04/data/fluxH2o/nsae")

k = .4
g = 9.81
#Specific heat of air J/kgK
cp = 1005
#Latent heat of vaporization of water J/kg
l = 2.26*10**6

#Sensor height in meters? CH45 is broken
 
##########Mixed Bowen Ratio Model######



##########Setup Flux Gradient########

z <- c(.54,5.62,16.59,22.85,29.93,39.16)
zg = 19.115

##########Air Density#########
for(x in list(1:length(P$mean))){
  rho = (P$mean[x]*1000)/((temp$mean[x]+273.15)*287)
  print(rho)
}

rho <- data.frame(rho)
  
##########Specific Humidity Flux##########

for(x in list(1:length(r$mean))){
  q = (r$mean[x]/1000)/(1+(r$mean[x]/1000))
  print(q)
}


##########Surface Flux Virtual Temperature ########

for(x in list(1:length(H$flux))){
  vpotflux <- (H$flux[x]/(rho$rho[x]*cp))+.61*(temp$mean[x]+273.15)*(LE$flux[x]/(rho$rho[x]*l))
  vpotflux <- vpotflux
  print(vpotflux)
}


##########Monin-Obukhov Length#########

for(x in list(1:length(vpotflux))){
  L <- -((Ufric$veloFric[x]**3)*(temp$mean[x]+273.15))/(k*g*(vpotflux[x]))
  print(L)
}

L <- data.frame(L)

##########Obukhov Surface Heat Function######### 
#CHECK WHAT THIS EQUATION ACTUALLY IS#

for(x in list(1:length(L))){
  psiH <- 1/(1-(16*(z[6]/L[x])))**(-1/2)
  print(psiH)
}



 ####### Eddy Diffusion##########

for(x in list(1:length(L))){
  K <- (k*(Ufric$veloFric)*(zg))/(psiH[x])
  print(K)
}

K <- data.frame(K)


########Eddy Diffusion Statistical#########














#######Dataframe######

df <- data.frame(H$timeBgn,H$timeEnd,L, vpotflux,H$flux,psiH,K,temp$mean, r$mean, Ufric$veloFric)

#####Diurnal Average######
dfCLEAN <- df

dfCLEAN <- dfCLEAN %>% filter((abs(dfCLEAN$L) < 500) %>% replace_na(T))

dfCLEAN$H.timeBgn <- as.POSIXct(dfCLEAN$H.timeBgn, format = "%Y-%m-%dT%H:%M:%S") 

dfCLEAN$Time <- format(as.POSIXct(dfCLEAN$H.timeBgn), format = "%H:%M:%S") 

dfDAILY <- dfCLEAN %>% group_by(Time) %>% summarize(L = mean(L, na.rm = TRUE),
                                                    vpotflux = mean(vpotflux, na.rm =TRUE),
                                                    temp = mean(temp.mean,na.rm=TRUE),
                                                    r = mean(r.mean, na.rm=TRUE),
                                                    ufric = mean(Ufric.veloFric, na.rm =TRUE))





dfDAILY$Time <- as.POSIXct(dfDAILY$Time, format = "%H:%M:%S") 



######Plot M-O Length#######
for(x in list(1:length(Harvard_Forest))){
  L <- -(dfDAILY$ufric[x]**3)*(Harvard_Forest$`θ [K]`[x])/(k*g*(Harvard_Forest$`w'θ'(s) [K m s-¹]`[x]))
  
}

L <- na.omit(L)

plot(Harvard_Forest$`time [h]`,dfDAILY$L, type="l",lwd=1, xlab = "Time of Day",
     ylab = "L (m)"
)
lines(Harvard_Forest$`time [h]`, L, col="red",lty=2)
legend(0,-150,legend=c("Observations","CLASS Model"), col=c("black","red"),
       lty=c(1,2), ncol=1)


title("Monin-Obukhov Length Diurnal Average")
subtitle = "HARV 05/01/22 - 05/30/22"
mtext(subtitle)


L <- data.frame(L)
#L <- replace_with_na(replace = L$L > 1000)
plot(dfDAILY$L,L)





#####Plot Pot. Temp ###########

plot(Harvard_Forest$`time [h]`,dfDAILY$vpotflux, type="l",lwd=1, xlab = "Time of Day",
     ylab = expression(bar("w'θ'"[v])*"[K m s-¹]"
), ylim = c(-.05,.2))
lines(Harvard_Forest$`time [h]`, Harvard_Forest$`w'θ'(s) [K m s-¹]`, col="red",lty=2)

legend(0,.15,legend=c("Observations","CLASS Model"), col=c("black","red"),
       lty=c(1,2), ncol=1)

title("Virtual Potential Temperature Flux Diurnal Average")
subtitle = "HARV 05/01/22 - 05/30/22"
mtext(subtitle)

plot(dfDAILY$vpotflux,Harvard_Forest$`w'θ'(s) [K m s-¹]`,xlab = "Observations", ylab = "Model")
x=c(0,1)
y=c(0,1)
abline(a = 0, b = 1) 






##### Plot Temp######

dfDAILY$temp <- dfDAILY$temp + 273.15

plot(Harvard_Forest$`time [h]`,dfDAILY$temp, type="l",lwd=1, xlab = "Hour of Day",
     ylab = "Temperature [K]"
     )
title("Temperature Diurnal Average")
subtitle = "HARV 05/01/22 - 05/30/22"
mtext(subtitle)



######Flux Gradient Method########


######SPIKE FILTER#####


#######TESTING PLOT#######
df$H.timeBgn <- as.POSIXct(df$H.timeBgn, format = "%Y-%m-%dT%H:%M:%S") 

plot(df$H.timeBgn,df$vpotflux, type="l",lwd=1, xlab = "Time",
     ylab = expression(bar(paste(omega,"'",theta[v],"'"))
))
title("Virtual Potential Temperature Covariance")
subtitle = "HARV 05/01/22 - 05/30/22"
mtext(subtitle)


dfCLEAN$H.timeBgn <- as.POSIXct(dfCLEAN$H.timeBgn, format = "%Y-%m-%dT%H:%M:%S") 


plot(dfCLEAN$H.timeBgn,dfCLEAN$L, type="l",lwd=1, xlab = "Time",
     ylab = "L (m)"
     )
title("Monin-Obukhov Length")
subtitle = "HARV 05/01/22 - 05/30/22"
mtext(subtitle)
 
