
SITES_AE$KONZ$site <- "KONZ"
SITES_AE$BONA$site <- "BONA"
SITES_AE$CPER$site <- "CPER"
SITES_AE$GUAN$site <- "GUAN"
SITES_AE$HARV$site <- "HARV"
SITES_AE$JORN$site <- "JORN"
SITES_AE$NIWO$site <- "NIWO"
SITES_AE$TOOL$site <- "TOOL"

all.sites <- bind_rows(SITES_AE)


co2 <- all.sites %>% filter(gas == "CO2")
#head(difftime(co2$timeEnd_A, co2$timeBgn_A, units = "min"))
co2$diffTime <- round(difftime(co2$timeEnd_A, co2$timeBgn_A, units = "min"))
unique(co2$diffTime)
unique(co2[which(co2$diffTime==6),"site"])
unique(co2[which(co2$diffTime==2),"timeBgn_A"])
unique(co2[which(co2$diffTime==1),"timeBgn_A"])
unique(co2[which(co2$diffTime==5),"timeBgn_A"])
unique(co2[which(co2$diffTime==4),"timeBgn_A"])
unique(co2[which(co2$diffTime==3),"timeBgn_A"])
unique(co2[which(co2$diffTime==0),"timeBgn_A"])
unique(co2[which(co2$diffTime==9),"timeBgn_A"])
hist(as.numeric(co2$diffTime), main = "Histogram CO2 Time Difference", xlab = "timeEnd-timeBgn")

h2o <- all.sites %>% filter(gas == "H2O")
h2o$diffTime <- round(difftime(h2o$timeEnd_A, h2o$timeBgn_A, units = "min"))
unique(h2o$diffTime)
unique(h2o[which(h2o$diffTime==6),"site"])
unique(h2o[which(h2o$diffTime==2),"timeBgn_A"])
unique(h2o[which(h2o$diffTime==1),"timeBgn_A"])
unique(h2o[which(h2o$diffTime==5),"timeBgn_A"])
unique(h2o[which(h2o$diffTime==4),"timeBgn_A"])
unique(h2o[which(h2o$diffTime==3),"timeBgn_A"])
unique(h2o[which(h2o$diffTime==0),"timeBgn_A"])
unique(h2o[which(h2o$diffTime==9),"timeBgn_A"])
hist(as.numeric(h2o$diffTime), main = "Histogram H2O Time Difference", xlab = "timeEnd-timeBgn")


ch4 <- all.sites %>% filter(gas == "CH4")
ch4$diffTime <- round(difftime(ch4$timeEnd_A, ch4$timeBgn_A, units = "min"))
unique(ch4$diffTime)
unique(ch4[which(ch4$diffTime==6),"timeBgn_A"])
unique(ch4[which(ch4$diffTime==2),"timeBgn_A"])
unique(ch4[which(ch4$diffTime==1),"timeBgn_A"])
unique(ch4[which(ch4$diffTime==5),"timeBgn_A"])
unique(ch4[which(ch4$diffTime==4),"timeBgn_A"])
unique(ch4[which(ch4$diffTime==3),"timeBgn_A"])
unique(ch4[which(ch4$diffTime==0),"timeBgn_A"])
unique(ch4[which(ch4$diffTime==9),"site"])
unique(ch4[which(ch4$diffTime==7),"timeBgn_A"])
unique(ch4[which(ch4$diffTime==8),"timeBgn_A"])
hist(as.numeric(ch4$diffTime), main = "Histogram CH4 Time Difference", xlab = "timeEnd-timeBgn")


