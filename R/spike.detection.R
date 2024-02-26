#' spike.detection
#'
#' @param site df of calculated flux from specific site taken from Validation df
#'
#' @return df with flag for spikes
#' 
#'
#' @author Alexis Helgeson
spike.detection <- function(site){
  #add column for daytime/nighttime based on converted global radiation threshold of 20 W/m2 converted to PAR units umol/m2/s by using a conversion factor of 0.217, so 20/0.217 = ~92 umol/m2/s
  for(p in 1:dim(site)[1]){
    if(is.na(site[p, "PAR"])){
      next
    }
    if(site[p, "PAR"] < 92){
      site[p, "day_night"] <- "night"
    }else{
      site[p, "day_night"] <- "day"
    }
  }
  # check.day <- site %>% filter(day_night == "day")
  # check.night <- site %>% filter(day_night == "night")
  #add date column
  site$date <- as_date(site$timeBgn_A)
  #Spike Detection for CO2
  #filter df for desired gas
  site.CO2 <- site %>% filter(gas == "CO2")
  #applying the spike detection methodology laid out in Papale et al 2006
  #separate data into 13 day groups
  site.days <- unique(site.CO2$date)
  site.groups <- cut(site.days, breaks = "13 days", labels = FALSE)
  #add spike.bin column for separating out data
  for(g in 1:length(site.days)){
    site.CO2[which(site.CO2$date==site.days[g]),"spike.bin"] <- site.groups[g]
  }
  #separate out fluxes into daytime and nighttime
  site.CO2.day <- site.CO2 %>% filter(day_night=="day")
  site.CO2.night <- site.CO2 %>% filter(day_night=="night")
  #calculate median of differences (Md)/MAD for day/night fluxes for each spike.bin
  num.groups <- unique(site.groups)
  #daytime
  for(d in 1:length(num.groups)){
    day.bin <- site.CO2.day %>% filter(spike.bin==num.groups[d])
    #add 1 obs from next bin to calculate differences
    day.bin.plus1 <- rbind(day.bin, site.CO2.day[which(site.CO2.day$spike.bin==(d+1))[1],])
    #calculate difference of FG
    day.bin$day.d <- diff(day.bin.plus1$FG)
    #calculate median of differences
    day.Md <- median(day.bin$day.d, na.rm = T)
    #calculate MAD
    day.MAD <- mad(day.bin$FG, na.rm = T)
    #set upper/lower threshold for difference using z=5.5
    lower.threshold <- day.Md - ((5.5*day.MAD)/0.6745)
    upper.threshold <- day.Md + ((5.5*day.MAD)/0.6745)
    #flag which day.d fall outside of range, use NEON convention 1 = bad data, 0 = good data
    day.bin[which(day.bin$day.d < lower.threshold),"spike.flag"] <- "1"
    day.bin[which(day.bin$day.d > upper.threshold), "spike.flag"] <- "1"
    day.bin[which(day.bin$day.d >= lower.threshold & day.bin$day.d <= upper.threshold),"spike.flag"] <- "0"
    #add flag to larger df to save
    site.CO2.day[which(site.CO2.day$spike.bin==num.groups[d]),"spike.flag"] <- day.bin$spike.flag
  }
  #check.spike <- site.CO2.day %>% filter(spike.flag=="1")
  
  #nighttime
  for(n in 1:length(num.groups)){
    night.bin <- site.CO2.night %>% filter(spike.bin==num.groups[n])
    #add 1 obs from next bin to calculate differences
    night.bin.plus1 <- rbind(night.bin, site.CO2.night[which(site.CO2.night$spike.bin==(n+1))[1],])
    #calculate difference of FG
    night.bin$day.d <- diff(night.bin.plus1$FG)
    #calculate median of differences
    night.Md <- median(night.bin$day.d, na.rm = T)
    #calculate MAD
    night.MAD <- mad(night.bin$FG, na.rm = T)
    #set upper/lower threshold for difference using z=5.5
    lower.threshold <- night.Md - ((5.5*night.MAD)/0.6745)
    upper.threshold <- night.Md + ((5.5*night.MAD)/0.6745)
    #flag which day.d fall outside of range, use NEON convention 1 = bad data, 0 = good data
    night.bin[which(night.bin$day.d < lower.threshold),"spike.flag"] <- "1"
    night.bin[which(night.bin$day.d > upper.threshold), "spike.flag"] <- "1"
    night.bin[which(night.bin$day.d >= lower.threshold & night.bin$day.d <= upper.threshold),"spike.flag"] <- "0"
    #add flag to larger df to save
    site.CO2.night[which(site.CO2.night$spike.bin==num.groups[n]),"spike.flag"] <- night.bin$spike.flag
  }
  #check.spike <- site.CO2.night %>% filter(spike.flag =="1")
  #combine day/night
  site.CO2.spike <- rbind(site.CO2.day, site.CO2.night)
  #calculate how much data would be remain after de-spiking, print message
  percent.good.data <- round(length(site.CO2.spike[which(site.CO2.spike$spike.flag=="0"),"FG"])/length(site.CO2.spike[,"FG"]),3)*100 #good data/total data
  print(paste0("After de-spiking fluxes there is ~", percent.good.data, "% good data remaining for CO2"))
  
  #Spike Detection for H2O
  #filter df for desired gas
  site.H2O <- site %>% filter(gas == "H2O")
  #applying the spike detection methodology laid out in Papale et al 2006
  #separate data into 13 day groups
  site.days <- unique(site.H2O$date)
  site.groups <- cut(site.days, breaks = "13 days", labels = FALSE)
  #add spike.bin column for separating out data
  for(g in 1:length(site.days)){
    site.H2O[which(site.H2O$date==site.days[g]),"spike.bin"] <- site.groups[g]
  }
  #separate out fluxes into daytime and nighttime
  site.H2O.day <- site.H2O %>% filter(day_night=="day")
  site.H2O.night <- site.H2O %>% filter(day_night=="night")
  #calculate median of differences (Md)/MAD for day/night fluxes for each spike.bin
  num.groups <- unique(site.groups)
  #daytime
  for(d in 1:length(num.groups)){
    day.bin <- site.H2O.day %>% filter(spike.bin==num.groups[d])
    #add 1 obs from next bin to calculate differences
    day.bin.plus1 <- rbind(day.bin, site.H2O.day[which(site.H2O.day$spike.bin==(d+1))[1],])
    #calculate difference of FG
    day.bin$day.d <- diff(day.bin.plus1$FG)
    #calculate median of differences
    day.Md <- median(day.bin$day.d, na.rm = T)
    #calculate MAD
    day.MAD <- mad(day.bin$FG, na.rm = T)
    #set upper/lower threshold for difference using z=5.5
    lower.threshold <- day.Md - ((5.5*day.MAD)/0.6745)
    upper.threshold <- day.Md + ((5.5*day.MAD)/0.6745)
    #flag which day.d fall outside of range, use NEON convention 1 = bad data, 0 = good data
    day.bin[which(day.bin$day.d < lower.threshold),"spike.flag"] <- "1"
    day.bin[which(day.bin$day.d > upper.threshold), "spike.flag"] <- "1"
    day.bin[which(day.bin$day.d >= lower.threshold & day.bin$day.d <= upper.threshold),"spike.flag"] <- "0"
    #add flag to larger df to save
    site.H2O.day[which(site.H2O.day$spike.bin==num.groups[d]),"spike.flag"] <- day.bin$spike.flag
  }
  #check.spike <- site.H2O.day %>% filter(spike.flag=="1")
  #nighttime
  for(n in 1:length(num.groups)){
    night.bin <- site.H2O.night %>% filter(spike.bin==num.groups[n])
    #add 1 obs from next bin to calculate differences
    night.bin.plus1 <- rbind(night.bin, site.H2O.night[which(site.H2O.night$spike.bin==(n+1))[1],])
    #calculate difference of FG
    night.bin$day.d <- diff(night.bin.plus1$FG)
    #calculate median of differences
    night.Md <- median(night.bin$day.d, na.rm = T)
    #calculate MAD
    night.MAD <- mad(night.bin$FG, na.rm = T)
    #set upper/lower threshold for difference using z=5.5
    lower.threshold <- night.Md - ((5.5*night.MAD)/0.6745)
    upper.threshold <- night.Md + ((5.5*night.MAD)/0.6745)
    #flag which day.d fall outside of range, use NEON convention 1 = bad data, 0 = good data
    night.bin[which(night.bin$day.d < lower.threshold),"spike.flag"] <- "1"
    night.bin[which(night.bin$day.d > upper.threshold), "spike.flag"] <- "1"
    night.bin[which(night.bin$day.d >= lower.threshold & night.bin$day.d <= upper.threshold),"spike.flag"] <- "0"
    #add flag to larger df to save
    site.H2O.night[which(site.H2O.night$spike.bin==num.groups[n]),"spike.flag"] <- night.bin$spike.flag
  }
  #check.spike <- site.H2O.night %>% filter(spike.flag =="1")
  #combine day/night
  site.H2O.spike <- rbind(site.H2O.day, site.H2O.night)
  #calculate how much data would be remain after de-spiking, print message
  percent.good.data <- round(length(site.H2O.spike[which(site.H2O.spike$spike.flag=="0"),"FG"])/length(site.H2O.spike[,"FG"]),3)*100 #good data/total data
  print(paste0("After de-spiking fluxes there is ~", percent.good.data, "% good data remaining for H2O"))
  
  #Spike Detection for CH4
  #filter df for desired gas
  site.CH4 <- site %>% filter(gas == "CH4")
  #applying the spike detection methodology laid out in Papale et al 2006
  #separate data into 13 day groups
  site.days <- unique(site.CH4$date)
  site.groups <- cut(site.days, breaks = "13 days", labels = FALSE)
  #add spike.bin column for separating out data
  for(g in 1:length(site.days)){
    site.CH4[which(site.CH4$date==site.days[g]),"spike.bin"] <- site.groups[g]
  }
  #separate out fluxes into daytime and nighttime
  site.CH4.day <- site.CH4 %>% filter(day_night=="day")
  site.CH4.night <- site.CH4 %>% filter(day_night=="night")
  #calculate median of differences (Md)/MAD for day/night fluxes for each spike.bin
  num.groups <- unique(site.groups)
  #daytime
  for(d in 1:length(num.groups)){
    day.bin <- site.CH4.day %>% filter(spike.bin==num.groups[d])
    #add 1 obs from next bin to calculate differences
    day.bin.plus1 <- rbind(day.bin, site.CH4.day[which(site.CH4.day$spike.bin==(d+1))[1],])
    #calculate difference of FG
    day.bin$day.d <- diff(day.bin.plus1$FG)
    #calculate median of differences
    day.Md <- median(day.bin$day.d, na.rm = T)
    #calculate MAD
    day.MAD <- mad(day.bin$FG, na.rm = T)
    #set upper/lower threshold for difference using z=5.5
    lower.threshold <- day.Md - ((5.5*day.MAD)/0.6745)
    upper.threshold <- day.Md + ((5.5*day.MAD)/0.6745)
    #flag which day.d fall outside of range, use NEON convention 1 = bad data, 0 = good data
    day.bin[which(day.bin$day.d < lower.threshold),"spike.flag"] <- "1"
    day.bin[which(day.bin$day.d > upper.threshold), "spike.flag"] <- "1"
    day.bin[which(day.bin$day.d >= lower.threshold & day.bin$day.d <= upper.threshold),"spike.flag"] <- "0"
    #add flag to larger df to save
    site.CH4.day[which(site.CH4.day$spike.bin==num.groups[d]),"spike.flag"] <- day.bin$spike.flag
  }
  #check.spike <- site.CH4.day %>% filter(spike.flag=="1")
  #nighttime
  for(n in 1:length(num.groups)){
    night.bin <- site.CH4.night %>% filter(spike.bin==num.groups[n])
    #add 1 obs from next bin to calculate differences
    night.bin.plus1 <- rbind(night.bin, site.CH4.night[which(site.CH4.night$spike.bin==(n+1))[1],])
    #calculate difference of FG
    night.bin$day.d <- diff(night.bin.plus1$FG)
    #calculate median of differences
    night.Md <- median(night.bin$day.d, na.rm = T)
    #calculate MAD
    night.MAD <- mad(night.bin$FG, na.rm = T)
    #set upper/lower threshold for difference using z=5.5
    lower.threshold <- night.Md - ((5.5*night.MAD)/0.6745)
    upper.threshold <- night.Md + ((5.5*night.MAD)/0.6745)
    #flag which day.d fall outside of range, use NEON convention 1 = bad data, 0 = good data
    night.bin[which(night.bin$day.d < lower.threshold),"spike.flag"] <- "1"
    night.bin[which(night.bin$day.d > upper.threshold), "spike.flag"] <- "1"
    night.bin[which(night.bin$day.d >= lower.threshold & night.bin$day.d <= upper.threshold),"spike.flag"] <- "0"
    #add flag to larger df to save
    site.CH4.night[which(site.CH4.night$spike.bin==num.groups[n]),"spike.flag"] <- night.bin$spike.flag
  }
  #check.spike <- site.CH4.night %>% filter(spike.flag =="1")
  #combine day/night
  site.CH4.spike <- rbind(site.CH4.day, site.CH4.night)
  #calculate how much data would be remain after de-spiking, print message
  percent.good.data <- round(length(site.CH4.spike[which(site.CH4.spike$spike.flag=="0"),"FG"])/length(site.CH4.spike[,"FG"]),3)*100 #good data/total data
  print(paste0("After de-spiking fluxes there is ~", percent.good.data, "% good data remaining for CH4"))
  
  #return df with spike.flag
  site.spike <- rbind(site.CO2.spike, site.H2O.spike, site.CH4.spike)
  
  return(site.spike)
}