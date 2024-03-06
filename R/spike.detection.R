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
  #calculate flux difference threshold and add spike.flag column where 1 = spike and 0 = no spike
  site.CO2.day <- calculate.flux.diff.add.flag(day.night.df = site.CO2.day)
  site.CO2.night <- calculate.flux.diff.add.flag(day.night.df = site.CO2.night)
  #combine day/night along with rows with PAR NA where we could not calculate day/night so were not included in spike detection: We want this dataframe to have the same number of rows as the original WE ARE NOT REMOVING ANY DATA 
  site.CO2.spike <- bind_rows(site.CO2.day, site.CO2.night, site.CO2[which(is.na(site.CO2$PAR)),])
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
  #calculate flux difference threshold and add spike.flag column where 1 = spike and 0 = no spike
  site.H2O.day <- calculate.flux.diff.add.flag(day.night.df = site.H2O.day)
  site.H2O.night <- calculate.flux.diff.add.flag(day.night.df = site.H2O.night)
  #combine day/night along with rows with PAR NA where we could not calculate day/night so were not included in spike detection: We want this dataframe to have the same number of rows as the original WE ARE NOT REMOVING ANY DATA 
  site.H2O.spike <- bind_rows(site.H2O.day, site.H2O.night, site.H2O[which(is.na(site.H2O$PAR)),])
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
  #calculate flux difference threshold and add spike.flag column where 1 = spike and 0 = no spike
  site.CH4.day <- calculate.flux.diff.add.flag(day.night.df = site.CH4.day)
  site.CH4.night <- calculate.flux.diff.add.flag(day.night.df = site.CH4.night)
  #combine day/night along with rows with PAR NA where we could not calculate day/night so were not included in spike detection: We want this dataframe to have the same number of rows as the original WE ARE NOT REMOVING ANY DATA 
  site.CH4.spike <- bind_rows(site.CH4.day, site.CH4.night, site.CH4[which(is.na(site.CH4$PAR)),])
  #calculate how much data would be remain after de-spiking, print message
  percent.good.data <- round(length(site.CH4.spike[which(site.CH4.spike$spike.flag=="0"),"FG"])/length(site.CH4.spike[,"FG"]),3)*100 #good data/total data
  print(paste0("After de-spiking fluxes there is ~", percent.good.data, "% good data remaining for CH4"))
  
  #return df with spike.flag
  site.spike <- bind_rows(site.CO2.spike, site.H2O.spike, site.CH4.spike)
  
  return(site.spike)
}