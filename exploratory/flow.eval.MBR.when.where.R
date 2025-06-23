##############################################################################################
#' Characterize all-site data for the MBR method making it through the filtering 
#' and best-height characterization
#' 
##############################################################################################
rm(list=ls())
library(plotly)
source("./functions/calc.lins.ccc.R")
source('./functions/calculate.diel.ptrn.R')

# ----- SETUP -----

# Pull data from google drive
email <- 'csturtevant@battelleecology.org'

localdir <- '/scratch/FluxGradient'
fileEval <- 'AllSites_MBR_Eval.Rdata'
fileTowerInfo <- 'Site_Attributes.csv'

cccMin <- 0.5 # minimum ccc value to indicate successful/usable MBR fluxes from the tower level pair

# ----- Load data -----

load(file=fs::path(localdir,fileEval))
towerInfo <- read.csv(file=fs::path(localdir,fileTowerInfo))
sites <- unique(cccAll$site)

siteInfo <- data.frame(site=sites)

# Assign canopy position 
towerInfo$distToCanopy <- towerInfo$DistZaxsLvlMeasTow-towerInfo$DistZaxsCnpy
setAbove <- towerInfo$DistZaxsLvlMeasTow > towerInfo$DistZaxsCnpy # Above canopy 
setBelow <- towerInfo$DistZaxsLvlMeasTow <= towerInfo$DistZaxsCnpy # Below canopy 
# setAt <- towerInfo$DistZaxsLvlMeasTow > (towerInfo$DistZaxsCnpy-1) &
#   towerInfo$DistZaxsLvlMeasTow < (towerInfo$DistZaxsCnpy+1)
towerInfo$canopyPosition[setAbove] <- "above"
towerInfo$canopyPosition[setBelow] <- "below"
# towerInfo$canopyPosition[setAt] <- "at"
for(site in sites){
  setSite <- which(towerInfo$Site == site)
  towerInfoSite <- towerInfo[setSite,]
  setAbove <- towerInfoSite$canopyPosition == 'above'
  setBelow <- towerInfoSite$canopyPosition == 'below'

  # Grab tower levels and canopy height
  siteInfo$tower_levels[siteInfo$site==site] <- max(towerInfo$LvlMeasTow[setSite])
  siteInfo$canopy_height[siteInfo$site==site] <- max(towerInfo$DistZaxsCnpy[setSite])
  

  for (idxRow in setSite){
    
    # Mark closest below-canopy position to the canopy height
    try({
      if(towerInfo$canopyPosition[idxRow] == "below" &
         towerInfo$distToCanopy[idxRow] == max(towerInfoSite$distToCanopy[setBelow])){
        towerInfo$canopyPosition[idxRow] <- "closestBelow"
      }
    },
      silent=TRUE)

    # Mark closest above-canopy position to the canopy height
    try({
      if(towerInfo$canopyPosition[idxRow] == "above" &
         towerInfo$distToCanopy[idxRow] == min(towerInfoSite$distToCanopy[setAbove])){
        towerInfo$canopyPosition[idxRow] <- "closestAbove"
      }
    },
    silent=TRUE)
  }

}


# Pull out and categorize the individual tower levels
cccAll$lvlHi=substr(cccAll$tower_pair,start=1,stop=1)
cccAll$lvlLo=substr(cccAll$tower_pair,start=3,stop=3)
# Assign canopy position to tower levels
for (idxRow in seq_len(nrow(cccAll))){
  # Higher level
  idxMtchHi <- which(towerInfo$Site == cccAll$site[idxRow] & 
                     towerInfo$TowerPosition == cccAll$lvlHi[idxRow])
  cccAll$canopyPositionHi[idxRow] <- towerInfo$canopyPosition[idxMtchHi]
  
  # Lower level
  idxMtchLo <- which(towerInfo$Site == cccAll$site[idxRow] & 
                     towerInfo$TowerPosition == cccAll$lvlLo[idxRow])
  cccAll$canopyPositionLo[idxRow] <- towerInfo$canopyPosition[idxMtchLo]
  
  # Level Pair
  cccAll$canopyPositionHiLo[idxRow] <- paste0(cccAll$canopyPositionHi[idxRow],"-",cccAll$canopyPositionLo[idxRow])
  cccAll$distHiLo[idxRow] <- towerInfo$DistZaxsLvlMeasTow[idxMtchHi] - towerInfo$DistZaxsLvlMeasTow[idxMtchLo]
  
}


cccAll$nFrac <- cccAll$n/cccAll$nraw

# Characterize success, when ccc > cccMin
cccAll0 <- cccAll
gas="CO2"
cccAll <- cccAll[cccAll$n > 30 & cccAll$gas == gas,]
cccAll$success <- cccAll$ccc > cccMin
cccAll$outcome[cccAll$success==TRUE] <- "success"
cccAll$outcome[cccAll$success==FALSE] <- "fail"
table_data <- table(cccAll$canopyPositionHiLo,cccAll$outcome)
table_totals <- table_data[,"success"]+table_data[,"fail"]
table_data[,"success"] <- table_data[,"success"]/table_totals*100
table_data <- data.frame(position=row.names(table_data),successRate=table_data[,"success"])
table_data$position <- factor(table_data$position, levels = unique(table_data$position)[order(table_data$successRate, decreasing = TRUE)])
table_data_CO2  <- table_data

# How many sites had at least one successful tower pair with both levels above/at canopy level
siteSuccessCO2Above <- unique(cccAll$site[(grepl('above',cccAll$canopyPositionHi,ignore.case = TRUE) | grepl('closest',cccAll$canopyPositionHi,ignore.case = TRUE)) &
    (grepl('above',cccAll$canopyPositionLo,ignore.case = TRUE) |  grepl('closest',cccAll$canopyPositionLo,ignore.case = TRUE)) & 
    cccAll$ccc > cccMin])
siteSuccessCO2Any <- unique(cccAll$site[cccAll$ccc > cccMin])

# Get max ccc per site
siteBest_CO2 <- cccAll %>% group_by(site) %>% summarise(maxccc =max(ccc,na.rm=TRUE))
siteBest_CO2$gas <- gas

cccAll <- cccAll0
gas="H2O"
cccAll <- cccAll[cccAll$n > 30 & cccAll$gas == gas,]
cccAll$success <- cccAll$ccc > cccMin
cccAll$outcome[cccAll$success==TRUE] <- "success"
cccAll$outcome[cccAll$success==FALSE] <- "fail"
table_data <- table(cccAll$canopyPositionHiLo,cccAll$outcome)
table_totals <- table_data[,"success"]+table_data[,"fail"]
table_data[,"success"] <- table_data[,"success"]/table_totals*100
table_data <- data.frame(position=row.names(table_data),successRate=table_data[,"success"])
table_data$position <- factor(table_data$position, levels = unique(table_data$position)[order(table_data$successRate, decreasing = TRUE)])
table_data_H2O  <- table_data

# How many sites had at least one successful tower pair with both levels above/at canopy level
siteSuccessH2OAbove <- unique(cccAll$site[(grepl('above',cccAll$canopyPositionHi,ignore.case = TRUE) | grepl('closest',cccAll$canopyPositionHi,ignore.case = TRUE)) &
                                       (grepl('above',cccAll$canopyPositionLo,ignore.case = TRUE) |  grepl('closest',cccAll$canopyPositionLo,ignore.case = TRUE)) & 
                                       cccAll$ccc > cccMin])
siteSuccessH2OAny <- unique(cccAll$site[cccAll$ccc > cccMin])

# Get max ccc per site
siteBest_H2O <- cccAll %>% group_by(site) %>% summarise(maxccc =max(ccc,na.rm=TRUE))
siteBest_H2O$gas <- gas

# Plot successful tower-pair instances
fig <- plot_ly(data=table_data_H2O,
               x = ~position,
               y = ~successRate,
               name='FH2O',
               type = "bar"
) %>% add_trace(data=table_data_CO2, y = ~successRate, name = 'FCO2')%>% 
  layout(
    barmode = 'group',
    title = list(text="Tower Level Pairs for Good MBR Predictions (CCC > 0.5; n > 30)"),
    xaxis = list(title = 'Tower Level Pair'),
    yaxis = list(title = 'Success Rate (%)',showgrid=FALSE)
  )
print(fig)




# For the filtered data and good tower level pairs, what was the typical atmospheric stability?
cccAll <- cccAll0
cccAll <- cccAll[cccAll$ccc > cccMin & cccAll$n > 30,]
fig <- plot_ly(data=cccAll[cccAll$gas=="H2O",], y = ~zoL_median,type = "box", name="FH2O") %>% #, color = 'rgb(70,130,180)') %>%
add_trace(data=cccAll[cccAll$gas=="CO2",], y=~zoL_median, type = "box", name="FCO2") #, color = 'rgb(255,140,0)')

fig <- fig %>%
  layout(title = paste0('Atmospheric Stability for Good MBR Predictions (CCC > 0.5; n > 30)'),
         # xaxis = list(title = 'Flux'), 
         yaxis = list(title = 'Median z/L',
                      range=c(-1,1.3),
                      showgrid=FALSE)
  )
print(fig)

# Characterize the diel pattern. When do you get conformance between the fully filtered MBR and the EC (same data points)
dielEvalFH2O <- list()
dielEvalFCO2 <- list()
for(site in sites){
  gas="CO2"
  setUseEC <- flux_diel_all$site==site & flux_diel_all$gas == gas & flux_diel_all$typeComb == "filtered EC"
  setUseMBR <- flux_diel_all$site==site & flux_diel_all$gas == gas & flux_diel_all$typeComb == "filtered MBR"
  dielMBR <- flux_diel_all$flux[setUseMBR]
  dielEC <- flux_diel_all$flux[setUseEC]
  dielDiff <- dielMBR-dielEC
  dielEC[is.na(dielDiff)] <- NA
  relDielDiff <- sum(abs(dielDiff),na.rm=TRUE)/sum(abs(dielEC),na.rm=TRUE)*100
  dielEvalFCO2[[site]] <- data.frame(site=site,gas=gas,relativeDielDiff=relDielDiff,nBin=sum(!is.na(dielDiff)),nMBR=sum(flux_diel_all$n[setUseMBR],na.rm=TRUE))

  gas="H2O"
  setUseEC <- flux_diel_all$site==site & flux_diel_all$gas == gas & flux_diel_all$typeComb == "filtered EC"
  setUseMBR <- flux_diel_all$site==site & flux_diel_all$gas == gas & flux_diel_all$typeComb == "filtered MBR"
  dielMBR <- flux_diel_all$flux[setUseMBR]
  dielEC <- flux_diel_all$flux[setUseEC]
  dielDiff <- dielMBR-dielEC
  dielEC[is.na(dielDiff)] <- NA
  relDielDiff <- sum(abs(dielDiff),na.rm=TRUE)/sum(abs(dielEC),na.rm=TRUE)*100
  dielEvalFH2O[[site]] <- data.frame(site=site,gas=gas,relativeDielDiff=relDielDiff,nBin=sum(!is.na(dielDiff)),nMBR=sum(flux_diel_all$n[setUseMBR],na.rm=TRUE))
  
}

dielEvalFCO2Filt <- do.call(rbind,dielEvalFCO2)
dielEvalFH2OFilt <- do.call(rbind,dielEvalFH2O)
dielEvalFilt <- rbind(dielEvalFCO2Filt,dielEvalFH2OFilt)

t <- list(
  family = "ariel",
  size = 10,
  color = toRGB("grey50"))

setCO2 <- dielEvalFilt$gas == "CO2"
setH2O <- dielEvalFilt$gas == "H2O"
fig <- plot_ly(data = dielEvalFilt[setH2O,], x = ~nMBR, y = ~relativeDielDiff, name='FH2O') %>%
  add_text(text = ~site,textfont = t, textposition = "top right")  %>%
  add_markers(marker = list(color = 'rgb(70,130,180)')) %>%
  add_trace(data = dielEvalFilt[setCO2,], x = ~nMBR, y = ~relativeDielDiff, name='FCO2', mode='scatter',marker = list(color = 'rgb(255,140,0)')) %>% 
  add_text(text = ~site, textfont = t, textposition = "top right")

fig <- fig %>%
  layout(title = paste0('Diel Pattern Difference (MBR vs. EC) - Same points'),
         xaxis = list(title = 'MBR sample size',showgrid=FALSE), 
         yaxis = list(title = 'Relative difference (%)',
                      range=c(0,320),showgrid=FALSE)
  )
print(fig)


# Characterize the diel pattern. When do you get conformance between the MBR (fully filtered) and the EC (turbulent filters only)
# What sites were able to achieve this?
dielEvalFH2O <- list()
dielEvalFCO2 <- list()
for(site in sites){
  gas="CO2"
  setUseEC <- flux_diel_all$site==site & flux_diel_all$gas == gas & flux_diel_all$typeComb == "unfiltered EC"
  setUseMBR <- flux_diel_all$site==site & flux_diel_all$gas == gas & flux_diel_all$typeComb == "filtered MBR"
  dielMBR <- flux_diel_all$flux[setUseMBR]
  dielEC <- flux_diel_all$flux[setUseEC]
  dielDiff <- dielMBR-dielEC
  dielEC[is.na(dielDiff)] <- NA
  relDielDiff <- sum(abs(dielDiff),na.rm=TRUE)/sum(abs(dielEC),na.rm=TRUE)*100
  dielEvalFCO2[[site]] <- data.frame(site=site,gas=gas,relativeDielDiff=relDielDiff,nBin=sum(!is.na(dielDiff)),nMBR=sum(flux_diel_all$n[setUseMBR],na.rm=TRUE))

  gas="H2O"
  setUseEC <- flux_diel_all$site==site & flux_diel_all$gas == gas & flux_diel_all$typeComb == "unfiltered EC"
  setUseMBR <- flux_diel_all$site==site & flux_diel_all$gas == gas & flux_diel_all$typeComb == "filtered MBR"
  dielMBR <- flux_diel_all$flux[setUseMBR]
  dielEC <- flux_diel_all$flux[setUseEC]
  dielDiff <- dielMBR-dielEC
  dielEC[is.na(dielDiff)] <- NA
  relDielDiff <- sum(abs(dielDiff),na.rm=TRUE)/sum(abs(dielEC),na.rm=TRUE)*100
  dielEvalFH2O[[site]] <- data.frame(site=site,gas=gas,relativeDielDiff=relDielDiff,nBin=sum(!is.na(dielDiff)),nMBR=sum(flux_diel_all$n[setUseMBR],na.rm=TRUE))

}

dielEvalFCO2Eco <- do.call(rbind,dielEvalFCO2)
dielEvalFH2OEco <- do.call(rbind,dielEvalFH2O)
dielEvalEco <- rbind(dielEvalFCO2Eco,dielEvalFH2OEco)

setCO2 <- dielEvalEco$gas == "CO2"
setH2O <- dielEvalEco$gas == "H2O"
fig <- plot_ly(data = dielEvalEco[setH2O,], x = ~nMBR, y = ~relativeDielDiff, name='FH2O') %>%
  add_text(text = ~site,textfont = t, textposition = "top right")  %>%
  add_markers(marker = list(color = 'rgb(70,130,180)')) %>%
  add_trace(data = dielEvalEco[setCO2,], x = ~nMBR, y = ~relativeDielDiff, name='FCO2', mode='scatter',marker = list(color = 'rgb(255,140,0)')) %>% 
  add_text(text = ~site, textfont = t, textposition = "top right")

fig <- fig %>%
  layout(title = paste0('Diel Pattern Difference (MBR vs. EC) - All EC'),
         xaxis = list(title = 'MBR sample size',showgrid=FALSE), 
         yaxis = list(title = 'Relative difference (%)',
                      range=c(0,320),showgrid=FALSE)
  )
print(fig)


# Plot an example diel pattern to demonstrate diel pattern difference
setA <- flux_diel_all$site=="KONZ" & flux_diel_all$gas=="CO2" & flux_diel_all$typeComb == "filtered EC"
setB <- flux_diel_all$site=="KONZ" & flux_diel_all$gas=="CO2" & flux_diel_all$typeComb == "unfiltered MBR"
fig <- plot_ly(x = flux_diel_all$time[setA], y = flux_diel_all$flux[setA], type = 'scatter', name="EC",
               mode = 'lines',line = list(color = 'rgb(110,110,110)', width = 2)) %>% # rgb(81,188,34)
       add_trace(x = flux_diel_all$time[setB], y = flux_diel_all$flux[setB], type = 'scatter', name="MBR",
                 mode = 'lines',line = list(color = 'rgb(255,140,0)', width = 2)) # rgb(176,18,158)
fig <- fig %>%
  layout(title = paste0('Example Diel Pattern'),
         xaxis = list(title = 'Time (hours)',
                      range=c(0,24),
                      showgrid=FALSE), 
         yaxis = list(title = "FCO2 (umol m-2 s-1)",
                      showgrid=FALSE))

print(fig)

# What is the diel pattern deviation for this example
sum(abs(flux_diel_all$flux[setB]-flux_diel_all$flux[setA]))/sum(abs(flux_diel_all$flux[setA]))*100