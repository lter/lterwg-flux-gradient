# Creates tables and figures for MS1


# Import the datafiles: #### 
# Filter Data:
# SITES_WP_9min_FILTER,SITES_AE_9min_FILTER, SITES_MBR_9min_FILTER ,

#------ CHANGE THIS STUFF ------

email <- 'csturtevant@battelleecology.org'
DirRepo <- 'C:/Users/csturtevant/Documents/Git/lterwg-flux-gradient' # Relative or absolute path to lterwg-flux-gradient git repo on your local machine. Make sure you've pulled the latest from main!
localdir <- 'C:/Users/csturtevant/OneDrive - Battelle Ecology/FluxGradient/filterTesting' # We'll deposit output files here prior to uploading to Google Drive
DnldFromGoogleDrive <- FALSE # Enter TRUE if you don't have the files listed in dnld_files below locally in the localdir directory
sffx <- c('ALLSites','MS1Sites')[2]

# ------------------------------

# Download necessary files from Google Drive
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3")
googledrive::drive_auth(email = email) # Likely will not work on RStudio Server. If you get an error, try email=TRUE to open an interactive auth session.
data_folder <- googledrive::drive_ls(path = drive_url)

dnld_files=c(paste0("FilteredData_",sffx,".Rdata"),
             paste0("FilteredData_",sffx,"_BH.Rdata"),
             paste0("FilterReport_",sffx,".Rdata"),
             paste0("One2One_",sffx,".Rdata"),
             paste0("Diurnal_",sffx,"_BH.Rdata"),
             paste0("DiurnalSummary_",sffx,"_BH.Rdata"),
             paste0("CarbonParms_",sffx,".Rdata"),
             'Sites_AOP_Summary.Rdata')
if(DnldFromGoogleDrive == TRUE){
  for (focal_file in dnld_files){
    message('Downloading ',focal_file, ' to ',localdir)
    file_id <- subset(data_folder, name == focal_file)
    pathDnld <- fs::path(localdir,focal_file)
    googledrive::drive_download(file = file_id$id, 
                                path = fs::path(localdir,focal_file),
                                overwrite = T)
    
  }
}

# Load the files
for (focal_file in dnld_files){
  message('Loading ',focal_file, ' from ',localdir)
  load(fs::path(localdir,focal_file))
}


# Filter for MS1 Sites
sites <- c("KONZ" ,"HARV" ,"JORN", "GUAN")
# load( file="/Volumes/MaloneLab/Research/FluxGradient/FilteredData_MS1Sites.Rdata")
SITES_WP_9min_FILTER <- SITES_WP_9min_FILTER[sites]
SITES_AE_9min_FILTER <- SITES_AE_9min_FILTER[sites]
SITES_MBR_9min_FILTER <- SITES_MBR_9min_FILTER[sites]

 
# SITES_WP_9min.report,SITES_AE_9min.report, SITES_MBR_9min.report ,
# load("/Volumes/MaloneLab/Research/FluxGradient/FilterReport_MS1Sites.Rdata")
SITES_WP_9min.report <- SITES_WP_9min.report[SITES_One2One$site %in% sites,]
SITES_AE_9min.report <- SITES_AE_9min.report[SITES_One2One$site %in% sites,]
SITES_MBR_9min.report <- SITES_MBR_9min.report[SITES_One2One$site %in% sites,]

# One2One: 
# SITES_One2One
# SITES_WP_9min_FILTER_BH,SITES_AE_9min_FILTER_BH, SITES_MBR_9min_FILTER_BH
# load("/Volumes/MaloneLab/Research/FluxGradient/FilteredData_MS1Sites_BH.Rdata")
SITES_WP_9min_FILTER_BH <- SITES_WP_9min_FILTER_BH[sites]
SITES_AE_9min_FILTER_BH <- SITES_AE_9min_FILTER_BH[sites]
SITES_MBR_9min_FILTER_BH <- SITES_MBR_9min_FILTER_BH[sites]


Diurnal.AE.H2O <- Diurnal.AE.H2O[sites] 
Diurnal.MBR.H2O <- Diurnal.MBR.H2O[sites] 
Diurnal.WP.H2O <- Diurnal.WP.H2O[sites] 

Diurnal.AE.CO2 <- Diurnal.AE.CO2[sites] 
Diurnal.MBR.CO2 <- Diurnal.MBR.CO2[sites] 
Diurnal.WP.CO2 <- Diurnal.WP.CO2[sites] 

# Diurnals:
# load(file="/Volumes/MaloneLab/Research/FluxGradient/DiurnalSummary_ALLSites_BH.Rdata")
#diurnal.summary.H2O ,diurnal.summary.CO2, 
# diurnal.summary.H2O <- diurnal.summary.H2O[diurnal.summary.H2O$Site %in% sites,]
# diurnal.summary.CO2 <- diurnal.summary.CO2[diurnal.summary.CO2$Site %in% sites,]

# CPARMS:
# SITES_MBR_9min_CPARMS_FG , SITES_MBR_9min_CPARMS_EC , SITES_AE_9min_CPARMS_FG, SITES_AE_9min_CPARMS_EC,
# SITES_WP_9min_CPARMS_EC, SITES_WP_9min_CPARMS_FG , MBR.CPARMS, AE.CPARMS , WP.CPARMS,
# load('/Volumes/MaloneLab/Research/FluxGradient/CarbonParms_MS1Sites.Rdata')
SITES_MBR_9min_CPARMS_FG <- SITES_MBR_9min_CPARMS_FG[sites]
SITES_MBR_9min_CPARMS_EC <- SITES_MBR_9min_CPARMS_EC[sites]
SITES_AE_9min_CPARMS_FG <- SITES_AE_9min_CPARMS_FG[sites]
SITES_AE_9min_CPARMS_EC <- SITES_AE_9min_CPARMS_EC[sites]
SITES_WP_9min_CPARMS_FG <- SITES_WP_9min_CPARMS_FG[sites]
SITES_WP_9min_CPARMS_EC <- SITES_WP_9min_CPARMS_EC[sites]
MBR.CPARMS <- MBR.CPARMS[sites]
AE.CPARMS <- AE.CPARMS[sites]
WP.CPARMS <- WP.CPARMS[sites]

## One2One_Plots: ####
source(fs::path(DirRepo,'exploratory/FUNCTION_One2One.R' ))

oldDir <- getwd()
dir <- fs::path(localdir,'FIGURES')
dir.create(dir)


for ( i in sites){
  print(i)
  
  # Subset the MBR Heights
  
  th.filter <- SITES_MBR_9min_FILTER[[i]]$TowerH %>% unique()

  SITES_MBR <- list()
  SITES_MBR[[i]] <- SITES_MBR_9min_FILTER[[i]] %>% filter(TowerH %in% th.filter)
  
  plot.it.CO2 <- one2one.plots ( MBR.DF = SITES_MBR[i] , 
                                 AE.DF = SITES_AE_9min_FILTER[i], 
                                 WP.DF = SITES_WP_9min_FILTER[i] , gas = 'CO2')
  print("Ready to plot") 
  
  print(plot.it.CO2)
  
  setwd(dir)
  png(paste("One2One_CO2", i,".png", sep=""), width=10, 
      height=5, units="in", res=1200)
  print(plot.it.CO2)
  dev.off()
  setwd(oldDir)
  
  print("done")       
  
  try(plot.it.H2O <- one2one.plots ( MBR.DF = SITES_MBR[i] , 
                                     AE.DF = SITES_AE_9min_FILTER[i], 
                                     WP.DF = SITES_WP_9min_FILTER[i] , gas = 'H2O'), silent = T)
  print("Ready to plot") 
  
  print(plot.it.H2O)
  
  setwd(dir)
  png(paste("One2One_H2O", i,".png", sep=""), width=10, 
      height=5, units="in", res=1200)
  print(plot.it.H2O)
  dev.off()
  setwd(oldDir)
  
  print("done")       
}

# DIURNAL PLOTS:
for ( i in sites){
  
  df.MBR <-  Diurnal.MBR.CO2[i] %>% as.data.frame
  names( df.MBR ) <- substring( names(df.MBR ), 6)
  
  df.AE <-  Diurnal.AE.CO2[i] %>% as.data.frame
  names( df.AE ) <- substring( names(df.AE ), 6)
  
  df.WP <-  Diurnal.WP.CO2[i] %>% as.data.frame
  names( df.WP ) <- substring( names(df.WP ), 6)
  
  p1 <- p2 <- p3 <- NULL
  try({
    p1 <- ggplot( data = df.MBR) + stat_smooth(aes(x = Hour , y = FG), col="black") + stat_smooth(aes(x = Hour , y = EC), col="black", linetype="dashed") + theme_bw() + ylab("MBR") +  facet_wrap(~TowerH, ncol = length(unique(df.MBR$TowerH)) ) 
    
    p2 <- ggplot( data = df.AE) + stat_smooth(aes(x = Hour , y = FG), col="black") + stat_smooth(aes(x = Hour , y = EC), col="black", linetype="dashed") + theme_bw() + ylab("AE") +  facet_wrap(~TowerH, ncol = length(unique(df.AE$TowerH)) )
    
    p3 <-ggplot( data = df.WP) + stat_smooth(aes(x = Hour , y = FG), col="black") + stat_smooth(aes(x = Hour , y = EC), col="black", linetype="dashed") + theme_bw() + ylab("WP") +  facet_wrap(~TowerH, ncol = length(unique(df.WP$TowerH)) )
  })
  
  print(ggarrange( p1, p2, p3, nrow=3))
  
  setwd(dir)
  png(paste("Diurnal_CO2_", i,".png", sep=""), width=6, 
      height=5, units="in", res=1200)
  
  print(ggarrange( p1, p2, p3, nrow=3))
  dev.off()
  setwd(oldDir)
  
  print("done with CO2 Diel")       
  
  
  
  
  df.MBR <-  Diurnal.MBR.H2O[i] %>% as.data.frame
  names( df.MBR ) <- substring( names(df.MBR ), 6)
  
  df.AE <-  Diurnal.AE.H2O[i] %>% as.data.frame
  names( df.AE ) <- substring( names(df.AE ), 6)
  
  df.WP <-  Diurnal.WP.H2O[i] %>% as.data.frame
  names( df.WP ) <- substring( names(df.WP ), 6)
  
  p1 <- p2 <- p3 <- NULL
  try({
    p1 <- ggplot( data = df.MBR) + stat_smooth(aes(x = Hour , y = FG), col="black") + stat_smooth(aes(x = Hour , y = EC), col="black", linetype="dashed") + theme_bw() + ylab("MBR") +  facet_wrap(~TowerH, ncol = length(unique(df.MBR$TowerH)) ) 
    
    p2 <- ggplot( data = df.AE) + stat_smooth(aes(x = Hour , y = FG), col="black") + stat_smooth(aes(x = Hour , y = EC), col="black", linetype="dashed") + theme_bw() + ylab("AE") +  facet_wrap(~TowerH, ncol = length(unique(df.AE$TowerH)) )
    
    p3 <-ggplot( data = df.WP) + stat_smooth(aes(x = Hour , y = FG), col="black") + stat_smooth(aes(x = Hour , y = EC), col="black", linetype="dashed") + theme_bw() + ylab("WP") +  facet_wrap(~TowerH, ncol = length(unique(df.WP$TowerH)) )
  })
  
  print(ggarrange( p1, p2, p3, nrow=3))
  
  setwd(dir)
  png(paste("Diurnal_H2O_", i,".png", sep=""), width=6, 
      height=5, units="in", res=1200)
  print(ggarrange( p1, p2, p3, nrow=3))
  dev.off()
  setwd(oldDir)
  
  print("done with H2O diel")   
  
}
setwd(oldDir)

# DIURNAL DIFF PLOTS:
source(fs::path(DirRepo,'exploratory/FUNCTION_DIURNAL.R' ))
diurnal.summary <- Diurnal.Summary(diurnal.tibble = Diurnal.MBR.CO2, TYP='MBR' ) %>% rbind(Diurnal.Summary(diurnal.tibble = Diurnal.AE.CO2, TYP='AE' ) ) %>% rbind(Diurnal.Summary(diurnal.tibble = Diurnal.WP.CO2, TYP='WP' ) )  

# Adjust the order of type:

#standardize across sites:

diurnal.summary$Type <- factor( diurnal.summary$Type, levels= c('MBR', 'AE', 'WP'))
setwd(dir)
for ( i in sites){
  
  p1 <- diurnal.summary %>% filter( Site == i) %>%  ggplot( ) + geom_col( aes( y =Flux.deviation, x = TowerH)) + ylab('Diurnal Difference (%)') + xlab( 'Tower Height') + facet_wrap(~Type, ncol = length(unique(diurnal.summary$Type)) )
  
  
  print(ggarrange( p1, nrow=1))
  
  setwd(dir)
  png(paste("Diurnal_DIFF_", i,".png", sep=""), width=4, 
      height=4, units="in", res=1200)
  print(ggarrange( p1, nrow=1))
  dev.off()
  setwd(oldDir)
  
  print("done")       
  
}


p1 <- diurnal.summary %>% filter( Site == 'HARV' ) %>%  ggplot( ) + geom_col( aes( y =Flux.deviation, x = TowerH)) + ylab('Diurnal Difference (%)') + xlab( 'Tower Height') + facet_wrap(~Type, ncol = length(unique(diurnal.summary$Type)) ) + ylim(0, 300)

p2 <- diurnal.summary %>% filter( Site == 'KONZ' ) %>%  ggplot( ) + geom_col( aes( y =Flux.deviation, x = TowerH)) + ylab('Diurnal Difference (%)') + xlab( 'Tower Height') + facet_wrap(~Type, ncol = length(unique(diurnal.summary$Type)) )  + ylim(0, 300)

p3 <- diurnal.summary %>% filter( Site == 'GUAN' ) %>%  ggplot( ) + geom_col( aes( y =Flux.deviation, x = TowerH)) + ylab('Diurnal Difference (%)') + xlab( 'Tower Height') + facet_wrap(~Type, ncol = length(unique(diurnal.summary$Type)) )  + ylim(0, 300)

p4 <- diurnal.summary %>% filter( Site == 'JORN' ) %>%  ggplot( ) + geom_col( aes( y =Flux.deviation, x = TowerH)) + ylab('Diurnal Difference (%)') + xlab( 'Tower Height') + facet_wrap(~Type, ncol = length(unique(diurnal.summary$Type)) )  + ylim(0, 300)

setwd(dir)
png("Diurnal_Diff_sites.png", width=6, 
    height=8, units="in", res=1200)
print(ggarrange( p1,p2, p3, p4, nrow=2,ncol=2, labels=c("a.", "b.", "c.", "d")))
dev.off()
setwd(oldDir)

# Carbon Exchange PARMS: ####


setwd(dir)
png("CarbonExchange_LRC_MBR.png", width=10, 
    height=8, units="in", res=1200)

ggarrange( 
  ggplot() + geom_violin(data = MBR.CPARMS$HARV , aes( x= TYP, y = a1.mean, col= TowerH))  + ylab("Quantum Yield") ,
  ggplot() + geom_violin(data = MBR.CPARMS$KONZ , aes( x= TYP, y = a1.mean, col= TowerH))  + ylab("Quantum Yield"),
  ggplot() + geom_violin(data = MBR.CPARMS$GUAN , aes( x= TYP, y = a1.mean, col= TowerH))  + ylab("Quantum Yield") ,
  ggplot() + geom_violin(data = MBR.CPARMS$JORN , aes( x= TYP, y = a1.mean, col= TowerH)) + ylab("Quantum Yield") , 
  
  ggplot() + geom_violin(data = MBR.CPARMS$HARV , aes( x= TYP, y = ax.mean, col= TowerH))  + ylab("Amax") ,
  ggplot() + geom_violin(data = MBR.CPARMS$KONZ , aes( x= TYP, y = ax.mean, col= TowerH))  + ylab("Amax"),
  ggplot() + geom_violin(data = MBR.CPARMS$GUAN , aes( x= TYP, y = ax.mean, col= TowerH))  + ylab("Amax") ,
  ggplot() + geom_violin(data = MBR.CPARMS$JORN , aes( x= TYP, y = ax.mean, col= TowerH)) + ylab("Amax"), 
  
  ggplot() + geom_violin(data = MBR.CPARMS$HARV , aes( x= TYP, y = r.mean, col= TowerH))  + ylab("Reco") ,
  ggplot() + geom_violin(data = MBR.CPARMS$KONZ , aes( x= TYP, y = r.mean, col= TowerH))  + ylab("Reco") ,
  ggplot() + geom_violin(data = MBR.CPARMS$GUAN , aes( x= TYP, y = r.mean, col= TowerH))  + ylab("Reco")  ,
  ggplot() + geom_violin(data = MBR.CPARMS$JORN , aes( x= TYP, y = r.mean, col= TowerH)) + ylab("Amax"), ncol=4, nrow=3, labels= c("a.", "b." ,"c.", "d", "e.", "f." ,"g.", "h.","i.", "j." ,"k.", "l"))

dev.off()

png("CarbonExchange_LRC_AE.png", width=10, 
    height=8, units="in", res=1200)

ggarrange( 
  ggplot() + geom_violin(data = AE.CPARMS$HARV , aes( x= TYP, y = a1.mean, col= TowerH))  + ylab("Quantum Yield") ,
  ggplot() + geom_violin(data = AE.CPARMS$KONZ , aes( x= TYP, y = a1.mean, col= TowerH))  + ylab("Quantum Yield"),
  ggplot() + geom_violin(data = AE.CPARMS$GUAN , aes( x= TYP, y = a1.mean, col= TowerH))  + ylab("Quantum Yield") ,
  ggplot() + geom_violin(data = AE.CPARMS$JORN , aes( x= TYP, y = a1.mean, col= TowerH)) + ylab("Quantum Yield") , 
  
  ggplot() + geom_violin(data = AE.CPARMS$HARV , aes( x= TYP, y = ax.mean, col= TowerH))  + ylab("Amax") ,
  ggplot() + geom_violin(data = AE.CPARMS$KONZ , aes( x= TYP, y = ax.mean, col= TowerH))  + ylab("Amax"),
  ggplot() + geom_violin(data = AE.CPARMS$GUAN , aes( x= TYP, y = ax.mean, col= TowerH))  + ylab("Amax") ,
  ggplot() + geom_violin(data = AE.CPARMS$JORN , aes( x= TYP, y = ax.mean, col= TowerH)) + ylab("Amax") , 
  
  ggplot() + geom_violin(data = AE.CPARMS$HARV , aes( x= TYP, y = r.mean, col= TowerH))  + ylab("Reco") ,
  ggplot() + geom_violin(data = AE.CPARMS$KONZ , aes( x= TYP, y = r.mean, col= TowerH))  + ylab("Reco")  ,
  ggplot() + geom_violin(data = AE.CPARMS$GUAN , aes( x= TYP, y = r.mean, col= TowerH))  + ylab("Reco") ,
  ggplot() + geom_violin(data = AE.CPARMS$JORN , aes( x= TYP, y = r.mean, col= TowerH)) + ylab("Amax"), ncol=4, nrow=3, labels= c("a.", "b." ,"c.", "d", "e.", "f." ,"g.", "h.","i.", "j." ,"k.", "l"))

dev.off()

png("CarbonExchange_LRC_WP.png", width=10, 
    height=8, units="in", res=1200)

ggarrange( 
  ggplot() + geom_violin(data = WP.CPARMS$HARV , aes( x= TYP, y = a1.mean, col= TowerH))  + ylab("Quantum Yield") ,
  ggplot() + geom_violin(data = WP.CPARMS$KONZ , aes( x= TYP, y = a1.mean, col= TowerH))  + ylab("Quantum Yield"),
  ggplot() + geom_violin(data = WP.CPARMS$GUAN , aes( x= TYP, y = a1.mean, col= TowerH))  + ylab("Quantum Yield") ,
  ggplot() + geom_violin(data = WP.CPARMS$JORN , aes( x= TYP, y = a1.mean, col= TowerH)) + ylab("Quantum Yield") , 
  
  ggplot() + geom_violin(data = WP.CPARMS$HARV , aes( x= TYP, y = ax.mean, col= TowerH))  + ylab("Amax") ,
  ggplot() + geom_violin(data = WP.CPARMS$KONZ , aes( x= TYP, y = ax.mean, col= TowerH))  + ylab("Amax"),
  ggplot() + geom_violin(data = WP.CPARMS$GUAN , aes( x= TYP, y = ax.mean, col= TowerH))  + ylab("Amax") ,
  ggplot() + geom_violin(data = WP.CPARMS$JORN , aes( x= TYP, y = ax.mean, col= TowerH)) + ylab("Amax"), 
  
  ggplot() + geom_violin(data = WP.CPARMS$HARV , aes( x= TYP, y = r.mean, col= TowerH))  + ylab("Reco") ,
  ggplot() + geom_violin(data = WP.CPARMS$KONZ , aes( x= TYP, y = r.mean, col= TowerH))  + ylab("Reco")  ,
  ggplot() + geom_violin(data = WP.CPARMS$GUAN , aes( x= TYP, y = r.mean, col= TowerH))  + ylab("Reco")  ,
  ggplot() + geom_violin(data = WP.CPARMS$JORN , aes( x= TYP, y = r.mean, col= TowerH)) + ylab("Amax") , ncol=4, nrow=3, labels= c("a.", "b." ,"c.", "d", "e.", "f." ,"g.", "h.","i.", "j." ,"k.", "l"))

dev.off()
setwd(oldDir)

# Create a file to compile the AOP and the flux data
#Sites.Summary

Sites.Summary.sub <- Sites.Summary %>% filter(site %in% sites)

plot.EVI <- Sites.Summary.sub %>% ggplot(aes(x= EVI.mean, y = site, xmin = EVI.mean - EVI.sd , 
                                 xmax = EVI.mean + EVI.sd)) + geom_point( aes( )) +
  geom_errorbar(  aes(),width=0.3) + theme_bw() + ylab("") + xlab("EVI")

plot.NDVI <- Sites.Summary.sub %>% ggplot(aes(x= NDVI.mean, y = site, xmin = NDVI.mean - NDVI.sd , 
                                             xmax = NDVI.mean + NDVI.sd)) + geom_point( aes( )) +
  geom_errorbar(  aes(),width=0.3) + theme_bw() + ylab("") + xlab("NDVI")


plot.LAI <- Sites.Summary.sub %>% ggplot(aes(x= LAI.mean, y = site, xmin = LAI.mean - LAI.sd , 
                                             xmax = LAI.mean + LAI.sd)) + geom_point( aes( )) +
  geom_errorbar(  aes(),width=0.3) + theme_bw() + ylab("") + xlab("LAI")

plot.CHM <- Sites.Summary.sub %>% ggplot(aes(x= CHM.mean, y = site, xmin = CHM.mean - CHM.sd , 
                                             xmax = CHM.mean + CHM.sd)) + geom_point( aes( )) +
  geom_errorbar(  aes(),width=0.3) + theme_bw() + ylab("") + xlab("CHM")

setwd(dir)

png("Structure_Summary_MS1.png", width=8, 
    height=6, units="in", res=1200)

ggarrange(plot.LAI, plot.EVI, plot.NDVI, plot.CHM, labels=c("a", "b", "c", "d") )

dev.off()
setwd(oldDir)


# flow.attr.map - makes a map of the sites for manuscript...
