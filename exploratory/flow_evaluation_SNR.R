# Need to check the impact of different filters for SNR:
library(tidyverse)
library(sf)

source(fs::path(DirRepo,'exploratory/FUNCTION_Filter_FG.R' ))
source(fs::path(DirRepo,'exploratory/FUNCTION_SITELIST_FORMATTING.R' ))

for( site in site.list){
  
  print( site)
  
  # Load the files:
  localdir.site <- paste(localdir,"/", site, sep = "")
  
  files <- paste(site, "_Evaluation.Rdata", sep = "")
  
  load(paste(localdir.site, "/", files, sep=""))
  
  # Change the time to local:
  
  library(lutz)
  # Get NEON sites from the server and find the time zones: https://cran.r-project.org/web/packages/lutz/readme/README.html
  sites.location <- read.csv('/Volumes/MaloneLab/Research/FluxGradient/Ameriflux_NEON field-sites.csv') %>%  st_as_sf(coords = c("Longitude..degrees.", "Latitude..degrees."),
                                                                                                                      crs = "+proj=longlat +datum=WGS84")
  
  sites.location$TZ <- tz_lookup(sites.location, method = "accurate")
  sites.location.sub <- sites.location %>%  select( "Site_Id.NEON" , "TZ")
  
  site.tz <- sites.location.sub$TZ[which( sites.location.sub$Site_Id.NEON == site)]
  
  MBR_9min.df.final$timeEndA.local <- MBR_9min.df.final$timeEndA %>%  as.POSIXlt( tz = site.tz)
  AE_9min.df.final$timeEndA.local <- AE_9min.df.final$timeEnd_A %>%  as.POSIXlt( tz = site.tz)
  WP_9min.df.final$timeEndA.local <- WP_9min.df.final$timeEnd_A %>%  as.POSIXlt( tz = site.tz)
  
  MBR_9min.df.final$Month.local <- MBR_9min.df.final$timeEndA.local %>% format("%m")
  MBR_9min.df.final$Month.local <- MBR_9min.df.final$timeEndA.local %>% format("%m")
  MBR_9min.df.final$Month.local <- MBR_9min.df.final$timeEndA.local %>% format("%m")
  
  MBR_9min.df.final$time.local <- MBR_9min.df.final$timeEndA.local %>% format("%H:%M")
  AE_9min.df.final$time.local <- AE_9min.df.final$timeEndA.local %>% format("%H:%M")
  WP_9min.df.final$time.local <- WP_9min.df.final$timeEndA.local %>% format("%H:%M")
  
  MBR_9min.df.final$hour.local <- MBR_9min.df.final$timeEndA.local %>% format("%H")
  AE_9min.df.final$hour.local <- AE_9min.df.final$timeEndA.local %>% format("%H")
  WP_9min.df.final$hour.local <- WP_9min.df.final$timeEndA.local %>% format("%H")
  
  # Apply

  dataframe <- MBR_9min.df.final
  
  
  get.rmse <- function( dataframe,  SNR.threshold, approach, gas.t, MLevel){

    
    df.filter <- filter_fluxes( df = dataframe ,
                                        dConcSNR.min = SNR.threshold,
                                        approach = approach) %>% filter( gas == gas.t , dLevelsAminusB == MLevel)
 
    lm_object <- lm(data = df.filter , 
                    FC_nee_interp~FG_mean) 
    
    rmse <- sqrt(mean(lm_object$residuals^2,na.rm=T))
    
    return(rmse)
  }
  

  
  # Get RMSE
  
  
  summary.rmse <- data.frame( dConcSNR.min = as.numeric(),
                              MBR.rmse  = as.numeric(), 
                              AE.rmse = as.numeric(), 
                              WP.rmse = as.numeric(),
                              dLevelsAminusB= as.numeric(),
                              gas = as.numeric())

  for( j in seq(0, 5, 1)){
    for ( i in MBR_9min.df.final$dLevelsAminusB %>% unique %>% na.omit){
      print( i)
      print(j)
      
      
      MBR.rmse  =  try(get.rmse( dataframe = MBR_9min.df.final,
                                 SNR.threshold = j, 
                                 approach="MBR", 
                                 gas.t = "CO2",
                                 MLevel= i), silent = T) %>% as.numeric
      
      AE.rmse =  try(get.rmse( dataframe = AE_9min.df.final,
                               SNR.threshold = j, 
                               approach="AE", 
                               gas = "CO2",
                               MLevel= i),  silent = T) %>% as.numeric
      
      WP.rmse =  try(get.rmse( dataframe = WP_9min.df.final,
                               SNR.threshold = j, 
                               approach="WP", 
                               gas = "CO2",
                               MLevel= i), silent = T) %>% as.numeric
    
      df <- data.frame( dConcSNR.min= j,
                        MBR.rmse = MBR.rmse,
                        AE.rmse = AE.rmse,
                        WP.rmse =  WP.rmse,
                        dLevelsAminusB= i,
                        gas = "CO2")
      
      summary.rmse <- rbind( summary.rmse, df)
      
    }
    
    
  }
  
  
  
  # plots: 
  plot <- summary.rmse %>% ggplot() + geom_smooth(aes(x=dConcSNR.min ,y=MBR.rmse), col="Black")  + 
    geom_smooth(aes(x=dConcSNR.min ,y=AE.rmse), col="goldenrod") + 
    geom_smooth(aes(x=dConcSNR.min ,y=WP.rmse), col="gray40") + theme_bw() + ylim(0,15)+
    annotate(geom="text", x=2, y=10, label="MBR ",color="black")+
    annotate(geom="text", x=2.5, y=10, label="AE ",color="goldenrod")+
    annotate(geom="text", x=3, y=10, label="WP",color="gray40") + geom_vline(xintercept=3, linetype="dashed", 
                                                                            color = "red", size=2) + xlim(0, 5)
  print(  plot )

  # Save plots
  plot.name <- paste(site,"_SNR.png", sep="")
  SNR.dir <- '/Volumes/MaloneLab/Research/FluxGradient/SNR_plot'
ggsave( plot.name , 
        plot=plot,
  path =  SNR.dir)
message( paste(site, "plot saved"))
# Save Summary:
summary.rmse <- summary.rmse %>% mutate(site = site)

summary.file.name <- paste("/Volumes/MaloneLab/Research/FluxGradient/SNR_Summary/",site, "_SNR.csv", sep="")
write.csv(summary.rmse ,summary.file.name )

message( paste(site, "file saved"))

message( paste(site, "is done"))
}  
