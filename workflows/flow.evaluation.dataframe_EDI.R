# EDI Dataset for Publication:

# See WorkFlow_master.R to set the variables needed to run this script
# Variables needed: gh_repo, eval_dir, edi_dir, site.list

# Need to check the impact of different filters for SNR:
library(tidyverse)
library(sf)
library(lutz)

for(site in site.list){
  
  print(site)
  
  # Load the files:
  localdir.site <- file.path(eval_dir, site)
  load(file.path(localdir.site, paste0(site, "_Evaluation.Rdata")))
  
  # Change the time to local:
  
  # Get NEON sites from the server and find the time zones: https://cran.r-project.org/web/packages/lutz/readme/README.html
  sites.location <- metadata %>%  st_as_sf(coords = c("Longitude..degrees.", "Latitude..degrees."),
                                           crs = "+proj=longlat +datum=WGS84")
  
  sites.location$TZ <- tz_lookup(sites.location, method = "accurate")
  sites.location.sub <- sites.location %>%  select("Site_Id.NEON", "TZ")
  
  site.tz <- sites.location.sub$TZ[which(sites.location.sub$Site_Id.NEON == site)]
  
  MBR_9min.df.final$timeEndA.local <- MBR_9min.df.final$timeEndA %>% as.POSIXlt(tz = site.tz)
  AE_9min.df.final$timeEndA.local <- AE_9min.df.final$timeEnd_A %>% as.POSIXlt(tz = site.tz)
  WP_9min.df.final$timeEndA.local <- WP_9min.df.final$timeEnd_A %>% as.POSIXlt(tz = site.tz)
  
  MBR_9min.df.final$Month.local <- MBR_9min.df.final$timeEndA.local %>% format("%m")
  AE_9min.df.final$Month.local  <- AE_9min.df.final$timeEndA.local %>% format("%m")
  WP_9min.df.final$Month.local  <- WP_9min.df.final$timeEndA.local %>% format("%m")
  
  MBR_9min.df.final$time.local <- MBR_9min.df.final$timeEndA.local %>% format("%H:%M")
  AE_9min.df.final$time.local <- AE_9min.df.final$timeEndA.local %>% format("%H:%M")
  WP_9min.df.final$time.local <- WP_9min.df.final$timeEndA.local %>% format("%H:%M")
  
  MBR_9min.df.final$hour.local <- MBR_9min.df.final$timeEndA.local %>% format("%H")
  AE_9min.df.final$hour.local <- AE_9min.df.final$timeEndA.local %>% format("%H")
  WP_9min.df.final$hour.local <- WP_9min.df.final$timeEndA.local %>% format("%H")
  
  # Create directory
  
  save.data.site <- file.path(edi_dir, site)
  dir.create(save.data.site)

  # Save the files

  write.csv(MBR_9min.df.final, file.path(save.data.site, paste0(site, "_MBR_9min.df.final.csv")))
  write.csv(AE_9min.df.final, file.path(save.data.site, paste0(site, "_AE_9min.df.final.csv")))
  write.csv(WP_9min.df.final, file.path(save.data.site, paste0(site, "_WP_9min.df.final.csv")))
  
  print("DONE")
}
  