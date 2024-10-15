# Pull data from google drive
email <- 'saj82@cornell.edu'
#copy this browser url from the site folder on the shared G drive (located at https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3) you wish to upload your zip files to
#this url should point to the NEONSITES_Validation folder
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/14Ga9sLRMlQVvorZdHBiYxCbUybiGwPNp")
# ------ Prerequisites! Make sure these packages are installed ----
# Also requires packages: googledrive
library(dplyr)
library(lubridate)
library(ggplot2)
library(gslnls)
library(ggh4x)
library(googledrive)

# Load functions in this repo
source(file.path("functions/plot.all.sites.1to1.R"))
source(file.path("functions/plot.single.site.1to1.R"))
source(file.path("functions/plot.all.sites.bar.R"))
source(file.path("functions/light.response.curve.R"))
source(file.path("functions/plot.light.response.R"))
source(file.path("functions/temp.response.curve.R"))
source(file.path("functions/plot.temp.response.R"))
source(file.path("functions/plot.all.sites.diurnal.R"))
source(file.path("functions/calculate.all.sites.diurnal.avg.R"))
source(file.path("functions/all.sites.light.response.curve.R"))
source(file.path("functions/all.sites.temp.response.curve.R"))

#Pull data from gdrive
# Authenticate with Google Drive and get site data
googledrive::drive_auth(email = email) # Likely will not work on RStudio Server. If you get an error, try email=TRUE to open an interactive auth session.
validation_folder <- googledrive::drive_ls(path = drive_url)
#site_folder <- googledrive::drive_ls(path = data_folder$id[data_folder$name==sitecode])
dirTmp <- fs::path(tempdir())
dir.create(dirTmp)
for(focal_file in validation_folder$name){
  
  # Find the file identifier for that file
  file_id <- subset(validation_folder, name == focal_file)
  
  # Download that file
  pathDnld <- fs::path(dirTmp,focal_file)
  googledrive::drive_download(file = file_id$id, 
                              path = pathDnld,
                              overwrite = T)
  # Unzip
  # if(grepl(pattern='.zip',focal_file)){
  #   utils::unzip(pathDnld,exdir=dirTmp)
  # }
  
}
#Load validation data frames 
#the R.data/zipfiles are labeled based on the method used to calculate the fluxes (i.e. AE, WP, MBR)
#problem loading in .Rdata objects currently being saved to 2 different directories?
#TO DO: better understand how to control where files are unzipped to when downloaded off of gdrive
fileIn <- fs::path(dirTmp, paste0("SITES_AE_val.Rdata"))
load(fileIn)
fileIn <- fs::path(dirTmp, paste0("SITES_WP_Stability.Rdata"))
load(fileIn)
fileIn <- fs::path(dirTmp, paste0("SITES_MBR_Stability.Rdata"))
load(fileIn)
#if data is already downloaded and saved
#load(file.path("data", "Validation", paste0("SITES_AE_val.Rdata")))
#load(file.path("data", "Validation", paste0("SITES_WP_val.Rdata")))
#load(file.path("data", "Validation", paste0("SITES_MBR.Rdata")))

#bind all site dataframes together
all.sites.ae <- bind_rows(SITES_AE_validation)
# all.sites.wp <- bind_rows(SITES_WP_validation)
# all.sites.mbr <- bind_rows(SITES_MBR)
#TO DO ADD MBR, NOT YET AVAILABLE
# all.sites.mbr <- bind_rows(SITES_MBR_validation)


#####Site Selection###
#Comparing all sites, use all.sites.[method]
#Comparing chosen sites, use SITES_[method]_validation

#Real Eddy Diff. converter - Sam J.

"This code is intended to take the EC fluxes and concetration observations from NEON
towers and convert them into an eddy diffusivity for comparison against the eddy
diffusivities calculated from flux gradient methods."

#'rho is dry air density kg/m3
#'flux is the EC flux obs. in conc/m^2s or W/m^2
#'dz is the height difference TowerHeight_A - TowerHeight_B
#'dx is the scalar difference in either concentration or temp between sensor heights
#'fluxtype is either LE for latent heat, H for sensible heat, or Gas for CO2 or CH4, this will convert 
#'W/m^2 to kinematic forms (h2O conc /m^2s or K/m^2s) 

###EC Eddy Diffusivity###

#add a filter for tower heights
eddy_diff_real <- function(rho,flux,dz,dx,fluxtype = "type"){
  
  cp = 1005 #J/kgK
  mol_air = rho*34.53
  if(fluxtype == "LE"){
    flux = (flux/(2.25*10**6)) #converted to kg/m^2s by dividing heat of vaporization
    flux = (flux/(.01801))*1000 # divided by molecular weight of water and multiplied by 1000 to get mmol/m^2s
    mol_air = rho*34.53 #multiply kg/m3 of air by # of moles per kg of air to get mol air/m3
    Kgas = -(flux*dz)/(dx*mol_air) #m2/s
  }else {
    Kgas = -(flux*dz)/(dx*mol_air)
  }
  
  return(Kgas)
}

###Choose one site or all###
site <- SITES_AE_validation$KONZ


###Choose which gas to use###
df_CO2 <- site  %>% filter(gas == "CO2")
df_H2O <- site  %>% filter(gas == "H2O")
df_CH4 <- site  %>% filter(gas == "CH4")


df_CO2$kco2 <- eddy_diff_real(df_CO2$rhoa_kgm3,df_CO2$FC_turb_interp,df_CO2$dHeight,df_CO2$dConc,fluxtype = "CO2")
df_H2O$kh2o <- eddy_diff_real(df_H2O$rhoa_kgm3,df_H2O$LE_turb_interp,df_H2O$dHeight,df_H2O$dConc,fluxtype = "LE")

#Compare FG eddy to EC eddy diffs

plot(df_H2O$EddyDiff,df_H2O$kh2o, ylim =c(-6,6), xlim = c(0,6))
hist(df_H2O$EddyDiff,breaks = 40,ylim = c(0,1000))

df_H2O_filtered <- df_H2O %>% filter(df_H2O$kh2o < 3)

df_H2O_filtered <-df_H2O_filtered%>% filter(df_H2O_filtered$kh2o > -2)

hist(df_H2O_filtered$kh2o,breaks=50)

plot(df_CO2$EddyDiff,df_CO2$kco2, ylim =c(-6,6), xlim = c(0,6))



#Compare EC H2O and CO2 eddy diffs


df_H2O_filtered <- df_H2O %>% filter(df_H2O$kh2o < 3)
df_H2O_filtered <-df_H2O_filtered%>% filter(df_H2O_filtered$kh2o > -2)
hist(df_H2O_filtered$kh2o,breaks=50, ylim = c(0,1000))


df_CO2_filtered <- df_CO2 %>% filter(df_CO2$kco2 < 3)
df_CO2_filtered <-df_CO2_filtered%>% filter(df_CO2_filtered$kco2 > -2)

hist(df_CO2_filtered$kco2,breaks=50,ylim = c(0,1000))


df_EC_k <- merge(df_H2O_filtered[, c('match_time', 'kh2o')], df_CO2_filtered[, c('match_time', 'kco2')], by = 'match_time')

plot(df_EC_k$kco2,df_EC_k$kh2o)
abline(a = 0, b = 1, col = "red")
cor.test(df_EC_k$kco2,df_EC_k$kh2o)


#Difference in EC and FG eddy diffs as a function of concentration difference

df_CO2_filtered$kdiff <- 100*(df_CO2_filtered$EddyDiff - df_CO2_filtered$kco2)/df_CO2_filtered$EddyDiff

df_H2O_filtered$kdiff <- 100*(df_H2O_filtered$EddyDiff - df_H2O_filtered$kh2o)/df_H2O_filtered$EddyDiff

plot(df_CO2_filtered$dConc,df_CO2_filtered$kdiff,xlim = c(-10,10),ylab = "Percent Difference")
plot(df_H2O_filtered$dConc,df_H2O_filtered$kdiff,xlim = c(-4,4),ylim = c(-100,500),ylab = "Percent Difference")




####Cross Gradient Flux Flagger###

#' dConc is a vector of the difference in concentration
#' flux is the EC flux measured
#' df is the dataframe of interest
#' 

cross_grad_flag <- function(df,dConc,flux){
  df <- cbind(df, cross_grad_flag = NA)
  df$cross_grad_flag <- ifelse(dConc < 0 & flux < 0 |dConc > 0 & flux > 0,1,0 )
  return(df)
}

sort(unique(SITES_AE_validation$KONZ$TowerPosition_B))
