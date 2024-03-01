#From Sam J.

library(neonUtilities)
library(readr)
library(BiocManager)
library(rhdf5)
library(dplyr)
library(oce)
library(tidyr)
library(lubridate)
library(naniar)
library(ggplot2)
library(stringr)
library(limma)
library(zoo)

install.packages("googledrive")

library("googledrive")
# Pull data from google drive
email <- 'saj82@cornell.edu'
#copy this browser url from the site folder on the shared G drive (located at https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3) you wish to upload your zip files to
drive_url <- googledrive::as_id("https://drive.google.com/drive/u/1/folders/1Ygq7mtpnR8fhLHv7fvDo2lDRbRoIV3Nm")
#add userinfo for saving and uploading the file to G drive
user <- "SJ"
sitecode <- 'GUAN'

# ------ Prerequisites! Make sure these packages are installed ----
# Also requires packages: googledrive
library(dplyr)

# Load functions in this repo #dont need these


# Final note: This script takes approx 10 min to run per site. 
# -------------------------------------------------------
# Authenticate with Google Drive and get site data
googledrive::drive_auth(email = email) # Likely will not work on RStudio Server. If you get an error, try email=TRUE to open an interactive auth session.
site_folder <- googledrive::drive_ls(path = drive_url)
#site_folder <- googledrive::drive_ls(path = data_folder$id[data_folder$name==sitecode])
dirTmp <- fs::path(tempdir(),sitecode)
dir.create(dirTmp)
#focal_file = "KONZ_30m.zip"

data_list <- c()
for(focal_file in site_folder$name){
  
  # Find the file identifier for that file
  file_id <- subset(site_folder, name == focal_file)
  
  # Download that file
  pathDnld <- fs::path(dirTmp,focal_file)
  googledrive::drive_download(file = file_id$id, 
                              path = pathDnld,
                              overwrite = T)
  # Unzip
  data <- unzip(pathDnld,exdir=dirTmp)
  data_list <- append(data,data_list)
}

data_list
load(data_list[6])
load(data_list[7])
load(data_list[5])

###ENSURE YOU HAVE THE CORRECT TOWER HEIGHT###

df_CO2_AE <- min9.FG.AE.list$CO2
df_H2O_AE <- min9.FG.AE.list$H2O
df_CH4_AE <- min9.FG.AE.list$CH4

df_CO2_WP <- min9.FG.WP.list$CO2
df_CO2_MBR <- MBRflux_align



#'data is a dataframe, x in is 1.5 fr outliers and 3 for extreme outliers.
#'This function eliminates outliers from a data colum
#'

outliers <- function(data,x){
  quant <- quantile(data$FG,prob=c(.25,.75), type=1,na.rm = TRUE)
  IQR <- quant[2]-quant[1]
  IQR <- IQR*x
  low <- quant[1]- IQR
  high <- quant[2] + IQR
  data <- data %>% filter(data$FG < high)
  data <- data %>% filter(data$FG > low)
  return(data)
}

outliers_MBR <- function(data,x){
  quant <- quantile(data$FCO2_MBR_H2Otrace,prob=c(.25,.75), type=1,na.rm = TRUE)
  IQR <- quant[2]-quant[1]
  IQR <- IQR*x
  low <- quant[1]- IQR
  high <- quant[2] + IQR
  data <- data %>% filter(data$FCO2_MBR_H2Otrace < high)
  data <- data %>% filter(data$FCO2_MBR_H2Otrace > low)
  return(data)
}


#diurnal average

df_CO2_AE<- df_CO2_AE  %>% filter(df_CO2_AE$dLevelsAminusB == "4_1")
df_CO2_AE <- outliers(df_CO2_AE,3)
df_CO2_WP<- df_CO2_WP  %>% filter(df_CO2_WP$dLevelsAminusB == "4_1")
df_CO2_WP <- outliers(df_CO2_WP,3)
df_CO2_MBR<- df_CO2_MBR  %>% filter(df_CO2_MBR$dLevelsAminusB_CO2 == "4_1")
df_CO2_MBR <- outliers_MBR(df_CO2_MBR,3)

df_CO2_MBR<- df_CO2_MBR  %>% filter(df_CO2_MBR$dLevelsAminusB_CO2 == "4_1")
df_CO2_MBR <- outliers_MBR(df_CO2_MBR,3)


####TIMEZONE####
df_CO2_WP$timeMid <- as.POSIXct(df_CO2_WP$timeMid, tz ="EST5EDT,M3.2.0/2:00:00,M11.1.0/2:00:00")
df_CO2_AE$timeMid <- as.POSIXct(df_CO2_AE$timeMid, tz ="EST5EDT,M3.2.0/2:00:00,M11.1.0/2:00:00")
df_CO2_MBR$timeMid_CO2 <- as.POSIXct(df_CO2_MBR$timeMid_CO2, tz ="EST5EDT,M3.2.0/2:00:00,M11.1.0/2:00:00")







df_CO2_WP$hour <- substr(as.character(df_CO2_WP$timeMid), start = 12, stop = 13)
df_CO2_AE$hour <- substr(as.character(df_CO2_AE$timeMid), start = 12, stop = 13)
df_CO2_MBR$hour <- substr(as.character(df_CO2_MBR$timeMid_CO2), start = 12, stop = 13)

df_CO2_WP_DIURNAL <- df_CO2_WP  %>% group_by(hour) %>% summarise(mean_WP_flux = mean(FG, na.rm=TRUE),
                                                                   EC_flux = mean(FC_interp, na.rm =TRUE),
                                                                   sd_WP_flux = sd(FG, na.rm=TRUE),
                                                                  n =n())
df_CO2_AE_DIURNAL <- df_CO2_AE  %>% group_by(hour) %>% summarise(mean_AE_flux = mean(FG, na.rm=TRUE),
                                                                 EC_flux = mean(FC_interp, na.rm =TRUE),
                                                                 sd_AE_flux = sd(FG, na.rm=TRUE),
                                                                 n =n(),
                                                                 sd_EC_flux = sd(FC_interp, na.rm =TRUE))

df_CO2_MBR_DIURNAL <- df_CO2_MBR %>% group_by(hour) %>% summarise(mean_MBR_flux = mean(FCO2_MBR_H2Otrace, na.rm=TRUE),
                                                                 EC_flux = mean(FC_interp_CO2, na.rm =TRUE),
                                                                 sd_MBR_flux = sd(FC_interp_CO2, na.rm =TRUE),
                                                                 n = n())

df_CO2_WP_DIURNAL$std_err <- df_CO2_WP_DIURNAL$sd_WP_flux/sqrt(df_CO2_WP_DIURNAL$n)
df_CO2_AE_DIURNAL$std_err <- df_CO2_AE_DIURNAL$sd_AE_flux/sqrt(df_CO2_AE_DIURNAL$n)
df_CO2_AE_DIURNAL$std_err_EC <- df_CO2_AE_DIURNAL$sd_EC_flux/sqrt(df_CO2_AE_DIURNAL$n)
df_CO2_MBR_DIURNAL$std_err <- df_CO2_MBR_DIURNAL$sd_MBR_flux/sqrt(df_CO2_MBR_DIURNAL$n)


#PLOT#
plot(df_CO2_WP_DIURNAL$hour,df_CO2_WP_DIURNAL$mean_WP_flux, type = "b",ylim = c(-12,34), col = "cadetblue3", lwd=2, pch = 16, cex = .75,
     xlab = "Hour (EST)",ylab="CO2 Flux")
points(df_CO2_AE_DIURNAL$hour,df_CO2_AE_DIURNAL$mean_AE_flux, type ="b", col = "brown3", lwd=2, pch = 17, cex = .75)
points(df_CO2_AE_DIURNAL$hour,df_CO2_AE_DIURNAL$EC_flux, type ="l", col = "black", lwd=3)
points(df_CO2_MBR_DIURNAL$hour,df_CO2_MBR_DIURNAL$mean_MBR_flux/5, type ="b", col = "orange", lwd=2, pch = 18, cex = .75)

arrows(x0 = as.numeric(df_CO2_WP_DIURNAL$hour),y0 = df_CO2_WP_DIURNAL$mean_WP_flux - df_CO2_WP_DIURNAL$std_err, x1 =as.numeric(df_CO2_WP_DIURNAL$hour), y1=df_CO2_WP_DIURNAL$mean_WP_flux + df_CO2_WP_DIURNAL$std_err,
       code=3, angle=90, length=0.05, lwd = 1.5, col=alpha("cadetblue3",.35))
arrows(x0 = as.numeric(df_CO2_AE_DIURNAL$hour),y0 = df_CO2_AE_DIURNAL$mean_AE_flux - df_CO2_AE_DIURNAL$std_err, x1 =as.numeric(df_CO2_AE_DIURNAL$hour), y1=df_CO2_AE_DIURNAL$mean_AE_flux + df_CO2_AE_DIURNAL$std_err,
       code=3, angle=90, length=0.05, lwd = 1.5, col=alpha("brown3",.35))
arrows(x0 = as.numeric(df_CO2_MBR_DIURNAL$hour),y0 = df_CO2_MBR_DIURNAL$mean_MBR_flux/5 - df_CO2_MBR_DIURNAL$std_err, x1 =as.numeric(df_CO2_MBR_DIURNAL$hour), y1=df_CO2_MBR_DIURNAL$mean_MBR_flux/5 + df_CO2_MBR_DIURNAL$std_err,
       code=3, angle=90, length=0.05, lwd = 1.5, col=alpha("orange",.35))
arrows(x0 = as.numeric(df_CO2_AE_DIURNAL$hour),y0 = df_CO2_AE_DIURNAL$EC_flux - df_CO2_AE_DIURNAL$std_err_EC, x1 =as.numeric(df_CO2_AE_DIURNAL$hour), y1=df_CO2_AE_DIURNAL$EC_flux + df_CO2_AE_DIURNAL$std_err_EC,
       code=3, angle=90, length=0.05, lwd = 1.5, col=alpha("black",.35))

title(paste(sitecode,"Diurnal Average", sep = " "))
subtitle = ("Neotropical Forest, Full Tower Height")
mtext(subtitle)
text(x=0, y=-35, '*MBR Method Divided by 5', xpd=NA, cex = .75)

