
# ------ Prerequisites! Make sure these packages are installed ----
# Also requires packages: googledrive
library(dplyr)
library(lubridate)
library(ggplot2)
library(gslnls)
library(ggh4x)
library(googledrive)


#Real Eddy Diff. converter - Sam J.

"This code is intended to take the EC fluxes and concetration observations from NEON
towers and convert them into an eddy diffusivity for comparison against the eddy
diffusivities calculated from flux gradient methods."

###EC Eddy Diffusivity###
"input site file is a list of 3 data frames, CO2,H2O,and CH4. Back calculates 
CO2 and H2O and returns original frames with added columns KCO2 and KH2O"




eddy_diff_real <- function(site) {
  # Define fluxtypes to process
  fluxtypes <- c("H2O", "CO2")
  
  for (fluxtype in fluxtypes) {
    df <- site[[fluxtype]] ####need to convert fluxtype into a non character here
    rho = df$rhoa_kgm3
    dz1 = df$TowerHeight_A
    dz2 = df$TowerHeight_B
    dx = df$dConc
    dz = as.numeric(dz1)-as.numeric(dz2)
    cp = 1005 #J/kgK
    mol_air = rho*34.53
    
    if(fluxtype == "H2O"){
      flux = df$LE_turb_interp
      flux = (flux/(2.25*10**6)) #converted to kg/m^2s by dividing heat of vaporization
      flux = (flux/(.01801))*1000 # divided by molecular weight of water and multiplied by 1000 to get mmol/m^2s
      mol_air = rho*34.53 #multiply kg/m3 of air by # of moles per kg of air to get mol air/m3
      Kgas = -(flux*dz)/(dx*mol_air) #m2/s
      
    } else {
      flux = df$FC_turb_interp
      Kgas = -(flux*dz)/(dx*mol_air)
    }
    
    new_col_name <- paste0("K", fluxtype)
    
    # Add the new column to the data frame
    df[[new_col_name]] <- Kgas
    site[[fluxtype]] <- df
  }
  
  return(site)
}





##########COMPARISON FUNCTION###########

###Site is the data in the format of a list of H2O and CO2 data frames
###Site name is a string such as "KONZ"

#means and 95% confidence difference in mean intervals are calculated from t.test
#p-value and corr variable calculated from cor.test

##################################WARNING#######################################
'Arbitrary detection limits constrain eddy diff between -2 and 4'
'Eventually change to detection limits Jackie and Cove find'
################################################################################


eddy_diff_compare <- function(site,site_name){
  
  df_comp <- data.frame( "Site" = character(0), "Levels" = character(0), 
                         "Mean_KH2O" = integer(0), "Mean_KCO2"= integer(0),
                         "low_95"=integer(0),"high_95"=integer(0), 
                         "p_value" = integer(0), "cor_test"=integer(0))
  
  df_CO2 <- site[["CO2"]]
  df_H2O <- site[["H2O"]]
  levels <- sort(unique(df_H2O$dLevelsAminusB)) #assuming same for all gases
  
  for (level in levels){
    df_level_CO2 <- df_CO2 %>% filter(dLevelsAminusB == level)
    df_level_H2O <- df_H2O %>% filter(dLevelsAminusB == level)
    
    ###Remove large spikes, REMOVE ONCE CONC DETECTION IS DONE###############
    df_H2O_filtered <- df_level_H2O %>% filter(df_level_H2O$KH2O < 4)
    df_H2O_filtered <-df_H2O_filtered%>% filter(df_H2O_filtered$KH2O > -2)
    
    df_CO2_filtered <- df_level_CO2  %>% filter(df_level_CO2$KCO2 < 4)
    df_CO2_filtered <-df_CO2_filtered%>% filter(df_CO2_filtered$KCO2 > -2)
    ###########################################################################
    data1 <- df_H2O_filtered$KH2O
    data2 <- df_CO2_filtered$KCO2
    
    t.test <- t.test(data1,data2)
    
    #Distribution comparison results
    Mean_H2O <- as.numeric(t.test$estimate[1])
    Mean_CO2 <- as.numeric(t.test$estimate[2])
    l_95 <- as.numeric(t.test$conf.int[1])
    h_95<- as.numeric(t.test$conf.int[2])
    
    df_corr_comp <- merge(df_H2O_filtered[, c('match_time', 'KH2O')], df_CO2_filtered[, c('match_time', 'KCO2')], by = 'match_time')
    cor_test <- cor.test(df_corr_comp$KH2O,df_corr_comp$KCO2)
    p <- as.numeric(cor_test$p.value)
    corr <- as.numeric(cor_test$estimate[1])
    
    #add to data_frame
    df_comp <- add_row(df_comp,Site = site_name, Levels = level, Mean_KH2O =Mean_H2O, 
                       Mean_KCO2 = Mean_CO2, low_95 = l_95, high_95 = h_95,
                       p_value = p, cor_test = corr)
    
    
  }
  return(df_comp)
}



####Cross Gradient Flux Flagger###
"Flags all instances of a cross gradient flux"
#' dConc is a vector of the difference in concentration
#' flux is the EC flux measured
#' df is the dataframe of interest
#' 

cross_grad_flag <- function(df,K){
  df <- cbind(df, cross_grad_flag = NA)
  df$cross_grad_flag <- ifelse(df$K < 0,1,0 )
  return(df)
}



####What percent of each dataset is cross gradient fluxes?#####

percent_cross_grad <- function(cross_grad_flag) {
  
  # Calculate the number of 1's
  num_ones <- sum(cross_grad_flag, na.rm = TRUE)
  
  # Calculate the total number of entries
  total_entries <- length(cross_grad_flag)
  
  # Calculate the percentage of 1's
  percentage_ones <- (num_ones / total_entries) * 100
  
  return(percentage_ones)
}


###TO MODIFY CHANGE SITE AND fileDnld zip files###

# Pull data from google drive
email <- 'saj82@cornell.edu'
#email <- 'jaclyn_matthes@g.harvard.edu'
#email <- 'kyle.delwiche@gmail.com'

site_list <- c('KONZ','JORN','HARV', 'GUAN')

# Authenticate with Google Drive
googledrive::drive_auth(email = email) # Likely will not work on RStudio Server. If you get an error, try email=TRUE to open an interactive auth session.
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3")
data_folder <- googledrive::drive_ls(path = drive_url)


#Initialize an empty dataframe
K_comp <- data.frame() 
cross_grad_perc <- data.frame() 

site_folder <- googledrive::drive_ls(path = data_folder$id[data_folder$name=="NEONSITES_Validation"])

# Download data
dirTmp <- fs::path(tempdir(),"NEONSITES_Validation")
dir.create(dirTmp)

fileDnld <- "NEONSITES_Validation" #insert zip file to pull here

# Find the file identifier for that file
file_id <- subset(site_folder, name == "SITES_AE_9min.Rdata" )

# Download that file
pathDnld <- fs::path(dirTmp, "SITES_AE_9min.Rdata" )
googledrive::drive_download(file = file_id$id, 
                            path = pathDnld,
                            overwrite = T)

# Load the data 
fileIn <- pathDnld
load(fileIn)


#Convert New Data format into old data format

# Assuming SITES_AE_9min is a list containing the data frames
# Example: SITES_AE_9min <- list(GUAN_AE_9min, KONZ_AE_9min, JORN_AE_9min, HARV_AE_9min)

# Initialize empty lists to store the results
HARV <- list()
GUAN <- list()
KONZ <- list()
JORN <- list()

# List of site names
site_names <- c("GUAN", "KONZ", "JORN", "HARV")

# Iterate over each site name in the list
for (site in site_names) {
  
  # Access the data frame for the current site
  site_df <- SITES_AE_9min[[paste0(site, "_AE_9min")]]
  
  # Filter for each gas type and store the filtered data frames in the site-specific list
  site_filtered <- list(
    H2O = site_df %>% filter(gas == "H2O"),
    CO2 = site_df %>% filter(gas == "CO2"),
    CH4 = site_df %>% filter(gas == "CH4")
  )
  
  # Assign the filtered data frames to the site-specific list
  assign(site, site_filtered)
}




for (site in site_list){
  
 
  ####DATA LOADED, ANALYSIS START####
  
  result <- eddy_diff_real(get(site)) #calculates eddy diffusivity 
  
  assign(site, result, envir = .GlobalEnv) #names new data frame after the site
  
  comparison_result <- eddy_diff_compare(get(site), site) #compares eddy diffusivities 
  
  K_comp <- rbind(K_comp, comparison_result)
  
  K_comp <- na.omit(K_comp)
  
  #####Analysis of Cross Gradient flux####
  
  site_CO2_Flagged <- cross_grad_flag(get(site)$CO2,get(site)$CO2$KCO2)
  
  # Loop through unique dLevelAminusB values
  for (level in unique(site_CO2_Flagged$dLevelsAminusB)) {
    # Filter the flagged data for the current level
    level_data <- site_CO2_Flagged[site_CO2_Flagged$dLevelsAminusB == level, ]
    
    # Compute the percent cross gradient for the current level
    site_percent_cross <- percent_cross_grad(level_data$cross_grad_flag)
    
    # Create a new dataframe for the current site and level
    current_result <- data.frame(Site = site, Percent = site_percent_cross, dLevelsAminusB = level)
    
    # Stack the results
    cross_grad_perc <- rbind(cross_grad_perc, current_result)
  }
  cross_grad_perc <- cross_grad_perc %>%
    filter(Percent != 0)
}


####Final Data in files Cross_grad_perc and comparison_result









