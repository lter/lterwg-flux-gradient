
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


###Broken because the Rdata files inside of the folders are all named wacky and differently
#GUAN, TOOL
site_list <- c('KONZ','BONA','CPER','HARV','JORN','NIWO', 'GUAN')

# Authenticate with Google Drive
googledrive::drive_auth(email = email) # Likely will not work on RStudio Server. If you get an error, try email=TRUE to open an interactive auth session.
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3")
data_folder <- googledrive::drive_ls(path = drive_url)


#Initialize an empty dataframe
K_comp <- data.frame() 
cross_grad_perc <- data.frame() 

for (site in site_list){

  site_folder <- googledrive::drive_ls(path = data_folder$id[data_folder$name==site])
  
  # Download data
  dirTmp <- fs::path(tempdir(),site)
  dir.create(dirTmp)
  
  fileDnld <- paste0(site,c("_AE_9min.zip")) #insert zip file to pull here
  
  message(paste0('Downloading AE data for ',site))
  for(focal_file in fileDnld){
    
    # Find the file identifier for that file
    file_id <- subset(site_folder, name == focal_file)
    
    # Download that file
    pathDnld <- fs::path(dirTmp,focal_file)
    googledrive::drive_download(file = file_id$id, 
                                path = pathDnld,
                                overwrite = T)
    # Unzip
    if(grepl(pattern='.zip',focal_file)){
      utils::unzip(pathDnld,exdir=dirTmp)
    }
    # Load the data 
    fileIn <- sub('.zip',".Rdata",pathDnld)
    load(fileIn)
  }
  
  ####DATA LOADED, ANALYSIS START####
  
  result <- eddy_diff_real(min9.FG.AE.list) #calculates eddy diffusivity 
  
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






#######Plotting#####################################################


# Calculate average low_95 and high_95 for each site
avg_values <- combined_df %>%
  group_by(Site) %>%
  summarize(
    avg_low_95 = mean(low_95, na.rm = TRUE),
    avg_high_95 = mean(high_95, na.rm = TRUE),
    #avg_perc_diff = mean(perc_diff, na.rm = TRUE)
  )

# Print the summarized data frame with averages
print(avg_values)

# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Reshape the data to long format
avg_values_long <- avg_values %>%
  pivot_longer(cols = starts_with("avg_"), names_to = "type", values_to = "value")

# Print the reshaped data frame
print(avg_values_long)

# Grouped
ggplot(avg_values_long, aes(fill=type, y=value, x=Site)) + 
  geom_bar(position="dodge", stat="identity")+
labs(title = "CO2 & H2O Eddy Diffusivity Differences by Site ",
     x = "Site",
     y = "KH2O - KCO2 [m2/s]",
     fill = "Type") +
  theme_minimal()



combined_df$perc_diff <- 100*abs(combined_df$Mean_KH2O-combined_df$Mean_KCO2)/(combined_df$Mean_KH2O+combined_df$Mean_KCO2/2)

# Calculate average low_95 and high_95 for each site
avg_values <- combined_df %>%
  group_by(Site) %>%
  summarize(
    avg_perc_diff = mean(perc_diff,na.rm=TRUE)
    #avg_perc_diff = mean(perc_diff, na.rm = TRUE)
  )


# Print the summarized data frame with averages
print(avg_values)
# Reshape the data to long format
avg_values_long <- avg_values %>%
  pivot_longer(cols = starts_with("avg_"), names_to = "type", values_to = "value")


ggplot(avg_values_long, aes(fill=type, y=value, x=Site)) + 
  geom_bar(position="dodge", stat="identity")+
  labs(title = "CO2 & H2O Eddy Diffusivity Percent Differences by Site ",
       x = "Site",
       y = "KH2O - KCO2 [%]",
       fill = "Type") +
  theme_minimal()

#####Correlations 

# Calculate average low_95 and high_95 for each site
avg_values <- combined_df %>%
  group_by(Site) %>%
  summarize(
    avg_corr = mean(cor_test,na.rm=TRUE)
    #avg_perc_diff = mean(perc_diff, na.rm = TRUE)
  )


# Print the summarized data frame with averages
print(avg_values)
# Reshape the data to long format
avg_values_long <- avg_values %>%
  pivot_longer(cols = starts_with("avg_"), names_to = "type", values_to = "value")


ggplot(avg_values_long, aes(fill=type, y=value, x=Site)) + 
  geom_bar(position="dodge", stat="identity")+
  labs(title = "Correlation of H2O and CO2 diffusivities by Site ",
       x = "Site",
       y = "Corr.t-test",
       fill = "Type") +
  theme_minimal()


####How to compare eddy diffusivities for different methods at different heights
####
# Create a sample structure for KONZ

# Function to summarize eddy diffusivities for a given gas
summarize_eddy_diffusivities <- function(df) {
  df %>%
    pivot_longer(cols = starts_with("EddyDiff"), names_to = "method", values_to = "EddyDiff") %>%
    group_by(dLevelsAminusB, method) %>%
    summarize(
      mean_EddyDiff = mean(EddyDiff, na.rm = TRUE),
      median_EddyDiff = median(EddyDiff, na.rm = TRUE),
      sd_EddyDiff = sd(EddyDiff, na.rm = TRUE),
      n = n()
    ) %>%
    arrange(dLevelsAminusB, method)
}

# Summarize for H2O
summary_HARV <- summarize_eddy_diffusivities(HARV$H2O)
print("Summary for H2O:")













####
plot(summary_HARV$mean_EddyDiff, pch = 1, ylab= "EddyDiff [m2/s]")
points(summary_NIWO$mean_EddyDiff, pch = 2)
points(summary_KONZ$mean_EddyDiff, pch = 3)
points(summary_BONA$mean_EddyDiff, pch = 4)
points(summary_CPER$mean_EddyDiff, pch = 5)




#Compare FG eddy diffs to EC eddy diffs

plot(KONZ$H2O$EddyDiff,KONZ$H2O$KH2O, xlim = c(0,4), ylim = c(-6,6), xlab = "WP K",
     ylab = "EC K")
title("KONZ EC vs WP.FG Eddy Diffusivity H2O")
abline(a=0,b=1,col="red")

hist(KONZ$H2O$EddyDiff,breaks = 40, xlab = "FG K", main ="Histogram KONZ H2O Eddy Diffusivity" )


plot(KONZ$CO2$EddyDiff,KONZ$CO2$KCO2, xlim = c(0,4), ylim = c(-6,6), xlab = "WP K",
     ylab = "EC K")
title("KONZ EC vs WP.FG Eddy Diffusivity CO2")
abline(a=0,b=1,col="red")

hist(KONZ$CO2$EddyDiff,breaks = 40, xlab = "FG K", main = "Histogram KONZ CO2 Eddy Diffusivity" )



###Back Calculated Eddy Diff

df_H2O_filtered <- KONZ$H2O %>% filter(KONZ$H2O$KH2O < 4)
df_H2O_filtered <-df_H2O_filtered%>% filter(df_H2O_filtered$KH2O > -2)

hist(df_H2O_filtered$KH2O,breaks=45, xlab = "EC K", 
     main = "Histogram KONZ H2O Eddy Diffusivity EC", ylim = c(0,12000))

df_CO2_filtered <-KONZ$CO2 %>% filter(KONZ$CO2$KCO2 < 4)
df_CO2_filtered <-df_CO2_filtered%>% filter(df_CO2_filtered$KCO2 > -2)

hist(df_CO2_filtered$KCO2,breaks=45, xlab = "EC K", 
     main ="Histogram KONZ CO2 Eddy Diffusivity EC",ylim = c(0,12000))



t.test(df_CO2_filtered$KCO2,df_H2O_filtered$KH2O)
#Compare EC back calculated H2O and CO2 eddy diffs


df_EC_k <- merge(df_H2O_filtered[, c('match_time', 'KH2O')], df_CO2_filtered[, c('match_time', 'KCO2')], by = 'match_time')

plot(df_EC_k$KCO2,df_EC_k$KH2O)
abline(a = 0, b = 1, col = "red")
cor.test(df_EC_k$KCO2,df_EC_k$KH2O)


#Difference in EC and FG eddy diffs as a function of concentration difference

df_CO2_filtered$kdiff <- 100*(abs(df_CO2_filtered$KCO2 - df_CO2_filtered$EddyDiff))/((df_CO2_filtered$EddyDiff+df_CO2_filtered$KCO2)/2)

df_H2O_filtered$kdiff <- 100*(abs(df_H2O_filtered$KH2O - df_H2O_filtered$EddyDiff))/((df_H2O_filtered$EddyDiff+df_H2O_filtered$KH2O)/2)

plot(df_CO2_filtered$dConc,df_CO2_filtered$kdiff,xlim = c(-100,50), ylim = c(0,10000),
     ylab = "Percent Difference", xlab ="Concentration Difference")
title("KONZ Conc. Diff and Percent Difference CO2")

plot(df_H2O_filtered$dConc,df_H2O_filtered$kdiff,xlim = c(-10,10),ylim = c(0,10000),
     ylab = "Percent Difference", xlab = "Concentration Difference")
title("KONZ Conc. Diff and Percent Difference H2O")



  
    
        