
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

###TO MODIFY CHANGE SITE AND fileDnld zip files###

# Pull data from google drive
email <- 'saj82@cornell.edu'
#email <- 'jaclyn_matthes@g.harvard.edu'
#email <- 'kyle.delwiche@gmail.com'
site <- 'NIWO' #'KONZ' BONA CPER GUAN HARV JORN NIWO TOOL
#PairLvl <- '4_3' # Not needed

# Authenticate with Google Drive
googledrive::drive_auth(email = email) # Likely will not work on RStudio Server. If you get an error, try email=TRUE to open an interactive auth session.
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3")
data_folder <- googledrive::drive_ls(path = drive_url)
site_folder <- googledrive::drive_ls(path = data_folder$id[data_folder$name==site])

# Download data
dirTmp <- fs::path(tempdir(),site)
dir.create(dirTmp)

fileDnld <- paste0(site,c("_AE_9min_crf_2024-10-16.zip"))

message(paste0('Downloading MBR bootstrapped data for ',site))
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






#Real Eddy Diff. converter - Sam J.

"This code is intended to take the EC fluxes and concetration observations from NEON
towers and convert them into an eddy diffusivity for comparison against the eddy
diffusivities calculated from flux gradient methods."

###EC Eddy Diffusivity###
"input site file is a list of 3 data frames, CO2,H2O,and CH4. Back calculates 
CO2 and H2O"


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


#Generate Back Calculated Eddy Diffusivity

NIWO <- eddy_diff_real(min9.FG.AE.list)



##########COMPARISON FUNCTION###########

###Site is the data in the format of a list of H2O and CO2 data frames
###Site name is a string such as "KONZ"
###method is the FG method employred "WP" or "AE"
#p-value is of correlation


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




# Assuming df1, df2, df3, df4, df5, and df6 are your data frames
combined_df <- rbind(test, test2, test3, test4, test5, test6)


setwd("/Users/jurado/Documents")
write.csv(combined_df, file = "eddy_diff_comp.csv")


#######Plotting combined_df#######


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



#######T-Test#####



#H2O is larger by around 20%

#####Difference by diurnal/ seasonal  Average####












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
