# Pull stacked data from GoogleDrive, spot-check data

email <- 'kyle.delwiche@gmail.com'
site <- 'GUAN'

# ------ Prerequisites! Make sure these packages are installed ----

library(foreach)
library(doParallel)
library(stringr)
library(plotly)
library(ggplot2)
library(gridExtra)

# -------------------------------------------------------
sites <- c('BONA','CPER')
for (site in sites){
  print(site)

  # Authenticate with Google Drive and get site data
  googledrive::drive_auth(email = email) # Likely will not work on RStudio Server. If you get an error, try email=TRUE to open an interactive auth session.
  drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3")
  data_folder <- googledrive::drive_ls(path = drive_url)
  site_folder <- googledrive::drive_ls(path = data_folder$id[data_folder$name==site])
  dirTmp <- fs::path(tempdir(),site)
  dir.create(dirTmp)
  files <- site_folder$name
  
  
  file_idx <- match(1, str_detect(files, "aligned"))  #get the file name containing "aligned"
  focal_file <- files[file_idx]
  pathDnld <- fs::path(dirTmp,focal_file)
  file_id <- subset(site_folder, name == focal_file)
  googledrive::drive_download(file = file_id$id,
                                path = pathDnld,
                                overwrite = T)
  
  if(grepl(pattern='.zip',focal_file)){
    utils::unzip(pathDnld,exdir=dirTmp)
  }
  
  #load data
  fileIn <- fs::path(dirTmp,paste0(site,'_aligned_conc_flux_9min.Rdata'))
  load(fileIn)
  
  #separate into dataframes
  ch4 <- data.frame(min9Diff.list$CH4)
  co2 <- data.frame(min9Diff.list$CO2)
  h2o <- data.frame(min9Diff.list$H2O)
  
  # create histogram of dConc values for plotting in log-y format
  temp <- hist(ch4$dConc, breaks = 400)
  hist_df <- data.frame(temp$mids, temp$counts)
  hist_df$log_counts = log10(temp$counts)
  hist_df[sapply(hist_df, is.infinite)] <- 0
  
  # Plot times series of dConc values
  p1 <- ggplot(ch4, aes(x = timeMid, y = dConc)) +
    geom_point() +
    theme_minimal()+
    labs(x = '', y = 'ch4_dConc')
  
  
  # plot log-y histogram of dConc values
  p2 <- ggplot(hist_df, aes(x = temp.mids, y = log_counts)) +
    geom_line() +
    theme_minimal() +
    labs(x = 'ch4_dConc', y = 'Log(Counts)')+
    xlim(-0.3, 0.3)
  
  #combine plots into one figure
  grid.arrange(p1, p2, top = textGrob(site,gp=gpar(fontsize=20,font=3)))
  g <- arrangeGrob(p1, p2, nrow=3) #generates g which can be saved with ggsave
  
  # Save the subplot as an image
  ggsave(paste("Figures/",site,"_ch4_dConc.png", sep = ""), g, width = 10, height = 5, dpi = 300)
}